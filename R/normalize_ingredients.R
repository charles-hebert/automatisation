# Strict ingredient normalization and unmatched terms exporter.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Normalize an ingredient text term against ingredients_ref and ingredient_synonyms.
#'
#' @param db SQLite connection
#' @param term Raw or canonical ingredient string
#' @return List with ref_ingredient_id and match_method
normalize_term <- function(db, term) {
  clean_term <- trimws(tolower(term %||% ""))
  if (!nzchar(clean_term)) {
    return(list(ref_ingredient_id = 0L, match_method = "unmatched"))
  }

  # 1. Exact match in ingredients_ref
  ref_match <- dbGetQuery(db, "
    SELECT ingredient_id
    FROM ingredients_ref
    WHERE lower(canonical_name_fr) = ? OR lower(canonical_name_en) = ?
  ", params = list(clean_term, clean_term))

  if (nrow(ref_match) > 0 && !is.na(ref_match$ingredient_id[[1]])) {
    return(list(ref_ingredient_id = as.integer(ref_match$ingredient_id[[1]]), match_method = "exact"))
  }

  # 2. Match in ingredient_synonyms
  syn_match <- dbGetQuery(db, "
    SELECT ingredient_id
    FROM ingredient_synonyms
    WHERE lower(synonym_text) = ?
  ", params = list(clean_term))

  if (nrow(syn_match) > 0 && !is.na(syn_match$ingredient_id[[1]])) {
    return(list(ref_ingredient_id = as.integer(syn_match$ingredient_id[[1]]), match_method = "synonym"))
  }

  # 3. Unmatched fallback
  list(ref_ingredient_id = 0L, match_method = "unmatched")
}

#' Run normalization on all unnormalized or pending rows in ingredients table.
#'
#' @param db_path Path to SQLite database
#' @return Number of ingredients normalized
normalize_all_ingredients <- function(db_path = "recipes.db") {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  # Fetch ingredients
  ings <- dbGetQuery(db, "
    SELECT ingredient_id, raw_text, canonical_name
    FROM ingredients
  ")

  if (nrow(ings) == 0) return(0)

  updated_count <- 0
  dbBegin(db)
  tryCatch({
    for (i in seq_len(nrow(ings))) {
      row_id <- ings$ingredient_id[[i]]
      # Try canonical_name first, then raw_text
      term <- if (!is.na(ings$canonical_name[[i]]) && nzchar(trimws(ings$canonical_name[[i]]))) {
        ings$canonical_name[[i]]
      } else {
        ings$raw_text[[i]]
      }

      res <- normalize_term(db, term)

      dbExecute(db, "
        UPDATE ingredients
        SET ref_ingredient_id = ?, match_method = ?
        WHERE ingredient_id = ?
      ", params = list(res$ref_ingredient_id, res$match_method, row_id))

      updated_count <- updated_count + 1
    }
    dbCommit(db)
  }, error = function(e) {
    dbRollback(db)
    stop("Failed to normalize ingredients: ", e$message)
  })

  updated_count
}

#' Export unmatched terms (ref_ingredient_id = 0) sorted by frequency to CSV.
#'
#' @param db_path Path to SQLite database
#' @param output_path File path for CSV export (defaults to root unmatched_ingredients.csv)
#' @return Data frame of unmatched terms
export_unmatched_ingredients <- function(db_path = "recipes.db", output_path = "unmatched_ingredients.csv") {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)

  unmatched <- dbGetQuery(db, "
    SELECT
      COALESCE(NULLIF(canonical_name, ''), raw_text) AS term,
      COUNT(DISTINCT recipe_id) AS recipe_count,
      COUNT(ingredient_id) AS total_occurrences
    FROM ingredients
    WHERE ref_ingredient_id = 0 OR match_method = 'unmatched'
    GROUP BY COALESCE(NULLIF(canonical_name, ''), raw_text)
    ORDER BY recipe_count DESC, total_occurrences DESC
  ")

  if (nrow(unmatched) == 0) {
    unmatched <- data.frame(term = character(0), recipe_count = integer(0), total_occurrences = integer(0))
  }

  utils::write.csv(unmatched, output_path, row.names = FALSE)
  message(sprintf("Exported %d unmatched terms to %s", nrow(unmatched), output_path))
  invisible(unmatched)
}
