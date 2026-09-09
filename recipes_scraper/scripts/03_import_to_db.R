#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(jsonlite)
  library(digest)
  library(DBI)
  library(RSQLite)
})

# Ensure database initialization function is available
if (!exists("init_recipe_db", mode = "function")) {
  db_script_path <- "R/recipe_db.R"
  if (file.exists(db_script_path)) {
    source(db_script_path)
  } else if (file.exists("../R/recipe_db.R")) {
    source("../R/recipe_db.R")
  }
}

#' Import scraped recipes CSV into SQLite raw_sources table
#'
#' @param csv_path Path to the CSV file produced by 02_scrape_selected_recipes.R
#' @param db_path Path to the SQLite database
#' @param default_book_id Optional book_id to associate with the imported raw_sources
#' @return Number of imported/updated rows (invisibly)
import_scraped_recipes <- function(csv_path, db_path = "recipes.db", default_book_id = NULL) {
  if (!file.exists(csv_path)) {
    stop(sprintf("Input CSV file does not exist: %s", csv_path))
  }

  if (exists("init_recipe_db", mode = "function")) {
    init_recipe_db(db_path)
  }

  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  recipes_df <- read_csv(csv_path, show_col_types = FALSE)
  required_cols <- c("recipe_url", "title", "ingredients", "instructions")
  missing_cols <- setdiff(required_cols, names(recipes_df))
  if (length(missing_cols) > 0) {
    stop(sprintf("Input CSV is missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  if (nrow(recipes_df) == 0) {
    message("CSV file contains no recipes to import.")
    return(invisible(0))
  }

  imported_count <- 0
  updated_count <- 0

  for (i in seq_len(nrow(recipes_df))) {
    row <- recipes_df[i, ]
    url_val <- as.character(row$recipe_url)
    if (is.na(url_val) || !nzchar(trimws(url_val))) next

    url_hash <- digest(trimws(url_val), algo = "md5", serialize = FALSE)
    title_val <- if (is.na(row$title)) "" else as.character(row$title)
    ing_val <- if (is.na(row$ingredients)) "" else as.character(row$ingredients)
    inst_val <- if (is.na(row$instructions)) "" else as.character(row$instructions)

    content_data <- list(
      recipe_url = url_val,
      title = title_val,
      ingredients = ing_val,
      instructions = inst_val
    )
    raw_content_json <- toJSON(content_data, auto_unbox = TRUE, pretty = FALSE)

    existing <- dbGetQuery(db, "SELECT source_id FROM raw_sources WHERE file_hash = ?", params = list(url_hash))

    b_id_param <- if (is.null(default_book_id)) NA_integer_ else default_book_id

    if (nrow(existing) > 0) {
      src_id <- existing$source_id[[1]]
      dbExecute(db, "
        UPDATE raw_sources
        SET book_id = ?, file_name = ?, raw_content = ?, status = 'pending', error_message = NULL
        WHERE source_id = ?
      ", params = list(b_id_param, url_val, raw_content_json, src_id))
      updated_count <- updated_count + 1
    } else {
      dbExecute(db, "
        INSERT INTO raw_sources (book_id, file_hash, file_name, file_type, raw_content, status)
        VALUES (?, ?, ?, 'url', ?, 'pending')
      ", params = list(b_id_param, url_hash, url_val, raw_content_json))
      imported_count <- imported_count + 1
    }
  }

  message(sprintf("Done. Imported %d new recipe source(s), updated %d existing recipe source(s) in %s.",
                  imported_count, updated_count, db_path))
  invisible(imported_count + updated_count)
}

# CLI execution support
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1) {
    cat("Usage: Rscript scripts/03_import_to_db.R <input_csv> [db_path]\n")
    quit(save = "no", status = 1)
  }

  csv_arg <- args[[1]]
  db_arg <- if (length(args) >= 2) args[[2]] else "recipes.db"

  import_scraped_recipes(csv_arg, db_arg)
}
