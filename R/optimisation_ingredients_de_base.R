# Optimisation des ingrédients de base (Pantry Items)
# Ce script extrait les ingrédients de catégorie "pantry" depuis inst/dictionaries/bilingual_ingredients.csv
# et identifie les aubaines correspondantes enregistrées dans la base de données SQLite (recipes.db).
# Les résultats sont ensuite exportés dans un fichier CSV.

ensure_packages <- function(pkgs = c("readr", "dplyr", "DBI", "RSQLite", "purrr")) {
  repos <- "https://cloud.r-project.org"
  missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    message("Missing required packages: ", paste(missing_pkgs, collapse = ", "), ". Attempting installation...")
    tryCatch({
      install.packages(missing_pkgs, repos = repos, dependencies = TRUE)
    }, error = function(e) {
      warning("Failed to install packages automatically: ", e$message)
    })

    still_missing <- missing_pkgs[!sapply(missing_pkgs, requireNamespace, quietly = TRUE)]
    if (length(still_missing) > 0) {
      stop("The following required R package(s) could not be loaded or installed: ",
           paste(still_missing, collapse = ", "),
           ". Please install them manually before proceeding.")
    }
  }
  for (pkg in pkgs) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

ensure_packages(c("readr", "dplyr", "DBI", "RSQLite", "purrr"))

#' Locate a dictionary file searching candidate paths
find_dict_path <- function(dict_path = "inst/dictionaries/bilingual_ingredients.csv") {
  for (candidate in c(dict_path, file.path("..", dict_path), file.path("../..", dict_path))) {
    if (!is.null(candidate) && file.exists(candidate)) {
      return(candidate)
    }
  }
  return(dict_path)
}

#' Load pantry ingredients from bilingual dictionary CSV
get_pantry_ingredients <- function(dict_path = "inst/dictionaries/bilingual_ingredients.csv") {
  resolved_path <- find_dict_path(dict_path)
  if (!file.exists(resolved_path)) {
    stop("Dictionary file not found: ", dict_path)
  }

  dict_df <- readr::read_csv(resolved_path, show_col_types = FALSE)
  if (!"category" %in% names(dict_df)) {
    stop("Dictionary CSV must contain a 'category' column.")
  }

  pantry_df <- dict_df %>%
    dplyr::filter(trimws(tolower(.data$category)) == "pantry")

  return(pantry_df)
}

#' Extract pantry grocery deals from database based on pantry dictionary items
#'
#' @param db_path Path to SQLite database
#' @param dict_path Path to bilingual ingredients dictionary CSV
#' @param output_csv Optional output CSV filepath. If provided, writes results to CSV.
#' @return A tibble of matched pantry grocery deals.
extract_pantry_deals <- function(db_path = "recipes.db",
                                dict_path = "inst/dictionaries/bilingual_ingredients.csv",
                                output_csv = "pantry_deals.csv") {
  pantry_df <- get_pantry_ingredients(dict_path = dict_path)

  if (nrow(pantry_df) == 0) {
    message("No pantry items found in dictionary.")
    empty_res <- dplyr::tibble(
      merchant = character(),
      name = character(),
      current_price = character(),
      pre_price = character(),
      valid_to = character(),
      category = character(),
      matched_canonical_ingredient = character(),
      postal_code = character(),
      pantry_canonical_name = character()
    )
    if (!is.null(output_csv)) {
      readr::write_csv(empty_res, output_csv)
    }
    return(empty_res)
  }

  pantry_canonicals <- unique(tolower(trimws(pantry_df$canonical_name)))

  # Build list of terms (canonical, french, english) for text matching fallback
  pantry_terms_map <- list()
  for (i in seq_len(nrow(pantry_df))) {
    can <- tolower(trimws(pantry_df$canonical_name[i]))
    fr <- tolower(trimws(pantry_df$french_name[i]))
    en <- tolower(trimws(pantry_df$english_name[i]))

    if (nzchar(can)) pantry_terms_map[[can]] <- can
    if (nzchar(fr)) pantry_terms_map[[fr]] <- can
    if (nzchar(en)) pantry_terms_map[[en]] <- can
  }

  if (!file.exists(db_path)) {
    warning("Database file not found at: ", db_path, ". Returning empty deals tibble.")
    empty_res <- dplyr::tibble(
      merchant = character(),
      name = character(),
      current_price = character(),
      pre_price = character(),
      valid_to = character(),
      category = character(),
      matched_canonical_ingredient = character(),
      postal_code = character(),
      pantry_canonical_name = character()
    )
    if (!is.null(output_csv)) {
      readr::write_csv(empty_res, output_csv)
    }
    return(empty_res)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (!DBI::dbExistsTable(conn, "grocery_deals")) {
    warning("Table 'grocery_deals' does not exist in database: ", db_path)
    empty_res <- dplyr::tibble(
      merchant = character(),
      name = character(),
      current_price = character(),
      pre_price = character(),
      valid_to = character(),
      category = character(),
      matched_canonical_ingredient = character(),
      postal_code = character(),
      pantry_canonical_name = character()
    )
    if (!is.null(output_csv)) {
      readr::write_csv(empty_res, output_csv)
    }
    return(empty_res)
  }

  deals_df <- DBI::dbReadTable(conn, "grocery_deals")

  if (nrow(deals_df) == 0) {
    message("Table 'grocery_deals' is empty.")
    empty_res <- dplyr::tibble(
      merchant = character(),
      name = character(),
      current_price = character(),
      pre_price = character(),
      valid_to = character(),
      category = character(),
      matched_canonical_ingredient = character(),
      postal_code = character(),
      pantry_canonical_name = character()
    )
    if (!is.null(output_csv)) {
      readr::write_csv(empty_res, output_csv)
    }
    return(empty_res)
  }

  # Match deals with pantry items either via matched_canonical_ingredient or string search
  pantry_deals <- deals_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      pantry_canonical_name = {
        matched_can <- if ("matched_canonical_ingredient" %in% names(.data) && !is.na(.data$matched_canonical_ingredient)) {
          tolower(trimws(.data$matched_canonical_ingredient))
        } else {
          NA_character_
        }

        if (!is.na(matched_can) && matched_can %in% pantry_canonicals) {
          matched_can
        } else {
          # Fallback check against deal name using pantry dictionary terms
          deal_name <- tolower(.data$name %||% "")
          found_match <- NA_character_
          if (!is.na(deal_name) && nzchar(deal_name)) {
            terms <- names(pantry_terms_map)
            # Find matching terms, sort by length descending to pick best/longest match
            matching_terms <- terms[sapply(terms, function(t) {
              escaped_t <- gsub("([\\|\\(\\)\\[\\]\\{\\}\\^\\$\\*\\+\\?\\.\\\\])", "\\\\\\1", t)
              grepl(paste0("\\b", escaped_t, "\\b"), deal_name, ignore.case = TRUE)
            })]
            matching_terms <- matching_terms[!is.na(matching_terms)]
            if (length(matching_terms) > 0) {
              matching_terms <- matching_terms[order(nchar(matching_terms), decreasing = TRUE)]
              found_match <- pantry_terms_map[[matching_terms[1]]] %||% NA_character_
            }
          }
          found_match
        }
      }
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(.data$pantry_canonical_name))

  if (!is.null(output_csv)) {
    readr::write_csv(pantry_deals, output_csv)
    message("Pantry deals exported successfully to: ", output_csv, " (", nrow(pantry_deals), " items found)")
  }

  return(pantry_deals)
}

# CLI execution support
if (!interactive() && identical(environment(), globalenv())) {
  args <- commandArgs(trailingOnly = TRUE)
  db_path <- if (length(args) >= 1) args[1] else "recipes.db"
  dict_path <- if (length(args) >= 2) args[2] else "inst/dictionaries/bilingual_ingredients.csv"
  output_csv <- if (length(args) >= 3) args[3] else "pantry_deals.csv"

  extract_pantry_deals(db_path = db_path, dict_path = dict_path, output_csv = output_csv)
}
