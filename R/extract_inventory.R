# Inventory Ingestion & Normalization Pipeline

# Defensive package loading helper
ensure_packages <- function(pkgs = c("readxl", "googlesheets4", "readr", "stringr", "dplyr", "purrr", "DBI", "RSQLite", "cli")) {
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

ensure_packages(c("readxl", "googlesheets4", "readr", "stringr", "dplyr", "purrr", "DBI", "RSQLite", "cli"))

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Standard pantry staples reference list
DEFAULT_PANTRY_STAPLES <- c(
  "oil", "olive oil", "vegetable oil", "canola oil", "sesame oil", "cooking spray",
  "salt", "sea salt", "kosher salt", "black pepper", "pepper", "white pepper",
  "sugar", "brown sugar", "white sugar", "powdered sugar", "honey", "maple syrup",
  "flour", "all-purpose flour", "baking powder", "baking soda", "cornstarch", "yeast",
  "soy sauce", "vinegar", "white vinegar", "apple cider vinegar", "balsamic vinegar",
  "garlic powder", "onion powder", "paprika", "cinnamon", "oregano", "basil", "thyme",
  "rosemary", "cumin", "chili powder", "cayenne", "mustard", "dijon mustard", "ketchup",
  "mayonnaise", "hot sauce", "worcestershire sauce", "vanilla extract", "rice", "pasta"
)

# 1. Multi-Source Ingestion Functions

#' Ingest inventory from Excel (.xlsx, .xls)
#'
#' @param source_path Path to the Excel file
#' @param sheet Sheet name or index (default NULL picks first sheet)
#' @return Standardized tibble (raw_ingredient, location, expiry_days_left, expiry_date)
extract_from_excel <- function(source_path, sheet = NULL) {
  if (!file.exists(source_path)) {
    stop("Excel file does not exist at path: ", source_path)
  }

  sheets <- readxl::excel_sheets(source_path)
  target_sheet <- if (is.null(sheet)) sheets[1] else sheet
  raw_df <- readxl::read_excel(source_path, sheet = target_sheet)

  if (nrow(raw_df) == 0) {
    return(dplyr::tibble(
      raw_ingredient = character(0),
      location = character(0),
      expiry_days_left = integer(0),
      expiry_date = character(0)
    ))
  }

  col_names <- colnames(raw_df)

  # Flexible column matching
  ing_col <- col_names[grep("item|ingredient|produit|nom|name|food", col_names, ignore.case = TRUE)][1]
  if (is.na(ing_col)) ing_col <- col_names[1] # fallback to first column

  loc_col <- col_names[grep("loc|place|endroit|emplacement|fridge|freezer|pantry|rangement", col_names, ignore.case = TRUE)][1]

  exp_days_col <- col_names[grep("days|jours|exp_days|expiry_days|restant", col_names, ignore.case = TRUE)][1]
  exp_date_col <- col_names[grep("date|exp_date|expiry_date|expiration", col_names, ignore.case = TRUE)][1]

  # If both matching days or date fell back to same column, split logic
  if (!is.na(exp_days_col) && !is.na(exp_date_col) && exp_days_col == exp_date_col) {
    if (grepl("date", exp_date_col, ignore.case = TRUE)) exp_days_col <- NA
    else exp_date_col <- NA
  }

  res <- dplyr::tibble(
    raw_ingredient = as.character(raw_df[[ing_col]]),
    location = if (!is.na(loc_col)) as.character(raw_df[[loc_col]]) else NA_character_,
    expiry_days_left = if (!is.na(exp_days_col)) suppressWarnings(as.integer(raw_df[[exp_days_col]])) else NA_integer_,
    expiry_date = if (!is.na(exp_date_col)) as.character(raw_df[[exp_date_col]]) else NA_character_
  ) |>
    dplyr::filter(!is.na(raw_ingredient) & nchar(trimws(raw_ingredient)) > 0)

  return(res)
}

#' Ingest inventory from Google Sheets
#'
#' @param ss_id_or_url Google Sheet ID or URL
#' @param sheet Sheet name or index (default 1)
#' @return Standardized tibble (raw_ingredient, location, expiry_days_left, expiry_date)
extract_from_gsheets <- function(ss_id_or_url, sheet = 1) {
  # Non-interactive authentication setup
  googlesheets4::gs4_deauth()

  raw_df <- tryCatch({
    googlesheets4::read_sheet(ss_id_or_url, sheet = sheet)
  }, error = function(e) {
    stop("Failed to read Google Sheet: ", e$message)
  })

  if (nrow(raw_df) == 0) {
    return(dplyr::tibble(
      raw_ingredient = character(0),
      location = character(0),
      expiry_days_left = integer(0),
      expiry_date = character(0)
    ))
  }

  col_names <- colnames(raw_df)

  ing_col <- col_names[grep("item|ingredient|produit|nom|name|food", col_names, ignore.case = TRUE)][1]
  if (is.na(ing_col)) ing_col <- col_names[1]

  loc_col <- col_names[grep("loc|place|endroit|emplacement|fridge|freezer|pantry|rangement", col_names, ignore.case = TRUE)][1]
  exp_days_col <- col_names[grep("days|jours|exp_days|expiry_days|restant", col_names, ignore.case = TRUE)][1]
  exp_date_col <- col_names[grep("date|exp_date|expiry_date|expiration", col_names, ignore.case = TRUE)][1]

  if (!is.na(exp_days_col) && !is.na(exp_date_col) && exp_days_col == exp_date_col) {
    if (grepl("date", exp_date_col, ignore.case = TRUE)) exp_days_col <- NA
    else exp_date_col <- NA
  }

  res <- dplyr::tibble(
    raw_ingredient = as.character(raw_df[[ing_col]]),
    location = if (!is.na(loc_col)) as.character(raw_df[[loc_col]]) else NA_character_,
    expiry_days_left = if (!is.na(exp_days_col)) suppressWarnings(as.integer(raw_df[[exp_days_col]])) else NA_integer_,
    expiry_date = if (!is.na(exp_date_col)) as.character(raw_df[[exp_date_col]]) else NA_character_
  ) |>
    dplyr::filter(!is.na(raw_ingredient) & nchar(trimws(raw_ingredient)) > 0)

  return(res)
}

#' Ingest inventory from free-form Text file (.txt, .md)
#'
#' @param source_path Path to the text file
#' @return Standardized tibble (raw_ingredient, location, expiry_days_left, expiry_date)
extract_from_text <- function(source_path) {
  if (!file.exists(source_path)) {
    stop("Text file does not exist at path: ", source_path)
  }

  lines <- readr::read_lines(source_path)
  lines <- trimws(lines)
  # Exclude empty lines and comment/header lines starting with #
  lines <- lines[lines != "" & !stringr::str_starts(lines, "#")]

  if (length(lines) == 0) {
    return(dplyr::tibble(
      raw_ingredient = character(0),
      location = character(0),
      expiry_days_left = integer(0),
      expiry_date = character(0)
    ))
  }

  parsed_list <- purrr::map(lines, function(line) {
    # Strip leading bullet indicators (e.g. - , * , 1. )
    clean_line <- stringr::str_replace(line, "^[-*+\\d\\.]+\\s*", "")

    # Check if line contains location or expiry annotations delimited by comma, dash, pipe, or parens
    # e.g., "2 chicken breasts - freezer - 5 days" or "milk, fridge, 2025-03-30" or "apples (fridge, 3 days)"
    clean_line <- stringr::str_replace_all(clean_line, "[\\(\\)]", " ")

    parts <- stringr::str_split(clean_line, "\\s*[,\\|\\-–—]\\s*")[[1]]
    parts <- trimws(parts)
    parts <- parts[parts != ""]

    raw_item <- parts[1]
    loc_val <- NA_character_
    days_val <- NA_integer_
    date_val <- NA_character_

    if (length(parts) > 1) {
      for (extra in parts[-1]) {
        # Check location keywords
        if (grepl("fridge|frigo|refrigerat|freezer|congelat|pantry|garde-manger", extra, ignore.case = TRUE)) {
          if (grepl("freezer|congelat", extra, ignore.case = TRUE)) loc_val <- "freezer"
          else if (grepl("pantry|garde-manger", extra, ignore.case = TRUE)) loc_val <- "pantry"
          else loc_val <- "fridge"
        }
        # Check expiry date pattern YYYY-MM-DD
        if (grepl("\\d{4}-\\d{2}-\\d{2}", extra)) {
          date_val <- stringr::str_extract(extra, "\\d{4}-\\d{2}-\\d{2}")
        }
        # Check expiry days pattern (e.g. "5 days", "3j", "7")
        if (grepl("\\b\\d+\\s*(days?|j|jours?)\\b", extra, ignore.case = TRUE)) {
          num <- stringr::str_extract(extra, "\\d+")
          days_val <- as.integer(num)
        } else if (is.na(days_val) && grepl("^\\d+$", extra)) {
          days_val <- as.integer(extra)
        }
      }
    }

    dplyr::tibble(
      raw_ingredient = raw_item,
      location = loc_val,
      expiry_days_left = days_val,
      expiry_date = date_val
    )
  })

  dplyr::bind_rows(parsed_list) |>
    dplyr::filter(!is.na(raw_ingredient) & nchar(trimws(raw_ingredient)) > 0)
}

# 2. Ingredient Normalization

#' Clean raw ingredient text
#'
#' @param text Vector of raw ingredient strings
#' @return Vector of cleaned strings
clean_ingredient_text <- function(text) {
  if (length(text) == 0) return(character(0))

  cleaned <- tolower(text)

  # Remove content in parentheses
  cleaned <- stringr::str_replace_all(cleaned, "\\(.*?\\)", " ")

  # Remove numbers / fractions / quantities (e.g., 500g, 1/2, 2.5)
  cleaned <- stringr::str_replace_all(cleaned, "\\b\\d+([\\.,/]\\d+)?\\b", " ")

  # Remove measurement units (g, kg, cups, tbsp, tsp, ml, l, oz, lb, lbs, pieces, pkg, package, etc.)
  units <- c("g", "kg", "mg", "ml", "l", "cl", "oz", "lb", "lbs", "cup", "cups",
             "tbsp", "tsp", "tablespoon", "tablespoons", "teaspoon", "teaspoons", "pinch",
             "piece", "pieces", "pkg", "package", "can", "cans", "bunch", "clove", "cloves",
             "slice", "slices", "head", "heads", "bag", "bags", "bottle", "bottles",
             "tasse", "tasses", "c. à soupe", "c. à thé", "gousse", "gousses", "tranche", "tranches")
  unit_pattern <- paste0("\\b(", paste(units, collapse = "|"), ")\\b")
  cleaned <- stringr::str_replace_all(cleaned, unit_pattern, " ")

  # Remove preparation descriptors (chopped, diced, frozen, thawed, leftover, etc.)
  descriptors <- c("chopped", "diced", "frozen", "thawed", "leftover", "sliced", "minced",
                   "cooked", "fresh", "raw", "cubed", "grated", "shredded", "crushed",
                   "peeled", "haché", "coupé", "congelé", "reste", "restes", "cuit", "frais")
  desc_pattern <- paste0("\\b(", paste(descriptors, collapse = "|"), ")\\b")
  cleaned <- stringr::str_replace_all(cleaned, desc_pattern, " ")

  # Remove non-alphanumeric punctuation except spaces
  cleaned <- stringr::str_replace_all(cleaned, "[^a-zA-Zàâçéèêëîïôûùüÿñæœ\\s]", " ")

  # Collapse whitespace and trim
  cleaned <- stringr::str_squish(cleaned)

  # Fallback to original text if cleaned string becomes empty
  ifelse(cleaned == "", tolower(trimws(text)), cleaned)
}

load_bilingual_dictionary <- function(dict_path = "inst/dictionaries/bilingual_ingredients.csv") {
  target_path <- NULL
  for (candidate in c(dict_path, file.path("..", dict_path), file.path("../..", dict_path))) {
    if (!is.null(candidate) && file.exists(candidate)) {
      target_path <- candidate
      break
    }
  }
  if (!is.null(target_path) && file.exists(target_path)) {
    return(tryCatch(readr::read_csv(target_path, show_col_types = FALSE), error = function(e) NULL))
  }
  return(NULL)
}

#' Match cleaned ingredient against SQLite database canonical names and bilingual dictionary
#'
#' @param cleaned_texts Vector of cleaned ingredient strings
#' @param db_path SQLite database path
#' @param dict_path Path to bilingual ingredients dictionary
#' @return Vector of canonical ingredient names
match_canonical_names <- function(cleaned_texts, db_path = NULL, dict_path = "inst/dictionaries/bilingual_ingredients.csv") {
  if (length(cleaned_texts) == 0) return(character(0))

  known_map <- list()
  known_canonicals <- character(0)

  bilingual_df <- load_bilingual_dictionary(dict_path)
  if (!is.null(bilingual_df) && nrow(bilingual_df) > 0) {
    for (r in seq_len(nrow(bilingual_df))) {
      can <- tolower(trimws(bilingual_df$canonical_name[r]))
      fr <- tolower(trimws(bilingual_df$french_name[r]))
      en <- tolower(trimws(bilingual_df$english_name[r]))
      if (nzchar(can)) {
        known_canonicals <- c(known_canonicals, can)
        known_map[[can]] <- can
        if (nzchar(fr)) { known_canonicals <- c(known_canonicals, fr); known_map[[fr]] <- can }
        if (nzchar(en)) { known_canonicals <- c(known_canonicals, en); known_map[[en]] <- can }
      }
    }
  }

  if (!is.null(db_path) && file.exists(db_path)) {
    conn <- tryCatch(DBI::dbConnect(RSQLite::SQLite(), db_path), error = function(e) NULL)
    if (!is.null(conn)) {
      on.exit(DBI::dbDisconnect(conn), add = TRUE)
      if (DBI::dbExistsTable(conn, "ingredients")) {
        res <- DBI::dbGetQuery(conn, "SELECT DISTINCT canonical_name FROM ingredients WHERE canonical_name IS NOT NULL AND canonical_name != ''")
        for (cn in res$canonical_name) {
          lcn <- tolower(cn)
          known_canonicals <- c(known_canonicals, lcn)
          if (is.null(known_map[[lcn]])) known_map[[lcn]] <- lcn
        }
      }
    }
  }

  known_canonicals <- unique(known_canonicals)

  sapply(cleaned_texts, function(item) {
    if (is.na(item) || item == "") return(NA_character_)
    item_lower <- tolower(item)

    if (length(known_canonicals) == 0) return(item_lower)

    # 1. Direct key match
    if (!is.null(known_map[[item_lower]])) return(known_map[[item_lower]])

    # 2. Substring / Grep match
    sub_matches <- known_canonicals[sapply(known_canonicals, function(k) {
      grepl(paste0("\\b", k, "\\b"), item_lower, ignore.case = TRUE) || grepl(paste0("\\b", item_lower, "\\b"), k, ignore.case = TRUE)
    })]

    if (length(sub_matches) > 0) {
      sub_matches <- sub_matches[order(nchar(sub_matches), decreasing = TRUE)]
      best <- sub_matches[1]
      val <- known_map[[best]]
      return(if (!is.null(val)) val else best)
    }

    # Default fallback to cleaned item
    return(item_lower)
  }, USE.NAMES = FALSE)
}

#' Normalize inventory data frame
#'
#' @param raw_df Data frame containing raw_ingredient, location, expiry_days_left, expiry_date
#' @param db_path Path to SQLite database for canonical matching
#' @param pantry_staples Vector of pantry staple strings
#' @return Normalized tibble
normalize_inventory <- function(raw_df, db_path = NULL, pantry_staples = DEFAULT_PANTRY_STAPLES) {
  if (nrow(raw_df) == 0) {
    return(dplyr::tibble(
      raw_ingredient = character(0),
      canonical_name = character(0),
      location = character(0),
      expiry_days_left = integer(0),
      expiry_date = character(0),
      is_pantry_staple = integer(0)
    ))
  }

  cleaned <- clean_ingredient_text(raw_df$raw_ingredient)
  canonicals <- match_canonical_names(cleaned, db_path = db_path)

  # Standardize location values (fridge, freezer, pantry)
  locations <- sapply(raw_df$location, function(loc) {
    if (is.na(loc) || loc == "") return(NA_character_)
    l <- tolower(loc)
    if (grepl("freez|congél", l)) "freezer"
    else if (grepl("pant|gard", l)) "pantry"
    else if (grepl("frig|refrig", l)) "fridge"
    else trimws(loc)
  }, USE.NAMES = FALSE)

  # Identify pantry staples
  is_staple <- sapply(canonicals, function(can) {
    if (is.na(can) || can == "") return(0L)
    match_found <- any(sapply(pantry_staples, function(p) {
      grepl(paste0("\\b", p, "\\b"), tolower(can), ignore.case = TRUE)
    }))
    if (match_found) 1L else 0L
  }, USE.NAMES = FALSE)

  dplyr::tibble(
    raw_ingredient = raw_df$raw_ingredient,
    canonical_name = canonicals,
    location = locations,
    expiry_days_left = raw_df$expiry_days_left,
    expiry_date = raw_df$expiry_date,
    is_pantry_staple = as.integer(is_staple)
  )
}

# 3. Database Ingestion & Pipeline Runner

#' Save normalized inventory to SQLite database
#'
#' @param inventory_df Normalized inventory tibble
#' @param db_path SQLite database path
#' @param overwrite Logical, whether to clear existing inventory before insertion
#' @return Number of rows inserted
save_inventory_to_db <- function(inventory_df, db_path = "recipes.db", overwrite = TRUE) {
  if (is.null(db_path) || !file.exists(db_path)) {
    if (exists("init_recipe_db", mode = "function")) {
      init_recipe_db(db_path %||% "recipes.db")
    } else if (file.exists("R/recipe_db.R")) {
      source("R/recipe_db.R")
      init_recipe_db(db_path %||% "recipes.db")
    }
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  if (isTRUE(overwrite)) {
    DBI::dbExecute(conn, "DELETE FROM inventory;")
  }

  if (nrow(inventory_df) > 0) {
    db_payload <- inventory_df |>
      dplyr::select(raw_ingredient, canonical_name, location, expiry_days_left, expiry_date, is_pantry_staple)

    DBI::dbWriteTable(conn, "inventory", db_payload, append = TRUE)
  }

  return(nrow(inventory_df))
}

#' Run full inventory extraction, normalization, and database update pipeline
#'
#' @param source_path_or_url Path to Excel/Text file or Google Sheet ID/URL
#' @param source_type Type of source ("auto", "excel", "gsheets", "text")
#' @param db_path SQLite database path
#' @param overwrite Logical, whether to overwrite existing database inventory
#' @return Standardized & normalized inventory tibble
run_inventory_pipeline <- function(source_path_or_url,
                                   source_type = c("auto", "excel", "gsheets", "text"),
                                   db_path = "recipes.db",
                                   overwrite = TRUE) {
  source_type <- match.arg(source_type)

  if (source_type == "auto") {
    if (grepl("docs.google.com|sheets", source_path_or_url)) {
      source_type <- "gsheets"
    } else if (grepl("\\.xlsx?$|\\.xls$", source_path_or_url, ignore.case = TRUE)) {
      source_type <- "excel"
    } else if (grepl("\\.txt$|\\.md$", source_path_or_url, ignore.case = TRUE)) {
      source_type <- "text"
    } else {
      # Fallback check file existence or error
      if (file.exists(source_path_or_url)) {
        source_type <- "text"
      } else {
        source_type <- "gsheets"
      }
    }
  }

  cli::cli_h1("Inventory Ingestion & Normalization Pipeline")
  cli::cli_inform(c("i" = "Source: {.path {source_path_or_url}} (type: {.val {source_type}})"))

  raw_df <- switch(
    source_type,
    "excel"   = extract_from_excel(source_path_or_url),
    "gsheets" = extract_from_gsheets(source_path_or_url),
    "text"    = extract_from_text(source_path_or_url)
  )

  cli::cli_alert_info("Extracted {nrow(raw_df)} raw inventory item(s).")

  normalized_df <- normalize_inventory(raw_df, db_path = db_path)

  # Stats
  matched_count <- sum(normalized_df$canonical_name != clean_ingredient_text(normalized_df$raw_ingredient), na.rm = TRUE)
  staple_count <- sum(normalized_df$is_pantry_staple == 1, na.rm = TRUE)
  match_rate <- if (nrow(normalized_df) > 0) round((matched_count / nrow(normalized_df)) * 100, 1) else 0

  cli::cli_alert_success("Normalized {nrow(normalized_df)} item(s) (Match rate vs DB recipes: {match_rate}%, Pantry staples: {staple_count}).")

  if (!is.null(db_path)) {
    saved_rows <- save_inventory_to_db(normalized_df, db_path = db_path, overwrite = overwrite)
    cli::cli_alert_success("Updated inventory table in SQLite ({saved_rows} row(s) inserted, overwrite = {overwrite}).")
  }

  return(normalized_df)
}
