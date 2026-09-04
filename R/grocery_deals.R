# Defensive package loading helper
ensure_packages <- function(pkgs = c("httr2", "jsonlite", "dplyr", "purrr", "DBI", "RSQLite")) {
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

ensure_packages(c("httr2", "jsonlite", "dplyr", "purrr", "DBI", "RSQLite"))

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# Broad category and expanded search terms default list
DEFAULT_SEARCH_TERMS <- c(
  # Broad categories
  "produce", "meat", "dairy", "bakery", "seafood", "pantry", "frozen", "beverage",
  # Staples
  "chicken", "beef", "pork", "milk", "butter", "cheese", "eggs", "bread", "salmon", "coffee",
  # Broad fruits & vegetables list
  "salad", "tomatoes", "strawberries", "raspberries", "blueberries", "peppers", "carrots",
  "broccoli", "spinach", "cucumber", "onions", "potatoes", "apples", "bananas", "avocado",
  "celery", "mushrooms", "garlic", "zucchini"
)

# 1. Fetch deals for a given query term
fetch_grocery_deals <- function(query_term, postal_code = "K2C 1K1", base_url = "https://wishabi.com", mock_response = NULL) {
  if (!is.null(mock_response)) {
    if (is.function(mock_response)) return(mock_response(query_term, postal_code))
    return(mock_response)
  }

  tryCatch({
    req <- httr2::request(base_url) |>
      httr2::req_url_query(
        q = query_term,
        postal_code = postal_code
      ) |>
      httr2::req_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
      ) |>
      httr2::req_timeout(10)

    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) == 200) {
      data <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      if (is.list(data) && !is.null(data$items)) {
        return(data$items)
      } else if (is.list(data) && is.null(names(data))) {
        return(data)
      }
    }
    return(NULL)
  }, error = function(e) {
    warning("HTTP request failed for query '", query_term, "': ", e$message)
    return(NULL)
  })
}

# 2. Extract & filter deals for target grocers
extract_grocery_deals <- function(postal_code = "K2C 1K1",
                                  target_stores = c("Metro", "Farm Boy", "Loblaws"),
                                  search_terms = DEFAULT_SEARCH_TERMS,
                                  base_url = "https://wishabi.com",
                                  mock_data = NULL) {
  if (!is.null(mock_data)) {
    raw_items <- mock_data
  } else {
    raw_items <- purrr::map(search_terms, ~fetch_grocery_deals(.x, postal_code = postal_code, base_url = base_url)) |>
      purrr::flatten()
  }

  if (length(raw_items) == 0) {
    return(dplyr::tibble(
      merchant = character(),
      name = character(),
      current_price = character(),
      pre_price = character(),
      valid_to = character(),
      category = character(),
      postal_code = character()
    ))
  }

  parsed_deals <- purrr::map_dfr(raw_items, function(item) {
    if (!is.list(item)) return(NULL)
    merchant_val <- item$merchant_name %||% item$merchant %||% NA_character_
    name_val <- item$name %||% item$item_name %||% NA_character_
    price_val <- item$current_price %||% item$price %||% NA_character_
    pre_price_val <- item$pre_price_text %||% item$original_price %||% NA_character_
    valid_to_val <- item$valid_to %||% item$valid_until %||% NA_character_
    category_val <- item$category_name %||% item$category %||% NA_character_

    dplyr::tibble(
      merchant = as.character(merchant_val),
      name = as.character(name_val),
      current_price = as.character(price_val),
      pre_price = as.character(pre_price_val),
      valid_to = as.character(valid_to_val),
      category = as.character(category_val)
    )
  })

  if (nrow(parsed_deals) == 0) {
    return(dplyr::mutate(parsed_deals, postal_code = character()))
  }

  filtered_deals <- parsed_deals |>
    dplyr::filter(!is.na(merchant) & !is.na(name)) |>
    dplyr::filter(sapply(merchant, function(m) {
      any(sapply(target_stores, function(ts) grepl(ts, m, ignore.case = TRUE)))
    })) |>
    dplyr::distinct(merchant, name, current_price, .keep_all = TRUE) |>
    dplyr::mutate(postal_code = postal_code)

  return(filtered_deals)
}

# 3. Match deals to canonical ingredient names from SQLite database
match_deals_to_ingredients <- function(deals, db_path = NULL, canonical_list = NULL) {
  if (nrow(deals) == 0) {
    deals$matched_canonical_ingredient <- character(0)
    return(deals)
  }

  known_canonicals <- character(0)
  if (!is.null(canonical_list)) {
    known_canonicals <- canonical_list
  } else if (!is.null(db_path) && file.exists(db_path)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)
    if (DBI::dbExistsTable(conn, "ingredients")) {
      res <- DBI::dbGetQuery(conn, "SELECT DISTINCT canonical_name FROM ingredients WHERE canonical_name IS NOT NULL AND canonical_name != ''")
      known_canonicals <- res$canonical_name
    }
  }

  matched <- sapply(deals$name, function(deal_name) {
    if (length(known_canonicals) == 0 || is.na(deal_name)) return(NA_character_)
    matches <- known_canonicals[sapply(known_canonicals, function(ing) {
      grepl(ing, deal_name, ignore.case = TRUE)
    })]
    if (length(matches) > 0) {
      matches <- matches[order(nchar(matches), decreasing = TRUE)]
      return(matches[1])
    }
    return(NA_character_)
  })

  deals$matched_canonical_ingredient <- as.character(matched)
  return(deals)
}

# 4. Main pipeline runner function
run_grocery_deals_pipeline <- function(db_path = "recipes.db",
                                        postal_code = "K2C 1K1",
                                        target_stores = c("Metro", "Farm Boy", "Loblaws"),
                                        search_terms = DEFAULT_SEARCH_TERMS,
                                        mock_data = NULL) {
  if (!is.null(db_path)) {
    if (exists("init_recipe_db", mode = "function")) {
      init_recipe_db(db_path)
    } else if (file.exists("R/recipe_db.R")) {
      source("R/recipe_db.R")
      init_recipe_db(db_path)
    }
  }

  message("Extracting Ottawa grocery deals for postal code: ", postal_code)
  deals <- extract_grocery_deals(
    postal_code = postal_code,
    target_stores = target_stores,
    search_terms = search_terms,
    mock_data = mock_data
  )

  deals <- match_deals_to_ingredients(deals, db_path = db_path)

  if (!is.null(db_path) && nrow(deals) > 0) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    DBI::dbWriteTable(conn, "grocery_deals", deals, append = TRUE)
    message("Saved ", nrow(deals), " grocery deals to database: ", db_path)
  }

  return(deals)
}
