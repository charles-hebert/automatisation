# Data Model Loading & Validation for Weekly Meal-Plan Optimizer

ensure_packages <- function(pkgs = c("DBI", "RSQLite", "dplyr", "purrr", "yaml", "tibble", "stringr")) {
  repos <- "https://cloud.r-project.org"
  missing_pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    tryCatch({
      install.packages(missing_pkgs, repos = repos, dependencies = TRUE)
    }, error = function(e) {
      warning("Package installation failed: ", e$message)
    })
  }
  for (pkg in pkgs) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
}

ensure_packages(c("DBI", "RSQLite", "dplyr", "purrr", "yaml", "tibble", "stringr"))

load_forbidden_rules <- function(config_path = "config/forbidden_rules.yml") {
  defaults <- list(
    forbidden_always_ingredients = c("orzo", "spaghettini", "cheveux d'ange", "soba"),
    forbidden_always_tags = c("forbidden"),
    forbidden_with_f_ingredients = c("cucumber", "concombre", "jambalaya", "risotto"),
    forbidden_with_f_tags = c("forbidden_f")
  )
  target_path <- NULL
  for (candidate in c(config_path, file.path("..", config_path), file.path("../..", config_path))) {
    if (!is.null(candidate) && file.exists(candidate)) {
      target_path <- candidate
      break
    }
  }

  if (!is.null(target_path)) {
    cfg <- tryCatch(yaml::read_yaml(target_path), error = function(e) NULL)
    if (is.list(cfg)) {
      return(list(
        forbidden_always_ingredients = cfg$forbidden_always_ingredients %||% defaults$forbidden_always_ingredients,
        forbidden_always_tags = cfg$forbidden_always_tags %||% defaults$forbidden_always_tags,
        forbidden_with_f_ingredients = cfg$forbidden_with_f_ingredients %||% defaults$forbidden_with_f_ingredients,
        forbidden_with_f_tags = cfg$forbidden_with_f_tags %||% defaults$forbidden_with_f_tags
      ))
    }
  }
  return(defaults)
}

load_soft_blacklist <- function(config_path = "config/soft_blacklist.yml") {
  defaults <- list(
    soft_blacklist_tags = character(0),
    soft_blacklist_recipe_ids = character(0)
  )
  target_path <- NULL
  for (candidate in c(config_path, file.path("..", config_path), file.path("../..", config_path))) {
    if (!is.null(candidate) && file.exists(candidate)) {
      target_path <- candidate
      break
    }
  }

  if (!is.null(target_path)) {
    cfg <- tryCatch(yaml::read_yaml(target_path), error = function(e) NULL)
    if (is.list(cfg)) {
      return(list(
        soft_blacklist_tags = cfg$soft_blacklist_tags %||% defaults$soft_blacklist_tags,
        soft_blacklist_recipe_ids = cfg$soft_blacklist_recipe_ids %||% defaults$soft_blacklist_recipe_ids
      ))
    }
  }
  return(defaults)
}

default_slots <- function(f_present_days = c("vendredi", "samedi", "dimanche"), hot_sunny_days = character(0)) {
  days <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
  tibble::tibble(
    day = days,
    is_weekday = days %in% c("lundi", "mardi", "mercredi", "jeudi", "vendredi"),
    f_present = days %in% f_present_days,
    weather_hot_sunny = days %in% hot_sunny_days
  )
}

validate_slots <- function(slots_df) {
  if (!is.data.frame(slots_df)) stop("slots_df must be a data frame or tibble.")
  if (nrow(slots_df) != 7) stop("slots_df must have exactly 7 rows (7 nights).")
  req_cols <- c("day", "is_weekday", "f_present", "weather_hot_sunny")
  missing <- setdiff(req_cols, colnames(slots_df))
  if (length(missing) > 0) stop("slots_df missing required columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

validate_recipes <- function(recipes_df) {
  if (!is.data.frame(recipes_df)) stop("recipes_df must be a data frame or tibble.")
  req_cols <- c(
    "recipe_id", "name", "tags", "prep_time_min", "method", "good_weather", "fiber_high",
    "source", "from_selected_book", "is_favorite", "forbidden_always", "forbidden_with_f",
    "fridge_garden_matches", "grocery_special_matches", "seasonal_matches", "last_used_date",
    "fiber_g", "protein_g", "magnesium_mg", "gut_health_score"
  )
  missing <- setdiff(req_cols, colnames(recipes_df))
  if (length(missing) > 0) stop("recipes_df missing required columns: ", paste(missing, collapse = ", "))
  invisible(TRUE)
}

load_recipe_dataset <- function(db_path = "recipes.db",
                                recipes_df = NULL,
                                tags_df = NULL,
                                ingredients_df = NULL,
                                books_df = NULL,
                                deals_df = NULL,
                                inventory_df = NULL,
                                forbidden_config = "config/forbidden_rules.yml",
                                soft_blacklist_config = "config/soft_blacklist.yml",
                                selected_book_ids = NULL,
                                last_used_dates = NULL) {
  f_rules <- load_forbidden_rules(forbidden_config)

  # If reading from SQLite database when recipes_df is not provided
  if (is.null(recipes_df) && !is.null(db_path) && file.exists(db_path)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    rec_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM recipes")
    tag_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM recipe_tags")
    ing_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM ingredients")
    book_tbl <- if (DBI::dbExistsTable(conn, "books")) DBI::dbGetQuery(conn, "SELECT * FROM books") else tibble::tibble()
    deal_tbl <- if (DBI::dbExistsTable(conn, "grocery_deals")) DBI::dbGetQuery(conn, "SELECT * FROM grocery_deals") else tibble::tibble()
    inv_tbl <- if (DBI::dbExistsTable(conn, "inventory")) DBI::dbGetQuery(conn, "SELECT * FROM inventory") else tibble::tibble()
    eq_tbl <- if (DBI::dbExistsTable(conn, "recipe_equipment")) DBI::dbGetQuery(conn, "SELECT * FROM recipe_equipment") else tibble::tibble()

    recipes_df <- rec_tbl
    tags_df <- tag_tbl
    ingredients_df <- ing_tbl
    books_df <- book_tbl
    deals_df <- deal_tbl
    inventory_df <- inv_tbl
  }

  if (is.null(recipes_df) || nrow(recipes_df) == 0) {
    return(tibble::tibble(
      recipe_id = character(0),
      name = character(0),
      tags = list(),
      prep_time_min = integer(0),
      method = character(0),
      good_weather = logical(0),
      fiber_high = logical(0),
      source = character(0),
      from_selected_book = logical(0),
      is_favorite = logical(0),
      forbidden_always = logical(0),
      forbidden_with_f = logical(0),
      fridge_garden_matches = integer(0),
      grocery_special_matches = integer(0),
      seasonal_matches = integer(0),
      last_used_date = as.Date(character(0)),
      fiber_g = numeric(0),
      protein_g = numeric(0),
      magnesium_mg = numeric(0),
      gut_health_score = numeric(0)
    ))
  }

  # Build list of tags per recipe
  if (!is.null(tags_df) && nrow(tags_df) > 0) {
    tags_df$recipe_id_chr <- as.character(tags_df$recipe_id)
    tags_by_rec <- split(tags_df$tag_name, tags_df$recipe_id_chr)
  } else {
    tags_by_rec <- list()
  }

  # Equipment by recipe
  eq_by_rec <- list()
  if (exists("eq_tbl") && !is.null(eq_tbl) && nrow(eq_tbl) > 0) {
    eq_tbl$recipe_id_chr <- as.character(eq_tbl$recipe_id)
    eq_by_rec <- split(eq_tbl$equipment_name, eq_tbl$recipe_id_chr)
  }

  # Ingredients by recipe
  ings_by_rec <- list()
  if (!is.null(ingredients_df) && nrow(ingredients_df) > 0) {
    ingredients_df$recipe_id_chr <- as.character(ingredients_df$recipe_id)
    raw_ing_vec <- if (!is.null(ingredients_df$raw_text)) ingredients_df$raw_text else ingredients_df$canonical_name
    ings_by_rec <- split(tolower(raw_ing_vec), ingredients_df$recipe_id_chr)
  }

  # Known inventory canonicals
  inv_items <- character(0)
  if (!is.null(inventory_df) && nrow(inventory_df) > 0) {
    inv_items <- tolower(unique(c(inventory_df$canonical_name, inventory_df$raw_ingredient)))
    inv_items <- inv_items[!is.na(inv_items) & inv_items != ""]
  }

  # Known deals canonicals
  deal_items <- character(0)
  if (!is.null(deals_df) && nrow(deals_df) > 0) {
    deal_items_raw <- if (!is.null(deals_df$matched_canonical_ingredient)) deals_df$matched_canonical_ingredient else deals_df$name
    deal_items <- tolower(unique(deal_items_raw))
    deal_items <- deal_items[!is.na(deal_items) & deal_items != ""]
  }

  # Book map
  book_map <- character(0)
  if (!is.null(books_df) && nrow(books_df) > 0) {
    book_map <- stats::setNames(books_df$title, as.character(books_df$book_id))
  }

  # Map each candidate recipe
  processed_list <- purrr::map(seq_len(nrow(recipes_df)), function(i) {
    row <- recipes_df[i, , drop = FALSE]

    # Helper for scalar column extraction
    get_col <- function(col_name, default_val = NULL) {
      if (col_name %in% colnames(row) && !is.null(row[[col_name]])) {
        val <- row[[col_name]]
        if (is.list(val)) val <- val[[1]]
        if (length(val) > 0 && !all(is.na(val))) return(val)
      }
      return(default_val)
    }

    rec_id <- as.character(get_col("recipe_id", get_col("id", i)))
    rec_name <- as.character(get_col("title", get_col("name", paste("Recipe", rec_id))))

    # Tags
    row_tags <- get_col("tags", tags_by_rec[[rec_id]])
    if (is.list(row_tags)) row_tags <- row_tags[[1]]
    row_tags <- tolower(unique(as.character(row_tags %||% character(0))))

    # Equipment
    row_eq <- eq_by_rec[[rec_id]] %||% character(0)

    # Ingredients
    row_ings <- ings_by_rec[[rec_id]] %||% character(0)

    # Active Prep time (minutes)
    p_time <- as.integer(get_col("prep_time_min", get_col("prep_min", 0L)))

    # Method
    method_val <- NA_character_
    if (any(c("actifry", "airfryer") %in% row_tags) || any(c("actifry", "airfryer") %in% row_eq)) {
      method_val <- "actifry"
    } else if (any(c("mijoteuse", "slow_cooker", "crockpot") %in% row_tags) || any(c("mijoteuse", "slow_cooker") %in% row_eq)) {
      method_val <- "mijoteuse"
    } else if (any(c("plaque", "sheet_pan") %in% row_tags) || any(c("plaque", "sheet_pan") %in% row_eq)) {
      method_val <- "plaque"
    } else {
      m_raw <- get_col("method", NA_character_)
      if (!is.na(m_raw)) method_val <- as.character(m_raw)
    }

    # Weather fit
    gw <- isTRUE(get_col("good_weather", FALSE)) || any(c("ete", "bbq", "bonne_meteo") %in% row_tags)

    # Favorite
    fav <- isTRUE(get_col("is_favorite", FALSE)) || any(c("favori", "favorite") %in% row_tags)

    # Fiber high
    fib_g <- as.numeric(get_col("fiber_g", 0.0))
    fib_hi <- isTRUE(get_col("fiber_high", FALSE)) || fib_g >= 6.0 || any(c("riche_en_fibres", "high_fiber") %in% row_tags)

    # Source / book
    b_id <- as.character(get_col("book_id", NA))
    src_val <- get_col("source", if (!is.na(b_id) && b_id %in% names(book_map)) book_map[[b_id]] else "unknown_source")
    if (is.na(src_val)) src_val <- "unknown_source"

    from_sel_book <- isTRUE(get_col("from_selected_book", FALSE)) ||
      (!is.na(b_id) && b_id != "" && (is.null(selected_book_ids) || b_id %in% as.character(selected_book_ids)))

    # Forbidden always check
    fb_always <- isTRUE(get_col("forbidden_always", FALSE)) ||
      any(sapply(f_rules$forbidden_always_tags, function(t) t %in% row_tags)) ||
      any(sapply(f_rules$forbidden_always_ingredients, function(pattern) {
        grepl(pattern, tolower(rec_name), fixed = TRUE) || (length(row_ings) > 0 && any(grepl(pattern, row_ings, fixed = TRUE)))
      }))

    # Forbidden with F check
    fb_f <- isTRUE(get_col("forbidden_with_f", FALSE)) ||
      any(sapply(f_rules$forbidden_with_f_tags, function(t) t %in% row_tags)) ||
      any(sapply(f_rules$forbidden_with_f_ingredients, function(pattern) {
        grepl(pattern, tolower(rec_name), fixed = TRUE) || (length(row_ings) > 0 && any(grepl(pattern, row_ings, fixed = TRUE)))
      }))

    # Matches counts
    fg_matches <- get_col("fridge_garden_matches", NULL)
    if (is.null(fg_matches)) {
      fg_matches <- if (length(inv_items) > 0 && length(row_ings) > 0) {
        sum(sapply(inv_items, function(inv) any(grepl(inv, row_ings, fixed = TRUE))))
      } else 0L
    }
    fg_matches <- as.integer(fg_matches)

    gs_matches <- get_col("grocery_special_matches", NULL)
    if (is.null(gs_matches)) {
      gs_matches <- if (length(deal_items) > 0 && length(row_ings) > 0) {
        sum(sapply(deal_items, function(deal) any(grepl(deal, row_ings, fixed = TRUE))))
      } else 0L
    }
    gs_matches <- as.integer(gs_matches)

    s_matches <- get_col("seasonal_matches", NULL)
    if (is.null(s_matches)) {
      s_matches <- if (any(c("saison", "ete", "automne", "printemps", "hiver") %in% row_tags)) 1L else 0L
    }
    s_matches <- as.integer(s_matches)

    # Last used date
    l_date <- get_col("last_used_date", if (!is.null(last_used_dates)) last_used_dates[[rec_id]] else NA)
    if (!is.null(l_date) && !is.na(l_date)) l_date <- as.Date(l_date) else l_date <- as.Date(NA)

    # Nutrition defaults / values
    prot_g <- as.numeric(get_col("protein_g", 20.0))
    mag_mg <- as.numeric(get_col("magnesium_mg", 50.0))
    gut_score <- as.numeric(get_col("gut_health_score", 5.0))

    tibble::tibble(
      recipe_id = rec_id,
      name = rec_name,
      tags = list(row_tags),
      prep_time_min = p_time,
      method = method_val,
      good_weather = gw,
      fiber_high = fib_hi,
      source = src_val,
      from_selected_book = from_sel_book,
      is_favorite = fav,
      forbidden_always = fb_always,
      forbidden_with_f = fb_f,
      fridge_garden_matches = fg_matches,
      grocery_special_matches = gs_matches,
      seasonal_matches = s_matches,
      last_used_date = l_date,
      fiber_g = fib_g,
      protein_g = prot_g,
      magnesium_mg = mag_mg,
      gut_health_score = gut_score
    )
  })

  res <- dplyr::bind_rows(processed_list)
  validate_recipes(res)
  return(res)
}
