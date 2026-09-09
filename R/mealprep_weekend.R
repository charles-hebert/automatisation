# Weekend Meal-Prep Selection (Rule-Based Sub-Problem)

ensure_packages <- function(pkgs = c("dplyr", "purrr", "tibble", "stringr")) {
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

ensure_packages(c("dplyr", "purrr", "tibble", "stringr"))

select_weekend_mealprep <- function(db_path = "recipes.db",
                                    recipes_df = NULL,
                                    tags_df = NULL,
                                    ingredients_df = NULL) {
  # If loading from DB and recipes_df not passed
  if (is.null(recipes_df) && !is.null(db_path) && file.exists(db_path)) {
    conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
    on.exit(DBI::dbDisconnect(conn), add = TRUE)

    rec_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM recipes")
    tag_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM recipe_tags")
    ing_tbl <- DBI::dbGetQuery(conn, "SELECT * FROM ingredients")

    recipes_df <- rec_tbl
    tags_df <- tag_tbl
    ingredients_df <- ing_tbl
  }

  if (is.null(recipes_df) || nrow(recipes_df) == 0) {
    warning("No recipes provided for weekend meal-prep selection.")
    return(tibble::tibble(
      category = character(0),
      recipe_id = character(0),
      title = character(0),
      tags = list(),
      source = character(0)
    ))
  }

  # Build tags by recipe
  if (!is.null(tags_df) && nrow(tags_df) > 0) {
    tags_df$recipe_id_chr <- as.character(tags_df$recipe_id)
    tags_by_rec <- split(tags_df$tag_name, tags_df$recipe_id_chr)
  } else {
    tags_by_rec <- list()
  }

  # Build ingredients by recipe
  if (!is.null(ingredients_df) && nrow(ingredients_df) > 0) {
    ingredients_df$recipe_id_chr <- as.character(ingredients_df$recipe_id)
    raw_ing_vec <- if (!is.null(ingredients_df$raw_text)) ingredients_df$raw_text else ingredients_df$canonical_name
    ings_by_rec <- split(tolower(raw_ing_vec), ingredients_df$recipe_id_chr)
  } else {
    ings_by_rec <- list()
  }

  # Process candidate list
  candidates <- purrr::map_dfr(seq_len(nrow(recipes_df)), function(i) {
    row <- recipes_df[i, , drop = FALSE]
    rec_id <- as.character(row$recipe_id %||% row$id %||% i)
    rec_title <- as.character(row$title %||% row$name %||% paste("Recipe", rec_id))

    row_tags <- row$tags[[1]] %||% tags_by_rec[[rec_id]] %||% character(0)
    row_tags <- tolower(unique(as.character(row_tags)))

    row_ings <- ings_by_rec[[rec_id]] %||% character(0)

    # Categories detection
    is_snack <- any(c("collation", "snack", "meal_prep") %in% row_tags) || grepl("snack|barre|muffin|biscuit", tolower(rec_title))
    has_oats <- any(c("avoine", "oats", "oatmeal") %in% row_tags) || any(grepl("avoine|oat", row_ings)) || grepl("avoine|oat", tolower(rec_title))

    is_savoury_baking <- any(c("baking_sale", "savoury_baking", "sale", "quiche", "pain_sale") %in% row_tags) || grepl("quiche|muffin sale|scone|focaccia", tolower(rec_title))
    is_sourdough <- any(c("levain", "sourdough") %in% row_tags) || any(grepl("levain|sourdough", row_ings)) || grepl("levain|sourdough", tolower(rec_title))

    src <- as.character(row$source %||% "unknown_source")

    tibble::tibble(
      recipe_id = rec_id,
      title = rec_title,
      tags = list(row_tags),
      source = src,
      is_snack = is_snack,
      has_oats = has_oats,
      is_savoury_baking = is_savoury_baking,
      is_sourdough = is_sourdough
    )
  })

  # 1. Select Snacks: Exactly 3 snacks, at least 1 containing oats
  oat_snacks <- candidates |> dplyr::filter(is_snack & has_oats)
  other_snacks <- candidates |> dplyr::filter(is_snack & !has_oats)

  chosen_oat_snack <- head(oat_snacks, 1)
  remaining_snacks <- dplyr::bind_rows(
    tail(oat_snacks, -1),
    other_snacks
  )

  chosen_other_snacks <- head(remaining_snacks, 2)
  selected_snacks <- dplyr::bind_rows(chosen_oat_snack, chosen_other_snacks)

  if (nrow(selected_snacks) < 3) {
    # Fallback to any candidates if pool is small
    needed <- 3 - nrow(selected_snacks)
    avail <- candidates |> dplyr::filter(!(recipe_id %in% selected_snacks$recipe_id))
    selected_snacks <- dplyr::bind_rows(selected_snacks, head(avail, needed))
  }
  selected_snacks <- head(selected_snacks, 3) |> dplyr::mutate(category = "Snack / Collation")

  # 2. Select Savoury Baking: Exactly 1 item
  chosen_ids <- selected_snacks$recipe_id
  savoury_pool <- candidates |>
    dplyr::filter(!(recipe_id %in% chosen_ids)) |>
    dplyr::filter(is_savoury_baking)

  if (nrow(savoury_pool) == 0) {
    savoury_pool <- candidates |> dplyr::filter(!(recipe_id %in% chosen_ids))
  }
  selected_savoury <- head(savoury_pool, 1) |> dplyr::mutate(category = "Savoury Baking / Cuisson Salée")

  # 3. Select Sourdough: Exactly 1 sourdough recipe
  chosen_ids <- c(selected_snacks$recipe_id, selected_savoury$recipe_id)
  sourdough_pool <- candidates |>
    dplyr::filter(!(recipe_id %in% chosen_ids)) |>
    dplyr::filter(is_sourdough)

  if (nrow(sourdough_pool) == 0) {
    sourdough_pool <- candidates |> dplyr::filter(!(recipe_id %in% chosen_ids))
  }
  selected_sourdough <- head(sourdough_pool, 1) |> dplyr::mutate(category = "Sourdough / Pain au Levain")

  # Combine final meal prep selection
  mealprep_plan <- dplyr::bind_rows(selected_snacks, selected_savoury, selected_sourdough) |>
    dplyr::select(category, recipe_id, title, tags, source)

  return(mealprep_plan)
}
