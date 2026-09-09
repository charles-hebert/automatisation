# Re-Ranking Top Alternatives per Night for Weekly Meal Plan

ensure_packages <- function(pkgs = c("dplyr", "purrr", "tibble")) {
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

ensure_packages(c("dplyr", "purrr", "tibble"))

get_top_alternatives_for_night <- function(solve_result, slot_index, top_n = 3) {
  schedule <- solve_result$schedule
  candidate_df <- solve_result$candidate_df
  slots_df <- solve_result$slots_df

  if (!(slot_index %in% schedule$slot_index)) {
    stop("Invalid slot_index: ", slot_index)
  }

  chosen_row <- schedule |> dplyr::filter(slot_index == !!slot_index)
  chosen_recipe_id <- chosen_row$recipe_id
  slot_info <- slots_df[slot_index, ]

  # Recipes chosen on other nights
  other_chosen_ids <- setdiff(schedule$recipe_id, chosen_recipe_id)

  # Filter candidate pool
  eligible <- candidate_df |>
    dplyr::filter(!(recipe_id %in% other_chosen_ids)) |>
    dplyr::filter(recipe_id != chosen_recipe_id)

  # Slot-specific eligibility
  if (isTRUE(slot_info$is_weekday)) {
    eligible <- eligible |> dplyr::filter(prep_time_min <= 30)
  }
  if (isTRUE(slot_info$f_present)) {
    eligible <- eligible |> dplyr::filter(forbidden_with_f == FALSE)
  }

  if (nrow(eligible) == 0) {
    warning("Candidate pool exhausted for slot ", slot_index, " (", slot_info$day, "). Returning 0 alternatives.")
    return(tibble::tibble())
  }

  # Sort by (fridge_garden_matches, grocery_special_matches, seasonal_matches, is_favorite) descending
  ranked <- eligible |>
    dplyr::arrange(
      dplyr::desc(fridge_garden_matches),
      dplyr::desc(grocery_special_matches),
      dplyr::desc(seasonal_matches),
      dplyr::desc(is_favorite),
      name
    )

  if (nrow(ranked) < top_n) {
    warning("Only ", nrow(ranked), " eligible alternative(s) found for slot ", slot_index, " (", slot_info$day, ") [requested ", top_n, "].")
  }

  out <- head(ranked, top_n) |>
    dplyr::mutate(
      slot_index = slot_index,
      day = slot_info$day
    ) |>
    dplyr::select(
      slot_index, day, recipe_id, name, prep_time_min,
      fridge_garden_matches, grocery_special_matches, seasonal_matches, is_favorite, source
    )

  return(out)
}

get_all_top_alternatives <- function(solve_result, top_n = 3) {
  purrr::map_dfr(1:7, function(t) {
    get_top_alternatives_for_night(solve_result, slot_index = t, top_n = top_n)
  })
}
