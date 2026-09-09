# Hard Constraints for Weekly Meal-Plan Optimizer (ompr MILP)

ensure_packages <- function(pkgs = c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk", "dplyr", "purrr", "tibble")) {
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

ensure_packages(c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk", "dplyr", "purrr", "tibble"))

build_base_mealplan_model <- function(recipes_df,
                                      slots_df,
                                      history_recipe_ids = character(0),
                                      soft_blacklist_ids = character(0),
                                      nutrition_targets = list(fiber_g = 210, protein_g = 350, magnesium_mg = 2800, gut_health_score = 42)) {
  validate_recipes(recipes_df)
  validate_slots(slots_df)

  # Filter out recipes that are forbidden_always or in 3-week history
  candidate_df <- recipes_df |>
    dplyr::filter(forbidden_always == FALSE) |>
    dplyr::filter(!(recipe_id %in% history_recipe_ids))

  N <- nrow(candidate_df)
  if (N < 7) {
    stop("Not enough eligible candidate recipes (N = ", N, ") to build a 7-night plan.")
  }

  T <- 7

  # Precompute helper index sets
  get_tag_indices <- function(tag_name) {
    which(sapply(candidate_df$tags, function(tg) tag_name %in% tolower(tg)))
  }

  med_idx <- get_tag_indices("mediterraneen")
  grains_idx <- get_tag_indices("grains_entiers")
  poisson_idx <- get_tag_indices("poisson")
  veg_idx <- get_tag_indices("vegetarien")

  boeuf_idx <- get_tag_indices("boeuf")
  porc_idx <- get_tag_indices("porc")
  boeuf_porc_idx <- unique(c(boeuf_idx, porc_idx))

  riz_idx <- get_tag_indices("riz")
  pates_idx <- get_tag_indices("pates")
  legum_idx <- get_tag_indices("legumineuses")

  fiber_hi_idx <- which(candidate_df$fiber_high == TRUE)
  method_idx <- which(candidate_df$method %in% c("actifry", "mijoteuse"))
  fav_idx <- which(candidate_df$is_favorite == TRUE)
  sel_book_idx <- which(candidate_df$from_selected_book == TRUE)

  # Soft blacklist indices
  soft_bl_idx <- which(candidate_df$recipe_id %in% soft_blacklist_ids)

  # Source grouping
  sources <- unique(candidate_df$source)
  sources <- sources[!is.na(sources) & sources != ""]

  # Initialize MIPModel
  M <- ompr::MIPModel()

  # Binary decision variables x[i, t]
  M <- ompr::add_variable(M, x[i, t], i = 1:N, t = 1:T, type = "binary")

  # Continuous slack variables for nutrition targets
  M <- ompr::add_variable(M, under_fiber, type = "continuous", lb = 0)
  M <- ompr::add_variable(M, over_fiber, type = "continuous", lb = 0)

  M <- ompr::add_variable(M, under_protein, type = "continuous", lb = 0)
  M <- ompr::add_variable(M, over_protein, type = "continuous", lb = 0)

  M <- ompr::add_variable(M, under_magnesium, type = "continuous", lb = 0)
  M <- ompr::add_variable(M, over_magnesium, type = "continuous", lb = 0)

  M <- ompr::add_variable(M, under_gut, type = "continuous", lb = 0)
  M <- ompr::add_variable(M, over_gut, type = "continuous", lb = 0)

  # Constraint 1: Exactly 1 recipe per night
  for (t in 1:T) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = 1:N) == 1)
  }

  # Constraint 2: Max 1 use per recipe across the week
  for (i in 1:N) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], t = 1:T) <= 1)
  }

  # Constraint 3 & 4: Night-specific ineligibilities
  for (t in 1:T) {
    is_wd <- slots_df$is_weekday[t]
    f_pres <- slots_df$f_present[t]

    for (i in 1:N) {
      if (isTRUE(f_pres) && isTRUE(candidate_df$forbidden_with_f[i])) {
        M <- ompr::add_constraint(M, x[i, t] == 0)
      }
      if (isTRUE(is_wd) && candidate_df$prep_time_min[i] > 30) {
        M <- ompr::add_constraint(M, x[i, t] == 0)
      }
    }
  }

  # Constraint 7: Exactly 1 mediterraneen
  if (length(med_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = med_idx, t = 1:T) == 1)
  }

  # Constraint 8: >= 1 grains_entiers
  if (length(grains_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = grains_idx, t = 1:T) >= 1)
  }

  # Constraint 9: Exactly 1 poisson
  if (length(poisson_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = poisson_idx, t = 1:T) == 1)
  }

  # Constraint 10: Exactly 3 vegetarien
  if (length(veg_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = veg_idx, t = 1:T) == 3)
  }

  # Constraint 11: boeuf + porc <= 1
  if (length(boeuf_porc_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = boeuf_porc_idx, t = 1:T) <= 1)
  }

  # Constraint 12: Exactly 1 riz, 1 pates, 1 legumineuses
  if (length(riz_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = riz_idx, t = 1:T) == 1)
  }
  if (length(pates_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = pates_idx, t = 1:T) == 1)
  }
  if (length(legum_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = legum_idx, t = 1:T) == 1)
  }

  # Constraint 13: >= 4 fiber_high == TRUE
  if (length(fiber_hi_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = fiber_hi_idx, t = 1:T) >= 4)
  }

  # Constraint 14: >= 1 method (actifry/mijoteuse)
  if (length(method_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = method_idx, t = 1:T) >= 1)
  }

  # Constraint 15: >= 1 is_favorite == TRUE
  if (length(fav_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = fav_idx, t = 1:T) >= 1)
  }

  # Constraint 16: >= 50% from_selected_book == TRUE (>= 4 of 7)
  if (length(sel_book_idx) > 0) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = sel_book_idx, t = 1:T) >= 4)
  }

  # Constraint 17: Max 2 uses per distinct source
  for (src in sources) {
    src_idx <- which(candidate_df$source == src)
    if (length(src_idx) > 0) {
      M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], i = src_idx, t = 1:T) <= 2)
    }
  }

  # Constraint 18: Soft blacklist (< 1 use / week = 0)
  for (sb_i in soft_bl_idx) {
    M <- ompr::add_constraint(M, ompr::sum_over(x[i, t], t = 1:T) == 0)
  }

  # Nutrition target slack equations
  target_fiber <- nutrition_targets$fiber_g %||% 210
  target_protein <- nutrition_targets$protein_g %||% 350
  target_magnesium <- nutrition_targets$magnesium_mg %||% 2800
  target_gut <- nutrition_targets$gut_health_score %||% 42

  # Vector values
  fib_v <- candidate_df$fiber_g
  prot_v <- candidate_df$protein_g
  mag_v <- candidate_df$magnesium_mg
  gut_v <- candidate_df$gut_health_score

  M <- ompr::add_constraint(
    M,
    ompr::sum_over(fib_v[i] * x[i, t], i = 1:N, t = 1:T) + under_fiber - over_fiber == target_fiber
  )
  M <- ompr::add_constraint(
    M,
    ompr::sum_over(prot_v[i] * x[i, t], i = 1:N, t = 1:T) + under_protein - over_protein == target_protein
  )
  M <- ompr::add_constraint(
    M,
    ompr::sum_over(mag_v[i] * x[i, t], i = 1:N, t = 1:T) + under_magnesium - over_magnesium == target_magnesium
  )
  M <- ompr::add_constraint(
    M,
    ompr::sum_over(gut_v[i] * x[i, t], i = 1:N, t = 1:T) + under_gut - over_gut == target_gut
  )

  list(
    model = M,
    candidate_df = candidate_df,
    slots_df = slots_df,
    N = N,
    T = T
  )
}
