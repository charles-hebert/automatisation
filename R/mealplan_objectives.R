# Lexicographic Solve Objectives for Weekly Meal-Plan Optimizer

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

solve_mealplan_lexicographic <- function(base_model_obj, solver = "glpk") {
  M <- base_model_obj$model
  candidate_df <- base_model_obj$candidate_df
  slots_df <- base_model_obj$slots_df
  N <- base_model_obj$N
  T <- base_model_obj$T

  # Tier 1: Minimize total weighted slack penalty for weekly nutrition target
  m1 <- M |> ompr::set_objective(
    under_fiber + over_fiber + under_protein + over_protein + under_magnesium + over_magnesium + under_gut + over_gut,
    "min"
  )

  res1 <- tryCatch(
    ompr::solve_model(m1, ompr.roi::with_ROI(solver = solver)),
    error = function(e) stop("MIP Solve failed at Tier 1 (Nutrition slack): ", e$message)
  )

  status1 <- ompr::solver_status(res1)
  if (!(status1 %in% c("success", "optimal"))) {
    stop("Weekly meal plan optimization is INFEASIBLE under current hard constraints (Status: ", status1, ").")
  }

  v1 <- ompr::objective_value(res1)
  M_locked1 <- m1 |> ompr::add_constraint(
    under_fiber + over_fiber + under_protein + over_protein + under_magnesium + over_magnesium + under_gut + over_gut <= v1 + 1e-4
  )

  # Tier 2: Maximize total fridge_garden_matches
  fg_v <- candidate_df$fridge_garden_matches
  m2 <- M_locked1 |> ompr::set_objective(
    ompr::sum_over(fg_v[i] * x[i, t], i = 1:N, t = 1:T),
    "max"
  )

  res2 <- ompr::solve_model(m2, ompr.roi::with_ROI(solver = solver))
  v2 <- ompr::objective_value(res2)
  M_locked2 <- m2 |> ompr::add_constraint(
    ompr::sum_over(fg_v[i] * x[i, t], i = 1:N, t = 1:T) >= v2 - 1e-4
  )

  # Tier 3: Maximize total grocery_special_matches
  gs_v <- candidate_df$grocery_special_matches
  m3 <- M_locked2 |> ompr::set_objective(
    ompr::sum_over(gs_v[i] * x[i, t], i = 1:N, t = 1:T),
    "max"
  )

  res3 <- ompr::solve_model(m3, ompr.roi::with_ROI(solver = solver))
  v3 <- ompr::objective_value(res3)
  M_locked3 <- m3 |> ompr::add_constraint(
    ompr::sum_over(gs_v[i] * x[i, t], i = 1:N, t = 1:T) >= v3 - 1e-4
  )

  # Tier 4: Maximize total seasonal_matches
  s_v <- candidate_df$seasonal_matches
  m4 <- M_locked3 |> ompr::set_objective(
    ompr::sum_over(s_v[i] * x[i, t], i = 1:N, t = 1:T),
    "max"
  )

  res4 <- ompr::solve_model(m4, ompr.roi::with_ROI(solver = solver))
  v4 <- ompr::objective_value(res4)
  M_locked4 <- m4 |> ompr::add_constraint(
    ompr::sum_over(s_v[i] * x[i, t], i = 1:N, t = 1:T) >= v4 - 1e-4
  )

  # Tier 5: Maximize weather fit (good_weather on weather_hot_sunny nights)
  w_fit_matrix <- matrix(0L, nrow = N, ncol = T)
  for (i in 1:N) {
    for (t in 1:T) {
      if (isTRUE(candidate_df$good_weather[i]) && isTRUE(slots_df$weather_hot_sunny[t])) {
        w_fit_matrix[i, t] <- 1L
      }
    }
  }

  m5 <- M_locked4 |> ompr::set_objective(
    ompr::sum_over(w_fit_matrix[i, t] * x[i, t], i = 1:N, t = 1:T),
    "max"
  )

  final_res <- ompr::solve_model(m5, ompr.roi::with_ROI(solver = solver))
  v5 <- ompr::objective_value(final_res)

  # Construct optimal plan output
  sol_df <- ompr::get_solution(final_res, x[i, t]) |>
    dplyr::filter(value > 0.5)

  plan_schedule <- purrr::map_dfr(seq_len(nrow(sol_df)), function(k) {
    i_idx <- sol_df$i[k]
    t_idx <- sol_df$t[k]

    rec_row <- candidate_df[i_idx, ]
    slot_row <- slots_df[t_idx, ]

    tibble::tibble(
      slot_index = t_idx,
      day = slot_row$day,
      is_weekday = slot_row$is_weekday,
      f_present = slot_row$f_present,
      weather_hot_sunny = slot_row$weather_hot_sunny,
      recipe_id = rec_row$recipe_id,
      recipe_name = rec_row$name,
      prep_time_min = rec_row$prep_time_min,
      method = rec_row$method,
      tags = list(rec_row$tags[[1]]),
      source = rec_row$source,
      is_favorite = rec_row$is_favorite,
      good_weather = rec_row$good_weather,
      fridge_garden_matches = rec_row$fridge_garden_matches,
      grocery_special_matches = rec_row$grocery_special_matches,
      seasonal_matches = rec_row$seasonal_matches,
      fiber_g = rec_row$fiber_g,
      protein_g = rec_row$protein_g,
      magnesium_mg = rec_row$magnesium_mg,
      gut_health_score = rec_row$gut_health_score
    )
  }) |> dplyr::arrange(slot_index)

  list(
    schedule = plan_schedule,
    final_model = final_res,
    tier_scores = list(
      nutrition_slack = v1,
      fridge_garden_matches = v2,
      grocery_special_matches = v3,
      seasonal_matches = v4,
      weather_fit = v5
    ),
    candidate_df = candidate_df,
    slots_df = slots_df
  )
}
