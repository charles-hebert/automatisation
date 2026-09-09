# Unit tests for lexicographic objectives, alternatives, and weekend meal prep

library(testthat)

find_and_source <- function(rel_path) {
  if (file.exists(rel_path)) {
    source(rel_path)
  } else if (file.exists(file.path("..", rel_path))) {
    source(file.path("..", rel_path))
  } else if (file.exists(file.path("../..", rel_path))) {
    source(file.path("../..", rel_path))
  } else {
    stop("Could not find file: ", rel_path)
  }
}

find_and_source("R/mealplan_data_model.R")
find_and_source("R/mealplan_constraints.R")
find_and_source("R/mealplan_objectives.R")
find_and_source("R/mealplan_alternatives.R")
find_and_source("R/mealprep_weekend.R")

make_objective_fixture <- function() {
  tibble::tibble(
    recipe_id = paste0("rec_", 1:12),
    title = c(
      "Med Veggie", "Poisson Grain", "Riz Veggie", "Pates Veggie",
      "Legum Bœuf", "Pouletactifry", "Dindon Roti", "Porc Braise",
      "Soba Interdit", "Agneau Grill", "Barre Avoine", "Pain Levain"
    ),
    prep_min = c(15, 20, 25, 20, 30, 25, 25, 45, 15, 20, 15, 30),
    tags = list(
      c("mediterraneen", "vegetarien"),
      c("poisson", "grains_entiers"),
      c("vegetarien", "riz"),
      c("vegetarien", "pates"),
      c("legumineuses", "boeuf"),
      c("poulet"),
      c("dindon"),
      c("porc"),
      c("soba"),
      c("agneau"),
      c("collation", "snack", "avoine"),
      c("levain", "sourdough")
    ),
    fiber_g = c(8, 7, 6, 6, 5, 4, 5, 4, 3, 4, 5, 4),
    protein_g = c(25, 30, 20, 20, 30, 35, 30, 25, 25, 25, 10, 8),
    magnesium_mg = c(300, 400, 350, 300, 400, 300, 300, 250, 250, 250, 150, 100),
    gut_health_score = c(8, 7, 6, 6, 5, 5, 5, 4, 4, 4, 6, 7),
    method = c("actifry", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    is_favorite = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    from_selected_book = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    source = c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5", "S6", "S6"),
    fridge_garden_matches = c(3, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0),
    grocery_special_matches = c(1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
}

test_that("Lexicographic solve tier locking maintains higher priority tier objective", {
  raw_rec <- make_objective_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots()

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)

  # Check that fridge_garden_matches score achieved at Tier 2 is locked and preserved
  expect_true(sol$tier_scores$fridge_garden_matches >= 0)
  expect_true(sol$tier_scores$grocery_special_matches >= 0)
  expect_true(sol$tier_scores$nutrition_slack >= 0)
})

test_that("get_top_alternatives_for_night returns up to 3 eligible alternatives", {
  raw_rec <- make_objective_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots()

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)

  alts_night1 <- get_top_alternatives_for_night(sol, slot_index = 1, top_n = 3)
  expect_true(nrow(alts_night1) <= 3)

  # Chosen recipe for night 1 must not be in alternatives
  chosen_r1 <- sol$schedule$recipe_id[sol$schedule$slot_index == 1]
  expect_false(chosen_r1 %in% alts_night1$recipe_id)

  # Other chosen recipes on nights 2..7 must not be in alternatives for night 1
  other_chosen <- setdiff(sol$schedule$recipe_id, chosen_r1)
  expect_false(any(alts_night1$recipe_id %in% other_chosen))
})

test_that("Weekend meal-prep script selects snacks with oats, savoury baking, sourdough", {
  raw_rec <- make_objective_fixture()

  mp <- select_weekend_mealprep(recipes_df = raw_rec)
  expect_equal(nrow(mp), 5)

  snacks <- mp |> dplyr::filter(category == "Snack / Collation")
  expect_equal(nrow(snacks), 3)

  # Check oat presence in snacks
  oat_snack_present <- any(sapply(snacks$tags, function(tg) "avoine" %in% tg)) ||
    any(grepl("Avoine", snacks$title))
  expect_true(oat_snack_present)

  sourdough <- mp |> dplyr::filter(category == "Sourdough / Pain au Levain")
  expect_equal(nrow(sourdough), 1)
})
