# Unit tests for meal plan hard constraints

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

make_small_fixture <- function() {
  tibble::tibble(
    recipe_id = paste0("rec_", 1:10),
    title = c(
      "Med Veggie", "Poisson Grain", "Riz Veggie", "Pates Veggie",
      "Legum Bœuf", "Pouletactifry", "Dindon Roti", "Porc Braise",
      "Soba Interdit", "Agneau Grill"
    ),
    prep_min = c(15, 20, 25, 20, 30, 25, 25, 45, 15, 20),
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
      c("agneau")
    ),
    fiber_g = c(8, 7, 6, 6, 5, 4, 5, 4, 3, 4),
    protein_g = c(25, 30, 20, 20, 30, 35, 30, 25, 25, 25),
    magnesium_mg = c(300, 400, 350, 300, 400, 300, 300, 250, 250, 250),
    gut_health_score = c(8, 7, 6, 6, 5, 5, 5, 4, 4, 4),
    method = c(NA, NA, NA, NA, NA, "actifry", NA, NA, NA, NA),
    is_favorite = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    from_selected_book = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    source = c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5")
  )
}

test_that("Exactly 1 recipe per night and no repeats", {
  raw_rec <- make_small_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots()

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)
  sched <- sol$schedule

  expect_equal(nrow(sched), 7)
  expect_equal(length(unique(sched$slot_index)), 7)
  expect_equal(length(unique(sched$recipe_id)), 7)
})

test_that("Forbidden always recipes are excluded from candidate pool", {
  raw_rec <- make_small_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)

  soba_row <- ds |> dplyr::filter(recipe_id == "rec_9")
  expect_true(soba_row$forbidden_always)

  slots <- default_slots()
  bm <- build_base_mealplan_model(ds, slots)
  expect_false("rec_9" %in% bm$candidate_df$recipe_id)
})

test_that("Forbidden with F excluded on nights where f_present is TRUE", {
  raw_rec <- make_small_fixture()
  raw_rec$tags[[1]] <- c(raw_rec$tags[[1]], "forbidden_f") # rec_1 has forbidden_f

  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots(f_present_days = c("vendredi", "samedi", "dimanche"))

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)
  sched <- sol$schedule

  f_nights <- sched |> dplyr::filter(f_present == TRUE)
  expect_false("rec_1" %in% f_nights$recipe_id)
})

test_that("Weekday prep time constraint (prep_time_min <= 30 min) holds", {
  raw_rec <- make_small_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots()

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)
  sched <- sol$schedule

  weekday_sched <- sched |> dplyr::filter(is_weekday == TRUE)
  expect_true(all(weekday_sched$prep_time_min <= 30))
})

test_that("Required tag counts hold (mediterraneen, poisson, vegetarien, etc.)", {
  raw_rec <- make_small_fixture()
  ds <- load_recipe_dataset(db_path = NULL, recipes_df = raw_rec)
  slots <- default_slots()

  bm <- build_base_mealplan_model(ds, slots)
  sol <- solve_mealplan_lexicographic(bm)
  sched <- sol$schedule

  expect_equal(sum(sapply(sched$tags, function(tg) "mediterraneen" %in% tg)), 1)
  expect_equal(sum(sapply(sched$tags, function(tg) "poisson" %in% tg)), 1)
  expect_equal(sum(sapply(sched$tags, function(tg) "vegetarien" %in% tg)), 3)
  expect_true(sum(sapply(sched$tags, function(tg) "grains_entiers" %in% tg)) >= 1)

  boeuf_porc_count <- sum(sapply(sched$tags, function(tg) any(c("boeuf", "porc") %in% tg)))
  expect_true(boeuf_porc_count <= 1)

  expect_true(sum(sched$fiber_g >= 6.0) >= 4)
  expect_true(sum(sched$is_favorite) >= 1)

  src_table <- table(sched$source)
  expect_true(all(src_table <= 2))
})
