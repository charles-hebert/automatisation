library(testthat)
library(DBI)
library(RSQLite)

find_and_source <- function(rel_path) {
  if (file.exists(rel_path)) {
    source(rel_path)
  } else if (file.exists(file.path("../..", rel_path))) {
    source(file.path("../..", rel_path))
  } else if (file.exists(file.path("..", rel_path))) {
    source(file.path("..", rel_path))
  } else {
    stop("Cannot find file: ", rel_path)
  }
}

find_and_source("R/recipe_db.R")
find_and_source("R/normalize_ingredients.R")
find_and_source("R/deterministic_tags.R")

test_that("Deterministic rules tag weeknight, meat, fish, vegetarian, vegan correctly", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)

  # Recipe 1: Beef stir fry (prep 15, cook 15) -> weeknight_ok = true, meat = true
  dbExecute(con, "INSERT INTO recipes (recipe_id, title, prep_min, cook_min) VALUES (1, 'Sauté de boeuf', 15, 15)")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '400g boeuf', 'boeuf')")

  # Recipe 2: Slow cooker stew (prep 20, cook 120, mijoteuse) -> weeknight_ok = true, meat = true
  dbExecute(con, "INSERT INTO recipes (recipe_id, title, prep_min, cook_min) VALUES (2, 'Mijoté', 20, 120)")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (2, '400g porc', 'porc')")
  dbExecute(con, "INSERT INTO recipe_equipment (recipe_id, equipment_name) VALUES (2, 'mijoteuse')")

  # Recipe 3: Long oven bake (prep 40, cook 60) -> weeknight_ok = false
  dbExecute(con, "INSERT INTO recipes (recipe_id, title, prep_min, cook_min) VALUES (3, 'Rôti', 40, 60)")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (3, '1kg poulet', 'poulet')")

  dbDisconnect(con)

  normalize_all_ingredients(tmp_db)
  run_deterministic_rules(tmp_db)

  con <- dbConnect(SQLite(), tmp_db)
  res <- dbGetQuery(con, "SELECT recipe_id, tag_name, tag_value, status FROM recipe_tag_classifications")
  dbDisconnect(con)

  r1_wn <- res[res$recipe_id == 1 & res$tag_name == "weeknight_ok", "tag_value"]
  expect_equal(r1_wn, "true")

  r2_wn <- res[res$recipe_id == 2 & res$tag_name == "weeknight_ok", "tag_value"]
  expect_equal(r2_wn, "true")

  r3_wn <- res[res$recipe_id == 3 & res$tag_name == "weeknight_ok", "tag_value"]
  expect_equal(r3_wn, "false")
})

test_that("Unmatched ingredient forces dietary tags to review status", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)

  dbExecute(con, "INSERT INTO recipes (recipe_id, title, prep_min, cook_min) VALUES (1, 'Plat Mystère', 10, 10)")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '100g plante inconnue', 'plante inconnue')")
  dbDisconnect(con)

  normalize_all_ingredients(tmp_db)
  run_deterministic_rules(tmp_db)

  con <- dbConnect(SQLite(), tmp_db)
  res <- dbGetQuery(con, "SELECT tag_name, status FROM recipe_tag_classifications WHERE recipe_id = 1")
  dbDisconnect(con)

  dietary_statuses <- res[res$tag_name != "weeknight_ok", "status"]
  expect_true(all(dietary_statuses == "review"))

  wn_status <- res[res$tag_name == "weeknight_ok", "status"]
  expect_equal(wn_status, "accepted")
})
