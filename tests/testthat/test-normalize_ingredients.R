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

test_that("Ingredient normalization handles exact, synonym, and unmatched terms", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)

  dbExecute(con, "INSERT INTO recipes (title) VALUES ('Test Recipe')")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '500g poulet', 'poulet')")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '1 dragon fruit', 'fruit du dragon inconnu')")
  dbDisconnect(con)

  updated <- normalize_all_ingredients(tmp_db)
  expect_equal(updated, 2)

  con <- dbConnect(SQLite(), tmp_db)
  ings <- dbGetQuery(con, "SELECT canonical_name, ref_ingredient_id, match_method FROM ingredients ORDER BY ingredient_id")
  dbDisconnect(con)

  expect_equal(ings$match_method[[1]], "exact")
  expect_true(ings$ref_ingredient_id[[1]] > 0)

  expect_equal(ings$match_method[[2]], "unmatched")
  expect_equal(ings$ref_ingredient_id[[2]], 0)

  # Test export unmatched
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)
  exported <- export_unmatched_ingredients(tmp_db, tmp_csv)

  expect_equal(nrow(exported), 1)
  expect_equal(exported$term[[1]], "fruit du dragon inconnu")
})
