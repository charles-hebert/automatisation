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
find_and_source("R/review_queue.R")

test_that("export_review_queue exports all review items to CSV", {
  tmp_db <- tempfile(fileext = ".db")
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit({
    unlink(tmp_db)
    unlink(tmp_csv)
  })

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)

  dbExecute(con, "INSERT INTO recipes (recipe_id, title) VALUES (1, 'Inconnu')")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, 'Truc', 'truc inconnu')")
  dbDisconnect(con)

  normalize_all_ingredients(tmp_db)
  run_deterministic_rules(tmp_db)

  exported <- export_review_queue(tmp_db, tmp_csv)
  expect_true(nrow(exported) > 0)
  expect_true(file.exists(tmp_csv))

  csv_content <- read.csv(tmp_csv, stringsAsFactors = FALSE)
  expect_equal(nrow(csv_content), nrow(exported))
  expect_true(all(csv_content$tag_name %in% c("contains_meat", "contains_fish", "vegetarian", "vegan")))
})
