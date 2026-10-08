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

test_that("init_recipe_db creates required tables and indexes", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)
  on.exit(dbDisconnect(con), add = TRUE, after = FALSE)

  tables <- dbListTables(con)
  expect_true("ingredients_ref" %in% tables)
  expect_true("ingredient_synonyms" %in% tables)
  expect_true("recipe_tag_classifications" %in% tables)
  expect_true("llm_cache" %in% tables)
  expect_true("v_recipe_tags_accepted" %in% tables)

  # Check reserved unmatched ingredient 0
  unmatched_ref <- dbGetQuery(con, "SELECT * FROM ingredients_ref WHERE ingredient_id = 0")
  expect_equal(nrow(unmatched_ref), 1)
  expect_equal(unmatched_ref$canonical_name_fr[[1]], "Inconnu")
})
