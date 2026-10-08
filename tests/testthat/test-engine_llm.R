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
find_and_source("R/engine_llm.R")

test_that("compute_cache_key produces consistent deterministic MD5 hash", {
  k1 <- compute_cache_key("Poulet Sauté", c(16, 5), "google/gemini-2.5-flash")
  k2 <- compute_cache_key("poulet sauté ", c(5, 16), "google/gemini-2.5-flash")
  expect_equal(k1, k2)
})

test_that("LLM pipeline handles missing API key gracefully by flagging review status", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  init_recipe_db(tmp_db)
  con <- dbConnect(SQLite(), tmp_db)

  dbExecute(con, "INSERT INTO recipes (recipe_id, title, prep_min, cook_min) VALUES (1, 'Salade', 10, 0)")
  dbExecute(con, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '1 concombre', 'concombre')")
  dbDisconnect(con)

  normalize_all_ingredients(tmp_db)

  q_path <- if (file.exists("inst/dictionaries/tag_questions.csv")) "inst/dictionaries/tag_questions.csv" else "../../inst/dictionaries/tag_questions.csv"
  t_path <- if (file.exists("inst/dictionaries/classification_thresholds.csv")) "inst/dictionaries/classification_thresholds.csv" else "../../inst/dictionaries/classification_thresholds.csv"

  run_llm_classification_pipeline(
    db_path = tmp_db,
    questions_path = q_path,
    thresholds_path = t_path,
    api_key = "", # empty API key
    model = "google/gemini-2.5-flash"
  )

  con <- dbConnect(SQLite(), tmp_db)
  res <- dbGetQuery(con, "SELECT tag_name, tag_value, confidence, status FROM recipe_tag_classifications WHERE recipe_id = 1")
  dbDisconnect(con)

  expect_true(nrow(res) > 0)
  expect_true(all(res$status == "review"))
})
