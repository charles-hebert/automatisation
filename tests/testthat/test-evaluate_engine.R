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

find_and_source("R/evaluate_engine.R")

test_that("evaluate_engine executes and measures accuracy against gold standard set", {
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db))

  mock_p <- if (file.exists("fixtures/mock_recipes.json")) "fixtures/mock_recipes.json" else "tests/testthat/fixtures/mock_recipes.json"
  gold_p <- if (file.exists("fixtures/gold_labels.csv")) "fixtures/gold_labels.csv" else "tests/testthat/fixtures/gold_labels.csv"
  q_p <- if (file.exists("inst/dictionaries/tag_questions.csv")) "inst/dictionaries/tag_questions.csv" else "../../inst/dictionaries/tag_questions.csv"
  t_p <- if (file.exists("inst/dictionaries/classification_thresholds.csv")) "inst/dictionaries/classification_thresholds.csv" else "../../inst/dictionaries/classification_thresholds.csv"

  eval_res <- evaluate_engine(
    db_path = tmp_db,
    mock_recipes_path = mock_p,
    gold_labels_path = gold_p,
    questions_path = q_p,
    thresholds_path = t_p,
    api_key = "", # deterministic evaluation only
    model = "google/gemini-2.5-flash"
  )

  expect_equal(eval_res$total_evaluations, 60)
  expect_true(eval_res$correct_matches >= 25) # 25 deterministic rules perfectly matched
})
