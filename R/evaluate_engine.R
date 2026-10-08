# Evaluation engine for measuring classification accuracy against gold standard labels.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(jsonlite)
  library(logger)
})

find_and_source_internal <- function(rel_path) {
  candidates <- c(rel_path, file.path("../..", rel_path), file.path("..", rel_path))
  for (cand in candidates) {
    if (file.exists(cand)) {
      source(cand)
      return(invisible(TRUE))
    }
  }
}
find_and_source_internal("R/config.R")
find_and_source_internal("R/recipe_db.R")
find_and_source_internal("R/normalize_ingredients.R")
find_and_source_internal("R/deterministic_tags.R")
find_and_source_internal("R/engine_llm.R")

find_file_path_eval <- function(rel_path) {
  candidates <- c(rel_path, file.path("../..", rel_path), file.path("..", rel_path))
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  rel_path
}

#' Evaluate tagging accuracy against a gold standard labels CSV.
#'
#' @param db_path Path to SQLite database populated with evaluation recipes
#' @param mock_recipes_path Path to mock recipes JSON fixture
#' @param gold_labels_path Path to gold labels CSV
#' @param questions_path Path to questions CSV
#' @param thresholds_path Path to thresholds CSV
#' @param api_key OpenRouter API key
#' @param model OpenRouter model ID
#' @return List containing summary metrics and mismatch details
evaluate_engine <- function(db_path = "recipes.db",
                            mock_recipes_path = "tests/testthat/fixtures/mock_recipes.json",
                            gold_labels_path = "tests/testthat/fixtures/gold_labels.csv",
                            questions_path = "inst/dictionaries/tag_questions.csv",
                            thresholds_path = "inst/dictionaries/classification_thresholds.csv",
                            api_key = Sys.getenv("OPENROUTER_API_KEY"),
                            model = "google/gemini-2.5-flash") {
  real_gold_path <- find_file_path_eval(gold_labels_path)
  real_mock_path <- find_file_path_eval(mock_recipes_path)
  real_questions_path <- find_file_path_eval(questions_path)
  real_thresholds_path <- find_file_path_eval(thresholds_path)

  if (!file.exists(real_gold_path)) {
    stop("Gold labels file not found: ", gold_labels_path)
  }

  gold_df <- read.csv(real_gold_path, stringsAsFactors = FALSE)

  # Seed mock recipes into DB if fixture exists
  if (file.exists(real_mock_path)) {
    init_recipe_db(db_path)
    db <- dbConnect(SQLite(), db_path)

    mock_recs <- fromJSON(real_mock_path, simplifyVector = FALSE)
    for (rec in mock_recs) {
      rid <- rec$recipe_id
      dbExecute(db, "
        INSERT OR REPLACE INTO recipes (recipe_id, title, prep_min, cook_min)
        VALUES (?, ?, ?, ?)
      ", params = list(rid, rec$title, rec$prep_min %||% 0, rec$cook_min %||% 0))

      dbExecute(db, "DELETE FROM ingredients WHERE recipe_id = ?", params = list(rid))
      for (ing in rec$ingredients %||% list()) {
        dbExecute(db, "
          INSERT INTO ingredients (recipe_id, raw_text, canonical_name)
          VALUES (?, ?, ?)
        ", params = list(rid, ing$raw_text %||% "", ing$canonical_name %||% ""))
      }

      dbExecute(db, "DELETE FROM recipe_equipment WHERE recipe_id = ?", params = list(rid))
      for (eq in rec$equipment %||% list()) {
        dbExecute(db, "
          INSERT OR IGNORE INTO recipe_equipment (recipe_id, equipment_name)
          VALUES (?, ?)
        ", params = list(rid, as.character(eq)))
      }
    }
    dbDisconnect(db)
  }

  # Run normalization, deterministic rules, and LLM classification
  normalize_all_ingredients(db_path)
  run_deterministic_rules(db_path)

  if (nzchar(api_key)) {
    run_llm_classification_pipeline(
      db_path = db_path,
      questions_path = real_questions_path,
      thresholds_path = real_thresholds_path,
      api_key = api_key,
      model = model
    )
  } else {
    log_warn("API key not supplied; evaluating deterministic rules only.")
  }

  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)

  actual_df <- dbGetQuery(db, "
    SELECT recipe_id, tag_name, tag_value, confidence, status, tag_source
    FROM recipe_tag_classifications
  ")

  merged <- merge(gold_df, actual_df, by = c("recipe_id", "tag_name"), all.x = TRUE)

  merged$expected_value <- tolower(trimws(merged$expected_value))
  merged$tag_value <- tolower(trimws(merged$tag_value %||% "unknown"))

  merged$is_match <- (merged$expected_value == merged$tag_value)

  total_evals <- nrow(merged)
  correct_matches <- sum(merged$is_match, na.rm = TRUE)
  accuracy_pct <- if (total_evals > 0) (correct_matches / total_evals) * 100 else 0

  mismatches <- merged[!merged$is_match, ]

  log_info("Evaluation Completed: {correct_matches}/{total_evals} correct ({sprintf('%.1f', accuracy_pct)}% accuracy).")

  list(
    total_evaluations = total_evals,
    correct_matches = correct_matches,
    accuracy_pct = accuracy_pct,
    mismatches = mismatches,
    results_df = merged
  )
}
