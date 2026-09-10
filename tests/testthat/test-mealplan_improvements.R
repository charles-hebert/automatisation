test_that("Bilingual ingredient normalization works across French and English terms", {
  source("../../R/extract_inventory.R", local = TRUE)
  source("../../R/grocery_deals.R", local = TRUE)

  # Check matching french to english canonical
  matched_fr <- match_canonical_names(c("carotte", "pomme", "poulet"), dict_path = "../../inst/dictionaries/bilingual_ingredients.csv")
  expect_equal(matched_fr, c("carrot", "apple", "chicken"))

  deals_df <- data.frame(name = c("Fresh Carrots 2lb", "Filet de Poulet"), stringsAsFactors = FALSE)
  matched_deals <- match_deals_to_ingredients(deals_df, dict_path = "../../inst/dictionaries/bilingual_ingredients.csv")
  expect_equal(matched_deals$matched_canonical_ingredient, c("carrot", "chicken"))
})

test_that("Configurable seasonality matching works with seasonal_ingredients.csv", {
  source("../../R/mealplan_data_model.R", local = TRUE)

  sep_items <- load_seasonal_ingredients(dict_path = "../../inst/dictionaries/seasonal_ingredients.csv", target_month = "September")
  expect_true("apples" %in% sep_items || "tomatoes" %in% sep_items || "peppers" %in% sep_items)

  # Test recipe dataset loading with seasonal matching
  sample_recipes <- data.frame(
    recipe_id = c("1", "2"),
    title = c("Pomme Tarte", "Beef Stew"),
    prep_time_min = c(15, 40),
    tags = I(list(c("vegetarien", "mediterraneen"), c("comfort_food"))),
    stringsAsFactors = FALSE
  )
  sample_ingredients <- data.frame(
    recipe_id = c("1", "2"),
    raw_text = c("2 apples sliced", "500g beef"),
    stringsAsFactors = FALSE
  )

  ds <- load_recipe_dataset(
    recipes_df = sample_recipes,
    ingredients_df = sample_ingredients,
    seasonal_dict_path = "../../inst/dictionaries/seasonal_ingredients.csv",
    target_month = "September"
  )

  expect_equal(ds$seasonal_matches[ds$recipe_id == "1"], 1)
  expect_equal(ds$seasonal_matches[ds$recipe_id == "2"], 0)
})

test_that("LLM rationale report generation works and persists to SQLite database", {
  source("../../R/recipe_db.R", local = TRUE)
  source("../../R/mealplan_rationale.R", local = TRUE)

  tmp_db <- tempfile(fileext = ".db")
  tmp_md <- tempfile(fileext = ".md")
  init_recipe_db(tmp_db)

  mock_schedule <- data.frame(
    slot_index = 1:7,
    day = c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche"),
    is_weekday = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    f_present = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    weather_hot_sunny = rep(FALSE, 7),
    recipe_id = as.character(1:7),
    recipe_name = paste("Recipe", 1:7),
    prep_time_min = rep(20, 7),
    method = rep("standard", 7),
    tags = I(rep(list(c("vegetarien")), 7)),
    source = rep("Book A", 7),
    is_favorite = rep(TRUE, 7),
    good_weather = rep(FALSE, 7),
    fridge_garden_matches = rep(1, 7),
    grocery_special_matches = rep(1, 7),
    seasonal_matches = rep(1, 7),
    fiber_g = rep(30, 7),
    protein_g = rep(50, 7),
    magnesium_mg = rep(400, 7),
    gut_health_score = rep(6, 7),
    stringsAsFactors = FALSE
  )

  mock_weekly_plan <- list(
    schedule = mock_schedule,
    tier_scores = list(
      nutrition_slack = 0,
      fridge_garden_matches = 7,
      grocery_special_matches = 7,
      seasonal_matches = 7,
      weather_fit = 0
    )
  )

  mock_llm <- function(prompt) {
    "# Mock Rationale Report\n\n## 1. Rationale\nSelected for high fiber and seasonal fit.\n\n## 2. Food Science\nUtilize Maillard reaction."
  }

  res_md <- generate_mealplan_rationale(
    weekly_plan_result = mock_weekly_plan,
    api_key = "fake_key",
    db_path = tmp_db,
    output_md_path = tmp_md,
    week_label = "Test Week",
    mock_response = mock_llm
  )

  expect_true(file.exists(tmp_md))
  expect_true(grepl("Maillard reaction", res_md))

  conn <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  reports <- DBI::dbGetQuery(conn, "SELECT * FROM mealplan_reports")
  DBI::dbDisconnect(conn)

  expect_equal(nrow(reports), 1)
  expect_equal(reports$week_label, "Test Week")
  expect_true(grepl("Maillard reaction", reports$report_markdown))
})
