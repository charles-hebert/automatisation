# Defensive package loading for tests
if (!exists("ensure_packages", mode = "function")) {
  if (file.exists("R/grocery_deals.R")) {
    source("R/grocery_deals.R")
  } else {
    stop("R/grocery_deals.R not found.")
  }
}

ensure_packages(c("httr2", "jsonlite", "dplyr", "purrr", "DBI", "RSQLite", "testthat"))

source("R/recipe_db.R")
source("R/grocery_deals.R")

testthat::test_that("ensure_packages handles package verification", {
  testthat::expect_silent(ensure_packages(c("dplyr", "jsonlite")))
})

testthat::test_that("extract_grocery_deals filters target Ottawa grocers correctly from raw items", {
  mock_items <- list(
    list(merchant_name = "Metro", name = "Fresh Boneless Chicken Breast", current_price = "$4.99/lb", category_name = "Meat"),
    list(merchant_name = "Farm Boy", name = "Organic Strawberries 454g", current_price = "$3.49", category_name = "Produce"),
    list(merchant_name = "Loblaws", name = "Whole Milk 4L", current_price = "$5.29", category_name = "Dairy"),
    list(merchant_name = "Walmart", name = "Paper Towels", current_price = "$12.99", category_name = "Household"),
    list(merchant_name = "Costco", name = "Bulk Cheese", current_price = "$15.99", category_name = "Dairy")
  )

  deals <- extract_grocery_deals(
    postal_code = "K2C 1K1",
    target_stores = c("Metro", "Farm Boy", "Loblaws"),
    mock_data = mock_items
  )

  testthat::expect_equal(nrow(deals), 3)
  testthat::expect_setequal(unique(deals$merchant), c("Metro", "Farm Boy", "Loblaws"))
  testthat::expect_false("Walmart" %in% deals$merchant)
  testthat::expect_false("Costco" %in% deals$merchant)
})

testthat::test_that("match_deals_to_ingredients maps deal items to canonical ingredient names", {
  mock_deals <- dplyr::tibble(
    merchant = c("Metro", "Farm Boy"),
    name = c("Organic Fresh Strawberries", "Boneless Chicken Breast"),
    current_price = c("$3.99", "$5.99"),
    pre_price = c(NA_character_, NA_character_),
    valid_to = c(NA_character_, NA_character_),
    category = c("Produce", "Meat"),
    postal_code = c("K2C 1K1", "K2C 1K1")
  )

  canonicals <- c("strawberries", "chicken", "carrots", "milk")
  matched_deals <- match_deals_to_ingredients(mock_deals, canonical_list = canonicals)

  testthat::expect_equal(matched_deals$matched_canonical_ingredient[1], "strawberries")
  testthat::expect_equal(matched_deals$matched_canonical_ingredient[2], "chicken")
})

testthat::test_that("run_grocery_deals_pipeline end-to-end with temporary SQLite database", {
  temp_db <- tempfile(fileext = ".db")
  on.exit(unlink(temp_db), add = TRUE)

  # Initialize DB & populate sample canonical ingredients
  init_recipe_db(temp_db)
  conn <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  DBI::dbExecute(conn, "INSERT INTO recipes (title) VALUES ('Berry Smoothie')")
  DBI::dbExecute(conn, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '1 cup fresh strawberries', 'strawberries')")
  DBI::dbExecute(conn, "INSERT INTO ingredients (recipe_id, raw_text, canonical_name) VALUES (1, '2 cups milk', 'milk')")
  DBI::dbDisconnect(conn)

  mock_items <- list(
    list(merchant_name = "Metro Ottawa", name = "Fresh Strawberries 1lb", current_price = "$2.99", category_name = "Produce"),
    list(merchant_name = "Loblaws", name = "Natrel Whole Milk 4L", current_price = "$5.49", category_name = "Dairy")
  )

  result <- run_grocery_deals_pipeline(
    db_path = temp_db,
    postal_code = "K2C 1K1",
    target_stores = c("Metro", "Farm Boy", "Loblaws"),
    mock_data = mock_items
  )

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$matched_canonical_ingredient[1], "strawberries")

  # Verify persistence in DB
  conn <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  saved_deals <- DBI::dbReadTable(conn, "grocery_deals")
  DBI::dbDisconnect(conn)

  testthat::expect_equal(nrow(saved_deals), 2)
  testthat::expect_equal(saved_deals$merchant[1], "Metro Ottawa")
})
