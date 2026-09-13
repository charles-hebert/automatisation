library(testthat)

# Helper function to source optimization script safely
source_script <- function() {
  if (file.exists("R/optimisation_ingredients_de_base.R")) {
    source("R/optimisation_ingredients_de_base.R")
  } else if (file.exists("../../R/optimisation_ingredients_de_base.R")) {
    source("../../R/optimisation_ingredients_de_base.R")
  }
}

source_script()

test_that("get_pantry_ingredients correctly extracts pantry items", {
  # Create a temporary dictionary file
  temp_dict <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_dict), add = TRUE)

  dict_data <- data.frame(
    canonical_name = c("rice", "apple", "coffee"),
    french_name = c("riz", "pomme", "café"),
    english_name = c("rice", "apple", "coffee"),
    category = c("pantry", "produce", "pantry"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(dict_data, temp_dict)

  pantry <- get_pantry_ingredients(dict_path = temp_dict)
  expect_equal(nrow(pantry), 2)
  expect_true(all(pantry$category == "pantry"))
  expect_setequal(pantry$canonical_name, c("rice", "coffee"))
})

test_that("extract_pantry_deals matches deals from database and writes CSV", {
  temp_dict <- tempfile(fileext = ".csv")
  temp_db <- tempfile(fileext = ".db")
  temp_csv <- tempfile(fileext = ".csv")
  on.exit({
    unlink(temp_dict)
    unlink(temp_db)
    unlink(temp_csv)
  }, add = TRUE)

  # 1. Mock dictionary
  dict_data <- data.frame(
    canonical_name = c("rice", "coffee"),
    french_name = c("riz", "café"),
    english_name = c("rice", "coffee"),
    category = c("pantry", "pantry"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(dict_data, temp_dict)

  # 2. Mock SQLite database with grocery_deals table
  conn <- DBI::dbConnect(RSQLite::SQLite(), temp_db)
  mock_deals <- data.frame(
    merchant = c("Metro", "Loblaws", "Farm Boy"),
    name = c("Uncle Ben Rice 1kg", "Gala Apples", "Coffee Beans"),
    current_price = c("3.99", "1.99", "8.99"),
    pre_price = c("4.99", "2.99", "10.99"),
    valid_to = c("2025-05-01", "2025-05-01", "2025-05-01"),
    category = c("Pantry", "Produce", "Pantry"),
    matched_canonical_ingredient = c("rice", "apple", NA),
    postal_code = c("K2C 1K1", "K2C 1K1", "K2C 1K1"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(conn, "grocery_deals", mock_deals)
  DBI::dbDisconnect(conn)

  # 3. Test extract_pantry_deals
  res <- extract_pantry_deals(db_path = temp_db, dict_path = temp_dict, output_csv = temp_csv)

  expect_equal(nrow(res), 2) # "Uncle Ben Rice 1kg" matched by matched_canonical_ingredient 'rice', "Coffee Beans" matched by fallback term search 'coffee'
  expect_setequal(res$pantry_canonical_name, c("rice", "coffee"))
  expect_true(file.exists(temp_csv))

  exported_df <- readr::read_csv(temp_csv, show_col_types = FALSE)
  expect_equal(nrow(exported_df), 2)
})
