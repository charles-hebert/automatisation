suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
  library(readr)
  library(jsonlite)
})

source("R/recipe_db.R")
source("recipes_scraper/scripts/03_import_to_db.R")

testthat::test_that("init_recipe_db creates raw_sources table supporting file_type 'url'", {
  temp_db <- tempfile(fileext = ".db")
  on.exit(unlink(temp_db), add = TRUE)

  init_recipe_db(temp_db)

  conn <- dbConnect(SQLite(), temp_db)
  on.exit(dbDisconnect(conn), add = TRUE)

  expect_true("raw_sources" %in% dbListTables(conn))

  # Test inserting a url record
  expect_no_error({
    dbExecute(conn, "
      INSERT INTO raw_sources (file_hash, file_name, file_type, raw_content, status)
      VALUES ('hash123', 'http://example.com/recipe1', 'url', '{\"title\":\"Test\"}', 'pending')
    ")
  })

  res <- dbGetQuery(conn, "SELECT * FROM raw_sources WHERE file_hash = 'hash123'")
  expect_equal(nrow(res), 1)
  expect_equal(res$file_type, "url")
  expect_equal(res$status, "pending")
})

testthat::test_that("import_scraped_recipes successfully imports CSV into SQLite database", {
  temp_db <- tempfile(fileext = ".db")
  temp_csv <- tempfile(fileext = ".csv")
  on.exit({
    unlink(temp_db)
    unlink(temp_csv)
  }, add = TRUE)

  sample_data <- tibble::tibble(
    recipe_url = c("https://example.com/recipes/pasta", "https://example.com/recipes/salad"),
    title = c("Pasta Carbonara", "Green Salad"),
    ingredients = c("Spaghetti | Eggs | Bacon", "Lettuce | Tomatoes | Olive Oil"),
    instructions = c("Boil pasta | Mix eggs | Combine", "Wash lettuce | Chop tomatoes | Mix")
  )
  write_csv(sample_data, temp_csv)

  count <- import_scraped_recipes(temp_csv, db_path = temp_db)
  expect_equal(count, 2)

  conn <- dbConnect(SQLite(), temp_db)
  on.exit(dbDisconnect(conn), add = TRUE)

  sources <- dbGetQuery(conn, "SELECT * FROM raw_sources ORDER BY source_id")
  expect_equal(nrow(sources), 2)
  expect_equal(sources$file_type, c("url", "url"))
  expect_equal(sources$status, c("pending", "pending"))

  content1 <- fromJSON(sources$raw_content[[1]])
  expect_equal(content1$recipe_url, "https://example.com/recipes/pasta")
  expect_equal(content1$title, "Pasta Carbonara")
  expect_equal(content1$ingredients, "Spaghetti | Eggs | Bacon")
  expect_equal(content1$instructions, "Boil pasta | Mix eggs | Combine")
})

testthat::test_that("import_scraped_recipes updates existing record when recipe_url is repeated", {
  temp_db <- tempfile(fileext = ".db")
  temp_csv1 <- tempfile(fileext = ".csv")
  temp_csv2 <- tempfile(fileext = ".csv")
  on.exit({
    unlink(temp_db)
    unlink(temp_csv1)
    unlink(temp_csv2)
  }, add = TRUE)

  sample1 <- tibble::tibble(
    recipe_url = "https://example.com/recipes/soup",
    title = "Tomato Soup",
    ingredients = "Tomatoes | Water",
    instructions = "Boil tomatoes"
  )
  write_csv(sample1, temp_csv1)
  import_scraped_recipes(temp_csv1, db_path = temp_db)

  sample2 <- tibble::tibble(
    recipe_url = "https://example.com/recipes/soup",
    title = "Rich Tomato Soup",
    ingredients = "Tomatoes | Water | Cream",
    instructions = "Boil tomatoes | Add cream"
  )
  write_csv(sample2, temp_csv2)
  import_scraped_recipes(temp_csv2, db_path = temp_db)

  conn <- dbConnect(SQLite(), temp_db)
  on.exit(dbDisconnect(conn), add = TRUE)

  sources <- dbGetQuery(conn, "SELECT * FROM raw_sources WHERE file_name = 'https://example.com/recipes/soup'")
  expect_equal(nrow(sources), 1)

  content <- fromJSON(sources$raw_content[[1]])
  expect_equal(content$title, "Rich Tomato Soup")
  expect_equal(content$ingredients, "Tomatoes | Water | Cream")
})
