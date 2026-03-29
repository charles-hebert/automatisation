#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/02_scrape_selected_recipes.R <input_csv> <output_csv>")
}

input_csv <- args[[1]]
output_csv <- args[[2]]

source("R/sitemap_utils.R")

recipe_list <- read_csv(input_csv, show_col_types = FALSE)
required_cols <- c("recipe_url", "selected")
missing_cols <- setdiff(required_cols, names(recipe_list))
if (length(missing_cols) > 0) {
  stop(sprintf("Input CSV is missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

selected_urls <- recipe_list |>
  filter(str_to_lower(selected) == "y") |>
  pull(recipe_url) |>
  unique()

if (length(selected_urls) == 0) {
  warning("No rows selected (selected='y'). Writing an empty output file.")
  write_csv(tibble(recipe_url = character(), title = character(), ingredients = character(), instructions = character()), output_csv)
  quit(save = "no")
}

results <- map_dfr(selected_urls, scrape_recipe_page)
dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
write_csv(results, output_csv)

message(sprintf("Done. Scraped %d selected recipes to %s", nrow(results), output_csv))
