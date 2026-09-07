#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript scripts/01_extract_recipes_from_sitemap.R <sitemap_url> <output_csv> [recipe_pattern]")
}

sitemap_url <- args[[1]]
output_csv <- args[[2]]
recipe_pattern <- ifelse(length(args) >= 3, args[[3]], "recipe|recipes")

source("R/sitemap_utils.R")

all_urls <- extract_locs_from_sitemap(sitemap_url)
recipe_urls <- filter_recipe_urls(all_urls, recipe_pattern = recipe_pattern)
out <- initial_recipe_table(recipe_urls)

dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
write_csv(out, output_csv)

message(sprintf("Done. %d recipe candidate URLs written to %s", nrow(out), output_csv))
message("Set selected='y' for rows you want to scrape in the next step.")
