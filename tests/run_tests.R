#!/usr/bin/env Rscript

# Ensure packages required for running tests
repos <- "https://cloud.r-project.org"
required_pkgs <- c("httr2", "jsonlite", "dplyr", "purrr", "DBI", "RSQLite", "testthat")

missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  message("Installing missing packages for test runner: ", paste(missing_pkgs, collapse = ", "))
  tryCatch({
    install.packages(missing_pkgs, repos = repos, dependencies = TRUE)
  }, error = function(e) {
    warning("Automated package installation warning: ", e$message)
  })
}

for (pkg in required_pkgs) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

message("=== Running Ottawa Grocery Deals Test Suite ===")
test_results <- testthat::test_file("tests/test_grocery_deals.R")
print(test_results)

if (any(as.data.frame(test_results)$failed > 0) || any(as.data.frame(test_results)$error)) {
  stop("Some tests failed!")
} else {
  message("All tests passed successfully!")
}
