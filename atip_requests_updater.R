#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(xml2)
  library(rvest)
})

# -------------------------
# Configuration (override via env vars)
# Example CRON:
#   ATIP_DEPARTMENT=\"Office of the Superintendent of Financial Institutions Canada\" \\
#   ATIP_OUTPUT_CSV=\"/data/atip_requests.csv\" \\
#   Rscript /path/to/atip_requests_updater.R
# -------------------------
department <- Sys.getenv(
  "ATIP_DEPARTMENT",
  "Office of the Superintendent of Financial Institutions Canada"
)
output_csv <- Sys.getenv("ATIP_OUTPUT_CSV", "atip_requests.csv")
search_base <- Sys.getenv("ATIP_SEARCH_BASE", "https://open.canada.ca/en/search/ati")
max_pages <- as.integer(Sys.getenv("ATIP_MAX_PAGES", "30"))
if (is.na(max_pages) || max_pages < 1) max_pages <- 30

# -------------------------
# Helpers
# -------------------------
clean_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

first_matching_col <- function(cols, patterns) {
  for (pattern in patterns) {
    idx <- grep(pattern, cols, ignore.case = TRUE)
    if (length(idx) > 0) return(cols[idx[1]])
  }
  NA_character_
}

parse_any_date <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  formats <- c("%Y-%m-%d", "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d %b %Y", "%d %B %Y")
  out <- as.Date(rep(NA_character_, length(x)))

  for (fmt in formats) {
    missing <- is.na(out) & !is.na(x)
    if (!any(missing)) break
    out[missing] <- as.Date(x[missing], format = fmt)
  }

  out
}

extract_rows_from_url <- function(url) {
  page <- read_html(url)
  tables <- html_elements(page, "table")

  if (length(tables) == 0) {
    return(data.frame())
  }

  rows <- list()

  for (tbl in tables) {
    parsed <- tryCatch(html_table(tbl, fill = TRUE), error = function(e) NULL)
    if (is.null(parsed) || nrow(parsed) == 0 || ncol(parsed) == 0) next

    colnames(parsed) <- clean_names(colnames(parsed))

    date_col <- first_matching_col(colnames(parsed), c("^date$", "request_date", "received", "completion", "closed"))
    pages_col <- first_matching_col(colnames(parsed), c("pages", "number_of_pages", "page_count"))
    desc_col <- first_matching_col(colnames(parsed), c("description", "summary", "request_text", "subject"))
    file_col <- first_matching_col(colnames(parsed), c("file", "request_number", "reference", "a_?[0-9]{4}"))

    # fallback for file number if no obvious match
    if (is.na(file_col)) {
      candidate <- grep("number|no|id|ref", colnames(parsed), ignore.case = TRUE)
      if (length(candidate) > 0) file_col <- colnames(parsed)[candidate[1]]
    }

    if (any(is.na(c(date_col, pages_col, desc_col, file_col)))) next

    out <- data.frame(
      date = trimws(as.character(parsed[[date_col]])),
      pages = trimws(as.character(parsed[[pages_col]])),
      description = trimws(as.character(parsed[[desc_col]])),
      file_number = trimws(as.character(parsed[[file_col]])),
      stringsAsFactors = FALSE
    )

    out <- out[out$file_number != "" & !is.na(out$file_number), , drop = FALSE]
    if (nrow(out) == 0) next

    out$source_url <- url
    rows[[length(rows) + 1]] <- out
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

build_search_url <- function(base, dept) {
  encoded_dept <- URLencode(dept, reserved = TRUE)
  paste0(base, "?ati%5B0%5D=ati_organization_en%3A", encoded_dept)
}

# -------------------------
# Collect pages
# -------------------------
query_url <- build_search_url(search_base, department)
all_rows <- list()
empty_streak <- 0

for (page_idx in 0:(max_pages - 1)) {
  page_url <- if (page_idx == 0) query_url else paste0(query_url, "&page=", page_idx)

  message("Checking: ", page_url)
  page_rows <- tryCatch(extract_rows_from_url(page_url), error = function(e) {
    warning("Failed to parse ", page_url, " (", conditionMessage(e), ")")
    data.frame()
  })

  if (nrow(page_rows) == 0) {
    empty_streak <- empty_streak + 1
    if (empty_streak >= 2) break
    next
  }

  empty_streak <- 0
  all_rows[[length(all_rows) + 1]] <- page_rows
}

if (length(all_rows) == 0) {
  stop("No ATIP rows found. Check the department name or page structure.")
}

scraped <- do.call(rbind, all_rows)
scraped <- unique(scraped)

# Normalize data types
scraped$date_raw <- scraped$date
scraped$date <- parse_any_date(scraped$date)
scraped$pages <- suppressWarnings(as.integer(gsub("[^0-9]", "", scraped$pages)))
scraped$description <- gsub("\\s+", " ", scraped$description)
scraped$file_number <- gsub("\\s+", " ", scraped$file_number)
scraped$scraped_at_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
scraped$department <- department

# Keep requested columns first
scraped <- scraped[, c(
  "department", "date", "pages", "description", "file_number",
  "date_raw", "source_url", "scraped_at_utc"
)]

# -------------------------
# Merge with existing CSV and only append new requests
# -------------------------
if (file.exists(output_csv)) {
  existing <- tryCatch(read.csv(output_csv, stringsAsFactors = FALSE), error = function(e) data.frame())
} else {
  existing <- data.frame()
}

if (nrow(existing) > 0 && "file_number" %in% colnames(existing)) {
  new_rows <- scraped[!(scraped$file_number %in% existing$file_number), , drop = FALSE]
  combined <- rbind(existing, new_rows)
} else {
  new_rows <- scraped
  combined <- scraped
}

# sort newest first when date is available
if ("date" %in% colnames(combined)) {
  suppressWarnings({
    combined$date <- as.Date(combined$date)
  })
  combined <- combined[order(combined$date, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
}

write.csv(combined, output_csv, row.names = FALSE, na = "")

message("Department: ", department)
message("Total scraped this run: ", nrow(scraped))
message("New requests appended: ", nrow(new_rows))
message("CSV updated: ", output_csv)
