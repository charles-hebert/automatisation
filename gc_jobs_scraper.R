suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(rvest)
  library(stringr)
  library(tidyr)
  library(tibble)
})

# -----------------------------------------------------------------------------
# GC Jobs scraper for OSFI and PSIC.
#
# Notes:
# - Uses CSS selectors (preferred over XPath for stability).
# - Supports direct URL parameter filtering by department id(s), which is faster
#   than clicking dropdown filters in a browser session.
# - If the site layout changes or is JS-rendered only, use RSelenium/chromote as
#   a fallback to render and then pass the HTML into the same parsing functions.
# -----------------------------------------------------------------------------

base_url <- "https://emploisfp-psjobs.cfp-psc.gc.ca/psrs-srfp/applicant/page2440"

# If you know the exact department IDs, add/update them here.
# The script still filters by department name text to keep results safe.
department_ids <- c(
  OSFI = 119,
  PSIC = 140
)

target_departments <- c(
  "Office of the Superintendent of Financial Institutions",
  "Office of the Public Sector Integrity Commissioner"
)
target_departments_regex <- regex(paste(target_departments, collapse = "|"), ignore_case = TRUE)

safe_html_text <- possibly(
  .f = function(node, css) {
    node %>% html_element(css = css) %>% html_text2() %>% str_squish()
  },
  otherwise = NA_character_
)

safe_attr <- possibly(
  .f = function(node, css, attr_name = "href") {
    node %>% html_element(css = css) %>% html_attr(attr_name)
  },
  otherwise = NA_character_
)

extract_level <- function(x) {
  str_extract(x %||% "", "[A-Z]{2}-\\d{2}")
}

parse_date_multi <- function(x) {
  x <- str_squish(x %||% "")
  out <- as.Date(x, format = "%Y-%m-%d")

  if (is.na(out)) out <- as.Date(x, format = "%B %d, %Y")
  if (is.na(out)) out <- as.Date(x, format = "%d %B %Y")
  if (is.na(out)) out <- as.Date(x, format = "%d/%m/%Y")

  out
}

# Build URL by department id. tab=1 is the jobs list tab.
build_jobs_url <- function(department_id = NULL, tab = 1) {
  query <- list(tab = tab)

  if (!is.null(department_id)) {
    query$department <- department_id
  }

  paste0(base_url, "?", paste0(names(query), "=", query, collapse = "&"))
}

extract_rows <- function(page) {
  # Prefer table rows if present.
  tr_nodes <- page %>% html_elements("table tbody tr")

  if (length(tr_nodes) > 0) {
    return(tr_nodes)
  }

  # Fallback for card-like list layouts.
  page %>% html_elements("div.search-results > div, div.result-item, div.job-item")
}

parse_job_row <- function(row_node, department_hint = NA_character_) {
  row_text <- row_node %>% html_text2() %>% str_squish()

  title <- coalesce(
    safe_html_text(row_node, "a"),
    safe_html_text(row_node, "td:nth-child(1)"),
    NA_character_
  )

  closing_raw <- coalesce(
    safe_html_text(row_node, ".closing-date"),
    safe_html_text(row_node, "td:nth-child(4)"),
    str_extract(row_text, "(\\d{4}-\\d{2}-\\d{2})|(\\d{1,2}/\\d{1,2}/\\d{4})|([A-Za-z]+\\s+\\d{1,2},\\s+\\d{4})")
  )

  location <- coalesce(
    safe_html_text(row_node, ".location"),
    safe_html_text(row_node, "td:nth-child(3)"),
    NA_character_
  )

  process_type <- coalesce(
    safe_html_text(row_node, ".process-type"),
    str_extract(row_text, regex("Non-?advertised|Advertised", ignore_case = TRUE)),
    NA_character_
  )

  language_req <- coalesce(
    safe_html_text(row_node, ".language"),
    safe_html_text(row_node, ".linguistic-profile"),
    str_extract(row_text, "([A-Z]{3}/[A-Z]{3})|([A-Z]{3})"),
    NA_character_
  )

  employer <- coalesce(
    safe_html_text(row_node, ".department"),
    safe_html_text(row_node, "td:nth-child(2)"),
    department_hint,
    NA_character_
  )

  tibble(
    Title = str_squish(title),
    Date = parse_date_multi(closing_raw),
    Level = extract_level(row_text),
    Location = str_squish(location),
    Process_Type = str_to_title(str_squish(process_type)),
    Language_Req = str_squish(language_req),
    Department = str_squish(employer),
    Detail_URL = safe_attr(row_node, "a", "href")
  )
}

fetch_department_jobs <- function(department_id = NULL, department_name = NA_character_) {
  url <- build_jobs_url(department_id = department_id)

  message("Fetching: ", url)

  page <- read_html(url)
  rows <- extract_rows(page)

  if (length(rows) == 0) {
    return(tibble())
  }

  map_dfr(rows, parse_job_row, department_hint = department_name)
}

jobs_raw <- imap_dfr(department_ids, ~ {
  fetch_department_jobs(department_id = .x, department_name = .y)
})

jobs_final <- jobs_raw %>%
  mutate(
    Department = str_squish(Department),
    Title = str_squish(Title),
    Location = str_squish(Location),
    Language_Req = na_if(str_squish(Language_Req), ""),
    Process_Type = case_when(
      str_detect(Process_Type, regex("non-?advertised", ignore_case = TRUE)) ~ "Non-advertised",
      str_detect(Process_Type, regex("advertised", ignore_case = TRUE)) ~ "Advertised",
      TRUE ~ NA_character_
    ),
    Level = coalesce(Level, str_extract(Title, "[A-Z]{2}-\\d{2}"))
  ) %>%
  filter(
    str_detect(Department, target_departments_regex) |
      str_detect(Title, target_departments_regex)
  ) %>%
  distinct(Title, Date, Level, Location, Process_Type, Language_Req, Department, .keep_all = TRUE) %>%
  arrange(Date, Title) %>%
  select(Title, Date, Level, Location, Process_Type, Language_Req, Department, Detail_URL)

write_csv(jobs_final, "gc_jobs_output.csv")

message("Done. Rows written: ", nrow(jobs_final))
print(jobs_final)
