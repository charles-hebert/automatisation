#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(readr)
  library(rvest)
  library(stringr)
  library(lubridate)
  library(tidyr)
  library(tibble)
  library(httr2)
  library(xml2)
})

DEFAULT_TWITTER_MAPPING <- "twitter_handles_template.csv"
DEFAULT_OUTPUT_DIR <- "."
HOUSE_BASE_URL <- "https://www.ourcommons.ca"
SENATE_BASE_URL <- "https://sencanada.ca"
REQUIRED_COMMITTEE_CONTACTS <- tibble(
  chamber = c("house", "senate"),
  member_name = c("Adam Chambers", "Rosa Galvez"),
  twitter_handle = c("@adamchambersmp", "@SenRosaGalvez")
)

required_output_columns <- c(
  "organization_name",
  "representative_name",
  "committee_name",
  "meeting_date",
  "committee_members",
  "twitter_handles"
)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")) y else x
}

cli_value <- function(name, default = NULL) {
  args <- commandArgs(trailingOnly = TRUE)
  prefix <- paste0("--", name, "=")
  matches <- args[startsWith(args, prefix)]
  if (length(matches) > 0) {
    return(sub(prefix, "", matches[[1]], fixed = TRUE))
  }

  env_name <- toupper(gsub("-", "_", name))
  env_value <- Sys.getenv(env_name, unset = "")
  if (nzchar(env_value)) {
    return(env_value)
  }

  default
}

normalize_text <- function(x) {
  x |>
    str_replace_all("[\r\n\t]+", " ") |>
    str_replace_all("\\u00a0", " ") |>
    str_squish()
}

safe_name <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_replace_all("(^_+|_+$)", "")
}

message_info <- function(...) {
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
}

fetch_html <- function(url) {
  req <- request(url) |>
    req_user_agent("canadian-parliamentary-appearance-tracker/1.0") |>
    req_retry(max_tries = 2)

  req |>
    req_perform() |>
    resp_body_html()
}

extract_page_text <- function(doc) {
  doc |>
    html_element("body") |>
    html_text2() |>
    normalize_text()
}

make_absolute_url <- function(url, base) {
  ifelse(str_detect(url, "^https?://"), url, paste0(base, url))
}

extract_date_time <- function(page_text) {
  date_match <- str_match(
    page_text,
    "((Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday),\\s+[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4})"
  )
  time_match <- str_match(
    page_text,
    "(\\d{1,2}:\\d{2}\\s*(a\\.m\\.|p\\.m\\.|AM|PM)(?:\\s*(?:ET|EDT|EST))?)"
  )

  if (is.na(date_match[1, 2])) {
    return(NA_character_)
  }

  paste(date_match[1, 2], time_match[1, 2] %||% "") |> str_squish()
}

read_twitter_mapping <- function(path) {
  if (!file.exists(path)) {
    message_info("Twitter mapping file not found at ", path, "; continuing with empty handle lookup.")
    return(tibble(
      chamber = character(),
      committee_code = character(),
      member_name = character(),
      twitter_handle = character()
    ))
  }

  read_csv(path, show_col_types = FALSE) |>
    mutate(
      chamber = coalesce(chamber, "") |> str_to_lower(),
      committee_code = coalesce(committee_code, "") |> str_to_upper(),
      member_name = coalesce(member_name, "") |> normalize_text(),
      twitter_handle = coalesce(twitter_handle, "") |> str_trim()
    )
}

attach_handles <- function(target_chamber, target_committee_code, member_names, twitter_map) {
  required_contacts <- REQUIRED_COMMITTEE_CONTACTS |>
    filter(chamber == str_to_lower(target_chamber))

  normalized_members <- c(member_names, required_contacts$member_name) |>
    normalize_text() |>
    discard(~ !nzchar(.x)) |>
    unique()

  if (length(normalized_members) == 0) {
    return(list(committee_members = NA_character_, twitter_handles = NA_character_))
  }

  handle_rows <- twitter_map |>
    filter(
      chamber == str_to_lower(target_chamber),
      committee_code == str_to_upper(target_committee_code),
      member_name %in% normalized_members
    )

  handles <- c(
    handle_rows |>
      distinct(twitter_handle) |>
      pull(twitter_handle),
    required_contacts$twitter_handle
  ) |>
    discard(~ is.na(.x) || !nzchar(.x)) |>
    unique()

  list(
    committee_members = paste(normalized_members, collapse = " | "),
    twitter_handles = if (length(handles) == 0) NA_character_ else paste(handles, collapse = " | ")
  )
}

extract_house_notice_urls <- function(date_value) {
  business_url <- sprintf("%s/en/parliamentary-business/%s", HOUSE_BASE_URL, as.character(date_value))
  doc <- fetch_html(business_url)

  doc |>
    html_elements(xpath = "//a[contains(@href, '/notice')]") |>
    html_attr("href") |>
    unique() |>
    discard(is.na) |>
    discard(~ !nzchar(.x)) |>
    map_chr(make_absolute_url, base = HOUSE_BASE_URL)
}

extract_house_committee_members <- function(committee_code) {
  committee_url <- sprintf("%s/Committees/en/%s", HOUSE_BASE_URL, str_to_upper(committee_code))
  doc <- tryCatch(fetch_html(committee_url), error = function(e) NULL)
  if (is.null(doc)) {
    return(character())
  }

  doc |>
    html_elements(xpath = "//a[contains(@href, '/Members/en/')]") |>
    html_text2() |>
    normalize_text() |>
    discard(~ !nzchar(.x)) |>
    unique()
}

extract_house_representatives <- function(page_text, organization_pattern) {
  lines <- str_split(page_text, "(?<=\\.)\\s+|\\s{2,}", simplify = FALSE)[[1]]
  lines <- normalize_text(lines)

  matching_lines <- lines[str_detect(lines, regex(organization_pattern, ignore_case = TRUE))]
  if (length(matching_lines) == 0) {
    return(NA_character_)
  }

  cleaned <- matching_lines |>
    str_replace(regex(organization_pattern, ignore_case = TRUE), "") |>
    str_replace_all("^[,:;\\-]+|[,:;\\-]+$", "") |>
    str_squish() |>
    discard(~ !nzchar(.x))

  if (length(cleaned) == 0) {
    return(NA_character_)
  }

  paste(unique(cleaned), collapse = " | ")
}

parse_house_notice <- function(notice_url, organization_name, organization_pattern, twitter_map) {
  doc <- fetch_html(notice_url)
  page_text <- extract_page_text(doc)

  if (!str_detect(page_text, regex(organization_pattern, ignore_case = TRUE))) {
    return(NULL)
  }

  heading <- doc |>
    html_element("h1") |>
    html_text2() |>
    normalize_text()

  committee_code <- str_match(notice_url, "/([A-Z]+|[a-z]+)/meeting-")[[2]] |> str_to_upper()
  committee_name <- str_match(heading, "^(.*?)\\s+Committee Meeting$")[[2]] %||% heading
  members <- extract_house_committee_members(committee_code)
  member_payload <- attach_handles("house", committee_code, members, twitter_map)

  tibble(
    organization_name = organization_name,
    representative_name = extract_house_representatives(page_text, organization_pattern),
    committee_name = committee_name,
    meeting_date = extract_date_time(page_text),
    committee_members = member_payload$committee_members,
    twitter_handles = member_payload$twitter_handles,
    source_chamber = "House of Commons",
    source_url = notice_url
  )
}

extract_senate_notice_urls <- function() {
  schedule_url <- sprintf("%s/en/committees/allmeetings/", SENATE_BASE_URL)
  doc <- fetch_html(schedule_url)

  doc |>
    html_elements(xpath = "//a[contains(@href, '/noticeofmeeting/')]") |>
    html_attr("href") |>
    unique() |>
    discard(is.na) |>
    discard(~ !nzchar(.x)) |>
    map_chr(make_absolute_url, base = SENATE_BASE_URL)
}

extract_senate_committee_members <- function(committee_code) {
  membership_url <- sprintf("%s/en/committees/%s/members/", SENATE_BASE_URL, str_to_upper(committee_code))
  doc <- tryCatch(fetch_html(membership_url), error = function(e) NULL)
  if (is.null(doc)) {
    membership_url <- sprintf("%s/en/committees/%s/membership/", SENATE_BASE_URL, str_to_upper(committee_code))
    doc <- tryCatch(fetch_html(membership_url), error = function(e) NULL)
  }

  if (is.null(doc)) {
    return(character())
  }

  doc |>
    html_elements(xpath = "//a[contains(@href, '/senators/') or contains(@href, '/en/senators/')]") |>
    html_text2() |>
    normalize_text() |>
    discard(~ !nzchar(.x)) |>
    unique()
}

parse_senate_notice <- function(notice_url, organization_name, organization_pattern, twitter_map, start_date, end_date) {
  doc <- fetch_html(notice_url)
  page_text <- extract_page_text(doc)
  meeting_date_text <- extract_date_time(page_text)

  meeting_date_value <- suppressWarnings(parse_date_time(
    meeting_date_text,
    orders = c("A, B d, Y I:M p", "A, B d, Y H:M")
  ))

  if (!is.na(meeting_date_value)) {
    in_range <- as_date(meeting_date_value) >= start_date && as_date(meeting_date_value) <= end_date
    if (!in_range) {
      return(NULL)
    }
  }

  if (!str_detect(page_text, regex(organization_pattern, ignore_case = TRUE))) {
    return(NULL)
  }

  committee_code <- str_match(notice_url, "/committees/([A-Za-z0-9]+)/noticeofmeeting/")[[2]] |> str_to_upper()
  committee_name <- doc |>
    html_element("h1") |>
    html_text2() |>
    normalize_text()

  members <- extract_senate_committee_members(committee_code)
  member_payload <- attach_handles("senate", committee_code, members, twitter_map)

  tibble(
    organization_name = organization_name,
    representative_name = extract_house_representatives(page_text, organization_pattern),
    committee_name = committee_name,
    meeting_date = meeting_date_text,
    committee_members = member_payload$committee_members,
    twitter_handles = member_payload$twitter_handles,
    source_chamber = "Senate",
    source_url = notice_url
  )
}

empty_output <- function() {
  tibble(
    organization_name = character(),
    representative_name = character(),
    committee_name = character(),
    meeting_date = character(),
    committee_members = character(),
    twitter_handles = character()
  )
}

main <- function() {
  organization_name <- cli_value("organization-name", "OSFI")
  organization_aliases <- cli_value(
    "organization-aliases",
    "Office of the Superintendent of Financial Institutions|OSFI"
  )
  lookahead_days <- as.integer(cli_value("lookahead-days", "14"))
  start_date <- as_date(cli_value("start-date", as.character(Sys.Date())))
  end_date <- as_date(cli_value("end-date", as.character(start_date + days(lookahead_days))))
  output_dir <- cli_value("output-dir", DEFAULT_OUTPUT_DIR)
  twitter_map_path <- cli_value("twitter-map", DEFAULT_TWITTER_MAPPING)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  twitter_map <- read_twitter_mapping(twitter_map_path)

  message_info("Searching for parliamentary appearances for '", organization_name, "' between ", start_date, " and ", end_date, ".")

  house_dates <- seq.Date(start_date, end_date, by = "day")
  house_results <- map_dfr(house_dates, function(date_value) {
    message_info("Scanning House business for ", date_value, ".")
    urls <- tryCatch(extract_house_notice_urls(date_value), error = function(e) {
      message_info("Unable to load House business page for ", date_value, ": ", conditionMessage(e))
      character()
    })

    map_dfr(urls, function(url) {
      tryCatch(
        parse_house_notice(url, organization_name, organization_aliases, twitter_map),
        error = function(e) {
          message_info("Failed to parse House notice ", url, ": ", conditionMessage(e))
          NULL
        }
      )
    })
  })

  senate_urls <- tryCatch(extract_senate_notice_urls(), error = function(e) {
    message_info("Unable to load Senate meeting schedule: ", conditionMessage(e))
    character()
  })

  senate_results <- map_dfr(senate_urls, function(url) {
    tryCatch(
      parse_senate_notice(url, organization_name, organization_aliases, twitter_map, start_date, end_date),
      error = function(e) {
        message_info("Failed to parse Senate notice ", url, ": ", conditionMessage(e))
        NULL
      }
    )
  })

  results_raw <- bind_rows(house_results, senate_results)

  if (nrow(results_raw) == 0) {
    message_info("No matching appearance was found. Writing an empty CSV with the required schema.")
    results <- empty_output()
  } else {
    results <- results_raw |>
      distinct() |>
      mutate(sort_key = suppressWarnings(parse_date_time(
        meeting_date,
        orders = c("A, B d, Y I:M p z", "A, B d, Y I:M p", "A, B d, Y H:M")
      ))) |>
      arrange(sort_key, committee_name) |>
      select(all_of(required_output_columns), source_chamber, source_url)
  }

  output_file <- file.path(
    output_dir,
    sprintf("%s_Appearances_%s.csv", safe_name(organization_name), format(start_date, "%Y-%m-%d"))
  )

  write_csv(results, output_file, na = "")
  message_info("Finished. Output written to ", output_file)
}

main()
