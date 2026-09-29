#!/usr/bin/env Rscript
#
# OSFI speech ingestion pipeline for RAG workflows.
# - Scans the OSFI speeches listing and only fetches speeches that are new locally.
# - Saves each new speech as raw HTML plus cleaned plain text.
# - Produces vector-ready chunk tables, and optionally OpenAI embeddings when
#   OSFI_EMBED_PROVIDER=openai and OPENAI_API_KEY are available.
# - Tracks removed speeches by setting both deletion_date and
#   removed_detected_on when a previously known speech disappears from the
#   live OSFI speech index.
# - Designed to run unattended via CRON or n8n.

suppressPackageStartupMessages({
  library(dplyr)
  library(fs)
  library(httr2)
  library(jsonlite)
  library(purrr)
  library(readr)
  library(rvest)
  library(stringr)
  library(tibble)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

required_packages <- c(
  "dplyr", "fs", "httr2", "jsonlite", "purrr", "readr", "rvest", "stringr", "tibble"
)

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (length(hit) == 0 || hit[length(hit)] == length(args)) {
    return(default)
  }
  args[hit[length(hit)] + 1]
}

config <- list(
  output_dir = arg_value("--output-dir", Sys.getenv("OSFI_OUTPUT_DIR", "data/osfi_speeches")),
  max_pages = as.integer(arg_value("--max-pages", Sys.getenv("OSFI_MAX_PAGES", "30"))),
  chunk_words = as.integer(arg_value("--chunk-words", Sys.getenv("OSFI_CHUNK_WORDS", "350"))),
  chunk_overlap = as.integer(arg_value("--chunk-overlap", Sys.getenv("OSFI_CHUNK_OVERLAP", "50"))),
  user_agent = Sys.getenv("OSFI_USER_AGENT", "osfi-speech-rag-pipeline/1.0"),
  embed_provider = tolower(Sys.getenv("OSFI_EMBED_PROVIDER", "none")),
  openai_model = Sys.getenv("OSFI_OPENAI_EMBED_MODEL", "text-embedding-3-large"),
  openai_batch_size = as.integer(Sys.getenv("OSFI_OPENAI_BATCH_SIZE", "50"))
)

if (is.na(config$max_pages) || config$max_pages < 1) {
  stop("--max-pages must be a positive integer.")
}

if (is.na(config$chunk_words) || config$chunk_words < 50) {
  stop("--chunk-words must be at least 50.")
}

if (is.na(config$chunk_overlap) || config$chunk_overlap < 0 || config$chunk_overlap >= config$chunk_words) {
  stop("--chunk-overlap must be >= 0 and smaller than --chunk-words.")
}

base_dir <- path_abs(config$output_dir)
text_dir <- path(base_dir, "text")
html_dir <- path(base_dir, "html")
meta_dir <- path(base_dir, "metadata")
vector_dir <- path(base_dir, "vector")

dir_create(c(base_dir, text_dir, html_dir, meta_dir, vector_dir), recurse = TRUE)

speech_table_path <- path(meta_dir, "speech_table.csv")
chunk_table_path <- path(vector_dir, "speech_chunks.csv")
chunk_jsonl_path <- path(vector_dir, "speech_chunks.jsonl")
chunk_parquet_path <- path(vector_dir, "speech_chunks.parquet")

news_index_url <- "https://www.osfi-bsif.gc.ca/en/news?type=4"
known_speakers <- c("Peter Routledge", "Ben Gully")

message("Using packages: ", paste(required_packages, collapse = ", "))
message("Output directory: ", base_dir)
message("Embed provider: ", config$embed_provider)

today_utc <- as.Date(format(Sys.time(), tz = "UTC", usetz = FALSE), tz = "UTC")

empty_speech_table <- function() {
  tibble(
    speech_id = character(),
    title = character(),
    url = character(),
    slug = character(),
    speech_date = as.Date(character()),
    location = character(),
    primary_speaker = character(),
    speaker_names = character(),
    known_speaker_match = character(),
    speaker_detection_method = character(),
    text_path = character(),
    html_path = character(),
    text_sha256 = character(),
    chunk_count = integer(),
    status = character(),
    first_seen_on = as.Date(character()),
    last_seen_on = as.Date(character()),
    deletion_date = as.Date(character()),
    removed_detected_on = as.Date(character()),
    source_modified_date = as.Date(character()),
    notes = character()
  )
}

empty_chunk_table <- function() {
  tibble(
    chunk_id = character(),
    speech_id = character(),
    url = character(),
    title = character(),
    speech_date = as.Date(character()),
    primary_speaker = character(),
    speaker_names = character(),
    chunk_index = integer(),
    chunk_word_count = integer(),
    chunk_text = character(),
    embedding_provider = character(),
    embedding_model = character(),
    embedding_vector = character(),
    embedding_created_at = character()
  )
}

read_table_or_empty <- function(path_to_file, empty_fn, col_types = cols()) {
  prototype <- empty_fn()
  if (!file_exists(path_to_file)) {
    return(prototype)
  }

  out <- suppressMessages(read_csv(path_to_file, col_types = col_types, show_col_types = FALSE))
  missing_cols <- setdiff(names(prototype), names(out))

  if (length(missing_cols) > 0) {
    for (col_name in missing_cols) {
      out[[col_name]] <- prototype[[col_name]][rep(NA_integer_, nrow(out))]
    }
  }

  out |>
    select(all_of(names(prototype)))
}

existing_speeches <- read_table_or_empty(
  speech_table_path,
  empty_speech_table,
  cols(
    speech_id = col_character(),
    title = col_character(),
    url = col_character(),
    slug = col_character(),
    speech_date = col_date(),
    location = col_character(),
    primary_speaker = col_character(),
    speaker_names = col_character(),
    known_speaker_match = col_character(),
    speaker_detection_method = col_character(),
    text_path = col_character(),
    html_path = col_character(),
    text_sha256 = col_character(),
    chunk_count = col_integer(),
    status = col_character(),
    first_seen_on = col_date(),
    last_seen_on = col_date(),
    deletion_date = col_date(),
    removed_detected_on = col_date(),
    source_modified_date = col_date(),
    notes = col_character()
  )
)

existing_chunks <- read_table_or_empty(
  chunk_table_path,
  empty_chunk_table,
  cols(
    chunk_id = col_character(),
    speech_id = col_character(),
    url = col_character(),
    title = col_character(),
    speech_date = col_date(),
    primary_speaker = col_character(),
    speaker_names = col_character(),
    chunk_index = col_integer(),
    chunk_word_count = col_integer(),
    chunk_text = col_character(),
    embedding_provider = col_character(),
    embedding_model = col_character(),
    embedding_vector = col_character(),
    embedding_created_at = col_character()
  )
)

safe_html <- function(url) {
  request(url) |>
    req_user_agent(config$user_agent) |>
    req_retry(max_tries = 3, backoff = ~ min(2 ^ .x, 8)) |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_html()
}

safe_response <- function(url) {
  request(url) |>
    req_user_agent(config$user_agent) |>
    req_retry(max_tries = 3, backoff = ~ min(2 ^ .x, 8)) |>
    req_timeout(30) |>
    req_perform()
}

absolute_news_url <- function(href) {
  if (is.na(href) || href == "") {
    return(NA_character_)
  }
  if (str_detect(href, "^https?://")) {
    return(href)
  }
  paste0("https://www.osfi-bsif.gc.ca", href)
}

clean_lines <- function(x) {
  x |>
    str_split("\\n") |>
    unlist() |>
    str_squish() |>
    discard(~ .x == "")
}

is_person_name <- function(x) {
  x <- str_squish(x)
  if (x == "") {
    return(FALSE)
  }

  if (str_detect(x, "(Canada|Conference|Institute|Forum|Summit|Committee|Mortgage|Professionals|Economic Club|Morningstar|Scotiabank|Reuters|National Insurance|Credit Outlook|Quarterly Release|Release Day|Domestic Stability Buffer|Association|Banking Law|Panel Discussion|Networking Hour)")) {
    return(FALSE)
  }

  if (str_detect(x, "^(Speech|Virtual|Ottawa|Toronto|Montreal|Vancouver|Calgary|Edmonton|Quebec|Halifax|Winnipeg|Saskatoon|Regina|Gatineau|Canada|Topics|Image|News type)$")) {
    return(FALSE)
  }

  if (str_detect(x, "^[A-Z][A-Za-z'’.-]+(?:\\s+[A-Z][A-Za-z'’.-]+){1,4}$")) {
    return(TRUE)
  }

  FALSE
}

extract_card_speakers <- function(lines, idx_date) {
  if (length(lines) == 0 || idx_date <= 1) {
    return(character())
  }

  speakers <- character()
  pos <- idx_date - 1
  while (pos >= 1 && is_person_name(lines[pos])) {
    speakers <- c(lines[pos], speakers)
    pos <- pos - 1
  }
  unique(speakers)
}

extract_listing_cards <- function(doc, page_number) {
  cards <- html_elements(doc, ".view-content .views-row, .view-content article, main article, .news-listing-item")

  if (length(cards) == 0) {
    all_links <- html_elements(doc, "main a")
    hrefs <- html_attr(all_links, "href")
    titles <- html_text2(all_links)

    fallback <- tibble(
      title = str_squish(titles),
      href = hrefs
    ) |>
      filter(
        !is.na(href),
        str_detect(href, "^/en/news/"),
        !str_detect(href, "^/en/news\\?"),
        !str_detect(href, "/en/news/media"),
        !str_detect(href, "/en/news/speaking-engagements")
      ) |>
      mutate(
        url = map_chr(href, absolute_news_url),
        page_number = page_number,
        location = NA_character_,
        speech_date = as.Date(NA),
        speaker_names = NA_character_,
        primary_speaker = NA_character_
      ) |>
      distinct(url, .keep_all = TRUE) |>
      select(url, title, speech_date, location, speaker_names, primary_speaker, page_number)

    return(fallback)
  }

  map_dfr(cards, function(card) {
    anchors <- html_elements(card, "a")
    hrefs <- html_attr(anchors, "href")
    link_idx <- which(str_detect(hrefs, "^/en/news/") & !str_detect(hrefs, "^/en/news\\?"))

    if (length(link_idx) == 0) {
      return(tibble())
    }

    href <- hrefs[link_idx[1]]
    title <- html_text2(anchors[link_idx[1]]) |> str_squish()
    lines <- clean_lines(html_text2(card))
    date_idx <- which(str_detect(lines, "^[A-Z][a-z]+\\s+\\d{1,2},\\s+\\d{4}$"))
    speech_date <- if (length(date_idx) > 0) as.Date(lines[date_idx[1]], format = "%B %d, %Y") else as.Date(NA)
    speakers <- if (length(date_idx) > 0) extract_card_speakers(lines, date_idx[1]) else character()
    location_idx <- if (length(date_idx) > 0) date_idx[1] - length(speakers) - 1L else NA_integer_
    location <- if (!is.na(location_idx) && location_idx >= 1) lines[location_idx] else NA_character_

    tibble(
      url = absolute_news_url(href),
      title = title,
      speech_date = speech_date,
      location = location,
      speaker_names = if (length(speakers) > 0) paste(speakers, collapse = " | ") else NA_character_,
      primary_speaker = if (length(speakers) > 0) speakers[1] else NA_character_,
      page_number = page_number
    )
  }) |>
    distinct(url, .keep_all = TRUE)
}

crawl_listing <- function() {
  message("Scanning OSFI speech index for currently published speeches...")
  all_cards <- list()
  seen_urls <- character()

  for (page_number in 0:(config$max_pages - 1)) {
    page_url <- if (page_number == 0) news_index_url else paste0(news_index_url, "&page=", page_number)
    message("  - Listing page ", page_number + 1, ": ", page_url)

    doc <- safe_html(page_url)
    cards <- extract_listing_cards(doc, page_number + 1)

    if (nrow(cards) == 0) {
      message("    No cards found; stopping pagination.")
      break
    }

    new_card_count <- sum(!cards$url %in% seen_urls)
    all_cards[[length(all_cards) + 1]] <- cards
    seen_urls <- union(seen_urls, cards$url)

    if (new_card_count == 0) {
      message("    Page introduced no new speech URLs; stopping pagination.")
      break
    }
  }

  bind_rows(all_cards) |>
    distinct(url, .keep_all = TRUE) |>
    arrange(desc(speech_date), title)
}

extract_page_text <- function(doc) {
  main <- html_element(doc, "main")
  if (inherits(main, "xml_missing")) {
    main <- html_element(doc, "body")
  }

  raw_text <- html_text2(main) |>
    str_replace_all("\u00a0", " ") |>
    str_replace_all("[ \t]+", " ") |>
    str_replace_all("\n{3,}", "\n\n")
  raw_text <- str_replace(raw_text, "^.*?Check against delivery", "Check against delivery")
  raw_text <- str_replace(raw_text, "Report a problem or mistake on this page.*$", "")
  raw_text <- str_replace(raw_text, "## About this site.*$", "")
  raw_text |>
    str_split("\n") |>
    unlist() |>
    str_squish() |>
    discard(~ .x == "") |>
    paste(collapse = "\n\n")
}

extract_source_modified_date <- function(doc) {
  page_text <- html_text2(doc)
  hit <- str_match(page_text, "Date modified:\\s*(\\d{4}-\\d{2}-\\d{2})")
  if (is.na(hit[1, 2])) {
    return(as.Date(NA))
  }
  as.Date(hit[1, 2])
}

extract_speakers_from_page <- function(title, body_text) {
  lines <- clean_lines(body_text)
  header_lines <- head(lines, 40)
  speaker_lines <- header_lines[str_detect(header_lines, ":$")]
  speaker_lines <- str_remove(speaker_lines, ":$")
  speaker_lines <- speaker_lines[str_detect(speaker_lines, "[A-Z][a-z]+")]

  title_hits <- str_extract_all(
    title,
    "[A-Z][A-Za-z'’.-]+(?:\\s+[A-Z][A-Za-z'’.-]+){1,3}"
  )[[1]]

  title_hits <- title_hits[!title_hits %in% c("Office of", "News type")]
  speaker_lines <- unique(c(speaker_lines, title_hits))

  normalized <- speaker_lines |>
    str_remove("^(Superintendent|Deputy Superintendent|Executive Director|Assistant Superintendent)\\s+") |>
    str_remove(",.*$") |>
    str_squish()

  normalized <- normalized[map_lgl(normalized, is_person_name)]
  unique(normalized)
}

chunk_text <- function(text, chunk_words = config$chunk_words, overlap = config$chunk_overlap) {
  words <- str_split(str_squish(text), "\\s+")[[1]]
  words <- words[words != ""]

  if (length(words) == 0) {
    return(tibble(chunk_index = integer(), chunk_word_count = integer(), chunk_text = character()))
  }

  step <- chunk_words - overlap
  starts <- seq(1, length(words), by = step)

  map_dfr(seq_along(starts), function(idx) {
    start <- starts[idx]
    end <- min(start + chunk_words - 1, length(words))
    tibble(
      chunk_index = idx,
      chunk_word_count = end - start + 1L,
      chunk_text = paste(words[start:end], collapse = " ")
    )
  })
}

openai_embed_texts <- function(texts) {
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (config$embed_provider != "openai" || api_key == "") {
    return(rep(NA_character_, length(texts)))
  }

  batches <- split(texts, ceiling(seq_along(texts) / config$openai_batch_size))
  vectors <- vector("list", length(batches))

  for (i in seq_along(batches)) {
    payload <- list(model = config$openai_model, input = unname(batches[[i]]))
    resp <- request("https://api.openai.com/v1/embeddings") |>
      req_method("POST") |>
      req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      req_user_agent(config$user_agent) |>
      req_body_json(payload, auto_unbox = TRUE) |>
      req_retry(max_tries = 3, backoff = ~ min(2 ^ .x, 8)) |>
      req_timeout(60) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)

    vectors[[i]] <- map_chr(resp$data$embedding, toJSON, auto_unbox = TRUE)
  }

  unlist(vectors, use.names = FALSE)
}

hash_file_sha256 <- function(path_to_file) {
  digest::digest(file = path_to_file, algo = "sha256")
}

process_new_speech <- function(listing_row) {
  message("Fetching new speech: ", listing_row$url)
  resp <- safe_response(listing_row$url)
  doc <- resp |> resp_body_html()
  title <- html_element(doc, "h1") |> html_text2() |> str_squish()
  body_text <- extract_page_text(doc)
  source_modified_date <- extract_source_modified_date(doc)
  page_speakers <- extract_speakers_from_page(title, body_text)

  speaker_names <- listing_row$speaker_names
  if (is.na(speaker_names) || speaker_names == "") {
    speaker_names <- if (length(page_speakers) > 0) paste(page_speakers, collapse = " | ") else NA_character_
  }

  primary_speaker <- listing_row$primary_speaker
  if (is.na(primary_speaker) || primary_speaker == "") {
    primary_speaker <- if (length(page_speakers) > 0) page_speakers[1] else NA_character_
  }

  if (!is.na(primary_speaker) && primary_speaker %in% known_speakers) {
    known_match <- primary_speaker
  } else if (!is.na(speaker_names) && any(map_lgl(known_speakers, ~ str_detect(speaker_names, fixed(.x))))) {
    known_match <- known_speakers[map_lgl(known_speakers, ~ str_detect(speaker_names, fixed(.x)))][1]
  } else {
    known_match <- "other"
  }

  slug <- str_remove(listing_row$url, "^https://www\\.osfi-bsif\\.gc\\.ca/en/news/")
  speech_id <- str_c(format(listing_row$speech_date %||% today_utc, "%Y%m%d"), "_", slug) |>
    str_replace_all("[^A-Za-z0-9_-]+", "_")

  html_path <- path(html_dir, paste0(speech_id, ".html"))
  text_path <- path(text_dir, paste0(speech_id, ".txt"))

  writeLines(resp_body_string(resp), html_path, useBytes = TRUE)
  writeLines(body_text, text_path, useBytes = TRUE)

  chunk_tbl <- chunk_text(body_text)
  if (nrow(chunk_tbl) > 0) {
    chunk_tbl <- chunk_tbl |>
      mutate(
        chunk_id = str_c(speech_id, "_chunk_", str_pad(chunk_index, width = 4, side = "left", pad = "0")),
        speech_id = speech_id,
        url = listing_row$url,
        title = title,
        speech_date = listing_row$speech_date,
        primary_speaker = primary_speaker,
        speaker_names = speaker_names,
        embedding_provider = config$embed_provider,
        embedding_model = if (config$embed_provider == "openai") config$openai_model else NA_character_,
        embedding_vector = openai_embed_texts(chunk_text),
        embedding_created_at = if (config$embed_provider == "openai" && Sys.getenv("OPENAI_API_KEY", "") != "") as.character(Sys.time()) else NA_character_
      ) |>
      select(
        chunk_id, speech_id, url, title, speech_date, primary_speaker, speaker_names,
        chunk_index, chunk_word_count, chunk_text, embedding_provider, embedding_model,
        embedding_vector, embedding_created_at
      )
  }

  speech_record <- tibble(
    speech_id = speech_id,
    title = title,
    url = listing_row$url,
    slug = slug,
    speech_date = listing_row$speech_date,
    location = listing_row$location,
    primary_speaker = primary_speaker,
    speaker_names = speaker_names,
    known_speaker_match = known_match,
    speaker_detection_method = case_when(
      !is.na(listing_row$primary_speaker) ~ "listing_page",
      length(page_speakers) > 0 ~ "speech_page",
      TRUE ~ "unknown"
    ),
    text_path = path_rel(text_path, start = base_dir),
    html_path = path_rel(html_path, start = base_dir),
    text_sha256 = hash_file_sha256(text_path),
    chunk_count = nrow(chunk_tbl),
    status = "active",
    first_seen_on = today_utc,
    last_seen_on = today_utc,
    deletion_date = as.Date(NA),
    removed_detected_on = as.Date(NA),
    source_modified_date = source_modified_date,
    notes = NA_character_
  )

  list(speech = speech_record, chunks = chunk_tbl)
}

listing_tbl <- crawl_listing()

if (nrow(listing_tbl) == 0) {
  stop("No speeches were discovered on the OSFI listing pages.")
}

current_urls <- unique(listing_tbl$url)
new_urls <- setdiff(current_urls, existing_speeches$url)
missing_content_urls <- existing_speeches |>
  filter(status != "removed", is.na(text_path) | !file_exists(path(base_dir, text_path))) |>
  pull(url)
urls_to_fetch <- union(new_urls, missing_content_urls)

message("Currently listed speeches: ", length(current_urls))
message("New speeches to extract: ", length(new_urls))
message("Speeches missing local content: ", length(missing_content_urls))

new_results <- map(urls_to_fetch, function(url) {
  listing_row <- listing_tbl |>
    filter(url == !!url) |>
    slice(1)
  process_new_speech(listing_row)
})

new_speech_rows <- bind_rows(map(new_results, "speech"))
new_chunk_rows <- bind_rows(map(new_results, "chunks"))

updated_existing <- existing_speeches |>
  filter(!url %in% urls_to_fetch) |>
  mutate(
    status = if_else(url %in% current_urls, "active", "removed"),
    last_seen_on = if_else(url %in% current_urls, today_utc, last_seen_on),
    deletion_date = case_when(
      url %in% current_urls ~ as.Date(NA),
      is.na(deletion_date) ~ today_utc,
      TRUE ~ deletion_date
    ),
    removed_detected_on = case_when(
      url %in% current_urls ~ as.Date(NA),
      is.na(removed_detected_on) ~ today_utc,
      TRUE ~ removed_detected_on
    )
  )

refreshed_existing <- listing_tbl |>
  filter(url %in% current_urls) |>
  select(url, title, speech_date, location, primary_speaker, speaker_names) |>
  rename(
    listing_title = title,
    listing_date = speech_date,
    listing_location = location,
    listing_primary_speaker = primary_speaker,
    listing_speaker_names = speaker_names
  )

combined_speeches <- bind_rows(updated_existing, new_speech_rows) |>
  select(names(empty_speech_table())) |>
  distinct(url, .keep_all = TRUE) |>
  left_join(refreshed_existing, by = "url") |>
  mutate(
    title = coalesce(title, listing_title),
    speech_date = coalesce(speech_date, listing_date),
    location = coalesce(location, listing_location),
    primary_speaker = coalesce(primary_speaker, listing_primary_speaker),
    speaker_names = coalesce(speaker_names, listing_speaker_names),
    last_seen_on = if_else(url %in% current_urls, today_utc, last_seen_on),
    status = if_else(url %in% current_urls, "active", status),
    deletion_date = if_else(url %in% current_urls, as.Date(NA), deletion_date),
    removed_detected_on = if_else(url %in% current_urls, as.Date(NA), removed_detected_on)
  ) |>
  select(names(empty_speech_table())) |>
  arrange(desc(speech_date), title)

combined_chunks <- bind_rows(
  existing_chunks |>
    filter(!speech_id %in% new_speech_rows$speech_id),
  new_chunk_rows
) |>
  distinct(chunk_id, .keep_all = TRUE) |>
  arrange(desc(speech_date), speech_id, chunk_index)

write_csv(combined_speeches, speech_table_path, na = "")
write_csv(combined_chunks, chunk_table_path, na = "")
chunk_jsonl_lines <- pmap_chr(
  as.list(combined_chunks),
  function(...) toJSON(list(...), auto_unbox = TRUE, null = "null")
)
writeLines(chunk_jsonl_lines, chunk_jsonl_path)

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow::write_parquet(combined_chunks, chunk_parquet_path)
}

message("Wrote speech table: ", speech_table_path)
message("Wrote chunk table: ", chunk_table_path)
message("Wrote chunk JSONL: ", chunk_jsonl_path)
if (file_exists(chunk_parquet_path)) {
  message("Wrote chunk Parquet: ", chunk_parquet_path)
} else {
  message("Parquet skipped because package 'arrow' is not installed.")
}

message("Run complete.")
message("Suggestions:")
message("  1) Use speech_table.csv as the canonical registry for active/removed speeches, including deletion_date and removed_detected_on.")
message("  2) Use speech_chunks.parquet or speech_chunks.jsonl as the hand-off file into pgvector, Qdrant, Pinecone, Milvus, or a warehouse.")
message("  3) If you want true embeddings, set OSFI_EMBED_PROVIDER=openai and OPENAI_API_KEY in CRON/n8n, or embed downstream in n8n before loading your vector DB.")
message("  4) Schedule with CRON or n8n daily; the script only fetches new speeches and updates removal status for missing ones.")
