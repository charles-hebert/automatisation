#!/usr/bin/env Rscript

# OSFI speech ingestion for recurring CRON / n8n jobs.
#
# What this script does:
#   1) Crawls the OSFI speeches listing.
#   2) Builds / updates a speech catalog with speaker metadata.
#   3) Downloads only new (or changed) speeches.
#   4) Stores each speech as plain text for downstream auditability.
#   5) Stores chunk-level vectors for RAG.
#   6) Marks speeches that disappear from the live OSFI listing with the date
#      they were first noticed as removed.
#
# Default vector strategy:
#   - deterministic hashed embeddings (offline / no API key required)
# Recommended production upgrade paths:
#   - OpenAI embeddings (better semantic retrieval quality)
#   - Ollama / sentence-transformers embeddings (private / self-hosted)
#
# Environment variables:
#   OSFI_DATA_DIR          output folder (default: data/osfi_osfi_speeches)
#   OSFI_VECTOR_BACKEND    hash | none (default: hash)
#   OSFI_VECTOR_DIM        vector dimension for hash backend (default: 384)
#   OSFI_CHUNK_SIZE        words per chunk (default: 220)
#   OSFI_CHUNK_OVERLAP     overlap in words (default: 40)
#   OSFI_USER_AGENT        HTTP user agent
#
# Cron example:
#   15 6 * * * /usr/bin/Rscript /path/to/osfi_speeches_ingest.R >> /var/log/osfi_speeches.log 2>&1
#
# n8n example:
#   Use an Execute Command node pointing to Rscript, then schedule the workflow.

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(lubridate)
  library(purrr)
  library(readr)
  library(rvest)
  library(stringr)
  library(tibble)
  library(xml2)
  library(httr2)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

cfg <- list(
  speeches_url = "https://www.osfi-bsif.gc.ca/en/news?type=4",
  base_url = "https://www.osfi-bsif.gc.ca",
  data_dir = Sys.getenv("OSFI_DATA_DIR", unset = file.path("data", "osfi_speeches")),
  vector_backend = tolower(Sys.getenv("OSFI_VECTOR_BACKEND", unset = "hash")),
  vector_dim = as.integer(Sys.getenv("OSFI_VECTOR_DIM", unset = "384")),
  chunk_size = as.integer(Sys.getenv("OSFI_CHUNK_SIZE", unset = "220")),
  chunk_overlap = as.integer(Sys.getenv("OSFI_CHUNK_OVERLAP", unset = "40")),
  user_agent = Sys.getenv(
    "OSFI_USER_AGENT",
    unset = "osfi-speech-rag-bot/1.0 (+https://www.osfi-bsif.gc.ca/)"
  )
)

cfg$text_dir <- file.path(cfg$data_dir, "text")
cfg$vector_dir <- file.path(cfg$data_dir, "vectors")
cfg$catalog_path <- file.path(cfg$data_dir, "speech_catalog.csv")
cfg$chunks_path <- file.path(cfg$data_dir, "speech_chunks.csv")
cfg$vectors_path <- file.path(cfg$vector_dir, "speech_chunk_vectors.jsonl")
cfg$run_log_path <- file.path(cfg$data_dir, "run_log.csv")

dir.create(cfg$data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$text_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfg$vector_dir, recursive = TRUE, showWarnings = FALSE)

if (cfg$chunk_overlap >= cfg$chunk_size) {
  stop("OSFI_CHUNK_OVERLAP must be smaller than OSFI_CHUNK_SIZE.")
}

safe_read_html <- function(url) {
  req <- request(url) |>
    req_user_agent(cfg$user_agent) |>
    req_headers(`Accept-Language` = "en") |>
    req_retry(max_tries = 3, backoff = ~ min(8, 2 ^ .x))

  resp <- req_perform(req)
  resp_body_html(resp)
}

normalize_url <- function(x) {
  x <- x %||% NA_character_
  ifelse(
    is.na(x),
    NA_character_,
    url_absolute(x, cfg$base_url)
  )
}

slugify <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-|-$)", "")
}

na_if_blank <- function(x) {
  x <- str_squish(x %||% NA_character_)
  ifelse(is.na(x) | x == "", NA_character_, x)
}

parse_osfi_date <- function(x) {
  parsed <- suppressWarnings(mdy(x))
  if (is.na(parsed)) return(as.Date(NA))
  as.Date(parsed)
}

extract_topics <- function(lines) {
  idx <- which(lines == "Topics")
  if (!length(idx) || idx[1] >= length(lines)) return(NA_character_)

  topics <- lines[(idx[1] + 1):length(lines)]
  topics <- topics[topics != ""]
  if (!length(topics)) return(NA_character_)
  paste(topics, collapse = " | ")
}

find_listing_pages <- function() {
  page1 <- safe_read_html(cfg$speeches_url)
  hrefs <- page1 |>
    html_elements("a") |>
    html_attr("href") |>
    na.omit() |>
    unique()

  pager_hrefs <- hrefs[str_detect(hrefs, "^/en/news\\?type=4(&|&amp;)?page=")]
  pager_urls <- normalize_url(pager_hrefs)
  unique(c(cfg$speeches_url, pager_urls))
}

extract_card_nodes <- function(page) {
  links <- page |>
    html_elements("main h3 a")

  hrefs <- html_attr(links, "href")
  titles <- html_text2(links)

  keep_idx <- which(
    str_detect(hrefs %||% "", "^/en/news/") &
      !str_detect(hrefs %||% "", "^/en/news\\?") &
      str_squish(titles) != ""
  )

  links[keep_idx]
}

extract_listing_entry <- function(link_node) {
  href <- html_attr(link_node, "href")
  title <- html_text2(link_node) |> str_squish()

  parent_levels <- list(
    html_parent(link_node),
    html_parent(html_parent(link_node)),
    html_parent(html_parent(html_parent(link_node)))
  )

  candidate_texts <- parent_levels |>
    map(~ tryCatch(html_text2(.x), error = function(e) "")) |>
    map_chr(~ str_replace_all(.x, "\r", ""))

  card_text <- candidate_texts[which.max(nchar(candidate_texts))]
  lines <- str_split(card_text, "\\n", simplify = FALSE)[[1]] |>
    str_squish() |>
    discard(~ .x == "")

  date_idx <- which(!is.na(suppressWarnings(mdy(lines))))
  speech_date <- if (length(date_idx)) parse_osfi_date(lines[date_idx[1]]) else as.Date(NA)

  speaker <- NA_character_
  location <- NA_character_

  if (length(date_idx) && date_idx[1] >= 2) {
    speaker <- lines[date_idx[1] - 1]
  }

  if (length(date_idx) && date_idx[1] >= 3) {
    location <- lines[date_idx[1] - 2]
  }

  tibble(
    title = title,
    url = normalize_url(href),
    speech_date = speech_date,
    location = na_if_blank(location),
    speaker_listing = na_if_blank(speaker),
    topics = extract_topics(lines),
    listing_text = card_text
  )
}

collect_listing_entries <- function() {
  page_urls <- find_listing_pages()

  entries <- map_dfr(page_urls, function(page_url) {
    message("Scanning listing page: ", page_url)
    page <- safe_read_html(page_url)
    nodes <- extract_card_nodes(page)

    if (!length(nodes)) return(tibble())

    map_dfr(nodes, extract_listing_entry) |>
      mutate(listing_page = page_url)
  })

  entries |>
    distinct(url, .keep_all = TRUE) |>
    filter(!is.na(url), !is.na(speech_date) | !is.na(title)) |>
    arrange(desc(speech_date), title)
}

extract_best_content_node <- function(page) {
  selectors <- c(
    "main article",
    "article",
    "main .node__content",
    "main .field--name-body",
    "main"
  )

  nodes <- map(selectors, ~ html_elements(page, .x)) |> flatten()
  if (!length(nodes)) return(NULL)

  texts <- map_chr(nodes, ~ tryCatch(html_text2(.x), error = function(e) ""))
  nodes[[which.max(nchar(texts))]]
}

clean_body_text <- function(text, title) {
  lines <- str_split(text, "\\n", simplify = FALSE)[[1]] |>
    str_replace_all("\\u00a0", " ") |>
    str_squish() |>
    discard(~ .x == "")

  boilerplate <- c(
    "Skip to main content",
    "Skip to \"About this site\"",
    "Menu",
    "Search",
    "Language selection",
    "News",
    "About this site",
    "Corporate"
  )

  lines <- lines[!lines %in% boilerplate]

  if (length(lines) && identical(lines[1], title)) {
    lines <- lines[-1]
  }

  end_idx <- which(str_detect(lines, "^Date modified:?$|^Report a problem or mistake on this page$"))
  if (length(end_idx)) {
    lines <- lines[seq_len(end_idx[1] - 1)]
  }

  paste(lines, collapse = "\n") |>
    str_replace_all("\n{3,}", "\n\n") |>
    str_trim()
}

detect_speaker <- function(title, listing_speaker, body_text) {
  known_names <- c("Peter Routledge", "Ben Gully")

  if (!is.na(listing_speaker)) {
    cleaned <- listing_speaker |>
      str_replace_all("^Mr\\.?\\s+", "") |>
      str_replace_all("^Ms\\.?\\s+", "") |>
      str_squish()

    if (str_detect(cleaned, "Routledge")) return("Peter Routledge")
    if (str_detect(cleaned, "Gully")) return("Ben Gully")
    if (cleaned != "") return(cleaned)
  }

  for (name in known_names) {
    if (str_detect(title, fixed(name)) || str_detect(body_text, fixed(name))) {
      return(name)
    }
  }

  body_lines <- str_split(body_text, "\\n", simplify = FALSE)[[1]]
  candidates <- body_lines |>
    str_match("^([A-Z][A-Za-z.'\\- ]{2,80}):$")

  extracted <- candidates[, 2] %||% character()
  extracted <- extracted[!is.na(extracted)]
  extracted <- extracted[!str_detect(extracted, "^Moderator$")]

  if (length(extracted)) {
    return(names(sort(table(extracted), decreasing = TRUE))[1])
  }

  title_match <- str_match(title, "^([A-Z][A-Za-z.'\\- ]{2,80}),")
  if (!is.na(title_match[1, 2])) {
    return(title_match[1, 2] |> str_squish())
  }

  NA_character_
}

hash_token <- function(token, dims) {
  ints <- utf8ToInt(token)
  weights <- seq_along(ints)
  sum(ints * weights) %% dims + 1L
}

hash_sign <- function(token) {
  ints <- utf8ToInt(token)
  if ((sum(ints) %% 2L) == 0L) 1 else -1
}

hash_embedding <- function(text, dims) {
  vec <- numeric(dims)
  tokens <- text |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]+", " ") |>
    str_split("\\s+", simplify = FALSE) |>
    pluck(1)

  tokens <- tokens[nchar(tokens) > 1]
  if (!length(tokens)) return(vec)

  freqs <- table(tokens)
  for (token in names(freqs)) {
    idx <- hash_token(token, dims)
    vec[idx] <- vec[idx] + as.numeric(freqs[[token]]) * hash_sign(token)
  }

  norm <- sqrt(sum(vec ^ 2))
  if (norm == 0) return(vec)
  vec / norm
}

split_into_chunks <- function(text, chunk_size, overlap) {
  words <- str_split(text, "\\s+", simplify = FALSE)[[1]]
  words <- words[nchar(words) > 0]

  if (!length(words)) {
    return(tibble(chunk_index = integer(), chunk_text = character()))
  }

  step <- chunk_size - overlap
  starts <- seq(1, length(words), by = step)

  map_dfr(seq_along(starts), function(i) {
    start_idx <- starts[i]
    end_idx <- min(length(words), start_idx + chunk_size - 1)
    tibble(
      chunk_index = i,
      chunk_text = paste(words[start_idx:end_idx], collapse = " ")
    )
  })
}

write_text_file <- function(slug, text) {
  path <- file.path(cfg$text_dir, paste0(slug, ".txt"))
  write_lines(text, path)
  path
}

append_vectors_jsonl <- function(vector_rows) {
  if (!nrow(vector_rows)) return(invisible(NULL))

  con <- file(cfg$vectors_path, open = if (file.exists(cfg$vectors_path)) "a" else "w")
  on.exit(close(con), add = TRUE)

  pwalk(vector_rows, function(chunk_id, speech_id, url, speaker, speech_date, chunk_index, chunk_text, embedding_json, vector_backend, vector_dim) {
    line <- toJSON(
      list(
        chunk_id = chunk_id,
        speech_id = speech_id,
        speech_url = url,
        speaker = speaker,
        speech_date = as.character(speech_date),
        chunk_index = chunk_index,
        chunk_text = chunk_text,
        embedding = fromJSON(embedding_json),
        vector_backend = vector_backend,
        vector_dim = vector_dim
      ),
      auto_unbox = TRUE,
      digits = 8,
      null = "null"
    )
    writeLines(line, con)
  })
}

load_catalog <- function() {
  if (!file.exists(cfg$catalog_path)) {
    return(tibble(
      speech_id = character(),
      title = character(),
      url = character(),
      slug = character(),
      speech_date = as.Date(character()),
      location = character(),
      speaker = character(),
      speaker_listing = character(),
      topics = character(),
      first_seen_date = as.Date(character()),
      last_seen_date = as.Date(character()),
      removed_noticed_date = as.Date(character()),
      status = character(),
      content_hash = character(),
      text_file = character(),
      vector_backend = character(),
      vector_dim = integer(),
      chunk_count = integer(),
      source = character()
    ))
  }

  read_csv(
    cfg$catalog_path,
    show_col_types = FALSE,
    col_types = cols(
      speech_date = col_date(),
      first_seen_date = col_date(),
      last_seen_date = col_date(),
      removed_noticed_date = col_date(),
      vector_dim = col_integer(),
      chunk_count = col_integer()
    )
  )
}

load_chunks <- function() {
  if (!file.exists(cfg$chunks_path)) {
    return(tibble(
      chunk_id = character(),
      speech_id = character(),
      url = character(),
      speaker = character(),
      speech_date = as.Date(character()),
      chunk_index = integer(),
      chunk_text = character(),
      vector_backend = character(),
      vector_dim = integer()
    ))
  }

  read_csv(
    cfg$chunks_path,
    show_col_types = FALSE,
    col_types = cols(
      speech_date = col_date(),
      chunk_index = col_integer(),
      vector_dim = col_integer()
    )
  )
}

fetch_speech_details <- function(listing_row) {
  message("Fetching speech: ", listing_row$url)
  page <- safe_read_html(listing_row$url)
  content_node <- extract_best_content_node(page)
  if (is.null(content_node)) {
    stop("Could not identify a speech body node for: ", listing_row$url)
  }

  raw_text <- html_text2(content_node)
  body_text <- clean_body_text(raw_text, listing_row$title)
  speaker <- detect_speaker(listing_row$title, listing_row$speaker_listing, body_text)
  slug <- listing_row$slug
  text_path <- write_text_file(slug, body_text)
  content_hash <- unname(toJSON(list(body_text = body_text), auto_unbox = TRUE))

  chunks <- split_into_chunks(body_text, cfg$chunk_size, cfg$chunk_overlap)
  if (!nrow(chunks)) {
    chunks <- tibble(chunk_index = 1L, chunk_text = body_text)
  }

  if (cfg$vector_backend == "hash") {
    embeddings <- map(chunks$chunk_text, hash_embedding, dims = cfg$vector_dim)
  } else if (cfg$vector_backend == "none") {
    embeddings <- replicate(nrow(chunks), numeric(), simplify = FALSE)
  } else {
    stop("Unsupported OSFI_VECTOR_BACKEND: ", cfg$vector_backend, ". Use 'hash' or 'none'.")
  }

  chunk_rows <- chunks |>
    mutate(
      speech_id = listing_row$speech_id,
      url = listing_row$url,
      speaker = speaker,
      speech_date = listing_row$speech_date,
      chunk_id = paste0(listing_row$speech_id, "::", chunk_index),
      vector_backend = cfg$vector_backend,
      vector_dim = ifelse(cfg$vector_backend == "none", 0L, cfg$vector_dim),
      embedding_json = map_chr(embeddings, ~ toJSON(.x, auto_unbox = TRUE, digits = 8))
    ) |>
    select(chunk_id, speech_id, url, speaker, speech_date, chunk_index, chunk_text, vector_backend, vector_dim, embedding_json)

  speech_row <- listing_row |>
    mutate(
      speaker = speaker,
      content_hash = content_hash,
      text_file = text_path,
      vector_backend = cfg$vector_backend,
      vector_dim = ifelse(cfg$vector_backend == "none", 0L, cfg$vector_dim),
      chunk_count = nrow(chunk_rows),
      source = "OSFI speeches listing"
    )

  list(speech_row = speech_row, chunk_rows = chunk_rows)
}

run_date <- Sys.Date()
existing_catalog <- load_catalog()
existing_chunks <- load_chunks()
live_listing <- collect_listing_entries()

if (!nrow(live_listing)) {
  stop("No speeches were discovered from the OSFI listing. Aborting update.")
}

live_listing <- live_listing |>
  mutate(
    slug = if_else(!is.na(url), basename(url), slugify(title)),
    speech_id = if_else(
      !is.na(url),
      str_replace_all(url, "[^A-Za-z0-9]+", "_"),
      paste0("speech_", slugify(title), "_", format(speech_date, "%Y%m%d"))
    )
  )

catalog_base <- existing_catalog |>
  select(-any_of(c("listing_text", "listing_page")))

current_urls <- live_listing$url

upserted_catalog <- full_join(
  catalog_base,
  live_listing,
  by = "url",
  suffix = c("_old", "")
) |>
  mutate(is_live_row = url %in% current_urls) |>
  transmute(
    speech_id = coalesce(speech_id, speech_id_old),
    title = coalesce(title, title_old),
    url,
    slug = coalesce(slug, slug_old, slugify(title)),
    speech_date = coalesce(speech_date, speech_date_old),
    location = coalesce(location, location_old),
    speaker = speaker_old,
    speaker_listing = coalesce(speaker_listing, speaker_listing_old),
    topics = coalesce(topics, topics_old),
    first_seen_date = coalesce(first_seen_date, run_date),
    last_seen_date = if_else(is_live_row, run_date, last_seen_date_old),
    removed_noticed_date = removed_noticed_date_old,
    status = if_else(is_live_row, "active", coalesce(status_old, "removed")),
    content_hash = content_hash_old,
    text_file = text_file_old,
    vector_backend = vector_backend_old,
    vector_dim = vector_dim_old,
    chunk_count = chunk_count_old,
    source = coalesce(source_old, "OSFI speeches listing")
  )

upserted_catalog <- upserted_catalog |>
  mutate(
    removed_noticed_date = case_when(
      !url %in% current_urls & is.na(removed_noticed_date) ~ run_date,
      url %in% current_urls ~ as.Date(NA),
      TRUE ~ removed_noticed_date
    ),
    status = if_else(url %in% current_urls, "active", "removed")
  )

new_or_changed <- live_listing |>
  left_join(upserted_catalog |> select(url, content_hash), by = "url") |>
  filter(is.na(content_hash))

if (nrow(new_or_changed)) {
  results <- map(seq_len(nrow(new_or_changed)), function(i) {
    fetch_speech_details(new_or_changed[i, ])
  })

  new_speech_rows <- map_dfr(results, "speech_row")
  new_chunk_rows <- map_dfr(results, "chunk_rows")

  upserted_catalog <- bind_rows(
    new_speech_rows |>
      select(names(upserted_catalog)),
    upserted_catalog |>
      filter(!url %in% new_speech_rows$url)
  )

  active_urls <- new_speech_rows$url
  existing_chunks <- existing_chunks |>
    filter(!url %in% active_urls)

  updated_chunks <- bind_rows(
    existing_chunks,
    new_chunk_rows |>
      select(chunk_id, speech_id, url, speaker, speech_date, chunk_index, chunk_text, vector_backend, vector_dim)
  ) |>
    arrange(desc(speech_date), speech_id, chunk_index)

  write_csv(updated_chunks, cfg$chunks_path)
  append_vectors_jsonl(new_chunk_rows)
} else {
  updated_chunks <- existing_chunks
  message("No new speeches found; catalog updated without fetching body text.")
}

upserted_catalog <- upserted_catalog |>
  arrange(desc(speech_date), title)

write_csv(upserted_catalog, cfg$catalog_path)

run_log_row <- tibble(
  run_date = run_date,
  speeches_live = nrow(live_listing),
  speeches_catalog = nrow(upserted_catalog),
  new_speeches = nrow(new_or_changed),
  removed_speeches = sum(upserted_catalog$status == "removed", na.rm = TRUE),
  vector_backend = cfg$vector_backend,
  vector_dim = ifelse(cfg$vector_backend == "none", 0L, cfg$vector_dim)
)

if (file.exists(cfg$run_log_path)) {
  prior_log <- read_csv(cfg$run_log_path, show_col_types = FALSE, col_types = cols(run_date = col_date()))
  run_log <- bind_rows(prior_log, run_log_row)
} else {
  run_log <- run_log_row
}
write_csv(run_log, cfg$run_log_path)

message("OSFI speech ingestion complete.")
message("Catalog: ", cfg$catalog_path)
message("Chunks: ", cfg$chunks_path)
message("Vectors: ", cfg$vectors_path)
