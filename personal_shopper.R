#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

# -------------------------
# Configuration
# -------------------------
DEFAULT_ENGINES <- c("google_shopping", "bing_shopping", "ebay")

read_interest_items <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Input file not found: %s", path))
  }

  lines <- read_lines(path, lazy = FALSE, progress = FALSE)
  items <- lines |>
    str_trim() |>
    discard(~ .x == "" || str_starts(.x, "#"))

  if (length(items) == 0) {
    stop("No shopping items found in input file.")
  }

  items
}

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_real_)
  out <- str_extract(as.character(x), "[0-9]+(?:\\.[0-9]+)?")
  as.numeric(out)
}

openai_chat_json <- function(messages, model = "gpt-4.1-mini") {
  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (api_key == "") {
    stop("OPENAI_API_KEY is required for provider=openai")
  }

  body <- list(
    model = model,
    messages = messages,
    response_format = list(type = "json_object"),
    temperature = 0.2
  )

  resp <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(Authorization = paste("Bearer", api_key)) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_perform()

  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  parsed$choices[[1]]$message$content
}

ollama_chat_json <- function(messages, model = "llama3.1") {
  host <- Sys.getenv("OLLAMA_HOST", "http://localhost:11434")
  body <- list(
    model = model,
    messages = messages,
    format = "json",
    stream = FALSE,
    options = list(temperature = 0.2)
  )

  resp <- request(paste0(host, "/api/chat")) |>
    req_body_json(body, auto_unbox = TRUE) |>
    req_perform()

  parsed <- resp_body_json(resp, simplifyVector = TRUE)
  parsed$message$content
}

llm_chat_json <- function(provider, model, messages) {
  if (provider == "openai") {
    openai_chat_json(messages, model)
  } else if (provider == "ollama") {
    ollama_chat_json(messages, model)
  } else {
    stop("provider must be either 'openai' or 'ollama'")
  }
}

generate_queries <- function(item, provider, model) {
  prompt <- paste0(
    "Create exactly 3 short shopping search queries for this item: ", item,
    ". Return strict JSON: {\"queries\":[\"...\",\"...\",\"...\"]}."
  )

  content <- llm_chat_json(
    provider = provider,
    model = model,
    messages = list(
      list(role = "system", content = "You are a shopping query optimizer."),
      list(role = "user", content = prompt)
    )
  )

  parsed <- fromJSON(content)
  queries <- unique(unlist(parsed$queries))
  queries <- queries[!is.na(queries) & queries != ""]

  if (length(queries) == 0) {
    c(item, paste(item, "best deal"), paste(item, "buy online"))
  } else {
    queries
  }
}

search_serpapi <- function(query, engine, serpapi_key, max_results = 10) {
  resp <- request("https://serpapi.com/search.json") |>
    req_url_query(
      q = query,
      engine = engine,
      api_key = serpapi_key,
      num = max_results
    ) |>
    req_perform()

  data <- resp_body_json(resp, simplifyVector = TRUE)

  if (!is.null(data$shopping_results)) {
    results <- data$shopping_results
    return(tibble(
      title = results$title %||% NA_character_,
      price_text = results$price %||% NA_character_,
      price = map_dbl(results$price, safe_num),
      store = results$source %||% NA_character_,
      link = results$link %||% NA_character_,
      engine = engine,
      query = query
    ))
  }

  if (!is.null(data$organic_results)) {
    results <- data$organic_results
    return(tibble(
      title = results$title %||% NA_character_,
      price_text = NA_character_,
      price = NA_real_,
      store = results$source %||% engine,
      link = results$link %||% NA_character_,
      engine = engine,
      query = query
    ))
  }

  tibble(
    title = character(),
    price_text = character(),
    price = numeric(),
    store = character(),
    link = character(),
    engine = character(),
    query = character()
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

rank_suggestions <- function(item, suggestions, provider, model) {
  shortlist <- suggestions |>
    arrange(is.na(price), price) |>
    slice_head(n = min(8, n())) |>
    mutate(row_id = row_number())

  if (nrow(shortlist) == 0) {
    return(suggestions |> mutate(ai_pick = FALSE, ai_note = NA_character_))
  }

  choices_json <- toJSON(shortlist |> select(row_id, title, price_text, store, link), auto_unbox = TRUE)

  prompt <- paste0(
    "Choose up to 3 best shopping options for '", item, "' balancing low price and reputable store. ",
    "Return JSON: {\"picked_row_ids\":[1,2],\"note\":\"...\"}. Options: ", choices_json
  )

  content <- llm_chat_json(
    provider = provider,
    model = model,
    messages = list(
      list(role = "system", content = "You are a careful shopping assistant."),
      list(role = "user", content = prompt)
    )
  )

  parsed <- fromJSON(content)
  picked <- as.integer(unlist(parsed$picked_row_ids))
  note <- as.character(parsed$note %||% "")

  shortlist <- shortlist |>
    mutate(ai_pick = row_id %in% picked, ai_note = if_else(ai_pick, note, NA_character_)) |>
    select(-row_id)

  leftovers <- anti_join(suggestions, shortlist, by = c("title", "price_text", "price", "store", "link", "engine", "query")) |>
    mutate(ai_pick = FALSE, ai_note = NA_character_)

  bind_rows(shortlist, leftovers)
}

shop_item <- function(item, provider, model, engines, serpapi_key) {
  queries <- tryCatch(
    generate_queries(item, provider, model),
    error = function(e) {
      message(sprintf("LLM query generation failed for '%s': %s", item, e$message))
      c(item, paste(item, "best deal"), paste(item, "buy online"))
    }
  )

  results <- map_dfr(engines, function(engine) {
    map_dfr(queries, function(query) {
      tryCatch(
        search_serpapi(query, engine, serpapi_key),
        error = function(e) {
          message(sprintf("Search failed for engine=%s query='%s': %s", engine, query, e$message))
          tibble(
            title = character(), price_text = character(), price = numeric(),
            store = character(), link = character(), engine = character(), query = character()
          )
        }
      )
    })
  })

  cleaned <- results |>
    filter(!is.na(title), title != "") |>
    mutate(item = item) |>
    distinct(item, title, store, price_text, link, .keep_all = TRUE)

  rank_suggestions(item, cleaned, provider, model)
}

run_personal_shopper <- function(input_file,
                                 output_csv = "shopping_suggestions.csv",
                                 provider = c("openai", "ollama"),
                                 model = NULL,
                                 engines = DEFAULT_ENGINES) {
  provider <- match.arg(provider)

  if (is.null(model)) {
    model <- if (provider == "openai") "gpt-4.1-mini" else "llama3.1"
  }

  serpapi_key <- Sys.getenv("SERPAPI_KEY")
  if (serpapi_key == "") {
    stop("SERPAPI_KEY is required for search engine shopping lookups.")
  }

  items <- read_interest_items(input_file)

  all_results <- map_dfr(items, function(item) {
    message(sprintf("Scouting: %s", item))
    shop_item(item, provider, model, engines, serpapi_key)
  }) |>
    arrange(item, is.na(price), price)

  write_csv(all_results, output_csv)

  cat("\n=== Shopping Suggestions ===\n")
  print(all_results |>
          select(item, title, price_text, price, store, engine, ai_pick, ai_note, link), n = 100)

  invisible(all_results)
}

# CLI usage:
# Rscript personal_shopper.R interests.txt output.csv openai gpt-4.1-mini
args <- commandArgs(trailingOnly = TRUE)
if (sys.nframe() == 0) {
  if (length(args) < 1) {
    stop("Usage: Rscript personal_shopper.R <input_txt> [output_csv] [provider=openai|ollama] [model]")
  }

  input_file <- args[[1]]
  output_csv <- ifelse(length(args) >= 2, args[[2]], "shopping_suggestions.csv")
  provider <- ifelse(length(args) >= 3, args[[3]], "openai")
  model <- ifelse(length(args) >= 4, args[[4]], NA_character_)
  if (is.na(model)) model <- NULL

  run_personal_shopper(
    input_file = input_file,
    output_csv = output_csv,
    provider = provider,
    model = model
  )
}
