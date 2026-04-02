#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(argparse)
  library(gmailr)
  library(httr2)
  library(jsonlite)
  library(stringr)
})

read_preferences <- function(path = NULL) {
  if (is.null(path)) {
    return(paste(
      "Prefer newsletters about AI, software engineering, productivity, and practical tutorials.",
      "Avoid celebrity news and generic promotions unless highly relevant."
    ))
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_header <- function(headers, header_name) {
  matches <- Filter(function(h) identical(tolower(h$name), tolower(header_name)), headers)
  if (length(matches) == 0) return("")
  matches[[1]]$value %||% ""
}

`%||%` <- function(a, b) if (is.null(a)) b else a

extract_text <- function(payload) {
  if (!is.null(payload$parts)) {
    plain_part <- NULL
    html_part <- NULL

    for (part in payload$parts) {
      mime <- part$mimeType %||% ""
      if (identical(mime, "text/plain") && !is.null(part$body$data)) {
        plain_part <- part
        break
      }
      if (identical(mime, "text/html") && !is.null(part$body$data)) {
        html_part <- part
      }
    }

    target <- plain_part %||% html_part
    if (is.null(target)) return("")

    raw <- base64urldecode(target$body$data)
    txt <- rawToChar(raw)
    if (identical(target$mimeType, "text/html")) {
      txt <- str_replace_all(txt, "<[^>]+>", " ")
      txt <- str_squish(txt)
    }
    return(txt)
  }

  if (!is.null(payload$body$data)) {
    return(rawToChar(base64urldecode(payload$body$data)))
  }

  ""
}

fetch_newsletters <- function(query, max_messages) {
  ids <- gm_messages(search = query, num_results = max_messages)
  if (length(ids) == 0) return(list())

  out <- list()
  for (id in ids$id) {
    msg <- gm_message(id)
    payload <- msg$payload %||% list()
    headers <- payload$headers %||% list()

    item <- list(
      message_id = id,
      from = extract_header(headers, "From"),
      subject = extract_header(headers, "Subject"),
      date = extract_header(headers, "Date"),
      snippet = msg$snippet %||% "",
      content = str_sub(extract_text(payload), 1, 8000)
    )
    out[[length(out) + 1]] <- item
  }
  out
}

build_prompt <- function(messages, preferences) {
  serialized <- toJSON(messages, auto_unbox = TRUE, pretty = FALSE, null = "null")
  paste0(
    "You are a newsletter assistant. Given emails and user preferences, produce:\n",
    "1) A concise digest grouped by theme.\n",
    "2) Top 10 most relevant newsletters with one-line rationale.\n",
    "3) A scoring rubric (0-100) based on user preferences.\n",
    "4) A plan to improve matching over time.\n\n",
    "User preferences:\n", preferences, "\n\n",
    "Emails JSON:\n", str_sub(serialized, 1, 120000)
  )
}

call_ollama <- function(model, host, prompt) {
  req <- request(host) |>
    req_url_path_append("api", "generate") |>
    req_body_json(list(model = model, prompt = prompt, stream = FALSE)) |>
    req_timeout(120)

  resp <- req_perform(req)
  body <- resp_body_json(resp, simplifyVector = TRUE)
  body$response %||% ""
}

write_digest <- function(path, content) {
  writeLines(enc2utf8(content), con = path, useBytes = TRUE)
}

parse_args <- function() {
  parser <- ArgumentParser(description = "Create a Gmail newsletter digest with local Ollama")
  parser$add_argument("--query", default = "category:promotions newer_than:7d")
  parser$add_argument("--max-messages", type = "integer", default = 30)
  parser$add_argument("--ollama-model", default = "llama3.1:8b")
  parser$add_argument("--ollama-host", default = "http://127.0.0.1:11434")
  parser$add_argument("--preferences-file", default = NULL)
  parser$add_argument("--output", default = "newsletter_digest.md")
  parser$parse_args()
}

main <- function() {
  args <- parse_args()

  gm_auth_configure(path = "credentials.json")
  gm_auth(email = TRUE)

  preferences <- read_preferences(args$`preferences-file`)
  messages <- fetch_newsletters(args$query, args$`max-messages`)

  if (length(messages) == 0) {
    write_digest(args$output, "No messages found for this query.")
    cat("Digest created:", args$output, "\n")
    return(invisible(NULL))
  }

  prompt <- build_prompt(messages, preferences)
  digest <- call_ollama(args$`ollama-model`, args$`ollama-host`, prompt)
  write_digest(args$output, digest)

  cat("Digest created:", args$output, "\n")
}

main()
