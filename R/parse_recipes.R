# LLM-assisted parsing of staged recipe sources into normalized tables.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(httr2)
  library(jsonlite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

recipe_schema_prompt <- function() {
  'You are a professional recipe extraction API. Extract every recipe and return strict JSON with this shape: {"recipes":[{"title":"Recipe Name","prep_min":10,"cook_min":20,"servings":4,"equipment":["oven"],"instructions":["Step text"],"ingredients":[{"raw_text":"2 cups spinach","canonical_name":"spinach","quantity_num":2,"unit_standard":"cup"}]}]}. Use null for unknown numbers and [] for missing lists.'
}

openrouter_json <- function(api_key, messages, model = "google/gemini-2.5-flash") {
  req <- request("https://openrouter.ai/api/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = model,
      response_format = list(type = "json_object"),
      messages = messages
    ))

  resp <- req_perform(req)
  content <- resp_body_json(resp, simplifyVector = FALSE)$choices[[1]]$message$content
  fromJSON(content, simplifyVector = FALSE)
}

safe_number <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  as.numeric(x)
}

source_status_counts <- function(db) {
  dbGetQuery(db, "
    SELECT status, COUNT(*) AS source_count
    FROM raw_sources
    GROUP BY status
    ORDER BY status
  ")
}

format_source_status_counts <- function(counts) {
  if (nrow(counts) == 0) return("no staged sources")
  paste(sprintf("%s=%s", counts$status, counts$source_count), collapse = ", ")
}

insert_recipe <- function(db, rec, source_id, book_id) {
  ingredients <- rec$ingredients %||% list()
  instructions <- rec$instructions %||% list()
  title <- rec$title %||% "Unknown"
  needs_review <- as.integer(identical(title, "Unknown") || length(ingredients) == 0 || length(instructions) == 0)

  dbExecute(db, "
    INSERT INTO recipes (source_id, book_id, title, prep_min, cook_min, servings, needs_review)
    VALUES (?, ?, ?, ?, ?, ?, ?)
  ", params = list(
    source_id, book_id, title,
    safe_number(rec$prep_min, 0), safe_number(rec$cook_min, 0), safe_number(rec$servings, 4), needs_review
  ))

  recipe_id <- dbGetQuery(db, "SELECT last_insert_rowid() AS id")$id[[1]]

  for (idx in seq_along(instructions)) {
    text <- trimws(as.character(instructions[[idx]]))
    if (nzchar(text)) {
      dbExecute(db, "INSERT INTO recipe_steps (recipe_id, step_number, instruction_text) VALUES (?, ?, ?)",
                params = list(recipe_id, idx, text))
    }
  }

  for (ing in ingredients) {
    dbExecute(db, "
      INSERT INTO ingredients (recipe_id, raw_text, canonical_name, quantity_num, unit_standard)
      VALUES (?, ?, ?, ?, ?)
    ", params = list(
      recipe_id,
      ing$raw_text %||% "",
      tolower(trimws(ing$canonical_name %||% "")),
      safe_number(ing$quantity_num, NA_real_),
      tolower(trimws(ing$unit_standard %||% ""))
    ))
  }

  for (eq in rec$equipment %||% list()) {
    equipment <- tolower(trimws(as.character(eq)))
    if (nzchar(equipment)) {
      dbExecute(db, "INSERT OR IGNORE INTO recipe_equipment (recipe_id, equipment_name) VALUES (?, ?)",
                params = list(recipe_id, equipment))
    }
  }

  recipe_id
}

run_parsing_pipeline <- function(api_key, db_path = "recipes.db", model = "google/gemini-2.5-flash", limit = Inf) {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")
  if (!dbExistsTable(db, "raw_sources")) {
    stop(
      "Recipe database is not initialized at: ", normalizePath(db_path, mustWork = FALSE),
      ". Run init_recipe_db(db_path) and run_extraction_pipeline(..., db_path) first.",
      call. = FALSE
    )
  }

  pending <- dbGetQuery(db, "
    SELECT source_id, book_id, file_type, raw_content
    FROM raw_sources
    WHERE status = 'pending'
    ORDER BY source_id
  ")
  if (is.finite(limit)) pending <- utils::head(pending, limit)
  if (nrow(pending) == 0) {
    counts <- format_source_status_counts(source_status_counts(db))
    message(
      "No pending sources to parse in ", normalizePath(db_path, mustWork = FALSE),
      " (", counts, "). ",
      "A file in the inbox is not staged automatically; run run_extraction_pipeline() with this same db_path first."
    )
    return(invisible(NULL))
  }

  for (i in seq_len(nrow(pending))) {
    src_id <- pending$source_id[[i]]
    user_content <- if (pending$file_type[[i]] == "image") {
      list(
        list(type = "text", text = "Extract recipes from this image."),
        list(type = "image_url", image_url = list(url = pending$raw_content[[i]]))
      )
    } else {
      paste("Extract recipes from this text:\n\n", pending$raw_content[[i]])
    }

    parsed_json <- tryCatch(
      openrouter_json(api_key, list(
        list(role = "system", content = recipe_schema_prompt()),
        list(role = "user", content = user_content)
      ), model = model),
      error = function(e) e
    )

    if (inherits(parsed_json, "error")) {
      dbExecute(db, "UPDATE raw_sources SET status = 'error', error_message = ? WHERE source_id = ?",
                params = list(parsed_json$message, src_id))
      next
    }

    dbBegin(db)
    tryCatch({
      for (rec in parsed_json$recipes %||% list()) insert_recipe(db, rec, src_id, pending$book_id[[i]])
      dbExecute(db, "UPDATE raw_sources SET status = 'parsed', parsed_at = CURRENT_TIMESTAMP, error_message = NULL WHERE source_id = ?",
                params = list(src_id))
      dbCommit(db)
      message(sprintf("Successfully parsed source_id: %d", src_id))
    }, error = function(e) {
      dbRollback(db)
      dbExecute(db, "UPDATE raw_sources SET status = 'error', error_message = ? WHERE source_id = ?",
                params = list(e$message, src_id))
      message(sprintf("Parsing failed for source_id %d: %s", src_id, e$message))
    })
  }

  invisible(NULL)
}
