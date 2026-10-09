# LLM classification engine using OpenRouter.ai API with caching and error handling.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(httr2)
  library(jsonlite)
  library(digest)
  library(logger)
})

find_and_source_internal <- function(rel_path) {
  candidates <- c(rel_path, file.path("../..", rel_path), file.path("..", rel_path))
  for (cand in candidates) {
    if (file.exists(cand)) {
      source(cand)
      return(invisible(TRUE))
    }
  }
}
find_and_source_internal("R/config.R")

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

find_file_path <- function(rel_path) {
  candidates <- c(rel_path, file.path("../..", rel_path), file.path("..", rel_path))
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  rel_path
}

#' Compute cache key for a recipe classification request.
#'
#' @param recipe_title Recipe title
#' @param normalized_ingredient_ids Integer vector of normalized ingredient reference IDs
#' @param model LLM model name
#' @return MD5 hash string
compute_cache_key <- function(recipe_title, normalized_ingredient_ids, model) {
  clean_title <- tolower(trimws(recipe_title %||% ""))
  sorted_ids <- sort(as.integer(normalized_ingredient_ids[!is.na(normalized_ingredient_ids)]))
  raw_str <- paste0(clean_title, "|", paste(sorted_ids, collapse = ","), "|", model)
  digest(raw_str, algo = "md5")
}

#' Call OpenRouter.ai API to classify a recipe across multiple questions in a single request.
#'
#' @param recipe_data List with title, prep_min, cook_min, ingredients
#' @param questions Data frame or list of questions with tag_name and question prompt
#' @param api_key OpenRouter API key
#' @param model OpenRouter model identifier (default best value for recipe tagging)
#' @param timeout_sec HTTP request timeout in seconds
#' @return List of parsed tag classifications or error object
classify_recipe <- function(recipe_data,
                            questions,
                            api_key = Sys.getenv("OPENROUTER_API_KEY"),
                            model = "google/gemini-2.5-flash",
                            timeout_sec = 30) {
  if (!nzchar(api_key)) {
    log_warn("OPENROUTER_API_KEY is not set or empty.")
    return(structure(list(error = "Missing API Key"), class = "llm_error"))
  }

  q_desc <- character(0)
  if (is.data.frame(questions)) {
    for (i in seq_len(nrow(questions))) {
      q_desc <- c(q_desc, sprintf("- %s: %s", questions$tag_name[[i]], questions$question[[i]]))
    }
  } else if (is.list(questions)) {
    for (q in questions) {
      q_desc <- c(q_desc, sprintf("- %s: %s", q$tag_name %||% q$tag, q$question))
    }
  }

  system_prompt <- paste0(
    "You are a professional culinary and dietetics classification API.\n",
    "Analyze the provided recipe and answer the following questions.\n",
    "IMPORTANT: Return strict JSON where the top-level key is 'tags'.\n",
    "Each tag must contain 'value' (boolean, number, or string) and 'confidence' (integer from 1 to 100).\n",
    "Format example:\n",
    "{\n",
    "  \"tags\": {\n",
    "    \"mediterranean\": {\"value\": true, \"confidence\": 95},\n",
    "    \"family_friendly\": {\"value\": 8, \"confidence\": 80}\n",
    "  }\n",
    "}\n\n",
    "Questions to answer:\n",
    paste(q_desc, collapse = "\n")
  )

  ing_list <- if (is.data.frame(recipe_data$ingredients)) {
    paste(recipe_data$ingredients$raw_text, collapse = ", ")
  } else if (is.list(recipe_data$ingredients)) {
    paste(sapply(recipe_data$ingredients, function(x) x$raw_text %||% x), collapse = ", ")
  } else {
    as.character(recipe_data$ingredients %||% "")
  }

  user_prompt <- sprintf(
    "Recipe Title: %s\nPrep Time: %d mins | Cook Time: %d mins\nIngredients: %s",
    recipe_data$title %||% "Unknown",
    as.integer(recipe_data$prep_min %||% 0),
    as.integer(recipe_data$cook_min %||% 0),
    ing_list
  )

  # Note for OpenRouter headers:
  # HTTP-Referer and X-Title are recommended by OpenRouter for site rankings and identification.
  # Modify referer_url and app_title below as needed for production.
  referer_url <- "https://github.com/mealplan"
  app_title <- "MealPlan Recipe Classifier"

  req <- request("https://openrouter.ai/api/v1/chat/completions") |>
    req_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json",
      `HTTP-Referer` = referer_url,
      `X-Title` = app_title
    ) |>
    req_timeout(timeout_sec) |>
    req_body_json(list(
      model = model,
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      )
    ))

  resp <- tryCatch({
    req_perform(req)
  }, error = function(e) {
    log_error("OpenRouter HTTP request failed: {e$message}")
    return(structure(list(error = e$message), class = "llm_error"))
  })

  if (inherits(resp, "llm_error")) return(resp)

  status_code <- resp_status(resp)
  if (status_code < 200 || status_code >= 300) {
    log_error("OpenRouter API returned HTTP status {status_code}")
    return(structure(list(error = sprintf("HTTP %d", status_code)), class = "llm_error"))
  }

  body_json <- tryCatch({
    resp_body_json(resp, simplifyVector = FALSE)
  }, error = function(e) {
    log_error("Failed to parse OpenRouter JSON body: {e$message}")
    return(structure(list(error = e$message), class = "llm_error"))
  })

  if (inherits(body_json, "llm_error")) return(body_json)

  content_str <- body_json$choices[[1]]$message$content %||% "{}"

  parsed_payload <- tryCatch({
    fromJSON(content_str, simplifyVector = FALSE)
  }, error = function(e) {
    log_error("Failed to parse OpenRouter content JSON: {e$message}")
    return(structure(list(error = e$message), class = "llm_error"))
  })

  if (inherits(parsed_payload, "llm_error")) return(parsed_payload)

  parsed_payload
}

#' Run LLM classification pipeline for recipes in the database.
#'
#' @param db_path Path to SQLite database
#' @param questions_path Path to tag_questions.csv
#' @param thresholds_path Path to classification_thresholds.csv
#' @param api_key OpenRouter API Key
#' @param model OpenRouter model ID
run_llm_classification_pipeline <- function(db_path = "recipes.db",
                                           questions_path = "inst/dictionaries/tag_questions.csv",
                                           thresholds_path = "inst/dictionaries/classification_thresholds.csv",
                                           api_key = Sys.getenv("OPENROUTER_API_KEY"),
                                           model = "google/gemini-2.5-flash") {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  real_questions_path <- find_file_path(questions_path)
  real_thresholds_path <- find_file_path(thresholds_path)

  if (!file.exists(real_questions_path)) {
    log_error("Questions file not found: {questions_path}")
    return(invisible(0))
  }

  questions <- read.csv(real_questions_path, stringsAsFactors = FALSE)
  if (nrow(questions) == 0) {
    log_info("No questions in {questions_path}")
    return(invisible(0))
  }

  thresholds <- if (file.exists(real_thresholds_path)) {
    read.csv(real_thresholds_path, stringsAsFactors = FALSE)
  } else {
    data.frame(
      tag_name = questions$tag_name,
      accept_threshold = 80,
      reject_threshold = 40,
      stringsAsFactors = FALSE
    )
  }

  recs <- dbGetQuery(db, "SELECT recipe_id, title, prep_min, cook_min FROM recipes")
  if (nrow(recs) == 0) {
    log_info("No recipes found for LLM classification.")
    return(invisible(0))
  }

  log_info("Running LLM classification for {nrow(recs)} recipes using model '{model}'.")

  for (i in seq_len(nrow(recs))) {
    rid <- recs$recipe_id[[i]]
    title <- recs$title[[i]]
    prep_min <- recs$prep_min[[i]]
    cook_min <- recs$cook_min[[i]]

    ings <- dbGetQuery(db, "
      SELECT raw_text, ref_ingredient_id
      FROM ingredients
      WHERE recipe_id = ?
    ", params = list(rid))

    ref_ids <- ings$ref_ingredient_id
    cache_key <- compute_cache_key(title, ref_ids, model)

    # Check cache
    cached <- dbGetQuery(db, "
      SELECT response_json FROM llm_cache WHERE cache_key = ?
    ", params = list(cache_key))

    res_payload <- NULL

    if (nrow(cached) > 0 && !is.na(cached$response_json[[1]])) {
      log_info("LLM cache hit for recipe_id {rid} ({title})")
      res_payload <- tryCatch(
        fromJSON(cached$response_json[[1]], simplifyVector = FALSE),
        error = function(e) NULL
      )
    }

    if (is.null(res_payload)) {
      log_info("LLM cache miss for recipe_id {rid} ({title}). Calling OpenRouter...")
      rec_data <- list(
        title = title,
        prep_min = prep_min,
        cook_min = cook_min,
        ingredients = ings
      )

      llm_res <- classify_recipe(rec_data, questions, api_key = api_key, model = model)

      if (inherits(llm_res, "llm_error")) {
        log_warn("LLM error for recipe_id {rid}: {llm_res$error}. Setting tags to 'review'.")
        # Mark tags as review due to error
        for (q_idx in seq_len(nrow(questions))) {
          tname <- questions$tag_name[[q_idx]]
          dbExecute(db, "
            INSERT INTO recipe_tag_classifications (recipe_id, tag_name, tag_value, confidence, status, tag_source)
            VALUES (?, ?, 'unknown', 0, 'review', 'llm')
            ON CONFLICT(recipe_id, tag_name) DO UPDATE SET
              tag_value = 'unknown',
              confidence = 0,
              status = 'review',
              tag_source = 'llm',
              updated_at = CURRENT_TIMESTAMP
          ", params = list(rid, tname))
        }
        next
      }

      res_payload <- llm_res

      # Store in cache
      dbExecute(db, "
        INSERT OR REPLACE INTO llm_cache (cache_key, model, response_json)
        VALUES (?, ?, ?)
      ", params = list(cache_key, model, toJSON(res_payload, auto_unbox = TRUE)))
    }

    # Evaluate thresholds and update classifications
    tag_results <- res_payload$tags %||% res_payload

    for (q_idx in seq_len(nrow(questions))) {
      tname <- questions$tag_name[[q_idx]]
      tag_item <- tag_results[[tname]]

      t_row <- thresholds[thresholds$tag_name == tname, ]
      acc_thresh <- if (nrow(t_row) > 0) t_row$accept_threshold[[1]] else 80
      rej_thresh <- if (nrow(t_row) > 0) t_row$reject_threshold[[1]] else 40

      if (is.null(tag_item)) {
        val_str <- "unknown"
        conf <- 0L
        status <- "review"
      } else {
        val_raw <- tag_item$value %||% FALSE
        val_str <- tolower(as.character(val_raw))
        conf <- as.integer(tag_item$confidence %||% 50)

        status <- if (conf >= acc_thresh) {
          "accepted"
        } else if (conf <= rej_thresh) {
          "rejected"
        } else {
          "review"
        }
      }

      dbExecute(db, "
        INSERT INTO recipe_tag_classifications (recipe_id, tag_name, tag_value, confidence, status, tag_source)
        VALUES (?, ?, ?, ?, ?, 'llm')
        ON CONFLICT(recipe_id, tag_name) DO UPDATE SET
          tag_value = excluded.tag_value,
          confidence = excluded.confidence,
          status = excluded.status,
          tag_source = excluded.tag_source,
          updated_at = CURRENT_TIMESTAMP
      ", params = list(rid, tname, val_str, conf, status))
    }
  }

  log_info("LLM classification pipeline completed.")
  invisible(nrow(recs))
}
