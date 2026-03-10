#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(glue)
  library(httr2)
  library(jsonlite)
  library(stringr)
  library(purrr)
  library(dplyr)
  library(tidyr)
  library(xml2)
  library(rvest)
  library(igraph)
})

# -------------------------------
# Configuration
# -------------------------------
CONFIG <- list(
  db_host = Sys.getenv("PGHOST", "localhost"),
  db_port = as.integer(Sys.getenv("PGPORT", "5432")),
  db_name = Sys.getenv("PGDATABASE", "cookbook_ai"),
  db_user = Sys.getenv("PGUSER", "postgres"),
  db_password = Sys.getenv("PGPASSWORD", "postgres"),
  openai_api_key = Sys.getenv("OPENAI_API_KEY", ""),
  openai_model = Sys.getenv("OPENAI_MODEL", "gpt-4o-mini"),
  embedding_model = Sys.getenv("EMBEDDING_MODEL", "all-MiniLM-L6-v2")
)

# -------------------------------
# Database schema
# -------------------------------
SCHEMA_SQL <- "
CREATE EXTENSION IF NOT EXISTS vector;

CREATE TABLE IF NOT EXISTS recipes (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  source_book TEXT,
  servings TEXT,
  time_minutes INTEGER,
  instructions TEXT
);

CREATE TABLE IF NOT EXISTS ingredients_raw (
  id SERIAL PRIMARY KEY,
  recipe_id INTEGER REFERENCES recipes(id) ON DELETE CASCADE,
  raw_text TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS ingredients (
  id SERIAL PRIMARY KEY,
  recipe_id INTEGER REFERENCES recipes(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  quantity NUMERIC,
  unit TEXT
);

CREATE TABLE IF NOT EXISTS ingredient_alias (
  alias TEXT PRIMARY KEY,
  canonical_name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS ingredient_substitutions (
  ingredient TEXT NOT NULL,
  substitute TEXT NOT NULL,
  confidence NUMERIC NOT NULL,
  PRIMARY KEY (ingredient, substitute)
);

CREATE TABLE IF NOT EXISTS recipe_embeddings (
  recipe_id INTEGER PRIMARY KEY REFERENCES recipes(id) ON DELETE CASCADE,
  embedding VECTOR(384)
);
"

db_connect <- function(config = CONFIG) {
  dbConnect(
    RPostgres::Postgres(),
    host = config$db_host,
    port = config$db_port,
    dbname = config$db_name,
    user = config$db_user,
    password = config$db_password
  )
}

ensure_schema <- function(con) {
  dbExecute(con, SCHEMA_SQL)
}

# -------------------------------
# EPUB extraction
# -------------------------------
extract_epub_text <- function(epub_path) {
  stopifnot(file.exists(epub_path))

  listed <- unzip(epub_path, list = TRUE)
  html_files <- listed$Name[str_detect(listed$Name, "\\.(xhtml|html|htm)$")]

  if (length(html_files) == 0) stop("No HTML/XHTML files found in EPUB")

  blocks <- map_chr(html_files, function(f) {
    raw <- unzip(epub_path, files = f, exdir = tempdir(), overwrite = TRUE)
    read_html(raw) |>
      html_text2()
  })

  paste(blocks, collapse = "\n\n")
}

# -------------------------------
# Recipe parsing (heuristic + LLM)
# -------------------------------
split_recipe_blocks <- function(raw_text) {
  normalized <- str_replace_all(raw_text, "\r", "")
  blocks <- str_split(normalized, "\n{3,}")[[1]]
  blocks <- blocks[str_length(str_squish(blocks)) > 60]
  blocks
}

openai_chat_json <- function(system_prompt, user_prompt, config = CONFIG) {
  if (config$openai_api_key == "") {
    stop("OPENAI_API_KEY is not set.")
  }

  req <- request("https://api.openai.com/v1/chat/completions") |>
    req_headers(
      Authorization = paste("Bearer", config$openai_api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = config$openai_model,
      response_format = list(type = "json_object"),
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = user_prompt)
      )
    ))

  resp <- req_perform(req)
  payload <- resp_body_json(resp, simplifyVector = TRUE)
  payload$choices[[1]]$message$content
}

parse_recipes_with_llm <- function(raw_text, source_book = NULL, config = CONFIG) {
  prompt <- paste(
    "Extract recipes from this text. Return strict JSON with key recipes.",
    "Each recipe must include: title, ingredients (array of strings),",
    "instructions (array of strings), servings, time_minutes.",
    "If uncertain, return best effort and keep fields nullable.",
    "Text:", raw_text
  )

  body <- openai_chat_json(
    system_prompt = "You extract cookbook recipes into valid JSON only.",
    user_prompt = prompt,
    config = config
  )

  parsed <- fromJSON(body, simplifyVector = FALSE)
  recipes <- parsed$recipes %||% list()

  map(recipes, function(r) {
    list(
      title = r$title %||% "Untitled Recipe",
      source_book = source_book,
      servings = as.character(r$servings %||% NA),
      time_minutes = suppressWarnings(as.integer(r$time_minutes %||% NA)),
      instructions = paste(r$instructions %||% character(), collapse = "\n"),
      ingredients_raw = r$ingredients %||% character()
    )
  })
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

insert_recipe <- function(con, recipe) {
  rid <- dbGetQuery(
    con,
    "INSERT INTO recipes (title, source_book, servings, time_minutes, instructions)
     VALUES ($1,$2,$3,$4,$5)
     RETURNING id",
    params = list(recipe$title, recipe$source_book, recipe$servings, recipe$time_minutes, recipe$instructions)
  )$id[[1]]

  walk(recipe$ingredients_raw, function(raw_ing) {
    dbExecute(
      con,
      "INSERT INTO ingredients_raw (recipe_id, raw_text) VALUES ($1,$2)",
      params = list(rid, raw_ing)
    )
  })

  rid
}

# -------------------------------
# Ingredient normalization
# -------------------------------
parse_ingredient_line <- function(line) {
  m <- str_match(
    line,
    "^\\s*([0-9]+(?:[./][0-9]+)?(?:\\s+[0-9]+/[0-9]+)?)?\\s*([a-zA-Z]+)?\\s*(.*)$"
  )

  qty_raw <- str_squish(m[, 2])
  qty <- suppressWarnings(as.numeric(gsub(" ", "+", qty_raw, fixed = TRUE)))
  if (is.na(qty) && str_detect(qty_raw, "/")) {
    pieces <- str_split(qty_raw, "\\s+")[[1]]
    qty <- sum(map_dbl(pieces, function(p) {
      if (str_detect(p, "/")) {
        xy <- as.numeric(str_split(p, "/")[[1]])
        xy[1] / xy[2]
      } else {
        as.numeric(p)
      }
    }), na.rm = TRUE)
  }

  list(
    quantity = ifelse(is.na(qty), NA_real_, qty),
    unit = tolower(m[, 3] %||% NA_character_),
    descriptor = str_squish(m[, 4] %||% line)
  )
}

canonicalize_ingredient <- function(descriptor, config = CONFIG) {
  prompt <- glue(
    "Normalize ingredient phrase to canonical singular food name only.\nInput: {descriptor}\nOutput:"
  )

  out <- openai_chat_json(
    system_prompt = "Return JSON {\"canonical\": \"name\"} only.",
    user_prompt = prompt,
    config = config
  )

  fromJSON(out)$canonical |> tolower() |> str_trim()
}

normalize_ingredients <- function(con, config = CONFIG) {
  raw_rows <- dbGetQuery(
    con,
    "SELECT ir.id, ir.recipe_id, ir.raw_text
       FROM ingredients_raw ir
      LEFT JOIN ingredients i ON i.recipe_id = ir.recipe_id AND i.name = ir.raw_text
      ORDER BY ir.id"
  )

  if (nrow(raw_rows) == 0) return(invisible(NULL))

  apply(raw_rows, 1, function(r) {
    parsed <- parse_ingredient_line(r[["raw_text"]])
    alias <- tolower(str_trim(parsed$descriptor))

    known <- dbGetQuery(
      con,
      "SELECT canonical_name FROM ingredient_alias WHERE alias = $1",
      params = list(alias)
    )

    canonical <- if (nrow(known) > 0) {
      known$canonical_name[[1]]
    } else {
      c <- canonicalize_ingredient(alias, config = config)
      dbExecute(
        con,
        "INSERT INTO ingredient_alias (alias, canonical_name)
         VALUES ($1,$2)
         ON CONFLICT(alias) DO UPDATE SET canonical_name = EXCLUDED.canonical_name",
        params = list(alias, c)
      )
      c
    }

    dbExecute(
      con,
      "INSERT INTO ingredients (recipe_id, name, quantity, unit)
       VALUES ($1,$2,$3,$4)
       ON CONFLICT DO NOTHING",
      params = list(as.integer(r[["recipe_id"]]), canonical, parsed$quantity, parsed$unit)
    )
  })

  invisible(NULL)
}

# -------------------------------
# Graph building
# -------------------------------
build_ingredient_graph <- function(con) {
  edges <- dbGetQuery(
    con,
    "SELECT r.title AS recipe, i.name AS ingredient
       FROM ingredients i
       JOIN recipes r ON r.id = i.recipe_id"
  )

  g <- graph_from_data_frame(edges, directed = FALSE)
  V(g)$type <- ifelse(V(g)$name %in% unique(edges$ingredient), "ingredient", "recipe")
  g
}

build_substitution_graph <- function(con, config = CONFIG) {
  prompt <- paste(
    "List common culinary substitutions as JSON with key substitutions.",
    "Each item: ingredient, substitute, confidence (0-1).",
    "Return 30 diverse pairs."
  )

  out <- openai_chat_json(
    system_prompt = "Return JSON only.",
    user_prompt = prompt,
    config = config
  )

  subs <- fromJSON(out, simplifyDataFrame = TRUE)$substitutions
  if (is.null(subs) || nrow(subs) == 0) return(make_empty_graph())

  pmap(subs, function(ingredient, substitute, confidence) {
    dbExecute(
      con,
      "INSERT INTO ingredient_substitutions (ingredient, substitute, confidence)
       VALUES ($1,$2,$3)
       ON CONFLICT (ingredient, substitute)
       DO UPDATE SET confidence = EXCLUDED.confidence",
      params = list(tolower(ingredient), tolower(substitute), as.numeric(confidence))
    )
  })

  graph_from_data_frame(subs[, c("ingredient", "substitute")], directed = TRUE)
}

# -------------------------------
# Meal planning + shopping
# -------------------------------
meal_plan <- function(con, fridge_ingredients, max_time_minutes = 60, top_n = 10) {
  fridge <- tolower(fridge_ingredients)

  q <- dbGetQuery(
    con,
    "SELECT r.id, r.title, r.time_minutes,
            array_agg(i.name) AS ingredients
       FROM recipes r
       JOIN ingredients i ON i.recipe_id = r.id
      WHERE COALESCE(r.time_minutes, 0) <= $1
      GROUP BY r.id, r.title, r.time_minutes",
    params = list(as.integer(max_time_minutes))
  )

  scored <- q |>
    rowwise() |>
    mutate(
      total = length(ingredients),
      matched = sum(unlist(ingredients) %in% fridge),
      score = ifelse(total == 0, 0, matched / total)
    ) |>
    ungroup() |>
    arrange(desc(score), time_minutes) |>
    slice_head(n = top_n)

  scored
}

shopping_list <- function(con, recipe_ids, fridge_ingredients) {
  fridge <- tolower(fridge_ingredients)

  req <- dbGetQuery(
    con,
    "SELECT recipe_id, name, COALESCE(quantity, 0) AS quantity, COALESCE(unit, '') AS unit
       FROM ingredients
      WHERE recipe_id = ANY($1)",
    params = list(as.integer(recipe_ids))
  )

  req |>
    filter(!(name %in% fridge)) |>
    group_by(name, unit) |>
    summarise(total_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") |>
    arrange(name)
}

# -------------------------------
# Embeddings + semantic search
# -------------------------------
embed_recipes <- function(con, config = CONFIG) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate is required for sentence-transformers embeddings.")
  }

  st <- reticulate::import("sentence_transformers")
  model <- st$SentenceTransformer(config$embedding_model)

  rows <- dbGetQuery(
    con,
    "SELECT r.id, r.title, r.instructions,
            string_agg(i.name, ', ') AS ingredients
       FROM recipes r
       LEFT JOIN ingredients i ON i.recipe_id = r.id
      GROUP BY r.id, r.title, r.instructions"
  )

  apply(rows, 1, function(rr) {
    text <- paste(rr[["title"]], rr[["ingredients"]], rr[["instructions"]], sep = "\n")
    emb <- model$encode(text)$tolist()
    emb_sql <- paste0("[", paste(emb, collapse = ","), "]")

    dbExecute(
      con,
      "INSERT INTO recipe_embeddings (recipe_id, embedding)
       VALUES ($1, $2::vector)
       ON CONFLICT (recipe_id) DO UPDATE SET embedding = EXCLUDED.embedding",
      params = list(as.integer(rr[["id"]]), emb_sql)
    )
  })

  invisible(NULL)
}

semantic_search <- function(con, query_text, config = CONFIG, top_n = 5) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate is required for semantic search.")
  }

  st <- reticulate::import("sentence_transformers")
  model <- st$SentenceTransformer(config$embedding_model)
  q_emb <- model$encode(query_text)$tolist()
  q_sql <- paste0("[", paste(q_emb, collapse = ","), "]")

  dbGetQuery(
    con,
    "SELECT r.id, r.title, (1 - (re.embedding <=> $1::vector)) AS similarity
       FROM recipe_embeddings re
       JOIN recipes r ON r.id = re.recipe_id
      ORDER BY re.embedding <=> $1::vector
      LIMIT $2",
    params = list(q_sql, as.integer(top_n))
  )
}

# -------------------------------
# Workflow helper
# -------------------------------
write_n8n_workflow_markdown <- function(path = "workflow/n8n_pipeline.md") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "# n8n Cookbook Pipeline",
    "",
    "1. Watch `/cookbooks` for new EPUB files",
    "2. Run `extract_epub_text()`",
    "3. Run `parse_recipes_with_llm()`",
    "4. Insert recipes + raw ingredients",
    "5. Run `normalize_ingredients()`",
    "6. Refresh ingredient graph and substitution graph",
    "7. Run `embed_recipes()`",
    "8. Schedule weekly full refresh"
  )
  writeLines(lines, path)
}

# -------------------------------
# CLI
# -------------------------------
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) < 1) {
    cat("Usage: Rscript epub_cookbook_ingestor.r <cookbook.epub> [source_book]\n")
    quit(status = 1)
  }

  epub_path <- args[[1]]
  source_book <- ifelse(length(args) >= 2, args[[2]], basename(epub_path))

  con <- db_connect()
  on.exit(dbDisconnect(con), add = TRUE)

  ensure_schema(con)

  raw_text <- extract_epub_text(epub_path)
  recipes <- parse_recipes_with_llm(raw_text, source_book = source_book)

  if (length(recipes) == 0) {
    message("No recipes extracted.")
    quit(status = 0)
  }

  recipe_ids <- map_int(recipes, ~ insert_recipe(con, .x))
  normalize_ingredients(con)
  build_ingredient_graph(con)
  build_substitution_graph(con)
  embed_recipes(con)
  write_n8n_workflow_markdown()

  message(glue("Ingested {length(recipe_ids)} recipes from {source_book}."))
}

if (sys.nframe() == 0) {
  main()
}
