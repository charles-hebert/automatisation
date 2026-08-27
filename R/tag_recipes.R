# Deterministic and LLM-assisted recipe tagging.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(httr2)
  library(jsonlite)
})

load_allowed_tags <- function(path = "inst/dictionaries/recipe_tags.csv") {
  if (!file.exists(path)) {
    return(c("vegetarien", "mediterraneen", "bbq", "asiatique", "mexicain", "comfort_food", "sante"))
  }
  unique(utils::read.csv(path, stringsAsFactors = FALSE)$tag_name)
}

run_ontology_tags <- function(db) {
  dbExecute(db, "
    INSERT OR IGNORE INTO recipe_tags (recipe_id, tag_name, tag_source)
    WITH RECURSIVE tag_tree(child_id, ancestor_name) AS (
      SELECT child_id, p.name
      FROM ontology_edges e
      JOIN ontology_nodes p ON p.node_id = e.parent_id
      UNION ALL
      SELECT e.child_id, t.ancestor_name
      FROM ontology_edges e
      JOIN tag_tree t ON e.parent_id = t.child_id
    )
    SELECT DISTINCT i.recipe_id, tt.ancestor_name, 'ontology'
    FROM ingredients i
    JOIN ontology_nodes n ON lower(i.canonical_name) = lower(n.name)
    JOIN tag_tree tt ON n.node_id = tt.child_id
    WHERE i.canonical_name IS NOT NULL AND i.canonical_name <> ''
  ")
}

run_rule_tags <- function(db) {
  dbExecute(db, "
    INSERT OR IGNORE INTO recipe_tags (recipe_id, tag_name, tag_source)
    SELECT recipe_id, 'rapide', 'rule'
    FROM recipes
    WHERE (prep_min + cook_min) > 0 AND (prep_min + cook_min) <= 20
  ")
  dbExecute(db, "
    INSERT OR IGNORE INTO recipe_tags (recipe_id, tag_name, tag_source)
    SELECT DISTINCT r.recipe_id, 'vegetarien', 'rule'
    FROM recipes r
    WHERE NOT EXISTS (
      SELECT 1 FROM ingredients i
      WHERE i.recipe_id = r.recipe_id
        AND lower(i.canonical_name) IN ('beef','boeuf','pork','porc','chicken','poulet','fish','poisson','shrimp','crevette','lamb','agneau')
    )
  ")
}

run_llm_tags <- function(db, api_key, allowed_tags, model = "google/gemini-2.5-flash") {
  untagged <- dbGetQuery(db, "
    SELECT r.recipe_id, r.title
    FROM recipes r
    LEFT JOIN recipe_tags t ON r.recipe_id = t.recipe_id
    GROUP BY r.recipe_id
    HAVING COUNT(t.tag_name) = 0
  ")

  for (i in seq_len(nrow(untagged))) {
    rec_id <- untagged$recipe_id[[i]]
    ings <- dbGetQuery(db, "SELECT raw_text FROM ingredients WHERE recipe_id = ?", params = list(rec_id))
    prompt <- sprintf(
      "Pick only relevant tags from: [%s]. Title: %s. Ingredients: %s. Return strict JSON: {\"tags\":[\"tag1\"]}",
      paste(allowed_tags, collapse = ", "), untagged$title[[i]], paste(ings$raw_text, collapse = ", ")
    )

    resp <- tryCatch({
      request("https://openrouter.ai/api/v1/chat/completions") |>
        req_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json") |>
        req_body_json(list(
          model = model,
          response_format = list(type = "json_object"),
          messages = list(list(role = "user", content = prompt))
        )) |>
        req_perform()
    }, error = function(e) NULL)

    if (!is.null(resp)) {
      res <- fromJSON(resp_body_json(resp)$choices[[1]]$message$content)
      for (tag in intersect(res$tags, allowed_tags)) {
        dbExecute(db, "INSERT OR IGNORE INTO recipe_tags (recipe_id, tag_name, tag_source) VALUES (?, ?, 'llm')",
                  params = list(rec_id, tag))
      }
    }
  }
}

run_tagging_pipeline <- function(api_key = NULL, db_path = "recipes.db", allowed_tags_path = "inst/dictionaries/recipe_tags.csv", use_llm = !is.null(api_key)) {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  run_ontology_tags(db)
  run_rule_tags(db)
  if (isTRUE(use_llm)) run_llm_tags(db, api_key, load_allowed_tags(allowed_tags_path))

  message("Tagging pipeline executed successfully.")
  invisible(NULL)
}
