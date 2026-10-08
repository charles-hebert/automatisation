# SQLite schema for recipe extraction, parsing, and tagging.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

find_dict_path <- function(rel_path) {
  candidates <- c(
    rel_path,
    file.path("../..", rel_path),
    file.path("..", rel_path)
  )
  for (cand in candidates) {
    if (file.exists(cand)) return(cand)
  }
  rel_path
}

init_recipe_db <- function(db_path = "recipes.db") {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)

  dbExecute(db, "PRAGMA foreign_keys = ON;")

  statements <- c(
    "CREATE TABLE IF NOT EXISTS books (
      book_id INTEGER PRIMARY KEY AUTOINCREMENT,
      title TEXT NOT NULL,
      author TEXT,
      isbn TEXT,
      publisher TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )",
    "CREATE TABLE IF NOT EXISTS raw_sources (
      source_id INTEGER PRIMARY KEY AUTOINCREMENT,
      book_id INTEGER,
      file_hash TEXT UNIQUE NOT NULL,
      file_name TEXT NOT NULL,
      file_type TEXT NOT NULL CHECK(file_type IN ('epub', 'pdf', 'image', 'url')),
      raw_content TEXT,
      status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending', 'parsed', 'error')),
      error_message TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      parsed_at DATETIME,
      FOREIGN KEY(book_id) REFERENCES books(book_id) ON DELETE SET NULL
    )",
    "CREATE TABLE IF NOT EXISTS recipes (
      recipe_id INTEGER PRIMARY KEY AUTOINCREMENT,
      source_id INTEGER,
      book_id INTEGER,
      title TEXT NOT NULL,
      prep_min INTEGER DEFAULT 0 CHECK(prep_min >= 0),
      cook_min INTEGER DEFAULT 0 CHECK(cook_min >= 0),
      servings REAL DEFAULT 4 CHECK(servings > 0),
      needs_review INTEGER NOT NULL DEFAULT 0 CHECK(needs_review IN (0, 1)),
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(source_id) REFERENCES raw_sources(source_id) ON DELETE SET NULL,
      FOREIGN KEY(book_id) REFERENCES books(book_id) ON DELETE SET NULL
    )",
    "CREATE TABLE IF NOT EXISTS recipe_steps (
      step_id INTEGER PRIMARY KEY AUTOINCREMENT,
      recipe_id INTEGER NOT NULL,
      step_number INTEGER NOT NULL CHECK(step_number > 0),
      instruction_text TEXT NOT NULL,
      UNIQUE(recipe_id, step_number),
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS ingredients_ref (
      ingredient_id INTEGER PRIMARY KEY,
      canonical_name_fr TEXT NOT NULL,
      canonical_name_en TEXT,
      category TEXT
    )",
    "CREATE TABLE IF NOT EXISTS ingredient_synonyms (
      synonym_id INTEGER PRIMARY KEY AUTOINCREMENT,
      synonym_text TEXT UNIQUE NOT NULL,
      ingredient_id INTEGER NOT NULL,
      FOREIGN KEY(ingredient_id) REFERENCES ingredients_ref(ingredient_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS ingredients (
      ingredient_id INTEGER PRIMARY KEY AUTOINCREMENT,
      recipe_id INTEGER NOT NULL,
      raw_text TEXT NOT NULL,
      canonical_name TEXT,
      quantity_num REAL,
      unit_standard TEXT,
      ref_ingredient_id INTEGER DEFAULT 0,
      match_method TEXT DEFAULT 'unmatched',
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE,
      FOREIGN KEY(ref_ingredient_id) REFERENCES ingredients_ref(ingredient_id) ON DELETE SET DEFAULT
    )",
    "CREATE TABLE IF NOT EXISTS recipe_tag_classifications (
      classification_id INTEGER PRIMARY KEY AUTOINCREMENT,
      recipe_id INTEGER NOT NULL,
      tag_name TEXT NOT NULL,
      tag_value TEXT NOT NULL,
      confidence INTEGER NOT NULL CHECK(confidence >= 0 AND confidence <= 100),
      status TEXT NOT NULL CHECK(status IN ('accepted', 'rejected', 'review')),
      tag_source TEXT NOT NULL CHECK(tag_source IN ('rule', 'llm', 'manual')),
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(recipe_id, tag_name),
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS llm_cache (
      cache_key TEXT PRIMARY KEY,
      model TEXT NOT NULL,
      prompt_json TEXT,
      response_json TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )",
    "CREATE TABLE IF NOT EXISTS recipe_equipment (
      recipe_id INTEGER NOT NULL,
      equipment_name TEXT NOT NULL,
      PRIMARY KEY (recipe_id, equipment_name),
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS ontology_nodes (
      node_id INTEGER PRIMARY KEY AUTOINCREMENT,
      name TEXT UNIQUE NOT NULL,
      node_type TEXT NOT NULL CHECK(node_type IN ('category', 'ingredient', 'allergen', 'diet', 'cuisine', 'season', 'health'))
    )",
    "CREATE TABLE IF NOT EXISTS ontology_edges (
      parent_id INTEGER NOT NULL,
      child_id INTEGER NOT NULL,
      relation_type TEXT NOT NULL DEFAULT 'is_a',
      PRIMARY KEY (parent_id, child_id),
      FOREIGN KEY(parent_id) REFERENCES ontology_nodes(node_id) ON DELETE CASCADE,
      FOREIGN KEY(child_id) REFERENCES ontology_nodes(node_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS recipe_tags (
      recipe_id INTEGER NOT NULL,
      tag_name TEXT NOT NULL,
      tag_source TEXT NOT NULL DEFAULT 'rule' CHECK(tag_source IN ('rule', 'ontology', 'llm', 'manual')),
      PRIMARY KEY (recipe_id, tag_name),
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE
    )",
    "CREATE TABLE IF NOT EXISTS grocery_deals (
      deal_id INTEGER PRIMARY KEY AUTOINCREMENT,
      merchant TEXT NOT NULL,
      name TEXT NOT NULL,
      current_price TEXT,
      pre_price TEXT,
      valid_to TEXT,
      category TEXT,
      matched_canonical_ingredient TEXT,
      postal_code TEXT,
      fetched_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )",
    "CREATE TABLE IF NOT EXISTS inventory (
      inventory_id INTEGER PRIMARY KEY AUTOINCREMENT,
      raw_ingredient TEXT NOT NULL,
      canonical_name TEXT,
      location TEXT,
      expiry_days_left INTEGER,
      expiry_date TEXT,
      is_pantry_staple INTEGER DEFAULT 0 CHECK(is_pantry_staple IN (0, 1)),
      updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )",
    "CREATE TABLE IF NOT EXISTS mealplan_reports (
      report_id INTEGER PRIMARY KEY AUTOINCREMENT,
      week_label TEXT,
      report_markdown TEXT NOT NULL,
      model_used TEXT,
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )"
  )

  for (statement in statements) dbExecute(db, statement)

  indexes <- c(
    "CREATE INDEX IF NOT EXISTS idx_raw_sources_status ON raw_sources(status)",
    "CREATE INDEX IF NOT EXISTS idx_ingredients_canonical ON ingredients(canonical_name)",
    "CREATE INDEX IF NOT EXISTS idx_ingredients_ref ON ingredients(ref_ingredient_id)",
    "CREATE INDEX IF NOT EXISTS idx_recipe_tags_tag ON recipe_tags(tag_name)",
    "CREATE INDEX IF NOT EXISTS idx_recipe_tag_classifications_status ON recipe_tag_classifications(status)",
    "CREATE INDEX IF NOT EXISTS idx_grocery_deals_merchant ON grocery_deals(merchant)",
    "CREATE INDEX IF NOT EXISTS idx_grocery_deals_matched ON grocery_deals(matched_canonical_ingredient)",
    "CREATE INDEX IF NOT EXISTS idx_inventory_canonical ON inventory(canonical_name)",
    "CREATE INDEX IF NOT EXISTS idx_mealplan_reports_created ON mealplan_reports(created_at)"
  )
  for (idx in indexes) dbExecute(db, idx)

  # Migration logic for existing tables if columns are missing
  ing_cols <- dbGetQuery(db, "PRAGMA table_info(ingredients)")$name
  if (!("ref_ingredient_id" %in% ing_cols)) {
    dbExecute(db, "ALTER TABLE ingredients ADD COLUMN ref_ingredient_id INTEGER DEFAULT 0")
  }
  if (!("match_method" %in% ing_cols)) {
    dbExecute(db, "ALTER TABLE ingredients ADD COLUMN match_method TEXT DEFAULT 'unmatched'")
  }

  # Create Power BI / Optimizer view
  dbExecute(db, "
    CREATE VIEW IF NOT EXISTS v_recipe_tags_accepted AS
    SELECT recipe_id, tag_name, tag_value, confidence, tag_source
    FROM recipe_tag_classifications
    WHERE status = 'accepted';
  ")

  # Seed initial reference ingredients
  seed_reference_ingredients(db)

  message("Database schema initialized successfully: ", db_path)
  invisible(db_path)
}

seed_reference_ingredients <- function(db, csv_path = NULL) {
  # Insert reserved unmatched ID 0
  dbExecute(db, "
    INSERT OR IGNORE INTO ingredients_ref (ingredient_id, canonical_name_fr, canonical_name_en, category)
    VALUES (0, 'Inconnu', 'Unknown', 'unmatched')
  ")

  real_csv_path <- if (is.null(csv_path)) find_dict_path("inst/dictionaries/bilingual_ingredients.csv") else csv_path

  if (!file.exists(real_csv_path)) return(invisible(NULL))

  df <- tryCatch(
    utils::read.csv(real_csv_path, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))

  for (i in seq_len(nrow(df))) {
    canon <- trimws(tolower(df$canonical_name[[i]] %||% ""))
    french <- trimws(tolower(df$french_name[[i]] %||% ""))
    english <- trimws(tolower(df$english_name[[i]] %||% ""))
    cat <- trimws(tolower(df$category[[i]] %||% "other"))

    fr_primary <- trimws(strsplit(french, ",")[[1]][1])
    en_primary <- if (nzchar(english)) trimws(strsplit(english, ",")[[1]][1]) else canon

    if (!nzchar(fr_primary)) next

    dbExecute(db, "
      INSERT OR IGNORE INTO ingredients_ref (canonical_name_fr, canonical_name_en, category)
      VALUES (?, ?, ?)
    ", params = list(fr_primary, en_primary, cat))

    ref_row <- dbGetQuery(db, "SELECT ingredient_id FROM ingredients_ref WHERE canonical_name_fr = ?", params = list(fr_primary))
    if (nrow(ref_row) == 0) next
    ref_id <- ref_row$ingredient_id[[1]]

    # Process all synonym terms
    synonyms <- c(canon, strsplit(french, ",")[[1]], strsplit(english, ",")[[1]])
    synonyms <- unique(trimws(tolower(synonyms)))
    synonyms <- synonyms[nzchar(synonyms)]

    for (syn in synonyms) {
      dbExecute(db, "
        INSERT OR IGNORE INTO ingredient_synonyms (synonym_text, ingredient_id)
        VALUES (?, ?)
      ", params = list(syn, ref_id))
    }
  }

  invisible(NULL)
}
