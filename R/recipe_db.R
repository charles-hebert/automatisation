# SQLite schema for recipe extraction, parsing, and tagging.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
})

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
    "CREATE TABLE IF NOT EXISTS ingredients (
      ingredient_id INTEGER PRIMARY KEY AUTOINCREMENT,
      recipe_id INTEGER NOT NULL,
      raw_text TEXT NOT NULL,
      canonical_name TEXT,
      quantity_num REAL,
      unit_standard TEXT,
      FOREIGN KEY(recipe_id) REFERENCES recipes(recipe_id) ON DELETE CASCADE
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
    )"
  )

  for (statement in statements) dbExecute(db, statement)

  indexes <- c(
    "CREATE INDEX IF NOT EXISTS idx_raw_sources_status ON raw_sources(status)",
    "CREATE INDEX IF NOT EXISTS idx_ingredients_canonical ON ingredients(canonical_name)",
    "CREATE INDEX IF NOT EXISTS idx_recipe_tags_tag ON recipe_tags(tag_name)",
    "CREATE INDEX IF NOT EXISTS idx_grocery_deals_merchant ON grocery_deals(merchant)",
    "CREATE INDEX IF NOT EXISTS idx_grocery_deals_matched ON grocery_deals(matched_canonical_ingredient)",
    "CREATE INDEX IF NOT EXISTS idx_inventory_canonical ON inventory(canonical_name)"
  )
  for (idx in indexes) dbExecute(db, idx)

  message("Database schema initialized successfully: ", db_path)
  invisible(db_path)
}
