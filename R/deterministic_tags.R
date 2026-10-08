# Deterministic tagging rules and fallback status management.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
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

MEAT_CATEGORIES <- c("meat", "poultry", "game")
MEAT_KEYWORDS <- c("beef", "boeuf", "pork", "porc", "chicken", "poulet", "lamb", "agneau",
                   "turkey", "dinde", "veal", "veau", "bacon", "sausage", "saucisse",
                   "ham", "jambon", "canard", "duck", "prosciutto")

FISH_CATEGORIES <- c("fish", "seafood", "poisson", "crustacean")
FISH_KEYWORDS <- c("fish", "poisson", "salmon", "saumon", "tuna", "thon", "shrimp", "crevette",
                   "cod", "morue", "trout", "truite", "crab", "crabe", "lobster", "homard",
                   "mussel", "moule", "anchois", "anchovy", "halibut", "flétan")

NON_VEGAN_CATEGORIES <- c("dairy", "egg", "honey")
NON_VEGAN_KEYWORDS <- c("butter", "beurre", "cheese", "fromage", "milk", "lait", "egg", "oeuf",
                        "honey", "miel", "cream", "crème", "yogurt", "yaourt", "parmesan",
                        "ghee", "mayo", "mayonnaise")

PLANT_MILKS <- c("lait de coco", "lait d'avoine", "lait d'amande", "lait de soja",
                 "coconut milk", "oat milk", "almond milk", "soy milk")

WEEKNIGHT_EQUIPMENT <- c("mijoteuse", "slow cooker", "friteuse_a_air_chaud", "friteuse a air chaud",
                         "air fryer", "plaque", "sheet pan")

#' Run deterministic rules for all recipes or a specific recipe_id.
#'
#' @param db_path Path to SQLite database
#' @param recipe_id Optional specific recipe_id to evaluate
run_deterministic_rules <- function(db_path = "recipes.db", recipe_id = NULL) {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  recs <- if (is.null(recipe_id)) {
    dbGetQuery(db, "SELECT recipe_id, title, prep_min, cook_min FROM recipes")
  } else {
    dbGetQuery(db, "SELECT recipe_id, title, prep_min, cook_min FROM recipes WHERE recipe_id = ?", params = list(recipe_id))
  }

  if (nrow(recs) == 0) {
    log_info("No recipes found for deterministic tagging.")
    return(invisible(0))
  }

  log_info("Running deterministic rules for {nrow(recs)} recipes.")

  dbBegin(db)
  tryCatch({
    for (i in seq_len(nrow(recs))) {
      rid <- recs$recipe_id[[i]]
      prep_min <- recs$prep_min[[i]] %||% 0
      cook_min <- recs$cook_min[[i]] %||% 0

      # Fetch normalized ingredients
      ings <- dbGetQuery(db, "
        SELECT
          i.ref_ingredient_id,
          i.raw_text,
          i.canonical_name,
          r.canonical_name_fr,
          r.canonical_name_en,
          r.category
        FROM ingredients i
        LEFT JOIN ingredients_ref r ON i.ref_ingredient_id = r.ingredient_id
        WHERE i.recipe_id = ?
      ", params = list(rid))

      # Fetch equipment
      eqs <- dbGetQuery(db, "
        SELECT equipment_name FROM recipe_equipment WHERE recipe_id = ?
      ", params = list(rid))$equipment_name %||% character(0)

      # Check for unmatched ingredients (ID = 0)
      has_unmatched <- any(ings$ref_ingredient_id == 0 | is.na(ings$ref_ingredient_id))

      # Lowercase ingredient terms for matching
      ing_terms <- unique(tolower(c(
        ings$canonical_name, ings$canonical_name_fr, ings$canonical_name_en, ings$raw_text
      )))
      ing_terms <- ing_terms[!is.na(ing_terms) & nzchar(trimws(ing_terms))]
      ing_cats <- unique(tolower(ings$category[!is.na(ings$category)]))

      # Evaluate meat
      has_meat <- any(ing_cats %in% MEAT_CATEGORIES) || any(sapply(MEAT_KEYWORDS, function(k) any(grepl(paste0("\\b", k, "\\b"), ing_terms))))

      # Evaluate fish
      has_fish <- any(ing_cats %in% FISH_CATEGORIES) || any(sapply(FISH_KEYWORDS, function(k) any(grepl(paste0("\\b", k, "\\b"), ing_terms))))

      # Evaluate vegetarian & vegan
      is_vegetarian <- !has_meat && !has_fish

      # Clean terms for vegan check by ignoring plant milks
      clean_ing_terms_vegan <- sapply(ing_terms, function(term) {
        t_clean <- term
        for (pm in PLANT_MILKS) {
          t_clean <- gsub(pm, "", t_clean, fixed = TRUE)
        }
        t_clean
      })

      has_non_vegan <- any(ing_cats %in% NON_VEGAN_CATEGORIES) ||
        any(sapply(NON_VEGAN_KEYWORDS, function(k) any(grepl(paste0("\\b", k, "\\b"), clean_ing_terms_vegan))))
      is_vegan <- is_vegetarian && !has_non_vegan

      # Evaluate weeknight_ok
      # Rule: prep_time <= 30 AND (cook_time <= 45 OR equipment IN (mijoteuse, air fryer, plaque) OR total_time <= 30)
      has_weeknight_eq <- any(tolower(eqs) %in% WEEKNIGHT_EQUIPMENT)
      total_time <- prep_min + cook_min
      is_weeknight <- (prep_min <= 30) && (cook_min <= 45 || has_weeknight_eq || total_time <= 30)

      # Status fallback: If unmatched ingredient exists, dietary/allergen tags -> 'review'
      dietary_status <- if (has_unmatched) "review" else "accepted"
      dietary_conf <- if (has_unmatched) 50L else 100L

      tags_to_insert <- list(
        list(tag = "contains_meat", val = tolower(as.character(has_meat)), conf = dietary_conf, status = dietary_status),
        list(tag = "contains_fish", val = tolower(as.character(has_fish)), conf = dietary_conf, status = dietary_status),
        list(tag = "vegetarian", val = tolower(as.character(is_vegetarian)), conf = dietary_conf, status = dietary_status),
        list(tag = "vegan", val = tolower(as.character(is_vegan)), conf = dietary_conf, status = dietary_status),
        list(tag = "weeknight_ok", val = tolower(as.character(is_weeknight)), conf = 100L, status = "accepted")
      )

      for (t in tags_to_insert) {
        dbExecute(db, "
          INSERT INTO recipe_tag_classifications (recipe_id, tag_name, tag_value, confidence, status, tag_source)
          VALUES (?, ?, ?, ?, ?, 'rule')
          ON CONFLICT(recipe_id, tag_name) DO UPDATE SET
            tag_value = excluded.tag_value,
            confidence = excluded.confidence,
            status = excluded.status,
            tag_source = excluded.tag_source,
            updated_at = CURRENT_TIMESTAMP
        ", params = list(rid, t$tag, t$val, t$conf, t$status))
      }
    }
    dbCommit(db)
    log_info("Deterministic rules completed successfully for {nrow(recs)} recipes.")
  }, error = function(e) {
    dbRollback(db)
    log_error("Failed running deterministic rules: {e$message}")
    stop(e)
  })

  invisible(nrow(recs))
}
