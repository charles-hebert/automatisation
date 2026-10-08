# Recipe Normalization & Tag Classification Pipeline Guide

This document describes the design, execution, and integration of the recipe normalization and tag classification pipeline.

---

## 🏗️ Architecture Overview

The pipeline strictly separates **deterministic calculations** (rule-based) from **probabilistic inference** (OpenRouter LLM), ensuring that the optimizer (`ompr`) receives only validated, high-confidence tags.

```
+-------------------------------------------------------------------+
|                        1. Ingestion / Parser                       |
|               (R/parse_recipes.R -> recipes, ingredients)         |
+-------------------------------------------------------------------+
                                  |
                                  v
+-------------------------------------------------------------------+
|                 2. Strict Ingredient Normalization                |
|      (R/normalize_ingredients.R -> ingredients_ref / synonyms)    |
|                  Unmatched ingredients assigned ID = 0            |
+-------------------------------------------------------------------+
                                  |
                                  v
+-------------------------------------------------------------------+
|                     3. Deterministic Rules                        |
|  (R/deterministic_tags.R -> contains_meat, contains_fish,        |
|                  vegetarian, vegan, weeknight_ok)                 |
|       *Fallback: ID = 0 forces dietary/allergen tags to 'review'  |
+-------------------------------------------------------------------+
                                  |
                                  v
+-------------------------------------------------------------------+
|                  4. OpenRouter LLM Classification                 |
|   (R/engine_llm.R -> Batched single payload per recipe + cache)   |
|   (Evaluates tag_questions.csv & classification_thresholds.csv)   |
+-------------------------------------------------------------------+
                                  |
                                  v
+-------------------------------------------------------------------+
|               5. Power BI & Optimizer Access View                 |
|               (v_recipe_tags_accepted: status == 'accepted')       |
+-------------------------------------------------------------------+
```

---

## 🚀 Execution Instructions

### 1. Environment Setup

Ensure `OPENROUTER_API_KEY` is configured in your `.Renviron` file:

```env
OPENROUTER_API_KEY=sk-or-v1-...
```

### 2. Full Synchronization Pipeline Script

Run the full pipeline using R:

```r
source("R/config.R")
source("R/recipe_db.R")
source("R/normalize_ingredients.R")
source("R/deterministic_tags.R")
source("R/engine_llm.R")
source("R/review_queue.R")

db_path <- "recipes.db"

# 1. Initialize SQLite database & seed reference dictionaries
init_recipe_db(db_path)

# 2. Run strict ingredient normalization
normalize_all_ingredients(db_path)

# 3. Export unmatched terms for dictionary enrichment
export_unmatched_ingredients(db_path, "unmatched_ingredients.csv")

# 4. Run deterministic rules & fallback status assignment
run_deterministic_rules(db_path)

# 5. Run batched LLM classification via OpenRouter
run_llm_classification_pipeline(
  db_path = db_path,
  questions_path = "inst/dictionaries/tag_questions.csv",
  thresholds_path = "inst/dictionaries/classification_thresholds.csv",
  model = "google/gemini-2.5-flash"
)

# 6. Export review queue for manual validation
export_review_queue(db_path, "review_queue.csv")
```

---

## 📊 Evaluation & Gold Set Benchmark

To evaluate the classification engine accuracy against the 5 gold standard recipes before production deployment, run:

```r
source("R/evaluate_engine.R")

eval_metrics <- evaluate_engine(
  db_path = "eval_recipes.db",
  mock_recipes_path = "tests/testthat/fixtures/mock_recipes.json",
  gold_labels_path = "tests/testthat/fixtures/gold_labels.csv",
  questions_path = "inst/dictionaries/tag_questions.csv",
  thresholds_path = "inst/dictionaries/classification_thresholds.csv",
  model = "google/gemini-2.5-flash"
)

print(sprintf("Accuracy: %.1f%%", eval_metrics$accuracy_pct))
```

---

## 🔌 Power BI & Optimizer Integration

The SQLite view `v_recipe_tags_accepted` exposes only tags with `status = 'accepted'`:

```sql
SELECT recipe_id, tag_name, tag_value, confidence, tag_source
FROM v_recipe_tags_accepted;
```

- **Power BI Query Folding:** Pure SQL `SELECT ... WHERE status = 'accepted'` allows Power BI to delegate query execution to SQLite.
- **Optimizer (`ompr`):** Any tag with `review` or `rejected` status is excluded from `v_recipe_tags_accepted`. The optimizer treats the absence of a tag row as a formal `"No"` condition.
