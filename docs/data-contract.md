# Data Contract: Recipe Normalization & Tag Classification Pipeline

This document defines the data contract, table mappings, and schema specifications for the ingredient normalization and recipe tag classification pipeline.

---

## 1. Source Data (Ingestion & Parser Outputs)

The pipeline consumes data from the existing SQLite tables populated by recipe parsing (`R/parse_recipes.R`):

| Source Table | Source Field | Type | Description / Constraints |
|---|---|---|---|
| `recipes` | `recipe_id` | `INTEGER` | Primary key. |
| `recipes` | `title` | `TEXT` | Recipe title. |
| `recipes` | `prep_min` | `INTEGER` | Active preparation time in minutes (must be separate from cooking time). |
| `recipes` | `cook_min` | `INTEGER` | Passive cooking time in minutes. |
| `recipes` | `servings` | `REAL` | Number of servings. |
| `ingredients` | `ingredient_id_pk` | `INTEGER` | Primary key of ingredient row (`ingredient_id` in `ingredients` table). |
| `ingredients` | `recipe_id` | `INTEGER` | Foreign key referencing `recipes(recipe_id)`. |
| `ingredients` | `raw_text` | `TEXT` | Raw text of ingredient line. |
| `ingredients` | `canonical_name` | `TEXT` | Parsed canonical ingredient name (raw string before normalization). |
| `ingredients` | `quantity_num` | `REAL` | Quantity numeric value. |
| `ingredients` | `unit_standard` | `TEXT` | Unit of measurement. |
| `recipe_equipment` | `equipment_name` | `TEXT` | Equipment tags (e.g. `mijoteuse`, `friteuse_a_air_chaud`, `plaque`). |

---

## 2. Ingredient Normalization Data Model

Strict normalization without fuzzy matching.

### `ingredients_ref`
Reference dictionary of canonical ingredients.

| Field Name | Type | Description / Constraints |
|---|---|---|
| `ingredient_id` | `INTEGER PRIMARY KEY` | Primary key. ID = 0 is reserved for `"Unmatched / Inconnu"`. |
| `canonical_name_fr` | `TEXT UNIQUE NOT NULL` | Standardized French name (e.g. `"poulet"`). |
| `canonical_name_en` | `TEXT` | Standardized English name (e.g. `"chicken"`). |
| `category` | `TEXT` | Category (`meat`, `fish`, `vegetable`, `dairy`, `grain`, `spice`, `pantry`, etc.). |

### `ingredient_synonyms`
Mapping from synonyms/variations to reference `ingredient_id`.

| Field Name | Type | Description / Constraints |
|---|---|---|
| `synonym_id` | `INTEGER PRIMARY KEY` | Primary key. |
| `synonym_text` | `TEXT UNIQUE NOT NULL` | Normalized lower-case synonym string. |
| `ingredient_id` | `INTEGER NOT NULL` | Foreign key to `ingredients_ref(ingredient_id)`. |

### Normalization Mapping in `ingredients` Table
Each row in `ingredients` is mapped as follows:

| Field Name | Type | Mapping / Logic |
|---|---|---|
| `ref_ingredient_id` | `INTEGER` | Foreign key referencing `ingredients_ref(ingredient_id)`. Default: `0` (Unmatched). |
| `match_method` | `TEXT` | `'exact'`, `'synonym'`, or `'unmatched'`. |

---

## 3. Deterministic Tagging Rules

Deterministic tags are computed strictly from normalized ingredients, preparation/cook times, and equipment.

| Tag Name | Derivation Logic | Default Status if Unmatched Ingredient Present |
|---|---|---|
| `contains_meat` | `TRUE` if any normalized ingredient has category `'meat'` or matches meat terms. | `review` |
| `contains_fish` | `TRUE` if any normalized ingredient has category `'fish'` or matches fish/seafood terms. | `review` |
| `vegetarian` | `TRUE` if `contains_meat == FALSE` AND `contains_fish == FALSE`. | `review` |
| `vegan` | `TRUE` if `vegetarian == TRUE` and no dairy/egg/honey ingredients. | `review` |
| `weeknight_ok` | `TRUE` if (`prep_min <= 30` AND (`cook_min <= 45` OR equipment IN (`'mijoteuse'`, `'slow cooker'`, `'friteuse_a_air_chaud'`, `'air fryer'`, `'plaque'`) OR `prep_min + cook_min <= 30`)). | `accepted` (not impacted by unmatched ingredient) |

---

## 4. Probabilistic LLM Engine & Cache Data Model

LLM classification calls OpenRouter (`https://openrouter.ai/api/v1/chat/completions`) using a single batched payload per recipe.

### Cache Table (`llm_cache`)
| Field Name | Type | Description |
|---|---|---|
| `cache_key` | `TEXT PRIMARY KEY` | `MD5(recipe_title + sorted(normalized_ingredient_ids) + model_name)` |
| `model` | `TEXT NOT NULL` | OpenRouter model ID used. |
| `prompt_json` | `TEXT` | JSON payload sent to OpenRouter. |
| `response_json` | `TEXT` | Raw JSON response received. |
| `created_at` | `DATETIME` | Timestamp of cache creation. |

---

## 5. Tag Classification Statuses & Thresholds

All recipe tags (deterministic and LLM) are stored in `recipe_tag_classifications`.

### `recipe_tag_classifications`
| Field Name | Type | Constraints / Description |
|---|---|---|
| `classification_id` | `INTEGER PRIMARY KEY` | Primary key. |
| `recipe_id` | `INTEGER NOT NULL` | Foreign key referencing `recipes(recipe_id)`. |
| `tag_name` | `TEXT NOT NULL` | Name of tag (e.g. `mediterranean`, `family_friendly`, `vegetarian`). |
| `tag_value` | `TEXT NOT NULL` | Value (`"true"`, `"false"`, numeric string, or label). |
| `confidence` | `INTEGER NOT NULL` | Confidence score between 1 and 100. (Deterministic rules set confidence = 100). |
| `status` | `TEXT NOT NULL` | Status: `'accepted'`, `'rejected'`, or `'review'`. |
| `tag_source` | `TEXT NOT NULL` | Source: `'rule'`, `'llm'`, or `'manual'`. |
| `updated_at` | `DATETIME` | Last update timestamp. |

### Classification Threshold Rules
Configured in `inst/dictionaries/classification_thresholds.csv`:
- If `confidence >= accept_threshold` $\rightarrow$ `status = 'accepted'`
- If `confidence <= reject_threshold` $\rightarrow$ `status = 'rejected'`
- Otherwise $\rightarrow$ `status = 'review'`
- Any LLM API network error/timeout $\rightarrow$ all tags for the recipe assigned `status = 'review'`.
- Any recipe with `ref_ingredient_id = 0` $\rightarrow$ dietary/allergen tags assigned `status = 'review'`.

---

## 6. Optimizer & Power BI Interface View

### View: `v_recipe_tags_accepted`
Pure SQL view designed for SQLite Query Folding in Power BI and `ompr` optimizer query filtering:

```sql
CREATE VIEW IF NOT EXISTS v_recipe_tags_accepted AS
SELECT
    recipe_id,
    tag_name,
    tag_value,
    confidence,
    tag_source
FROM recipe_tag_classifications
WHERE status = 'accepted';
```

---

## 7. Operational Exports

- `export_unmatched_ingredients()` $\rightarrow$ exports `unmatched_ingredients.csv` sorted by frequency (recipes count) to the root directory.
- `export_review_queue()` $\rightarrow$ exports `review_queue.csv` containing all tag classifications with `status = 'review'` to the root directory for human validation.
