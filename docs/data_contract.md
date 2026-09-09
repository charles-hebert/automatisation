# Data Contract for Weekly Meal-Plan Optimizer

This document establishes the data mapping between the existing SQLite database / parser outputs and the data structure expected by the weekly meal-plan optimizer (`ompr` MILP model).

## Candidate Recipe Table

| Optimizer Field | Required Type | Source in Existing DB / Config | Mapping / Derivation Logic |
|---|---|---|---|
| `recipe_id` | `chr` | `recipes.recipe_id` | Converted to character string ID. |
| `name` | `chr` | `recipes.title` | Direct mapping from `title`. |
| `tags` | `list-col of chr` | `recipe_tags.tag_name` | Grouped list of tags per recipe from `recipe_tags` table (e.g., `c("vegetarien", "riz", "mediterraneen")`). |
| `prep_time_min` | `int` | `recipes.prep_min` (+ `recipes.cook_min`) | `recipes.prep_min` (total active prep time in minutes). |
| `method` | `chr` / `NA` | `recipe_equipment.equipment_name` & `recipe_tags.tag_name` | `"actifry"`, `"mijoteuse"`, `"plaque"`, or `NA`. Derived from equipment or technique tags. |
| `good_weather` | `lgl` | `recipe_tags.tag_name` | `TRUE` if tagged with `"ete"`, `"bbq"`, `"mauvaise_meteo"` (inverse), or explicit weather tag. |
| `fiber_high` | `lgl` | `recipe_tags.tag_name` / `fiber_g` | `TRUE` if tagged `"riche_en_fibres"` or `fiber_g >= 6.0`. |
| `source` | `chr` | `books.title` / `raw_sources.file_name` | Title of source book or source filename. Defaults to `"unknown_source"`. |
| `from_selected_book` | `lgl` | `recipes.book_id` | `TRUE` if `book_id` is present or matches selected book filter. |
| `is_favorite` | `lgl` | `recipe_tags.tag_name` | `TRUE` if recipe has tag `"favori"`. |
| `forbidden_always` | `lgl` | `ingredients` / `config/forbidden_rules.yml` | `TRUE` if recipe contains forbidden items (e.g. orzo, spaghettini, cheveux d'ange, soba). |
| `forbidden_with_f` | `lgl` | `ingredients` / `config/forbidden_rules.yml` | `TRUE` if recipe contains items forbidden when `f_present == TRUE`. |
| `fridge_garden_matches` | `int` | `ingredients` x `inventory` | Count of canonical ingredients matching available items in `inventory` table. |
| `grocery_special_matches` | `int` | `ingredients` x `grocery_deals` | Count of canonical ingredients matching current deals in `grocery_deals` table. |
| `seasonal_matches` | `int` | `ingredients` / `recipe_tags` | Count of ingredients matching seasonal ingredients dictionary / tags. |
| `last_used_date` | `Date` / `NA` | Meal history log / input | Date when recipe was last used in meal history (for 3-week no-repeat rule). |
| `fiber_g` | `dbl` | Inferred / database | Fiber content in grams per serving. |
| `protein_g` | `dbl` | Inferred / database | Protein content in grams per serving. |
| `magnesium_mg` | `dbl` | Inferred / database | Magnesium content in mg per serving. |
| `gut_health_score` | `dbl` | Inferred / database | Composite gut health rating (0.0 to 10.0 scale). |

## Daily Slot Structure (7 Nights)

| Slot Field | Required Type | Source / Derivation |
|---|---|---|
| `day` | `chr` | Day identifier (`"lundi"`, `"mardi"`, `"mercredi"`, `"jeudi"`, `"vendredi"`, `"samedi"`, `"dimanche"`). |
| `is_weekday` | `lgl` | `TRUE` for Monday through Friday. |
| `f_present` | `lgl` | Slot flag indicating if `f` is attending dinner. |
| `weather_hot_sunny` | `lgl` | Scraped or forecasted météo flag for hot/sunny weather. |

## Open Questions & Config Placeholders

- **Soft Blacklist**: Configured in `config/soft_blacklist.yml` with `TODO` placeholder for recipes/tags restricted to `< 1 use/week`.
- **Forbidden Rules**: Configured in `config/forbidden_rules.yml` for `forbidden_always` and `forbidden_with_f`.
