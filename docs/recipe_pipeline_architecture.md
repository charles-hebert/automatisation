# Pipeline recettes — extraction, parsing, taggage, optimisation et génération de rapport

## Structure du projet

Le code est séparé en modules idempotents et modulaires :

1. `R/recipe_db.R` initialise la base de données SQLite et les tables normalisées (incluant `grocery_deals`, `inventory`, et `mealplan_reports`).
2. `R/extract_sources.R` transforme PDF, EPUB et images en contenu brut dans `raw_sources`.
3. `R/parse_recipes.R` demande au modèle LLM de produire un JSON strict puis insère recettes, étapes, ingrédients et équipement dans des tables dédiées.
4. `R/tag_recipes.R` ajoute les tags par règles déterministes, ontologie et dictionnaire versionné dans `inst/dictionaries/recipe_tags.csv`.
5. `R/extract_inventory.R` extrait et normalise l'inventaire frigo/garde-manger à partir de fichiers Excel, Google Sheets ou texte, en tirant parti du dictionnaire bilingue.
6. `R/grocery_deals.R` extrait les rabais d'épicerie (ex. Ottawa, code postal `K2C 1K1`) pour les bannières cibles (Metro, Farm Boy, Loblaws), fait la correspondance avec le dictionnaire bilingue d'ingrédients et persiste les résultats dans `grocery_deals`.
7. `R/mealplan_data_model.R`, `R/mealplan_constraints.R`, `R/mealplan_objectives.R` charge le jeu de données des recettes avec filtre de saisonnalité configurable (`inst/dictionaries/seasonal_ingredients.csv`), applique les contraintes strictes (`config/forbidden_rules.yml`, `config/soft_blacklist.yml`) et résout le modèle MILP (`ompr` / `glpk`).
8. `R/mealplan_rationale.R` génère un rapport de justification et d'améliorations culinaires basées sur la science alimentaire via un LLM, puis sauvegarde le rapport en fichier Markdown (`output/weekly_mealplan_report.md`) et dans la table SQLite `mealplan_reports`.

## Normalisation bilingue et Saisonnalité

- **Dictionnaire bilingue français/anglais** : Situé dans `inst/dictionaries/bilingual_ingredients.csv`, il permet une correspondance uniforme des ingrédients canoniques (ex. "pomme" / "apple" -> `apple`, "poulet" / "chicken" -> `chicken`) dans l'inventaire, les rabais d'épicerie et le modèle de recettes.
- **Saisonnalité configurable** : Le fichier `inst/dictionaries/seasonal_ingredients.csv` recense les ingrédients de saison par mois. La fonction `load_recipe_dataset()` accepte un paramètre `target_month` (ex. `"September"`) pour ajuster dynamiquement les scores de saisonnalité des recettes.

## Exemple d'utilisation complet

```r
# 1. Initialisation de la base de données
source("R/recipe_db.R")
init_recipe_db("recipes.db")

# 2. Ingestion des sources et des recettes
source("R/extract_sources.R")
source("R/parse_recipes.R")
source("R/tag_recipes.R")

run_extraction_pipeline("to_be_treated", "treated", "recipes.db")
run_parsing_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")
run_tagging_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")

# 3. Extraction de l'inventaire et des rabais d'épicerie avec dictionnaire bilingue
source("R/extract_inventory.R")
source("R/grocery_deals.R")

run_inventory_pipeline("data/inventory.xlsx", db_path = "recipes.db")
run_grocery_deals_pipeline(
  db_path = "recipes.db",
  postal_code = "K2C 1K1",
  target_stores = c("Metro", "Farm Boy", "Loblaws")
)

# 4. Optimisation du plan de repas hebdomadaire (ex. pour Septembre)
source("R/mealplan_data_model.R")
source("R/mealplan_constraints.R")
source("R/mealplan_objectives.R")

recipe_ds <- load_recipe_dataset(
  db_path = "recipes.db",
  target_month = "September"
)
slots_ds <- default_slots()
base_model <- build_base_mealplan_model(recipe_ds, slots_ds)
weekly_plan <- solve_mealplan_lexicographic(base_model)

# 5. Génération du rapport de justification et conseils culinaires / science alimentaire
source("R/mealplan_rationale.R")

report_md <- generate_mealplan_rationale(
  weekly_plan_result = weekly_plan,
  api_key = Sys.getenv("OPENROUTER_API_KEY"),
  db_path = "recipes.db",
  output_md_path = "output/weekly_mealplan_report.md",
  week_label = "Semaine du 15 Septembre"
)

cat("Rapport généré avec succès dans output/weekly_mealplan_report.md\n")
```

## Execution via targets

La planification complète est également orchestrée via `targets` (`_targets.R`) :

```r
library(targets)
tar_make()
```
