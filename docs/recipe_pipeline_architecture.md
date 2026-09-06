# Pipeline recettes — extraction, parsing, taggage, rabais d'épicerie et gestion des inventaires

## Structure proposée

Le code est séparé en étapes idempotentes :

1. `R/recipe_db.R` initialise SQLite et les tables normalisées (incluant `grocery_deals` et `inventory`).
2. `R/extract_sources.R` transforme PDF, EPUB et images en contenu brut dans `raw_sources`.
3. `R/parse_recipes.R` demande au modèle de produire un JSON strict puis insère recettes, étapes, ingrédients et équipement dans des tables dédiées.
4. `R/tag_recipes.R` ajoute les tags par règles déterministes, ontologie et, optionnellement, LLM.
5. `R/grocery_deals.R` extrait les rabais d'épicerie (ex. Ottawa, code postal `K2C 1K1`) pour les bannières cibles (Metro, Farm Boy, Loblaws), effectue un chargement défensif des packages R, fait la correspondance avec les ingrédients canoniques de SQLite et persiste les résultats dans `grocery_deals`.
6. `R/extract_inventory.R` ingère les inventaires d'ingrédients restants depuis plusieurs sources (Excel, Google Sheets, fichiers texte/Markdown), nettoie et normalise les noms d'ingrédients contre les ingrédients canoniques de SQLite, identifie les aliments de base du garde-manger (`is_pantry_staple`), et persiste les données dans la table `inventory`.

## Simplifications appliquées

- Les extracteurs spécialisés sont isolés derrière `extract_source_content()`.
- Les doublons ne sont plus supprimés par défaut : `delete_duplicates = FALSE` évite de perdre un fichier original.
- Les erreurs de parsing et d'extraction sont conservées dans `raw_sources.error_message`.
- Les tags LLM sont filtrés avec un dictionnaire versionné dans `inst/dictionaries/recipe_tags.csv`.
- Les inserts de parsing sont transactionnels par source.

## Améliorations structurelles recommandées

- Ajouter une table `parse_runs` pour historiser modèle, prompt, date, coût estimé et réponse brute.
- Ajouter une table `recipe_tag_rules` pour rendre les seuils et mots-clés modifiables sans changer le code R.
- Stocker les images optimisées dans un dossier d'artefacts et garder un chemin plutôt que de grosses chaînes base64 en SQLite si le volume augmente.
- Ajouter une étape OCR locale pour les PDF scannés avant l'appel LLM.
- Introduire des tests unitaires avec une petite base SQLite temporaire et des fixtures PDF/EPUB/images minimales.
- Normaliser davantage les ingrédients avec un dictionnaire bilingue français/anglais pour les protéines, allergènes, saisons et unités.

## Exemple d'utilisation

```r
source("R/recipe_db.R")
source("R/extract_sources.R")
source("R/parse_recipes.R")
source("R/tag_recipes.R")
source("R/grocery_deals.R")
source("R/extract_inventory.R")

init_recipe_db("recipes.db")
run_extraction_pipeline("to_be_treated", "treated", "recipes.db")
run_parsing_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")
run_tagging_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")

# Extraction des rabais d'épicerie Ottawa et association avec la table des ingrédients
run_grocery_deals_pipeline(
  db_path = "recipes.db",
  postal_code = "K2C 1K1",
  target_stores = c("Metro", "Farm Boy", "Loblaws")
)

# Ingestion et normalisation de l'inventaire de restes (ex: fichier texte, Excel ou Google Sheet)
run_inventory_pipeline(
  source_path_or_url = "path/to/inventory.txt",
  source_type = "auto",
  db_path = "recipes.db",
  overwrite = TRUE
)
```
