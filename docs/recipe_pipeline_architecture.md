# Pipeline recettes — extraction, parsing et taggage

## Structure proposée

Le code est séparé en trois étapes idempotentes :

1. `R/recipe_db.R` initialise SQLite et les tables normalisées.
2. `R/extract_sources.R` transforme PDF, EPUB et images en contenu brut dans `raw_sources`.
3. `R/parse_recipes.R` demande au modèle de produire un JSON strict puis insère recettes, étapes, ingrédients et équipement dans des tables dédiées.
4. `R/tag_recipes.R` ajoute les tags par règles déterministes, ontologie et, optionnellement, LLM.

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

init_recipe_db("recipes.db")
run_extraction_pipeline("to_be_treated", "treated", "recipes.db")
run_parsing_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")
run_tagging_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")
```

## Diagnostic : aucune source en attente de parsing

`run_parsing_pipeline()` ne lit pas les fichiers dans `to_be_treated` ou `treated` : il lit uniquement les lignes de `raw_sources` dont `status = 'pending'` dans la base indiquée par `db_path`.

Si le message indique qu'il n'y a aucune source en attente alors qu'un fichier est encore présent dans le répertoire d'entrée, relancez l'extraction avec le **même** chemin de base, puis relancez le parsing :

```r
run_extraction_pipeline("to_be_treated", "treated", "recipes.db")
run_parsing_pipeline(Sys.getenv("OPENROUTER_API_KEY"), "recipes.db")
```

La première extraction exécutée avec une version antérieure qui passait `default_book_id = NULL` pouvait échouer avant d'insérer la ligne `raw_sources`; le fichier restait alors dans l'entrée et il n'y avait donc rien à parser. Le message de parsing affiche désormais le chemin absolu de la base et le nombre de sources par statut pour faciliter ce diagnostic.
