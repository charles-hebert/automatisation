# Pipeline Recettes & Optimiseur de Plan de Repas Hebdomadaire

Ce document détaille l'architecture globale, la structure des modules, la configuration ainsi que les exemples d'utilisation du système de gestion de recettes, d'extraction d'inventaires, de rabais d'épicerie et d'optimisation de plans de repas hebdomadaires.

---

## Architecture Globale

Le système est découpé en modules indépendants et idempotents exécutables sous forme de scripts R ou orchestrés via le package `targets`.

```
                    +--------------------------------+
                    |  Sources Brutes (PDF/EPUB/Img) |
                    +---------------+----------------+
                                    |
                                    v
                       +------------------------+
                       |  R/extract_sources.R   |
                       +-----------+------------+
                                   |
                                   v
                       +------------------------+
                       |   R/parse_recipes.R    | (OpenRouter LLM)
                       +-----------+------------+
                                   |
                                   v
                       +------------------------+
                       |    R/tag_recipes.R     | (Règles + Ontologie)
                       +-----------+------------+
                                   |
                                   v
+------------------------+  +---------------+  +--------------------------+
|  R/extract_inventory.R |  |   SQLite DB   |  |   R/grocery_deals.R      |
| (Excel/GSheets/Txt)    |->|  (recipes.db) |<-| (Rabais Metro/Loblaws/..) |
+------------------------+  +-------+-------+  +--------------------------+
                                    |
                                    v
                     +----------------------------+
                     |  R/mealplan_data_model.R   |
                     +--------------+-------------+
                                    |
                                    v
                     +----------------------------+
                     | R/mealplan_constraints.R   | (ompr / MILP)
                     | R/mealplan_objectives.R    |
                     +--------------+-------------+
                                    |
                                    v
                     +----------------------------+
                     | R/mealplan_alternatives.R  |
                     | R/mealprep_weekend.R       |
                     | R/mealplan_rationale.R     | (Rapport Chef LLM)
                     +----------------------------+
```

---

## Description des Modules

### 1. Base de données & Schéma (`R/recipe_db.R`)
Initialise la base SQLite et définit le schéma relationnel normalisé :
- `books` : Livres ou collections de recettes.
- `raw_sources` : Fichiers sources importés (EPUB, PDF, images, URL).
- `recipes` : Métadonnées des recettes (titre, temps de préparation/cuisson, portions).
- `recipe_steps` : Étapes de préparation ordonnées.
- `ingredients` : Ingrédients extraits (texte brut, nom canonique, quantité, unité).
- `recipe_equipment` : Équipements requis (actifry, mijoteuse, plaque, etc.).
- `ontology_nodes` & `ontology_edges` : Graphe d'ontologie culinaire (catégories, régimes, allergènes).
- `recipe_tags` : Étiquettes associées (par règles, ontologie ou LLM).
- `grocery_deals` : Spéciaux d'épicerie extraits et associés aux ingrédients canoniques.
- `inventory` : État des stocks (frigo, congélateur, garde-manger, ingrédients de saison, produits du jardin).
- `mealplan_reports` : Historique des rapports d'optimisation et des conseils culinaires du chef.

### 2. Extraction des sources brutes (`R/extract_sources.R`)
Transforme les documents non structurés (PDF, EPUB, images, URL) en enregistrements bruts dans la table `raw_sources` avec hachage SHA-256 pour éviter le retraitement inutile.

### 3. Parsing de recettes par LLM (`R/parse_recipes.R`)
Envoie le contenu brut à un modèle de langage (OpenRouter) avec un prompt JSON strict. Extrait les recettes, étapes, équipements, ingrédients canoniques et valeurs nutritionnelles (`fiber_g`, `protein_g`, `magnesium_mg`, `gut_health_score`).

### 4. Taggage et classification (`R/tag_recipes.R`)
Enrichit automatiquement les recettes avec des étiquettes à partir :
- De règles déterministes (mots-clés, ingrédients, temps de préparation).
- De l'ontologie culinaire dans SQLite.
- Optionnellement d'un appel LLM filtré par un dictionnaire d'étiquettes autorisé (`inst/dictionaries/recipe_tags.csv`).

### 5. Extraction des rabais d'épicerie (`R/grocery_deals.R`)
Extrait les aubaines hebdomadaires d'épicerie pour une zone ciblée (ex. code postal `K2C 1K1` à Ottawa pour les bannières Metro, Farm Boy et Loblaws). Associe automatiquement les spéciaux aux noms d'ingrédients canoniques et les sauvegarde dans la table `grocery_deals`.

### 6. Gestion et normalisation de l'inventaire (`R/extract_inventory.R`)
Permet l'ingestion multi-source d'inventaires à partir de fichiers Excel (`.xlsx`), Google Sheets ou fichiers texte brut/Markdown (`.txt`, `.md`). Normalise les termes, effectue la correspondance bilingue avec le dictionnaire `inst/dictionaries/bilingual_ingredients.csv` et la base SQLite, identifie les agrafes de garde-manger (`is_pantry_staple`) et enregistre les données dans la table `inventory`.

### 7. Chargement du modèle de données de planification (`R/mealplan_data_model.R`)
Charge les recettes candidates depuis la base SQLite et applique les filtres configurables :
- Chargement des règles strictes depuis `config/forbidden_rules.yml` (`forbidden_always`, `forbidden_with_f`).
- Chargement de la liste d'exclusion douce depuis `config/soft_blacklist.yml`.
- Correspondance dynamique des ingrédients de saison selon le mois ciblé à partir de `inst/dictionaries/seasonal_ingredients.csv`.
- Définition de la structure des 7 soirs (`default_slots`) avec gestion des contraintes semaine/week-end, présence de `f` (`f_present`) et conditions météo (`weather_hot_sunny`).

### 8. Moteur d'optimisation MILP (`R/mealplan_constraints.R` & `R/mealplan_objectives.R`)
Utilise la programmation linéaire en nombres entiers (MILP) avec `ompr` et le résolveur GLPK (`ROI.plugin.glpk`) pour générer le plan de repas idéal sur 7 jours :
- **Contraintes d'incompatibilité** : Exactement 1 recette par soir, pas de répétition de recette dans la semaine, respect des règles d'exclusion (`forbidden_always`, `forbidden_with_f` lors des soirs où `f` est présent).
- **Contraintes de semaine** : Temps de préparation actif limité (ex. <= 30 min les soirs de semaine).
- **Diversité des méthodes de cuisson** : Maximum 1 recette de type `actifry`, 1 de type `mijoteuse` et 1 de type `plaque` par semaine.
- **Optimisation lexicographique par paliers (Tiers)** :
  1. *Palier 1* : Minimisation des pénalités d'écart nutritionnel (fibres, protéines, magnésium, santé intestinale).
  2. *Palier 2* : Maximisation de l'utilisation des ingrédients de l'inventaire (frigo/jardin).
  3. *Palier 3* : Maximisation de l'utilisation des spéciaux d'épicerie.
  4. *Palier 4* : Maximisation des ingrédients de saison.

### 9. Alternatives & Préparation du week-end (`R/mealplan_alternatives.R` & `R/mealprep_weekend.R`)
- Génère les meilleures alternatives de plans de repas hebdomadaires.
- Sélectionne automatiquement les recettes nécessitant une préparation préalable durant la fin de semaine (ex. marinades, mijotés, découpes).

### 10. Génération de rapports et conseils culinaires LLM (`R/mealplan_rationale.R`)
Analyse le plan de repas hebdomadaire optimisé et génère un rapport complet en Markdown :
- Justification de la planification (adaptation selon le temps disponible, la météo, la présence de `f`).
- Recommandations culinaires et de science alimentaire par un chef cuisinier (réaction de Maillard, équilibre gras/acide, fermentations, astuces de préparation).
- Sauvegarde dans le fichier `output/weekly_mealplan_report.md` et dans la table SQLite `mealplan_reports`.

---

## Dictionnaires et Configurations

- `config/forbidden_rules.yml` : Configuration des règles d'ingrédients/tags strictement interdits (toujours ou selon la présence de `f`).
- `config/soft_blacklist.yml` : Liste de restriction douce pour limiter la fréquence de certaines recettes.
- `inst/dictionaries/bilingual_ingredients.csv` : Mapping canonique bilingue français/anglais des ingrédients.
- `inst/dictionaries/seasonal_ingredients.csv` : Ingrédients de saison classés par mois.
- `inst/dictionaries/recipe_tags.csv` : Liste des tags autorisés pour le filtrage du taggage LLM.

---

## Améliorations Réalisées & Perspectives

### Réalisations récentes :
- [x] Implémentation du moteur d'optimisation MILP sous `ompr` avec solveur GLPK.
- [x] Intégration de l'ingestion multi-source d'inventaire (Excel, Google Sheets, texte/Markdown).
- [x] Scraping et intégration des rabais d'épicerie pour la région d'Ottawa (Metro, Farm Boy, Loblaws).
- [x] Normalisation bilingue (français/anglais) des ingrédients.
- [x] Modélisation des règles de sécurité alimentaire et restrictions personnalisées via YAML (`config/forbidden_rules.yml`).
- [x] Générateur de rapports culinaires enrichis par LLM avec archivage dans SQLite (`mealplan_reports`).
- [x] Pipeline d'orchestration R complet avec `targets` (`_targets.R`).

### Améliorations futures suggérées :
- Ajouter une étape d'OCR local (Tesseract) pour les PDF scannés ou images d'anciens livres de recettes.
- Ajouter un historique d'utilisation des recettes pour appliquer la règle de non-répétition sur 3 semaines calendaires.
- Interface utilisateur web légère (Shiny / R) pour visualiser et ajuster interactivement le plan de repas.

---

## Exemples d'utilisation

### Mode 1 : Exécution modulaire directe par script R

```r
# Load code modules
source("R/recipe_db.R")
source("R/extract_sources.R")
source("R/parse_recipes.R")
source("R/tag_recipes.R")
source("R/grocery_deals.R")
source("R/extract_inventory.R")
source("R/mealplan_data_model.R")
source("R/mealplan_constraints.R")
source("R/mealplan_objectives.R")
source("R/mealplan_alternatives.R")
source("R/mealprep_weekend.R")
source("R/mealplan_rationale.R")

# 1. Initialiser la base de données SQLite
db_path <- "recipes.db"
init_recipe_db(db_path)

# 2. Extraire et parser les recettes depuis un dossier de sources
run_extraction_pipeline(
  input_dir = "to_be_treated",
  archive_dir = "treated",
  db_path = db_path
)

api_key <- Sys.getenv("OPENROUTER_API_KEY")
run_parsing_pipeline(api_key = api_key, db_path = db_path)
run_tagging_pipeline(api_key = api_key, db_path = db_path)

# 3. Extraire les rabais d'épicerie hebdomadaires (Ottawa - K2C 1K1)
run_grocery_deals_pipeline(
  db_path = db_path,
  postal_code = "K2C 1K1",
  target_stores = c("Metro", "Farm Boy", "Loblaws")
)

# 4. Ingestion et normalisation de l'inventaire (ex. fichier texte ou Excel)
run_inventory_pipeline(
  source_path_or_url = "inventory.txt",
  source_type = "auto",
  db_path = db_path,
  overwrite = TRUE
)

# 5. Charger le jeu de données pour le planificateur de repas
target_month <- "September"
recipe_dataset <- load_recipe_dataset(
  db_path = db_path,
  forbidden_config = "config/forbidden_rules.yml",
  soft_blacklist_config = "config/soft_blacklist.yml",
  seasonal_dict_path = "inst/dictionaries/seasonal_ingredients.csv",
  target_month = target_month
)

# Définir la structure de la semaine (7 soirs)
# Presence de 'f' le vendredi, samedi et dimanche
slots_dataset <- default_slots(
  f_present_days = c("vendredi", "samedi", "dimanche"),
  hot_sunny_days = c("samedi")
)

# 6. Construire et résoudre le modèle d'optimisation MILP
base_model <- build_base_mealplan_model(recipe_dataset, slots_dataset)
weekly_plan <- solve_mealplan_lexicographic(base_model)

# Afficher l'horaire optimisé
print(weekly_plan$schedule)

# 7. Obtenir les meilleures alternatives
alternatives <- get_all_top_alternatives(weekly_plan, top_n = 3)

# 8. Sélectionner les préparations requises le week-end
weekend_prep <- select_weekend_mealprep(db_path = db_path)

# 9. Générer le rapport de justification et conseils du chef
report_md <- generate_mealplan_rationale(
  weekly_plan_result = weekly_plan,
  api_key = api_key,
  model = "google/gemini-2.5-flash",
  db_path = db_path,
  output_md_path = "output/weekly_mealplan_report.md",
  week_label = paste("Semaine du", Sys.Date())
)
```

### Mode 2 : Orchestration automatisée via `targets`

Le projet intègre une définition de pipeline `targets` dans `_targets.R`. Pour exécuter l'ensemble du flux d'optimisation de façon reproductible et automatique :

```r
library(targets)

# Vérifier l'état du pipeline
tar_visnetwork()

# Exécuter l'ensemble du pipeline
tar_make()

# Consulter les résultats générés
weekly_schedule <- tar_read(weekly_plan)
print(weekly_schedule$schedule)

weekend_prep <- tar_read(weekend_prep)
print(weekend_prep)

report <- tar_read(weekly_rationale_report)
cat(report)
```
