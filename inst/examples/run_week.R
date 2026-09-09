#!/usr/bin/env Rscript

# Executable example script for weekly meal-plan optimizer using synthetic data

source("R/mealplan_data_model.R")
source("R/mealplan_constraints.R")
source("R/mealplan_objectives.R")
source("R/mealplan_alternatives.R")
source("R/mealprep_weekend.R")

cat("=======================================================\n")
cat("      WEEKLY MEAL PLAN OPTIMIZER - EXAMPLE RUN\n")
cat("=======================================================\n\n")

# 1. Create synthetic candidate recipes dataset
synthetic_recipes <- tibble::tibble(
  recipe_id = paste0("rec_", sprintf("%02d", 1:14)),
  title = c(
    "Bowl Méditerranéen & Féta",
    "Filet de Saumon & Grains Entiers",
    "Sauté Tofu & Riz Jasmin",
    "Pâtes Végétaliennes aux Légumes",
    "Chili de Légumineuses & Bœuf",
    "Poulet Grillé au Citron",
    "Dindon Rôti aux Herbes",
    "Ragoût de Porc Braisé",
    "Canard Confit aux Figues",
    "Côtelettes d'Agneau Grillées",
    "Granola aux Flocons d'Avoine",
    "Muffin Salé aux Épinards",
    "Pain de Campagne au Levain",
    "Soba Sautées aux Graines de Sésame" # forbidden_always (soba)
  ),
  prep_min = c(15, 20, 25, 20, 30, 25, 25, 45, 50, 20, 15, 25, 30, 15),
  tags = list(
    c("mediterraneen", "vegetarien"),
    c("poisson", "grains_entiers"),
    c("vegetarien", "riz"),
    c("vegetarien", "pates"),
    c("legumineuses", "boeuf"),
    c("poulet"),
    c("dindon"),
    c("porc"),
    c("canard"),
    c("agneau"),
    c("collation", "avoine", "snack"),
    c("baking_sale", "sale"),
    c("levain", "sourdough"),
    c("soba", "asiatique")
  ),
  fiber_g = c(8, 7, 6, 6, 5, 4, 5, 4, 3, 4, 6, 3, 2, 3),
  protein_g = c(25, 32, 22, 20, 30, 35, 30, 26, 24, 25, 10, 8, 7, 10),
  magnesium_mg = c(320, 410, 360, 310, 420, 300, 300, 250, 250, 250, 160, 110, 90, 120),
  gut_health_score = c(8.5, 7.5, 6.5, 6.0, 5.5, 5.0, 5.0, 4.0, 4.0, 4.0, 7.0, 6.0, 8.0, 5.0),
  method = c("actifry", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  is_favorite = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  from_selected_book = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
  source = c("Livre 1", "Livre 1", "Livre 2", "Livre 2", "Livre 3", "Livre 3", "Livre 4", "Livre 4", "Livre 5", "Livre 5", "Snacks", "Bakery", "Breads", "Asian")
)

cat("[1/4] Loading & validating recipe dataset...\n")
ds <- load_recipe_dataset(db_path = NULL, recipes_df = synthetic_recipes)
slots <- default_slots(f_present_days = c("vendredi", "samedi", "dimanche"), hot_sunny_days = c("samedi"))

cat("[2/4] Building MILP base model with hard constraints...\n")
bm <- build_base_mealplan_model(ds, slots)

cat("[3/4] Solving 5-tier lexicographic cascade optimizer...\n")
plan <- solve_mealplan_lexicographic(bm)

cat("\n=======================================================\n")
cat("                OPTIMAL 7-NIGHT MEAL PLAN\n")
cat("=======================================================\n")
print(plan$schedule |> dplyr::select(slot_index, day, recipe_name, prep_time_min, source, fiber_g, protein_g))

cat("\nOptimization Tier Objective Scores:\n")
print(plan$tier_scores)

cat("\n[4/4] Generating top 3 alternative recipes per night...\n")
alts <- get_all_top_alternatives(plan, top_n = 3)
cat("First 6 alternatives preview:\n")
print(head(alts, 6))

cat("\n=======================================================\n")
cat("               WEEKEND MEAL-PREP SELECTION\n")
cat("=======================================================\n")
mp <- select_weekend_mealprep(recipes_df = synthetic_recipes)
print(mp)

cat("\nExecution completed successfully!\n")
