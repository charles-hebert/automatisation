# targets pipeline definition for weekly meal-plan optimizer

suppressPackageStartupMessages(library(targets))

tar_option_set(
  packages = c("ompr", "ompr.roi", "ROI", "ROI.plugin.glpk", "dplyr", "purrr", "tibble", "yaml", "DBI", "RSQLite")
)

# Source all R pipeline scripts
source("R/mealplan_data_model.R")
source("R/mealplan_constraints.R")
source("R/mealplan_objectives.R")
source("R/mealplan_alternatives.R")
source("R/mealprep_weekend.R")

list(
  tar_target(forbidden_cfg, "config/forbidden_rules.yml", format = "file"),
  tar_target(soft_bl_cfg, "config/soft_blacklist.yml", format = "file"),
  tar_target(recipe_dataset, load_recipe_dataset(db_path = "recipes.db", forbidden_config = forbidden_cfg, soft_blacklist_config = soft_bl_cfg)),
  tar_target(slots_dataset, default_slots()),
  tar_target(base_model, build_base_mealplan_model(recipe_dataset, slots_dataset)),
  tar_target(weekly_plan, solve_mealplan_lexicographic(base_model)),
  tar_target(weekly_alternatives, get_all_top_alternatives(weekly_plan, top_n = 3)),
  tar_target(weekend_prep, select_weekend_mealprep(db_path = "recipes.db"))
)
