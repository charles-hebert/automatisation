# Review queue export and threshold management.

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

#' Export all recipe tag classifications currently in 'review' status to CSV.
#'
#' @param db_path Path to SQLite database
#' @param output_path CSV output file path (defaults to root review_queue.csv)
#' @return Data frame of review queue items
export_review_queue <- function(db_path = "recipes.db", output_path = "review_queue.csv") {
  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)

  review_items <- dbGetQuery(db, "
    SELECT
      c.recipe_id,
      r.title AS recipe_title,
      c.tag_name,
      c.tag_value,
      c.confidence,
      c.tag_source,
      c.updated_at
    FROM recipe_tag_classifications c
    JOIN recipes r ON c.recipe_id = r.recipe_id
    WHERE c.status = 'review'
    ORDER BY c.recipe_id, c.tag_name
  ")

  if (nrow(review_items) == 0) {
    review_items <- data.frame(
      recipe_id = integer(0),
      recipe_title = character(0),
      tag_name = character(0),
      tag_value = character(0),
      confidence = integer(0),
      tag_source = character(0),
      updated_at = character(0)
    )
  }

  utils::write.csv(review_items, output_path, row.names = FALSE)
  log_info("Exported {nrow(review_items)} review items to {output_path}")
  invisible(review_items)
}
