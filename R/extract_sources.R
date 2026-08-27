# Source text extraction for PDF, EPUB, and image files.

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(magick)
  library(pdftools)
  library(digest)
  library(xml2)
  library(rvest)
  library(base64enc)
})

source_file_type <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("jpg", "jpeg", "png", "webp", "tif", "tiff")) return("image")
  if (identical(ext, "pdf")) return("pdf")
  if (identical(ext, "epub")) return("epub")
  NA_character_
}

extract_image_source <- function(file_path, max_dim = 1500, quality = 85) {
  img <- image_read(file_path)
  img_optimized <- image_resize(img, sprintf("%dx%d>", max_dim, max_dim))
  tmp_path <- tempfile(fileext = ".jpg")
  on.exit(unlink(tmp_path), add = TRUE)
  image_write(img_optimized, path = tmp_path, format = "jpeg", quality = quality)
  sprintf("data:image/jpeg;base64,%s", base64encode(tmp_path))
}

extract_pdf_source <- function(file_path) {
  pages <- pdf_text(file_path)
  paste(pages, collapse = "\n--- PAGE BREAK ---\n")
}

extract_epub_source <- function(file_path) {
  tmp_dir <- tempfile(pattern = "epub_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  utils::unzip(file_path, exdir = tmp_dir)
  html_files <- list.files(tmp_dir, pattern = "\\.(html|xhtml|htm)$", full.names = TRUE, recursive = TRUE)
  if (length(html_files) == 0) return("")

  chunks <- vapply(html_files, function(hf) {
    tryCatch(xml_text(read_html(hf)), error = function(e) "")
  }, character(1))
  paste(chunks[nzchar(chunks)], collapse = "\n")
}

extract_source_content <- function(file_path, file_type = source_file_type(file_path)) {
  switch(
    file_type,
    image = extract_image_source(file_path),
    pdf = extract_pdf_source(file_path),
    epub = extract_epub_source(file_path),
    stop("Unsupported file type: ", file_path, call. = FALSE)
  )
}

run_extraction_pipeline <- function(inbox_dir = "to_be_treated",
                                    treated_dir = "treated",
                                    db_path = "recipes.db",
                                    default_book_id = NULL,
                                    delete_duplicates = FALSE) {
  if (!dir.exists(inbox_dir)) stop("Inbox directory does not exist: ", inbox_dir, call. = FALSE)
  if (!dir.exists(treated_dir)) dir.create(treated_dir, recursive = TRUE)

  db <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(db), add = TRUE)
  dbExecute(db, "PRAGMA foreign_keys = ON;")

  files <- list.files(inbox_dir, full.names = TRUE, recursive = TRUE)
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0) {
    message("No files found in inbox.")
    return(invisible(NULL))
  }

  for (file_path in files) {
    file_name <- basename(file_path)
    file_type <- source_file_type(file_path)

    if (is.na(file_type)) {
      warning(sprintf("Unsupported file format: %s", file_name))
      next
    }

    file_hash <- digest(file_path, file = TRUE)
    existing <- dbGetQuery(db, "SELECT source_id FROM raw_sources WHERE file_hash = ?", params = list(file_hash))
    if (nrow(existing) > 0) {
      message(sprintf("Skipped duplicate: %s", file_name))
      if (isTRUE(delete_duplicates)) file.remove(file_path)
      next
    }

    tryCatch({
      raw_content <- extract_source_content(file_path, file_type)
      dbExecute(db, "
        INSERT INTO raw_sources (book_id, file_hash, file_name, file_type, raw_content, status)
        VALUES (?, ?, ?, ?, ?, 'pending')
      ", params = list(default_book_id, file_hash, file_name, file_type, raw_content))

      destination <- file.path(treated_dir, file_name)
      if (file.exists(destination)) {
        destination <- file.path(treated_dir, paste0(tools::file_path_sans_ext(file_name), "_", substr(file_hash, 1, 8), ".", tools::file_ext(file_name)))
      }
      if (!file.rename(file_path, destination)) warning("Could not move treated file: ", file_path)
      message(sprintf("Staged [%s]: %s", toupper(file_type), file_name))
    }, error = function(e) {
      warning(sprintf("Extraction failed for %s: %s", file_name, e$message))
    })
  }

  invisible(NULL)
}
