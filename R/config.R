# Central configuration and logging setup.

suppressPackageStartupMessages({
  library(logger)
})

# Setup logger configuration
setup_logging <- function(log_level = INFO, log_file = NULL) {
  log_threshold(log_level)
  log_formatter(formatter_glue)

  if (!is.null(log_file) && nzchar(log_file)) {
    log_appender(appender_tee(log_file))
  } else {
    log_appender(appender_console)
  }

  log_info("Logging initialized.")
  invisible(NULL)
}

# Auto-initialize on source
setup_logging()
