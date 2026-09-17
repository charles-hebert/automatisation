# Social Bookmark Extraction Script
# Extracts links and nearby title/snippet text from a local HTML export.

library(rvest)
library(dplyr)
library(stringr)

# ---- Input ----
# Update this path to point to your exported HTML file.
html_path <- "path/to/your/bookmarks_export.html"

# ---- Optional platform-specific pattern ----
# Examples:
# twitter_pattern  <- "status"
# linkedin_pattern <- "feed/update"
# facebook_pattern <- "posts|story.php|permalink"
post_pattern <- "status|feed/update|posts|story.php|permalink"

# ---- Read HTML ----
page <- read_html(html_path)

# Use file location as base URL for resolving any relative links.
base_url <- paste0("file://", normalizePath(html_path, winslash = "/", mustWork = FALSE))

# ---- Extract all <a> tags, not class-dependent ----
all_links <- page %>% html_elements("a")

extract_nearest_text <- function(node) {
  # Try parent span/div first
  parent_text <- node %>%
    html_element(xpath = "./ancestor::*[self::span or self::div][1]") %>%
    html_text2(trim = TRUE)

  # Fallback to nearest meaningful ancestor containing visible text
  if (is.na(parent_text) || parent_text == "") {
    parent_text <- node %>%
      html_element(xpath = "./ancestor::*[normalize-space(text()) != ''][1]") %>%
      html_text2(trim = TRUE)
  }

  if (is.na(parent_text) || str_squish(parent_text) == "") {
    return("No Title Found")
  }

  str_squish(parent_text)
}

bookmarks_cleaned <- tibble(
  url_raw = html_attr(all_links, "href"),
  title = vapply(all_links, extract_nearest_text, character(1))
) %>%
  filter(!is.na(url_raw), url_raw != "") %>%
  mutate(
    # Convert relative URLs to absolute
    url = url_absolute(url_raw, base = base_url),
    # Remove tracking/query params (everything after '?')
    url = str_remove(url, "\\?.*$")
  ) %>%
  # Keep likely social post links only
  filter(str_detect(url, regex(post_pattern, ignore_case = TRUE))) %>%
  mutate(
    title = if_else(is.na(title) | str_squish(title) == "", "No Title Found", title)
  ) %>%
  distinct(url, .keep_all = TRUE) %>%
  select(title, url)

print(bookmarks_cleaned)

write.csv(bookmarks_cleaned, "bookmarks_cleaned.csv", row.names = FALSE)
