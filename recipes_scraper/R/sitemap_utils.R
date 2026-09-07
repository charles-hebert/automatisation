suppressPackageStartupMessages({
  library(xml2)
  library(rvest)
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
})

safe_read_xml <- function(url) {
  tryCatch(read_xml(url), error = function(e) NULL)
}

extract_locs_from_sitemap <- function(sitemap_url) {
  doc <- safe_read_xml(sitemap_url)
  if (is.null(doc)) {
    warning(sprintf("Could not read sitemap: %s", sitemap_url))
    return(character())
  }

  root_name <- xml_name(xml_root(doc))

  if (str_detect(root_name, "sitemapindex")) {
    sub_sitemaps <- xml_text(xml_find_all(doc, ".//*[local-name()='sitemap']/*[local-name()='loc']"))
    sub_sitemaps <- sub_sitemaps[nzchar(sub_sitemaps)]
    urls <- map(sub_sitemaps, extract_locs_from_sitemap) |> unlist(use.names = FALSE)
    return(urls)
  }

  if (str_detect(root_name, "urlset")) {
    urls <- xml_text(xml_find_all(doc, ".//*[local-name()='url']/*[local-name()='loc']"))
    return(urls[nzchar(urls)])
  }

  warning(sprintf("Unknown sitemap root '%s' for %s", root_name, sitemap_url))
  character()
}

filter_recipe_urls <- function(urls, recipe_pattern = "recipe|recipes") {
  urls |> 
    unique() |>
    discard(is.na) |>
    keep(~ str_detect(str_to_lower(.x), recipe_pattern))
}

initial_recipe_table <- function(urls) {
  tibble(
    recipe_url = urls,
    selected = "n"
  )
}

scrape_recipe_page <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) {
    return(tibble(
      recipe_url = url,
      title = NA_character_,
      ingredients = NA_character_,
      instructions = NA_character_
    ))
  }

  title <- page |> html_element("h1") |> html_text2()
  if (is.na(title) || !nzchar(title)) {
    title <- page |> html_element("title") |> html_text2()
  }

  ingredients <- page |>
    html_elements("[class*='ingredient'], [id*='ingredient']") |>
    html_text2() |>
    str_squish() |>
    discard(~ .x == "") |>
    unique() |>
    paste(collapse = " | ")

  instructions <- page |>
    html_elements("[class*='instruction'], [class*='direction'], [id*='instruction'], [id*='direction']") |>
    html_text2() |>
    str_squish() |>
    discard(~ .x == "") |>
    unique() |>
    paste(collapse = " | ")

  tibble(
    recipe_url = url,
    title = ifelse(nzchar(title), title, NA_character_),
    ingredients = ifelse(nzchar(ingredients), ingredients, NA_character_),
    instructions = ifelse(nzchar(instructions), instructions, NA_character_)
  )
}
