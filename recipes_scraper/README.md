# Recipe Scraper (R Project)

This project does exactly the 2-step workflow you asked for:

1. Read a website sitemap and extract a list of recipe URLs.
2. Let user select recipes with a `selected` y/n column in CSV, then scrape only selected recipe pages.

## Setup

From the project root (`recipes_scraper`), install packages once:

```r
install.packages(c("xml2", "rvest", "readr", "dplyr", "stringr", "purrr"))
```

## Step 1: Extract recipe links from sitemap

```bash
Rscript scripts/01_extract_recipes_from_sitemap.R \
  "https://example.com/sitemap.xml" \
  "data/recipe_candidates.csv"
```

Optional third argument lets you override the URL filter regex:

```bash
Rscript scripts/01_extract_recipes_from_sitemap.R \
  "https://example.com/sitemap.xml" \
  "data/recipe_candidates.csv" \
  "recipe|recipes|food"
```

Output columns:
- `recipe_url`
- `selected` (defaults to `n`)

## Step 2: Mark selected recipes, then scrape

Open `data/recipe_candidates.csv` and set `selected` to `y` for rows you want.

Then run:

```bash
Rscript scripts/02_scrape_selected_recipes.R \
  "data/recipe_candidates.csv" \
  "data/selected_recipes_scraped.csv"
```

Output columns:
- `recipe_url`
- `title`
- `ingredients`
- `instructions`

## Notes

- The scraper is generic, so it relies on common HTML class/id names (`ingredient`, `instruction`, `direction`).
- Some websites have anti-bot protections or different page structures; selectors may need per-site tuning.
- Sitemaps can be indexes of other sitemaps; this project supports recursive sitemap discovery.
