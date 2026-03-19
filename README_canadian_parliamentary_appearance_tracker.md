# Canadian Parliamentary Appearance Tracker

This repository now includes an R script that can be scheduled via CRON or triggered from n8n to find the next scheduled appearance of a target federal organization before Canadian parliamentary committees.

## Files

- `canadian_parliamentary_appearance_tracker.R`: main tracker script.
- `twitter_handles_template.csv`: starter lookup table for committee-member Twitter/X handles.

## What the script does

1. Scans House of Commons parliamentary business pages day-by-day across a configurable window.
2. Pulls House committee notice links and filters them for the requested organization name or aliases.
3. Pulls Senate committee notice links from the Senate's schedule of all meetings and applies the same organization filter.
4. Enriches committee membership with Twitter/X handles from a static CSV mapping.
5. Writes a flat CSV using the required schema.
6. If no appearance is found, it logs the result and writes an empty CSV with the required headers instead of failing.

## Output schema

The generated CSV includes these columns:

- `organization_name`
- `representative_name`
- `committee_name`
- `meeting_date`
- `committee_members`
- `twitter_handles`

The script also retains `source_chamber` and `source_url` internally when matches are found, to help with troubleshooting and auditability.

## Requirements

Install the following R packages in the runtime environment:

```r
install.packages(c(
  "dplyr",
  "purrr",
  "readr",
  "rvest",
  "stringr",
  "lubridate",
  "tidyr",
  "tibble",
  "httr2",
  "xml2"
))
```

## Example usage

```bash
Rscript canadian_parliamentary_appearance_tracker.R \
  --organization-name=OSFI \
  --organization-aliases='Office of the Superintendent of Financial Institutions|OSFI' \
  --lookahead-days=14 \
  --output-dir=./output \
  --twitter-map=./twitter_handles_template.csv
```

Environment variables can be used instead of flags:

- `ORGANIZATION_NAME`
- `ORGANIZATION_ALIASES`
- `LOOKAHEAD_DAYS`
- `START_DATE`
- `END_DATE`
- `OUTPUT_DIR`
- `TWITTER_MAP`

## n8n implementation notes

Recommended node flow:

1. **Schedule Trigger**: run daily, for example at 8:00 AM.
2. **Execute Command**: invoke the R script with the organization name and output directory.
3. **Read Binary File / Local File Access**: load the generated CSV.
4. **Spreadsheet File**: optional if you want n8n to re-serialize or email the CSV.
5. **Logging / Notification node**: emit a success message or a "no appearance found" note.

Example n8n command:

```bash
Rscript /path/to/canadian_parliamentary_appearance_tracker.R \
  --organization-name="OSFI" \
  --organization-aliases='Office of the Superintendent of Financial Institutions|OSFI' \
  --lookahead-days=14 \
  --output-dir="/data/parliament-tracker" \
  --twitter-map="/data/parliament-tracker/twitter_handles.csv"
```

## CRON example

```cron
0 8 * * * /usr/bin/Rscript /workspace/automatisation/canadian_parliamentary_appearance_tracker.R --organization-name=OSFI --organization-aliases='Office of the Superintendent of Financial Institutions|OSFI' --lookahead-days=14 --output-dir=/workspace/automatisation/output --twitter-map=/workspace/automatisation/twitter_handles_template.csv >> /workspace/automatisation/output/parliament_tracker.log 2>&1
```

## Notes and assumptions

- The script uses official House of Commons and Senate web pages as its source of truth.
- House committee notices are harvested from daily parliamentary business pages.
- Senate notices are harvested from the Senate's schedule of all meetings page.
- Twitter/X handles are intentionally sourced from a static mapping file because committee rosters and social handles may require editorial review.
- The current implementation force-includes Adam Chambers in House outputs and Rosa Galvez in Senate outputs so their handles always appear in the corresponding committee enrichment fields.
- Because committee websites evolve over time, selectors were written defensively and the script logs recoverable failures instead of stopping the whole run.
