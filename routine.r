# Stock selection on the TSX using three factor assumptions
# ---------------------------------------------------------
# Strategy steps (common to all 3 portfolios):
# 1) Start from the 300 largest TSX stocks by market cap
# 2) Keep the 50 dividend payers with the lowest 260-day volatility
# 3) Select 10 names according to each assumption:
#    - lowest P/E
#    - lowest P/B
#    - highest dividend yield

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(rvest)
  library(yfR)
})

# ---------- Helpers ----------
parse_market_cap <- function(x) {
  # Converts strings like "12.3B", "450M", "1.2T" to numeric
  x <- str_replace_all(x, ",", "")
  mult <- case_when(
    str_detect(x, "T$") ~ 1e12,
    str_detect(x, "B$") ~ 1e9,
    str_detect(x, "M$") ~ 1e6,
    str_detect(x, "K$") ~ 1e3,
    TRUE ~ 1
  )
  val <- parse_number(x)
  val * mult
}

to_numeric <- function(x) {
  parse_number(str_replace_all(x, ",", ""))
}

# ---------- 1) Load TSX universe and keep largest 300 ----------
# Source includes TSX fundamentals (symbol, market cap, P/E, P/B, div yield, etc.)
# If this source changes format, adapt the select()/rename() section below.
url <- "https://stockanalysis.com/list/tsx-stocks/"

raw_tbl <- read_html(url) |> html_table(fill = TRUE)

tsx_tbl <- raw_tbl[[1]] |>
  transmute(
    symbol = Symbol,
    company = Company,
    market_cap = `Market Cap`,
    pe_ratio = `P/E`,
    pb_ratio = `P/B`,
    dividend_yield = `Div Yield`
  ) |>
  mutate(
    market_cap_num = parse_market_cap(market_cap),
    pe_ratio = to_numeric(pe_ratio),
    pb_ratio = to_numeric(pb_ratio),
    dividend_yield = to_numeric(dividend_yield),
    # Yahoo Finance TSX format
    yahoo_symbol = if_else(str_detect(symbol, "\\.TO$"), symbol, paste0(symbol, ".TO"))
  )

largest_300 <- tsx_tbl |>
  filter(!is.na(market_cap_num)) |>
  arrange(desc(market_cap_num)) |>
  slice_head(n = 300)

# ---------- 2) Compute 260-day realized volatility ----------
# Pull ~400 calendar days so we can get at least ~260 trading days
prices <- yf_get(
  tickers = unique(largest_300$yahoo_symbol),
  first_date = Sys.Date() - 420,
  last_date = Sys.Date(),
  do_cache = FALSE,
  bench_ticker = NULL,
  thresh_bad_data = 1
)

vol_260 <- prices |>
  group_by(ticker) |>
  arrange(ref_date, .by_group = TRUE) |>
  mutate(ret = log(price_adjusted / lag(price_adjusted))) |>
  summarise(
    n_obs = sum(!is.na(ret)),
    vol_260 = if_else(n_obs >= 260, sd(tail(na.omit(ret), 260)) * sqrt(252), as.numeric(NA)),
    .groups = "drop"
  )

# ---------- 3) Common filter: 50 lowest-vol dividend payers ----------
candidate_50 <- largest_300 |>
  left_join(vol_260, by = c("yahoo_symbol" = "ticker")) |>
  filter(
    !is.na(vol_260),
    !is.na(dividend_yield),
    dividend_yield > 0
  ) |>
  arrange(vol_260) |>
  slice_head(n = 50)

# ---------- Portfolio A: 10 lowest P/E ----------
portfolio_low_pe <- candidate_50 |>
  filter(!is.na(pe_ratio), pe_ratio > 0) |>
  arrange(pe_ratio) |>
  slice_head(n = 10) |>
  select(symbol, company, market_cap_num, vol_260, pe_ratio, pb_ratio, dividend_yield)

# ---------- Portfolio B: 10 lowest P/B ----------
portfolio_low_pb <- candidate_50 |>
  filter(!is.na(pb_ratio), pb_ratio > 0) |>
  arrange(pb_ratio) |>
  slice_head(n = 10) |>
  select(symbol, company, market_cap_num, vol_260, pe_ratio, pb_ratio, dividend_yield)

# ---------- Portfolio C: 10 highest dividend yields ----------
portfolio_high_div <- candidate_50 |>
  filter(!is.na(dividend_yield), dividend_yield > 0) |>
  arrange(desc(dividend_yield)) |>
  slice_head(n = 10) |>
  select(symbol, company, market_cap_num, vol_260, pe_ratio, pb_ratio, dividend_yield)

# ---------- Output ----------
write_csv(portfolio_low_pe, "tsx_portfolio_low_pe.csv")
write_csv(portfolio_low_pb, "tsx_portfolio_low_pb.csv")
write_csv(portfolio_high_div, "tsx_portfolio_high_div.csv")

cat("\n=== Portfolio 1: Lowest P/E (10 names) ===\n")
print(portfolio_low_pe)

cat("\n=== Portfolio 2: Lowest P/B (10 names) ===\n")
print(portfolio_low_pb)

cat("\n=== Portfolio 3: Highest dividend yield (10 names) ===\n")
print(portfolio_high_div)
