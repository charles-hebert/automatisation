# TSX stock selection with insider-activity indicator
# --------------------------------------------------
# Produces 3 base portfolios and 3 extended CSV files with insider-activity signals.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(rvest)
  library(yfR)
  library(purrr)
})

parse_market_cap <- function(x) {
  x <- str_replace_all(x, ",", "")
  mult <- case_when(
    str_detect(x, "T$") ~ 1e12,
    str_detect(x, "B$") ~ 1e9,
    str_detect(x, "M$") ~ 1e6,
    str_detect(x, "K$") ~ 1e3,
    TRUE ~ 1
  )
  parse_number(x) * mult
}

to_numeric <- function(x) {
  parse_number(str_replace_all(x, ",", ""))
}

# Pull "Net Shares Purchased (Sold)" from Yahoo insider-transactions page
get_insider_net_shares <- function(yahoo_symbol) {
  base <- str_remove(yahoo_symbol, "\\.TO$")
  url <- sprintf("https://ca.finance.yahoo.com/quote/%s.TO/insider-transactions?p=%s.TO", base, base)

  page <- tryCatch(read_html(url), error = function(e) NULL)
  if (is.null(page)) return(NA_real_)

  txt <- page |>
    html_text2() |>
    str_squish()

  # Example target pattern: "Net Shares Purchased (Sold) 123,456"
  m <- str_match(txt, "Net Shares Purchased \\(Sold\\)\\s*([+-]?[0-9,]+)")
  if (is.na(m[1, 2])) return(NA_real_)

  as.numeric(str_replace_all(m[1, 2], ",", ""))
}

insider_indicator <- function(net_shares) {
  case_when(
    is.na(net_shares) ~ "unknown",
    net_shares > 0 ~ "bullish",
    net_shares < 0 ~ "bearish",
    TRUE ~ "neutral"
  )
}

# 1) Universe: largest 300 TSX stocks
raw_tbl <- read_html("https://stockanalysis.com/list/tsx-stocks/") |>
  html_table(fill = TRUE)

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
    yahoo_symbol = if_else(str_detect(symbol, "\\.TO$"), symbol, paste0(symbol, ".TO"))
  )

largest_300 <- tsx_tbl |>
  filter(!is.na(market_cap_num)) |>
  arrange(desc(market_cap_num)) |>
  slice_head(n = 300)

# 2) 260-day realized volatility
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

# 3) Common 50-stock filter
candidate_50 <- largest_300 |>
  left_join(vol_260, by = c("yahoo_symbol" = "ticker")) |>
  filter(
    !is.na(vol_260),
    !is.na(dividend_yield),
    dividend_yield > 0
  ) |>
  arrange(vol_260) |>
  slice_head(n = 50)

base_cols <- c("symbol", "company", "market_cap_num", "vol_260", "pe_ratio", "pb_ratio", "dividend_yield", "yahoo_symbol")

# Portfolios
portfolio_low_pe <- candidate_50 |>
  filter(!is.na(pe_ratio), pe_ratio > 0) |>
  arrange(pe_ratio) |>
  slice_head(n = 10) |>
  select(all_of(base_cols))

portfolio_low_pb <- candidate_50 |>
  filter(!is.na(pb_ratio), pb_ratio > 0) |>
  arrange(pb_ratio) |>
  slice_head(n = 10) |>
  select(all_of(base_cols))

portfolio_high_div <- candidate_50 |>
  filter(!is.na(dividend_yield), dividend_yield > 0) |>
  arrange(desc(dividend_yield)) |>
  slice_head(n = 10) |>
  select(all_of(base_cols))

add_insider_signal <- function(df) {
  df |>
    mutate(
      insider_net_shares = map_dbl(yahoo_symbol, get_insider_net_shares),
      insider_activity = insider_indicator(insider_net_shares)
    )
}

portfolio_low_pe_insider <- add_insider_signal(portfolio_low_pe)
portfolio_low_pb_insider <- add_insider_signal(portfolio_low_pb)
portfolio_high_div_insider <- add_insider_signal(portfolio_high_div)

# Base CSV outputs
write_csv(select(portfolio_low_pe, -yahoo_symbol), "tsx_portfolio_low_pe.csv")
write_csv(select(portfolio_low_pb, -yahoo_symbol), "tsx_portfolio_low_pb.csv")
write_csv(select(portfolio_high_div, -yahoo_symbol), "tsx_portfolio_high_div.csv")

# Additional CSV outputs with insider indicator
write_csv(select(portfolio_low_pe_insider, -yahoo_symbol), "tsx_portfolio_low_pe_with_insider.csv")
write_csv(select(portfolio_low_pb_insider, -yahoo_symbol), "tsx_portfolio_low_pb_with_insider.csv")
write_csv(select(portfolio_high_div_insider, -yahoo_symbol), "tsx_portfolio_high_div_with_insider.csv")

cat("Generated 6 CSV files (3 base + 3 with insider activity indicator).\n")
