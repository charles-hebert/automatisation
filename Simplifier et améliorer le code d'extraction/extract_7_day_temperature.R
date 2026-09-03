# Extract the temperature and precipitation forecast for the next 7 days.
#
# This script uses the Open-Meteo public forecast API (no API key required).
# By default, it extracts daily minimum/maximum temperatures, precipitation
# probability, and precipitation quantity for Montreal.
# Override the defaults by setting environment variables before running:
#   LATITUDE=45.5017 LONGITUDE=-73.5673 LOCATION_NAME="Montreal" Rscript extract_7_day_temperature.R

suppressPackageStartupMessages({
  library(httr2)
  library(readr)
})

get_env_number <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) return(default)

  parsed <- suppressWarnings(as.numeric(value))
  if (is.na(parsed)) {
    stop(sprintf("Environment variable %s must be numeric; got '%s'.", name, value), call. = FALSE)
  }

  parsed
}

fetch_7_day_weather <- function(latitude = get_env_number("LATITUDE", 45.5017),
                                longitude = get_env_number("LONGITUDE", -73.5673),
                                location_name = Sys.getenv("LOCATION_NAME", "Montreal"),
                                timezone = Sys.getenv("TIMEZONE", "auto")) {
  response <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude = latitude,
      longitude = longitude,
      daily = paste(
        c(
          "temperature_2m_min",
          "temperature_2m_max",
          "precipitation_probability_max",
          "precipitation_sum"
        ),
        collapse = ","
      ),
      forecast_days = 7,
      timezone = timezone
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  daily <- response$daily

  data.frame(
    location = location_name,
    latitude = latitude,
    longitude = longitude,
    date = as.Date(daily$time),
    temperature_min_c = daily$temperature_2m_min,
    temperature_max_c = daily$temperature_2m_max,
    precipitation_expected = daily$precipitation_probability_max > 0 | daily$precipitation_sum > 0,
    precipitation_probability_max_percent = daily$precipitation_probability_max,
    precipitation_quantity_mm = daily$precipitation_sum
  )
}

fetch_7_day_temperature <- fetch_7_day_weather

output_path <- Sys.getenv("OUTPUT_PATH", "weather_next_7_days.csv")
weather_forecast <- fetch_7_day_weather()

write_csv(weather_forecast, output_path)
print(weather_forecast)
message(sprintf("Saved 7-day weather forecast to %s", output_path))
