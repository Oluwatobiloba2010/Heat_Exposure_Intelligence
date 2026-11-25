
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 03 - DATA CLEANING AND PREPARATION
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# 1. DEFINE PROJECT PATHS -------------------------------------

raw_file <- "data/raw/Nigeria_Weather_2021_2025.csv"

processed_file <- "data/processed/Nigeria_Weather_Cleaned.csv"

audit_directory <- "outputs/audit"

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  audit_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

# 2. LOAD RAW DATA --------------------------------------------

weather_raw <- read_csv(
  raw_file,
  show_col_types = FALSE
)

cat("\n--- RAW DATA DIMENSIONS ---\n")
print(dim(weather_raw))

# 3. VALIDATE REQUIRED COLUMNS --------------------------------

required_columns <- c(
  "location",
  "latitude",
  "longitude",
  "timestamp",
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "wind_direction_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature",
  "model",
  "data_source"
)

missing_columns <- setdiff(
  required_columns,
  names(weather_raw)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Retain only the expected project columns, in a consistent order.
weather_raw <- weather_raw |>
  select(all_of(required_columns))

# 4. STANDARDISE TIMESTAMP ------------------------------------

# readr normally imports this column as POSIXct.
# If it is character instead, parse it before changing timezone.

if (inherits(weather_raw$timestamp, "POSIXct")) {
  
  weather_raw <- weather_raw |>
    mutate(
      timestamp = with_tz(
        timestamp,
        tzone = "Africa/Lagos"
      )
    )
  
} else {
  
  weather_raw <- weather_raw |>
    mutate(
      timestamp = ymd_hms(
        timestamp,
        tz = "UTC",
        quiet = TRUE
      )
    )
  
  if (anyNA(weather_raw$timestamp)) {
    stop(
      "Some timestamps could not be parsed. Inspect the raw file."
    )
  }
  
  weather_raw <- weather_raw |>
    mutate(
      timestamp = with_tz(
        timestamp,
        tzone = "Africa/Lagos"
      )
    )
}

# 5. STANDARDISE DATA TYPES -----------------------------------

weather_clean <- weather_raw |>
  mutate(
    location = as.character(location),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    temperature_2m = as.numeric(temperature_2m),
    relative_humidity_2m = as.numeric(relative_humidity_2m),
    wind_speed_10m = as.numeric(wind_speed_10m),
    wind_direction_10m = as.numeric(wind_direction_10m),
    shortwave_radiation = as.numeric(shortwave_radiation),
    wet_bulb_temperature_2m = as.numeric(wet_bulb_temperature_2m),
    apparent_temperature = as.numeric(apparent_temperature),
    model = as.character(model),
    data_source = as.character(data_source)
  ) |>
  arrange(location, timestamp)

# 6. CHECK MISSING VALUES -------------------------------------

missing_summary <- weather_clean |>
  summarise(
    across(
      everything(),
      ~sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  )

cat("\n--- MISSING VALUE SUMMARY ---\n")
print(missing_summary, n = Inf)

write_csv(
  missing_summary,
  "outputs/audit/cleaning_missing_summary.csv"
)

# 7. CHECK EXACT DUPLICATES -----------------------------------

exact_duplicate_count <- sum(
  duplicated(weather_clean)
)

cat("\n--- EXACT DUPLICATE ROWS ---\n")
print(exact_duplicate_count)

# 8. CHECK DUPLICATE LOCATION-TIMESTAMP KEYS ------------------

duplicate_keys <- weather_clean |>
  count(
    location,
    timestamp,
    name = "records"
  ) |>
  filter(records > 1)

cat("\n--- DUPLICATE LOCATION-TIMESTAMP KEYS ---\n")
print(duplicate_keys, n = 20)

write_csv(
  duplicate_keys,
  "outputs/audit/cleaning_duplicate_keys.csv"
)

# 9. CHECK NUMERICAL AND PHYSICAL PLAUSIBILITY ----------------

cleaning_flags <- weather_clean |>
  mutate(
    invalid_humidity =
      relative_humidity_2m < 0 |
      relative_humidity_2m > 100,
    
    negative_wind_speed =
      wind_speed_10m < 0,
    
    invalid_wind_direction =
      wind_direction_10m < 0 |
      wind_direction_10m > 360,
    
    negative_solar_radiation =
      shortwave_radiation < 0,
    
    wet_bulb_above_air_temperature =
      wet_bulb_temperature_2m > temperature_2m,
    
    non_finite_numeric_value =
      !is.finite(latitude) |
      !is.finite(longitude) |
      !is.finite(temperature_2m) |
      !is.finite(relative_humidity_2m) |
      !is.finite(wind_speed_10m) |
      !is.finite(wind_direction_10m) |
      !is.finite(shortwave_radiation) |
      !is.finite(wet_bulb_temperature_2m) |
      !is.finite(apparent_temperature)
  )

flag_columns <- c(
  "invalid_humidity",
  "negative_wind_speed",
  "invalid_wind_direction",
  "negative_solar_radiation",
  "wet_bulb_above_air_temperature",
  "non_finite_numeric_value"
)

cleaning_flag_summary <- cleaning_flags |>
  summarise(
    across(
      all_of(flag_columns),
      ~sum(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "check",
    values_to = "flagged_records"
  )

flagged_cleaning_records <- cleaning_flags |>
  filter(
    if_any(
      all_of(flag_columns),
      ~coalesce(.x, FALSE)
    )
  )

cat("\n--- CLEANING PLAUSIBILITY SUMMARY ---\n")
print(cleaning_flag_summary, n = Inf)

cat("\n--- NUMBER OF FLAGGED RECORDS ---\n")
print(nrow(flagged_cleaning_records))

write_csv(
  cleaning_flag_summary,
  "outputs/audit/cleaning_plausibility_summary.csv"
)

write_csv(
  flagged_cleaning_records,
  "outputs/audit/cleaning_flagged_records.csv"
)

# 10. CHECK TEMPORAL COVERAGE ---------------------------------

temporal_coverage <- weather_clean |>
  group_by(location) |>
  summarise(
    records = n(),
    unique_timestamps = n_distinct(timestamp),
    first_timestamp = min(timestamp, na.rm = TRUE),
    last_timestamp = max(timestamp, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- TEMPORAL COVERAGE ---\n")
print(temporal_coverage, n = Inf)

write_csv(
  temporal_coverage,
  "outputs/audit/cleaning_temporal_coverage.csv"
)

# 11. SAVE THE PREPARED DATASET -------------------------------

# No observations are automatically deleted.
# No missing values are automatically imputed.
# No measurements are automatically replaced.

weather_clean <- weather_clean |>
  arrange(location, timestamp)

write_csv(
  weather_clean,
  processed_file
)

# 12. CREATE A CLEANING SUMMARY -------------------------------

cleaning_summary <- tibble(
  metric = c(
    "Raw records",
    "Prepared records",
    "Raw columns",
    "Prepared columns",
    "Exact duplicate rows",
    "Duplicate location-timestamp keys",
    "Missing cells",
    "Flagged records",
    "Records removed",
    "Missing values imputed"
  ),
  value = c(
    nrow(weather_raw),
    nrow(weather_clean),
    ncol(weather_raw),
    ncol(weather_clean),
    exact_duplicate_count,
    nrow(duplicate_keys),
    sum(missing_summary$missing_values),
    nrow(flagged_cleaning_records),
    0,
    0
  )
)

cat("\n--- FINAL CLEANING SUMMARY ---\n")
print(cleaning_summary, n = Inf)

write_csv(
  cleaning_summary,
  "outputs/audit/cleaning_summary.csv"
)

cat("\n============================================\n")
cat("DATA CLEANING AND PREPARATION COMPLETED\n")
cat("============================================\n")
cat("Prepared dataset saved to:", processed_file, "\n")
cat("Raw data have not been overwritten.\n")
cat("Flagged records, if any, have been preserved for review.\n")
