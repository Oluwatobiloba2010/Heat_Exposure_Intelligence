
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 02 - DATA QUALITY AUDIT
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

# 1. LOAD THE ACQUIRED DATA -----------------------------------

weather_all <- read_csv(
  "data/raw/Nigeria_Weather_2021_2025.csv",
  show_col_types = FALSE
) |>
  mutate(
    timestamp = with_tz(
      timestamp,
      tzone = "Africa/Lagos"
    )
  )

# 2. STRUCTURAL INSPECTION ------------------------------------

cat("\n--- DATA DIMENSIONS ---\n")
print(dim(weather_all))

cat("\n--- COLUMN NAMES ---\n")
print(names(weather_all))

cat("\n--- DATA TYPES ---\n")
str(weather_all)

cat("\n--- FIRST SIX RECORDS ---\n")
print(head(weather_all))

# 3. MISSING VALUE AUDIT --------------------------------------

missing_summary <- weather_all |>
  summarise(
    across(
      everything(),
      list(
        missing_count = ~sum(is.na(.x)),
        missing_percentage = ~mean(is.na(.x)) * 100
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = "measure",
    values_to = "value"
  )

cat("\n--- MISSING VALUES ---\n")
print(missing_summary)

# Missingness by location and variable
missing_by_location <- weather_all |>
  group_by(location) |>
  summarise(
    across(
      all_of(c(
        "temperature_2m",
        "relative_humidity_2m",
        "wind_speed_10m",
        "wind_direction_10m",
        "shortwave_radiation",
        "wet_bulb_temperature_2m",
        "apparent_temperature"
      )),
      ~sum(is.na(.x)),
      .names = "{.col}_missing"
    ),
    .groups = "drop"
  )

cat("\n--- MISSING VALUES BY LOCATION ---\n")
print(missing_by_location)

# 4. DUPLICATE RECORD AUDIT -----------------------------------

cat("\n--- EXACT DUPLICATE ROWS ---\n")
print(sum(duplicated(weather_all)))

cat("\n--- DUPLICATE LOCATION-TIMESTAMP KEYS ---\n")

duplicate_keys <- weather_all |>
  count(location, timestamp) |>
  filter(n > 1)

print(duplicate_keys)

# 5. TEMPORAL COVERAGE ----------------------------------------

coverage_summary <- weather_all |>
  group_by(location) |>
  summarise(
    total_records = n(),
    unique_timestamps = n_distinct(timestamp),
    first_timestamp = min(timestamp, na.rm = TRUE),
    last_timestamp = max(timestamp, na.rm = TRUE),
    missing_timestamps = 43824 - n_distinct(timestamp),
    .groups = "drop"
  )

cat("\n--- TEMPORAL COVERAGE ---\n")
print(coverage_summary)

# 6. CHECK HOURLY GAPS ----------------------------------------

expected_timestamps <- seq(
  from = ymd_hms(
    "2021-01-01 00:00:00",
    tz = "Africa/Lagos"
  ),
  to = ymd_hms(
    "2025-12-31 23:00:00",
    tz = "Africa/Lagos"
  ),
  by = "hour"
)

timestamp_gaps <- lapply(
  unique(weather_all$location),
  function(loc) {
    
    observed <- weather_all |>
      filter(location == loc) |>
      pull(timestamp) |>
      unique()
    
    tibble(
      location = loc,
      missing_timestamp = setdiff(
        expected_timestamps,
        observed
      )
    )
  }
) |>
  bind_rows()

cat("\n--- MISSING HOURLY TIMESTAMPS ---\n")
print(
  timestamp_gaps |>
    group_by(location) |>
    summarise(
      missing_hours = n(),
      .groups = "drop"
    )
)

# 7. NUMERICAL VARIABLE RANGES -------------------------------

range_summary <- weather_all |>
  group_by(location) |>
  summarise(
    across(
      c(
        temperature_2m,
        relative_humidity_2m,
        wind_speed_10m,
        wind_direction_10m,
        shortwave_radiation,
        wet_bulb_temperature_2m,
        apparent_temperature
      ),
      list(
        minimum = ~min(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        maximum = ~max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

cat("\n--- NUMERICAL VARIABLE RANGES ---\n")
print(range_summary)


# 8. PHYSICAL AND LOGICAL PLAUSIBILITY CHECKS ------------------

# A. Create logical flags for each plausibility check

plausibility_flags <- weather_all |>
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
      wet_bulb_temperature_2m > temperature_2m
  )

# B. Explicitly identify the logical flag columns

plausibility_check_columns <- c(
  "invalid_humidity",
  "negative_wind_speed",
  "invalid_wind_direction",
  "negative_solar_radiation",
  "wet_bulb_above_air_temperature"
)

# C. Count flagged observations for each check

plausibility_summary <- plausibility_flags |>
  summarise(
    across(
      all_of(plausibility_check_columns),
      ~sum(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "check",
    values_to = "flagged_records"
  )

cat("\n--- PHYSICAL AND LOGICAL PLAUSIBILITY ---\n")
print(plausibility_summary, n = Inf)

# D. Identify records that triggered at least one check

flagged_records <- plausibility_flags |>
  filter(
    if_any(
      all_of(plausibility_check_columns),
      ~.x
    )
  )

cat("\n--- FLAGGED RECORDS ---\n")
print(flagged_records, n = 20)

# E. Save the results

write_csv(
  plausibility_summary,
  "outputs/audit/plausibility_summary.csv"
)

write_csv(
  flagged_records,
  "outputs/audit/flagged_records.csv"
)


# 9. SAVE AUDIT OUTPUTS ---------------------------------------


dir.create(
  "outputs/audit",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  coverage_summary,
  "outputs/audit/temporal_coverage.csv"
)

write_csv(
  missing_by_location,
  "outputs/audit/missing_by_location.csv"
)

write_csv(
  range_summary,
  "outputs/audit/numerical_ranges.csv"
)

write_csv(
  timestamp_gaps,
  "outputs/audit/timestamp_gaps.csv"
)

write_csv(
  duplicate_keys,
  "outputs/audit/duplicate_keys.csv"
)

cat("\nDATA AUDIT COMPLETED.\n")
