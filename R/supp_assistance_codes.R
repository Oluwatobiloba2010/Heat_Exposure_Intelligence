






library(readr)
library(dplyr)

predictions <- read_csv(
  "outputs/tables/08b_model_test_predictions.csv",
  show_col_types = FALSE
)

# 1. Records by prediction task
cat("\nRECORDS BY TASK\n")

print(
  predictions |>
    count(task, frequency, name = "records")
)

# 2. Records by location and task
cat("\nRECORDS BY LOCATION AND TASK\n")

print(
  predictions |>
    count(task, location, name = "records")
)

# 3. Missing values
cat("\nMISSING VALUES\n")

print(
  predictions |>
    summarise(
      across(
        everything(),
        ~ sum(is.na(.x))
      )
    )
)

# 4. Duplicate observation keys
cat("\nDUPLICATE OBSERVATION KEYS\n")

print(
  predictions |>
    count(
      task,
      frequency,
      location,
      observation_date,
      name = "records"
    ) |>
    filter(records > 1)
)

# 5. Prediction ranges
cat("\nPREDICTION RANGES\n")

print(
  predictions |>
    summarise(
      across(
        c(
          actual,
          persistence_prediction,
          linear_prediction,
          random_forest_prediction
        ),
        list(
          minimum = ~ min(.x, na.rm = TRUE),
          maximum = ~ max(.x, na.rm = TRUE)
        )
      )
    )
)


library(readr)
library(dplyr)
library(tidyr)

predictions <- read_csv(
  "outputs/tables/08b_model_test_predictions.csv",
  show_col_types = FALSE
)

# A. Check missing values in every column
cat("\nMISSING VALUES BY COLUMN\n")

print(
  predictions |>
    summarise(
      across(everything(), ~ sum(is.na(.x)))
    ) |>
    pivot_longer(
      everything(),
      names_to = "column",
      values_to = "missing_values"
    )
)

# B. Check daily prediction uniqueness
cat("\nDAILY DUPLICATE CHECK\n")

print(
  predictions |>
    filter(frequency == "daily") |>
    count(
      task,
      location,
      observation_date
    ) |>
    summarise(
      total_groups = n(),
      duplicate_groups = sum(n > 1),
      maximum_records_per_group = max(n)
    )
)

# C. Check hourly observations per date
cat("\nHOURLY OBSERVATIONS PER DATE\n")

hourly_daily_counts <- predictions |>
  filter(frequency == "hourly") |>
  count(
    task,
    location,
    observation_date,
    name = "hourly_records"
  )

print(
  hourly_daily_counts |>
    count(
      hourly_records,
      name = "number_of_dates"
    )
)

# D. Identify dates with fewer than 24 hourly records
cat("\nDATES WITH FEWER THAN 24 HOURLY RECORDS\n")

print(
  hourly_daily_counts |>
    filter(hourly_records < 24) |>
    arrange(task, location, observation_date)
)

library(readr)
library(dplyr)
library(lubridate)

predictions <- read_csv(
  "outputs/tables/08b_model_test_predictions.csv",
  show_col_types = FALSE
)

cat("\nTOTAL RECORDS:", nrow(predictions), "\n")

cat("\nHOURLY TIMESTAMP SUMMARY\n")

hourly_check <- predictions |>
  filter(frequency == "hourly") |>
  mutate(
    observation_timestamp = ymd_hms(
      observation_timestamp,
      tz = "Africa/Lagos"
    )
  ) |>
  group_by(task, location) |>
  arrange(observation_timestamp, .by_group = TRUE) |>
  summarise(
    records = n(),
    first_timestamp = min(observation_timestamp),
    last_timestamp = max(observation_timestamp),
    missing_timestamps = sum(is.na(observation_timestamp)),
    duplicate_timestamps = sum(duplicated(observation_timestamp)),
    gaps_over_one_hour = sum(
      diff(as.numeric(observation_timestamp)) > 3600
    ),
    .groups = "drop"
  )

print(hourly_check, n = Inf)

cat("\nDAILY TIMESTAMP CHECK\n")

print(
  predictions |>
    filter(frequency == "daily") |>
    summarise(
      records = n(),
      missing_dates = sum(is.na(observation_date)),
      unique_dates = n_distinct(observation_date)
    )
)


library(readr)
library(dplyr)
library(lubridate)

predictions <- read_csv(
  "outputs/tables/08b_model_test_predictions.csv",
  col_types = cols(
    observation_timestamp = col_character()
  ),
  show_col_types = FALSE
)

# A. Missing timestamps by prediction frequency
cat("\nMISSING TIMESTAMPS BY FREQUENCY\n")

print(
  predictions |>
    group_by(frequency) |>
    summarise(
      records = n(),
      missing_timestamps = sum(
        is.na(observation_timestamp) |
          observation_timestamp == ""
      ),
      populated_timestamps = sum(
        !is.na(observation_timestamp) &
          observation_timestamp != ""
      ),
      .groups = "drop"
    )
)

# B. Inspect actual hourly timestamp values
cat("\nFIRST 15 HOURLY TIMESTAMP VALUES\n")

print(
  predictions |>
    filter(frequency == "hourly") |>
    select(
      task,
      location,
      observation_date,
      observation_timestamp
    ) |>
    slice_head(n = 15),
  n = 15
)

# C. Inspect the last 15 hourly timestamp values
cat("\nLAST 15 HOURLY TIMESTAMP VALUES\n")

print(
  predictions |>
    filter(frequency == "hourly") |>
    select(
      task,
      location,
      observation_date,
      observation_timestamp
    ) |>
    slice_tail(n = 15),
  n = 15
)

# D. Test timestamp parsing only on hourly records
hourly_parsed <- predictions |>
  filter(frequency == "hourly") |>
  mutate(
    parsed_timestamp = ymd_hms(
      observation_timestamp,
      tz = "Africa/Lagos",
      quiet = TRUE
    )
  )

cat("\nHOURLY TIMESTAMP PARSING\n")

print(
  hourly_parsed |>
    summarise(
      total_records = n(),
      missing_raw_values = sum(
        is.na(observation_timestamp) |
          observation_timestamp == ""
      ),
      failed_parses = sum(is.na(parsed_timestamp)),
      successfully_parsed = sum(
        !is.na(parsed_timestamp)
      )
    )
)

# E. Check the number of hourly records for
# each task and location
cat("\nHOURLY RECORD COUNTS\n")

print(
  hourly_parsed |>
    group_by(task, location) |>
    summarise(
      records = n(),
      valid_timestamps = sum(
        !is.na(parsed_timestamp)
      ),
      first_timestamp = if (
        all(is.na(parsed_timestamp))
      ) NA_character_ else as.character(
        min(parsed_timestamp, na.rm = TRUE)
      ),
      last_timestamp = if (
        all(is.na(parsed_timestamp))
      ) NA_character_ else as.character(
        max(parsed_timestamp, na.rm = TRUE)
      ),
      .groups = "drop"
    ),
  n = Inf
)


# mmmmmmmmmmmmmmmmmmmmm

library(dplyr)
library(readr)
library(lubridate)

prediction_file <- read_csv(
  "outputs/tables/08b_model_test_predictions.csv",
  show_col_types = FALSE
)

cat("\n1. JANUARY 1, 00:00 IN FEATURE DATA\n")

hourly_features |>
  filter(
    timestamp == as.POSIXct(
      "2025-01-01 00:00:00",
      tz = "Africa/Lagos"
    )
  ) |>
  select(
    location,
    timestamp,
    next_hour_apparent_temperature,
    next_hour_wet_bulb_temperature,
    next_hour_air_temperature
  ) |>
  print(n = Inf)

cat("\n2. JANUARY 1, 00:00 IN SAVED PREDICTIONS\n")

prediction_file |>
  filter(
    frequency == "hourly",
    observation_timestamp == "2025-01-01 00:00:00"
  ) |>
  select(
    task,
    location,
    observation_timestamp,
    actual,
    persistence_prediction,
    linear_prediction,
    random_forest_prediction
  ) |>
  print(n = Inf)

cat("\n3. HOURLY PREDICTION COUNTS BY TASK AND LOCATION\n")

prediction_file |>
  filter(frequency == "hourly") |>
  count(task, location) |>
  print(n = Inf)

# DIAGNOSTIC: INVESTIGATE DAILY OBSERVATION COUNTS

daily_count_diagnostic <- daily_environment |>
  filter(observations != 24) |>
  arrange(location, date)

cat("\nDAILY OBSERVATION COUNT DIAGNOSTIC\n")

cat(
  "Total location-dates:",
  nrow(daily_environment),
  "\n"
)

cat(
  "Location-dates with exactly 24 observations:",
  sum(daily_environment$observations == 24),
  "\n"
)

cat(
  "Location-dates with fewer than 24 observations:",
  sum(daily_environment$observations < 24),
  "\n"
)

cat(
  "Location-dates with more than 24 observations:",
  sum(daily_environment$observations > 24),
  "\n"
)

cat("\nAFFECTED DATES\n")

print(
  daily_count_diagnostic,
  n = 30
)

cat("\nCOUNTS BY LOCATION\n")

print(
  daily_environment |>
    group_by(location) |>
    summarise(
      minimum_daily_observations = min(observations),
      maximum_daily_observations = max(observations),
      affected_dates = sum(observations != 24),
      total_observations = sum(observations),
      .groups = "drop"
    ),
  n = Inf
)

cat("\nTIMESTAMP CLASS AND TIME ZONE\n")

print(class(weather_hourly$timestamp))

print(attr(weather_hourly$timestamp, "tzone"))

# CHECK HOURLY RECORDS AROUND AFFECTED DATES

affected_keys <- daily_count_diagnostic |>
  select(location, date)

affected_hourly_records <- weather_hourly |>
  semi_join(
    affected_keys,
    by = c("location", "date")
  ) |>
  arrange(location, timestamp)

print(
  affected_hourly_records |>
    select(
      location,
      timestamp,
      date
    ),
  n = 60
)