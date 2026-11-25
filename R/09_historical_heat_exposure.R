
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 09 - HISTORICAL HEAT EXPOSURE INTELLIGENCE
# ============================================================
#
# PURPOSE:
# 1. Examine historical high-temperature conditions.
# 2. Calculate location-specific P90 and P95 benchmarks.
# 3. Measure frequency and duration of high-temperature hours.
# 4. Identify consecutive high-temperature episodes.
# 5. Examine monthly, annual and hour-of-day patterns.
# 6. Compare historical patterns across study locations.
# 7. Produce evidence for environmental heat planning.
#
# DATA:
# Open-Meteo Historical Weather API - ERA5 reanalysis.
# Study period: 2021-2025.
# Locations: Abuja, Ilorin and Lagos.
#
# IMPORTANT:
# P90 and P95 are historical statistical benchmarks.
# They are NOT occupational heat-safety thresholds.
# Wet-bulb temperature is NOT WBGT.
# These results do not measure individual worker exposure
# or establish health outcomes.
#
# ============================================================


# 1. LOAD REQUIRED PACKAGES -----------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)


# 2. DEFINE PROJECT DIRECTORIES -------------------------------

hourly_file <- file.path(
  "data",
  "processed",
  "heat_exposure_hourly_features.csv"
)

table_directory <- file.path(
  "outputs",
  "tables"
)

figure_directory <- file.path(
  "outputs",
  "figures"
)

report_directory <- file.path(
  "outputs",
  "reports"
)

dir.create(
  table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  report_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


# 3. LOAD HISTORICAL HOURLY DATA -------------------------------

if (!file.exists(hourly_file)) {
  stop("Historical hourly feature dataset was not found.")
}

weather_hourly <- read_csv(
  hourly_file,
  show_col_types = FALSE
)

required_columns <- c(
  "timestamp",
  "location",
  "temperature_2m",
  "apparent_temperature",
  "wet_bulb_temperature_2m"
)

missing_columns <- setdiff(
  required_columns,
  names(weather_hourly)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Preserve the correct local clock time.

if (!inherits(weather_hourly$timestamp, "POSIXct")) {
  weather_hourly <- weather_hourly |>
    mutate(
      timestamp = ymd_hms(
        timestamp,
        tz = "Africa/Lagos"
      )
    )
} else {
  weather_hourly <- weather_hourly |>
    mutate(
      timestamp = with_tz(
        timestamp,
        tzone = "Africa/Lagos"
      )
    )
}

weather_hourly <- weather_hourly |>
  mutate(
    date = as.Date(
      timestamp,
      tz = "Africa/Lagos"
    ),
    year = year(timestamp),
    month_number = month(timestamp),
    month_name = factor(
      month(timestamp, label = TRUE, abbr = FALSE),
      levels = month.name,
      ordered = TRUE
    ),
    hour = hour(timestamp)
  ) |>
  arrange(location, timestamp)

cat("\nHISTORICAL DATA LOADED\n")

cat(
  "Observations:",
  nrow(weather_hourly),
  "\n"
)

cat(
  "Locations:",
  paste(
    sort(unique(weather_hourly$location)),
    collapse = ", "
  ),
  "\n"
)

cat(
  "First timestamp:",
  format(min(weather_hourly$timestamp)),
  "\n"
)

cat(
  "Last timestamp:",
  format(max(weather_hourly$timestamp)),
  "\n"
)


# 4. VALIDATE HISTORICAL DATA ----------------------------------

if (anyNA(weather_hourly$timestamp)) {
  stop("Missing timestamps detected.")
}

if (anyNA(weather_hourly$location)) {
  stop("Missing location values detected.")
}

if (anyDuplicated(
  weather_hourly[c("location", "timestamp")]
) > 0) {
  stop("Duplicate location-timestamp records detected.")
}

expected_locations <- c(
  "Abuja",
  "Ilorin",
  "Lagos"
)

if (!setequal(
  unique(weather_hourly$location),
  expected_locations
)) {
  stop("The dataset does not contain the three expected locations.")
}

if (any(!is.finite(weather_hourly$temperature_2m)) ||
    any(!is.finite(weather_hourly$apparent_temperature)) ||
    any(!is.finite(weather_hourly$wet_bulb_temperature_2m))) {
  stop("Non-finite values detected in core temperature variables.")
}

coverage_validation <- weather_hourly |>
  group_by(location) |>
  summarise(
    observations = n(),
    first_observation = min(timestamp),
    last_observation = max(timestamp),
    unique_dates = n_distinct(date),
    unique_years = n_distinct(year),
    .groups = "drop"
  )

print(coverage_validation)


# 5. DEFINE HISTORICAL HEAT INDICATORS -------------------------

heat_hourly <- weather_hourly |>
  select(
    timestamp,
    date,
    year,
    month_number,
    month_name,
    hour,
    location,
    temperature_2m,
    apparent_temperature,
    wet_bulb_temperature_2m
  ) |>
  pivot_longer(
    cols = c(
      temperature_2m,
      apparent_temperature,
      wet_bulb_temperature_2m
    ),
    names_to = "indicator",
    values_to = "temperature_c"
  ) |>
  mutate(
    indicator = recode(
      indicator,
      temperature_2m = "Air temperature",
      apparent_temperature = "Apparent temperature",
      wet_bulb_temperature_2m = "Wet-bulb temperature"
    )
  )

# These are environmental indicators, not measurements
# of individual worker exposure.


# 6. CALCULATE LOCATION-SPECIFIC HISTORICAL BENCHMARKS ------

# P90 and P95 are calculated using the complete 2021-2025
# historical period for descriptive historical analysis.
#
# These thresholds are not safety limits.

historical_thresholds <- heat_hourly |>
  group_by(
    location,
    indicator
  ) |>
  summarise(
    observations = n(),
    
    historical_mean = mean(temperature_c),
    
    historical_median = median(temperature_c),
    
    historical_P90 = quantile(
      temperature_c,
      0.90,
      names = FALSE
    ),
    
    historical_P95 = quantile(
      temperature_c,
      0.95,
      names = FALSE
    ),
    
    historical_maximum = max(temperature_c),
    
    .groups = "drop"
  )

print(
  historical_thresholds,
  n = Inf
)


# 7. CREATE HOURLY EXCEEDANCE DATASET --------------------------

hourly_threshold_long <- historical_thresholds |>
  select(
    location,
    indicator,
    historical_P90,
    historical_P95
  ) |>
  pivot_longer(
    cols = c(
      historical_P90,
      historical_P95
    ),
    names_to = "threshold_level",
    values_to = "threshold"
  ) |>
  mutate(
    threshold_level = recode(
      threshold_level,
      historical_P90 = "P90",
      historical_P95 = "P95"
    )
  )

hourly_exceedance <- heat_hourly |>
  inner_join(
    hourly_threshold_long,
    by = c(
      "location",
      "indicator"
    ),
    relationship = "many-to-many"
  ) |>
  mutate(
    exceedance = temperature_c >= threshold
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    timestamp
  )


# 8. SUMMARISE HOURLY EXCEEDANCE FREQUENCY ---------------------

hourly_exceedance_summary <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    threshold
  ) |>
  summarise(
    total_observations = n(),
    
    hours_above_threshold = sum(exceedance),
    
    percentage_of_hours = mean(exceedance) * 100,
    
    days_with_at_least_one_exceedance = n_distinct(
      date[exceedance]
    ),
    
    first_exceedance = ifelse(
      any(exceedance),
      as.character(min(timestamp[exceedance])),
      NA_character_
    ),
    
    last_exceedance = ifelse(
      any(exceedance),
      as.character(max(timestamp[exceedance])),
      NA_character_
    ),
    
    .groups = "drop"
  )

print(
  hourly_exceedance_summary,
  n = Inf
)


# 9. IDENTIFY CONSECUTIVE HIGH-TEMPERATURE EPISODES ----------

# An episode consists of consecutive hourly observations
# at or above the relevant historical threshold.
#
# A missing hour or a non-exceedance ends an episode.

hourly_episode_data <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level
  ) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    previous_exceedance = lag(
      exceedance,
      default = FALSE
    ),
    
    previous_timestamp = lag(timestamp),
    
    gap_hours = as.numeric(
      difftime(
        timestamp,
        previous_timestamp,
        units = "hours"
      )
    ),
    
    new_episode = exceedance &
      (
        !previous_exceedance |
          is.na(gap_hours) |
          gap_hours != 1
      ),
    
    episode_number = cumsum(new_episode)
  ) |>
  ungroup()

hourly_episodes <- hourly_episode_data |>
  filter(exceedance) |>
  group_by(
    location,
    indicator,
    threshold_level,
    episode_number
  ) |>
  summarise(
    episode_start = min(timestamp),
    
    episode_end = max(timestamp),
    
    duration_hours = n(),
    
    mean_temperature = mean(temperature_c),
    
    peak_temperature = max(temperature_c),
    
    .groups = "drop"
  )

hourly_episode_summary <- hourly_episodes |>
  group_by(
    location,
    indicator,
    threshold_level
  ) |>
  summarise(
    episode_count = n(),
    
    total_exceedance_hours = sum(duration_hours),
    
    mean_episode_duration_hours = mean(duration_hours),
    
    median_episode_duration_hours = median(duration_hours),
    
    longest_episode_hours = max(duration_hours),
    
    longest_episode_peak_temperature = peak_temperature[
      which.max(duration_hours)
    ][1],
    
    .groups = "drop"
  )

print(
  hourly_episode_summary,
  n = Inf
)


# 10. ANALYSE MONTHLY HIGH-TEMPERATURE PATTERNS -------------

monthly_exceedance <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    year,
    month_number,
    month_name
  ) |>
  summarise(
    observations = n(),
    
    exceedance_hours = sum(exceedance),
    
    percentage_of_hours = mean(exceedance) * 100,
    
    maximum_temperature = max(temperature_c),
    
    mean_temperature = mean(temperature_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    year,
    month_number
  )


# 11. ANALYSE ANNUAL HIGH-TEMPERATURE PATTERNS ---------------

annual_exceedance <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    year
  ) |>
  summarise(
    observations = n(),
    
    exceedance_hours = sum(exceedance),
    
    percentage_of_hours = mean(exceedance) * 100,
    
    days_with_exceedance = n_distinct(
      date[exceedance]
    ),
    
    mean_temperature = mean(temperature_c),
    
    maximum_temperature = max(temperature_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    year
  )


# 12. ANALYSE HOUR-OF-DAY PATTERNS ----------------------------

hourly_time_of_day <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    hour
  ) |>
  summarise(
    observations = n(),
    
    exceedance_hours = sum(exceedance),
    
    exceedance_percentage = mean(exceedance) * 100,
    
    mean_temperature = mean(temperature_c),
    
    maximum_temperature = max(temperature_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    hour
  )


# 13. CREATE DAILY ENVIRONMENTAL SUMMARY ----------------------

# Dates are assigned using Nigeria's local timezone.
# Each complete location-date should contain 24 hourly records.
#
# If this validation fails, the diagnostic output identifies
# the affected dates and helps distinguish missing records
# from timestamp/date-assignment problems.

daily_environment <- weather_hourly |>
  group_by(
    location,
    date
  ) |>
  summarise(
    mean_air_temperature = mean(temperature_2m),
    maximum_air_temperature = max(temperature_2m),
    
    mean_apparent_temperature = mean(apparent_temperature),
    maximum_apparent_temperature = max(apparent_temperature),
    
    mean_wet_bulb_temperature = mean(wet_bulb_temperature_2m),
    maximum_wet_bulb_temperature = max(wet_bulb_temperature_2m),
    
    observations = n(),
    
    first_timestamp = min(timestamp),
    last_timestamp = max(timestamp),
    
    .groups = "drop"
  ) |>
  mutate(
    year = year(date),
    month_number = month(date),
    month_name = factor(
      month(date, label = TRUE, abbr = FALSE),
      levels = month.name,
      ordered = TRUE
    )
  ) |>
  arrange(
    location,
    date
  )

# Identify location-dates that do not contain 24 observations.

daily_count_diagnostic <- daily_environment |>
  filter(observations != 24) |>
  arrange(location, date)

# Display daily coverage statistics.

cat("\nDAILY OBSERVATION COUNT VALIDATION\n")

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

# Expected coverage for the five-year study period.

expected_dates_per_location <- 1826

expected_observations_per_location <- 43824

cat("\nEXPECTED COVERAGE\n")

cat(
  "Expected dates per location:",
  expected_dates_per_location,
  "\n"
)

cat(
  "Expected hourly observations per location:",
  expected_observations_per_location,
  "\n"
)

# Validate location-level coverage.

daily_coverage_summary <- daily_environment |>
  group_by(location) |>
  summarise(
    daily_records = n(),
    
    minimum_daily_observations = min(observations),
    
    maximum_daily_observations = max(observations),
    
    affected_dates = sum(observations != 24),
    
    total_hourly_observations = sum(observations),
    
    .groups = "drop"
  )

cat("\nCOVERAGE BY LOCATION\n")

print(
  daily_coverage_summary,
  n = Inf
)

# Validate expected number of dates.

if (any(
  daily_coverage_summary$daily_records !=
  expected_dates_per_location
)) {
  cat("\nLOCATION-DATE COVERAGE ERROR\n")
  
  print(
    daily_coverage_summary,
    n = Inf
  )
  
  stop(
    "Unexpected number of daily records. Check the study period and timestamps."
  )
}

# Validate expected number of hourly observations.

if (any(
  daily_coverage_summary$total_hourly_observations !=
  expected_observations_per_location
)) {
  cat("\nHOURLY OBSERVATION COVERAGE ERROR\n")
  
  print(
    daily_coverage_summary,
    n = Inf
  )
  
  stop(
    "Unexpected hourly observation count. Check the source dataset."
  )
}

# If individual dates are incomplete, display the affected dates
# and their underlying hourly records before stopping.

if (nrow(daily_count_diagnostic) > 0) {
  
  cat("\nAFFECTED LOCATION-DATES\n")
  
  print(
    daily_count_diagnostic,
    n = Inf
  )
  
  affected_keys <- daily_count_diagnostic |>
    select(
      location,
      date
    )
  
  affected_hourly_records <- weather_hourly |>
    semi_join(
      affected_keys,
      by = c("location", "date")
    ) |>
    arrange(
      location,
      timestamp
    )
  
  cat("\nUNDERLYING HOURLY RECORDS FOR AFFECTED DATES\n")
  
  print(
    affected_hourly_records |>
      select(
        location,
        timestamp,
        date
      ),
    n = Inf
  )
  
  cat("\nTIMESTAMP INFORMATION\n")
  
  print(class(weather_hourly$timestamp))
  
  print(attr(weather_hourly$timestamp, "tzone"))
  
  stop(
    paste(
      "Daily observation validation failed.",
      "Inspect the diagnostic output above before proceeding."
    )
  )
}

# Validate the complete daily dataset.

if (nrow(daily_environment) != 5478) {
  stop(
    "Daily dataset should contain 5,478 location-date records."
  )
}

if (anyDuplicated(
  daily_environment[c("location", "date")]
) > 0) {
  stop(
    "Duplicate location-date records detected."
  )
}

cat("\nDAILY ENVIRONMENTAL SUMMARY VALIDATED SUCCESSFULLY\n")

cat(
  "Daily records:",
  nrow(daily_environment),
  "\n"
)

cat(
  "All location-dates contain exactly 24 hourly observations.\n"
)

# 14. BUILD DAILY MAXIMUM TEMPERATURE INDICATORS -------------

daily_heat <- daily_environment |>
  select(
    location,
    date,
    year,
    month_number,
    month_name,
    maximum_air_temperature,
    maximum_apparent_temperature,
    maximum_wet_bulb_temperature
  ) |>
  pivot_longer(
    cols = c(
      maximum_air_temperature,
      maximum_apparent_temperature,
      maximum_wet_bulb_temperature
    ),
    names_to = "indicator",
    values_to = "daily_maximum_c"
  ) |>
  mutate(
    indicator = recode(
      indicator,
      maximum_air_temperature = "Daily maximum air temperature",
      maximum_apparent_temperature = "Daily maximum apparent temperature",
      maximum_wet_bulb_temperature = "Daily maximum wet-bulb temperature"
    )
  )


# 15. CALCULATE DAILY HISTORICAL BENCHMARKS -------------------

daily_thresholds <- daily_heat |>
  group_by(
    location,
    indicator
  ) |>
  summarise(
    observations = n(),
    
    historical_mean = mean(daily_maximum_c),
    
    historical_P90 = quantile(
      daily_maximum_c,
      0.90,
      names = FALSE
    ),
    
    historical_P95 = quantile(
      daily_maximum_c,
      0.95,
      names = FALSE
    ),
    
    historical_maximum = max(daily_maximum_c),
    
    .groups = "drop"
  )

daily_threshold_long <- daily_thresholds |>
  select(
    location,
    indicator,
    historical_P90,
    historical_P95
  ) |>
  pivot_longer(
    cols = c(
      historical_P90,
      historical_P95
    ),
    names_to = "threshold_level",
    values_to = "threshold"
  ) |>
  mutate(
    threshold_level = recode(
      threshold_level,
      historical_P90 = "P90",
      historical_P95 = "P95"
    )
  )


# 16. DAILY HIGH-TEMPERATURE FREQUENCY ------------------------

daily_exceedance <- daily_heat |>
  inner_join(
    daily_threshold_long,
    by = c(
      "location",
      "indicator"
    ),
    relationship = "many-to-many"
  ) |>
  mutate(
    exceedance = daily_maximum_c >= threshold
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    date
  )

daily_exceedance_summary <- daily_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    threshold
  ) |>
  summarise(
    total_days = n(),
    
    days_above_threshold = sum(exceedance),
    
    percentage_of_days = mean(exceedance) * 100,
    
    .groups = "drop"
  )


# 17. IDENTIFY CONSECUTIVE HIGH-TEMPERATURE DAYS -------------

daily_episode_data <- daily_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level
  ) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    previous_exceedance = lag(
      exceedance,
      default = FALSE
    ),
    
    previous_date = lag(date),
    
    gap_days = as.numeric(
      date - previous_date
    ),
    
    new_episode = exceedance &
      (
        !previous_exceedance |
          is.na(gap_days) |
          gap_days != 1
      ),
    
    episode_number = cumsum(new_episode)
  ) |>
  ungroup()

daily_episodes <- daily_episode_data |>
  filter(exceedance) |>
  group_by(
    location,
    indicator,
    threshold_level,
    episode_number
  ) |>
  summarise(
    episode_start = min(date),
    
    episode_end = max(date),
    
    duration_days = n(),
    
    mean_daily_maximum = mean(daily_maximum_c),
    
    peak_daily_maximum = max(daily_maximum_c),
    
    .groups = "drop"
  )

daily_episode_summary <- daily_episodes |>
  group_by(
    location,
    indicator,
    threshold_level
  ) |>
  summarise(
    episode_count = n(),
    
    total_high_temperature_days = sum(duration_days),
    
    mean_episode_duration_days = mean(duration_days),
    
    longest_episode_days = max(duration_days),
    
    .groups = "drop"
  )


# 18. MONTHLY AND ANNUAL DAILY PATTERNS -----------------------

daily_monthly_summary <- daily_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    year,
    month_number,
    month_name
  ) |>
  summarise(
    days_observed = n(),
    
    high_temperature_days = sum(exceedance),
    
    percentage_of_days = mean(exceedance) * 100,
    
    mean_daily_maximum = mean(daily_maximum_c),
    
    maximum_daily_temperature = max(daily_maximum_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    year,
    month_number
  )

daily_annual_summary <- daily_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    year
  ) |>
  summarise(
    days_observed = n(),
    
    high_temperature_days = sum(exceedance),
    
    percentage_of_days = mean(exceedance) * 100,
    
    mean_daily_maximum = mean(daily_maximum_c),
    
    maximum_daily_temperature = max(daily_maximum_c),
    
    .groups = "drop"
  )


# 19. SUPPORTING ENVIRONMENTAL CONTEXT -------------------------

# These variables provide context for interpreting heat.
# They are not treated as direct worker-exposure measurements.

context_variables <- intersect(
  c(
    "relative_humidity_2m",
    "shortwave_radiation",
    "wind_speed_10m"
  ),
  names(weather_hourly)
)

if (length(context_variables) > 0) {
  
  environmental_context <- weather_hourly |>
    group_by(location) |>
    summarise(
      across(
        all_of(context_variables),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          P90 = ~ quantile(
            .x,
            0.90,
            na.rm = TRUE,
            names = FALSE
          )
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
} else {
  
  environmental_context <- tibble(
    location = character()
  )
  
  warning(
    "No optional humidity, radiation or wind variables were found."
  )
}


# 20. CREATE HISTORICAL TEMPERATURE COMPARISON TABLE ----------

historical_location_comparison <- heat_hourly |>
  group_by(
    location,
    indicator
  ) |>
  summarise(
    observations = n(),
    
    mean_temperature = mean(temperature_c),
    
    median_temperature = median(temperature_c),
    
    standard_deviation = sd(temperature_c),
    
    P90 = quantile(
      temperature_c,
      0.90,
      names = FALSE
    ),
    
    P95 = quantile(
      temperature_c,
      0.95,
      names = FALSE
    ),
    
    maximum_temperature = max(temperature_c),
    
    minimum_temperature = min(temperature_c),
    
    .groups = "drop"
  )


# 21. CREATE HISTORICAL MONTHLY TEMPERATURE DATA --------------

monthly_temperature_summary <- daily_environment |>
  group_by(
    location,
    year,
    month_number,
    month_name
  ) |>
  summarise(
    days_observed = n(),
    
    mean_daily_maximum_air_temperature =
      mean(maximum_air_temperature),
    
    mean_daily_maximum_apparent_temperature =
      mean(maximum_apparent_temperature),
    
    mean_daily_maximum_wet_bulb_temperature =
      mean(maximum_wet_bulb_temperature),
    
    maximum_air_temperature =
      max(maximum_air_temperature),
    
    maximum_apparent_temperature =
      max(maximum_apparent_temperature),
    
    maximum_wet_bulb_temperature =
      max(maximum_wet_bulb_temperature),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    year,
    month_number
  )


# 22. CREATE FIGURE: HISTORICAL TEMPERATURE DISTRIBUTIONS -----

temperature_distribution_plot <- ggplot(
  heat_hourly,
  aes(
    x = temperature_c,
    fill = location
  )
) +
  geom_density(
    alpha = 0.25
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free",
    ncol = 1
  ) +
  labs(
    title = "Historical Environmental Temperature Distributions",
    subtitle = "Hourly ERA5 estimates across the 2021-2025 study period",
    x = "Temperature (°C)",
    y = "Density",
    fill = "Location",
    caption = paste(
      "Historical distributions describe environmental conditions;",
      "they do not measure individual worker exposure."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

ggsave(
  file.path(
    figure_directory,
    "19a_historical_temperature_distributions.png"
  ),
  temperature_distribution_plot,
  width = 13,
  height = 11,
  dpi = 320,
  bg = "white"
)



# 23. CREATE FIGURE: MONTHLY EXCEEDANCE PATTERNS -------------

# Pool the five study years by calendar month.
# Percentages account for differences in month length.

monthly_climatology <- hourly_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    month_number,
    month_name
  ) |>
  summarise(
    observations = n(),
    
    exceedance_hours = sum(exceedance),
    
    percentage_of_hours = mean(exceedance) * 100,
    
    mean_temperature = mean(temperature_c),
    
    maximum_temperature = max(temperature_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    month_number
  )

monthly_exceedance_plot <- monthly_climatology |>
  filter(threshold_level == "P90") |>
  ggplot(
    aes(
      x = month_number,
      y = percentage_of_hours,
      colour = location,
      group = location
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Frequency of Historically High-Temperature Hours",
    subtitle = paste(
      "Percentage of hourly observations above location-specific",
      "historical P90 benchmarks, pooled across 2021-2025"
    ),
    x = "Month",
    y = "Hours above historical P90 (%)",
    colour = "Location"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "19b_monthly_high_temperature_frequency.png"
  ),
  monthly_exceedance_plot,
  width = 14,
  height = 12,
  dpi = 320,
  bg = "white"
)


# 24. CREATE FIGURE: HOUR-OF-DAY HEAT PATTERNS ---------------

time_of_day_plot <- hourly_time_of_day |>
  filter(threshold_level == "P90") |>
  ggplot(
    aes(
      x = hour,
      y = exceedance_percentage,
      colour = location,
      group = location
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.5
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 0:23,
    labels = function(x) sprintf("%02d:00", x)
  ) +
  labs(
    title = "Time-of-Day Patterns in High-Temperature Conditions",
    subtitle = "Percentage of hourly observations above each location's P90",
    x = "Local hour",
    y = "Hours above P90 (%)",
    colour = "Location"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    figure_directory,
    "19c_hour_of_day_high_temperature_patterns.png"
  ),
  time_of_day_plot,
  width = 14,
  height = 12,
  dpi = 320,
  bg = "white"
)


# 25. CREATE FIGURE: DAILY MAXIMUM TEMPERATURE TRENDS --------

daily_trend_plot <- daily_environment |>
  ggplot(
    aes(
      x = date,
      y = maximum_apparent_temperature,
      colour = location
    )
  ) +
  geom_line(
    alpha = 0.55,
    linewidth = 0.35
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linewidth = 0.9
  ) +
  facet_wrap(
    ~ location,
    ncol = 1
  ) +
  labs(
    title = "Historical Daily Maximum Apparent Temperature",
    subtitle = "Daily values and smoothed descriptive trends, 2021-2025",
    x = "Date",
    y = "Daily maximum apparent temperature (°C)",
    colour = "Location",
    caption = paste(
      "Smoothed lines are descriptive, not evidence of causation",
      "or a formal climate-trend attribution."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none"
  )

ggsave(
  file.path(
    figure_directory,
    "19d_daily_maximum_apparent_temperature_trends.png"
  ),
  daily_trend_plot,
  width = 14,
  height = 12,
  dpi = 320,
  bg = "white"
)


# 26. CREATE FIGURE: EPISODE DURATION --------------------------

episode_duration_plot <- hourly_episodes |>
  filter(threshold_level == "P90") |>
  ggplot(
    aes(
      x = location,
      y = duration_hours,
      fill = location
    )
  ) +
  geom_boxplot(
    outlier.alpha = 0.25
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Duration of Consecutive High-Temperature Episodes",
    subtitle = "Episodes defined by consecutive hours above location-specific P90",
    x = "Location",
    y = "Episode duration (hours)",
    fill = "Location"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none"
  )

ggsave(
  file.path(
    figure_directory,
    "19e_high_temperature_episode_durations.png"
  ),
  episode_duration_plot,
  width = 13,
  height = 12,
  dpi = 320,
  bg = "white"
)



# 27. CREATE FIGURE: DAILY HIGH-TEMPERATURE DAYS -------------

# Pool the five study years by calendar month.
# Percentages account for differences in month length.

daily_monthly_climatology <- daily_exceedance |>
  group_by(
    location,
    indicator,
    threshold_level,
    month_number,
    month_name
  ) |>
  summarise(
    days_observed = n(),
    
    high_temperature_days = sum(exceedance),
    
    percentage_of_days = mean(exceedance) * 100,
    
    mean_daily_maximum = mean(daily_maximum_c),
    
    maximum_daily_temperature = max(daily_maximum_c),
    
    .groups = "drop"
  ) |>
  arrange(
    location,
    indicator,
    threshold_level,
    month_number
  )

daily_exceedance_plot <- daily_monthly_climatology |>
  filter(threshold_level == "P90") |>
  ggplot(
    aes(
      x = month_number,
      y = percentage_of_days,
      colour = location,
      group = location
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_wrap(
    ~ indicator,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Frequency of High Daily Maximum Temperatures",
    subtitle = paste(
      "Percentage of days exceeding location-specific",
      "historical P90 benchmarks, pooled across 2021-2025"
    ),
    x = "Month",
    y = "High-temperature days (%)",
    colour = "Location"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "19f_monthly_high_temperature_days.png"
  ),
  daily_exceedance_plot,
  width = 14,
  height = 12,
  dpi = 320,
  bg = "white"
)


# 28. SAVE HISTORICAL ANALYSIS TABLES -------------------------

write_csv(
  coverage_validation,
  file.path(
    table_directory,
    "10a_historical_data_coverage.csv"
  )
)

write_csv(
  historical_thresholds,
  file.path(
    table_directory,
    "10b_hourly_historical_temperature_thresholds.csv"
  )
)

write_csv(
  hourly_exceedance_summary,
  file.path(
    table_directory,
    "10c_hourly_high_temperature_frequency.csv"
  )
)

write_csv(
  hourly_episodes,
  file.path(
    table_directory,
    "10d_consecutive_high_temperature_hourly_episodes.csv"
  )
)

write_csv(
  hourly_episode_summary,
  file.path(
    table_directory,
    "10e_hourly_heat_episode_summary.csv"
  )
)

write_csv(
  monthly_exceedance,
  file.path(
    table_directory,
    "10f_monthly_high_temperature_frequency.csv"
  )
)

write_csv(
  annual_exceedance,
  file.path(
    table_directory,
    "10g_annual_high_temperature_frequency.csv"
  )
)

write_csv(
  hourly_time_of_day,
  file.path(
    table_directory,
    "10h_hour_of_day_high_temperature_patterns.csv"
  )
)

write_csv(
  daily_environment,
  file.path(
    table_directory,
    "11a_daily_environmental_summary.csv"
  )
)

write_csv(
  daily_thresholds,
  file.path(
    table_directory,
    "11b_daily_historical_temperature_thresholds.csv"
  )
)

write_csv(
  daily_exceedance_summary,
  file.path(
    table_directory,
    "11c_daily_high_temperature_frequency.csv"
  )
)

write_csv(
  daily_episodes,
  file.path(
    table_directory,
    "11d_consecutive_high_temperature_daily_episodes.csv"
  )
)

write_csv(
  daily_episode_summary,
  file.path(
    table_directory,
    "11e_daily_heat_episode_summary.csv"
  )
)

write_csv(
  daily_monthly_summary,
  file.path(
    table_directory,
    "11f_monthly_high_temperature_days.csv"
  )
)

write_csv(
  daily_annual_summary,
  file.path(
    table_directory,
    "11g_annual_high_temperature_days.csv"
  )
)

write_csv(
  environmental_context,
  file.path(
    table_directory,
    "11h_supporting_environmental_context.csv"
  )
)

write_csv(
  historical_location_comparison,
  file.path(
    table_directory,
    "11i_historical_location_temperature_comparison.csv"
  )
)

write_csv(
  monthly_temperature_summary,
  file.path(
    table_directory,
    "11j_monthly_temperature_summary.csv"
  )
)


# 29. SAVE HISTORICAL ANALYSIS METHODS NOTE -------------------

methods_note <- c(
  
  "HEAT EXPOSURE INTELLIGENCE",
  
  "SCRIPT 09: HISTORICAL HEAT EXPOSURE INTELLIGENCE",
  
  "",
  
  "PURPOSE",
  
  "Examine historical environmental temperature conditions",
  "across Abuja, Ilorin and Lagos from 2021 to 2025.",
  
  "",
  
  "DATA SOURCE",
  
  "Open-Meteo Historical Weather API - ERA5 reanalysis.",
  
  "",
  
  "ANALYSIS INDICATORS",
  
  "Air temperature.",
  "Apparent temperature.",
  "Wet-bulb temperature.",
  "Daily maximum values of these indicators.",
  
  "",
  
  "HISTORICAL BENCHMARKS",
  
  "Location-specific 90th and 95th percentiles were calculated",
  "from the complete 2021-2025 historical record.",
  "These percentiles identify unusually high conditions relative",
  "to each location's historical distribution.",
  "They are not occupational exposure or safety limits.",
  
  "",
  
  "HOURLY EPISODES",
  
  "An hourly episode consists of consecutive hourly observations",
  "at or above the relevant location-specific threshold.",
  "A non-exceedance or missing hourly observation ends an episode.",
  
  "",
  
  "DAILY EPISODES",
  
  "A daily episode consists of consecutive days on which the",
  "daily maximum indicator equals or exceeds its threshold.",
  
  "",
  
  "TEMPORAL ANALYSIS",
  
  "Monthly, annual and hour-of-day patterns were examined.",
  "Daily summaries were derived from the hourly observations.",
  
  "",
  
  "SUPPORTING ENVIRONMENTAL CONTEXT",
  
  "Humidity, solar radiation and wind speed are retained as",
  "contextual environmental variables where available.",
  
  "",
  
  "IMPORTANT LIMITATIONS",
  
  "ERA5 is gridded reanalysis, not direct workplace measurement.",
  "The three locations do not represent all of Nigeria.",
  "Apparent temperature and wet-bulb temperature are distinct",
  "environmental indicators.",
  "Wet-bulb temperature is not WBGT.",
  "Historical percentiles do not establish workplace safety.",
  "No individual worker exposure, workload, clothing, shade,",
  "hydration, illness or productivity data were analysed.",
  "The analysis does not establish causal relationships.",
  "Smoothed trends are descriptive and are not formal climate",
  "trend attribution.",
  
  "",
  
  "POTENTIAL OPERATIONAL RELEVANCE",
  
  "The results can inform further assessment of work scheduling,",
  "hydration planning, rest periods and shade provision.",
  "Site-specific occupational measurements and relevant safety",
  "guidance are required before making workplace safety decisions.",
  
  "",
  
  "OUTPUT TABLES ARE STORED IN OUTPUTS/TABLES.",
  "FIGURES ARE STORED IN OUTPUTS/FIGURES."
)

writeLines(
  methods_note,
  file.path(
    report_directory,
    "05a_historical_heat_exposure_methods.txt"
  )
)


# 30. FINAL HISTORICAL ANALYSIS REPORT ------------------------

cat("\n")

cat("============================================================\n")
cat("HISTORICAL HEAT EXPOSURE ANALYSIS COMPLETED\n")
cat("============================================================\n")

cat("\nDATA COVERAGE\n")

print(
  coverage_validation,
  n = Inf
)

cat("\nHOURLY HISTORICAL THRESHOLDS\n")

print(
  historical_thresholds,
  n = Inf
)

cat("\nHOURLY HIGH-TEMPERATURE FREQUENCY\n")

print(
  hourly_exceedance_summary,
  n = Inf
)

cat("\nHOURLY EPISODE SUMMARY\n")

print(
  hourly_episode_summary,
  n = Inf
)

cat("\nDAILY HIGH-TEMPERATURE FREQUENCY\n")

print(
  daily_exceedance_summary,
  n = Inf
)

cat("\nDAILY EPISODE SUMMARY\n")

print(
  daily_episode_summary,
  n = Inf
)

cat("\nOUTPUT TABLES\n")

cat("10a_historical_data_coverage.csv\n")
cat("10b_hourly_historical_temperature_thresholds.csv\n")
cat("10c_hourly_high_temperature_frequency.csv\n")
cat("10d_consecutive_high_temperature_hourly_episodes.csv\n")
cat("10e_hourly_heat_episode_summary.csv\n")
cat("10f_monthly_high_temperature_frequency.csv\n")
cat("10g_annual_high_temperature_frequency.csv\n")
cat("10h_hour_of_day_high_temperature_patterns.csv\n")
cat("11a_daily_environmental_summary.csv\n")
cat("11b_daily_historical_temperature_thresholds.csv\n")
cat("11c_daily_high_temperature_frequency.csv\n")
cat("11d_consecutive_high_temperature_daily_episodes.csv\n")
cat("11e_daily_heat_episode_summary.csv\n")
cat("11f_monthly_high_temperature_days.csv\n")
cat("11g_annual_high_temperature_days.csv\n")
cat("11h_supporting_environmental_context.csv\n")
cat("11i_historical_location_temperature_comparison.csv\n")
cat("11j_monthly_temperature_summary.csv\n")

cat("\nOUTPUT FIGURES\n")

cat("19a_historical_temperature_distributions.png\n")
cat("19b_monthly_high_temperature_frequency.png\n")
cat("19c_hour_of_day_high_temperature_patterns.png\n")
cat("19d_daily_maximum_apparent_temperature_trends.png\n")
cat("19e_high_temperature_episode_durations.png\n")
cat("19f_monthly_high_temperature_days.png\n")

cat("\nOUTPUT REPORT\n")

cat("05a_historical_heat_exposure_methods.txt\n")

cat("\n============================================================\n")
cat("SCRIPT 09 COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")

gc()
