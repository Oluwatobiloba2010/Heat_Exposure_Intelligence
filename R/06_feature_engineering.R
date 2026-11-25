
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 06 - FEATURE ENGINEERING
# ============================================================
#
# PURPOSE:
# 1. Create time-based features.
# 2. Encode cyclical time and wind-direction variables.
# 3. Generate historical lag features.
# 4. Generate rolling historical averages.
# 5. Create daily analytical and predictive features.
# 6. Create next-hour and next-day prediction targets.
#
# IMPORTANT:
# Historical features must use past observations only.
# Future target variables are kept separate from predictors.
# Missing lag values are not automatically imputed.
#
# DATA SOURCE:
# Open-Meteo Historical Weather API - ERA5
# ============================================================


# 1. PACKAGES -------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "lubridate"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste(
      "Install the following packages first:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)


# 2. PROJECT PATHS --------------------------------------------

raw_file <- file.path(
  "data",
  "raw",
  "Nigeria_Weather_2021_2025.csv"
)

processed_directory <- file.path(
  "data",
  "processed"
)

table_directory <- file.path(
  "outputs",
  "tables"
)

report_directory <- file.path(
  "outputs",
  "reports"
)

dir.create(
  processed_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  report_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(raw_file)) {
  stop(
    paste("Source dataset not found:", raw_file)
  )
}


# 3. LOAD AND PREPARE DATA ------------------------------------

weather_all <- read_csv(
  raw_file,
  show_col_types = FALSE
)

required_columns <- c(
  "timestamp",
  "location",
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

missing_columns <- setdiff(
  required_columns,
  names(weather_all)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

if (inherits(weather_all$timestamp, "POSIXct")) {
  
  weather_all <- weather_all |>
    mutate(
      timestamp = lubridate::with_tz(
        timestamp,
        tzone = "Africa/Lagos"
      )
    )
  
} else {
  
  weather_all <- weather_all |>
    mutate(
      timestamp = lubridate::parse_date_time(
        as.character(timestamp),
        orders = c(
          "ymd HMS z",
          "ymd HM z",
          "ymd HMS",
          "ymd HM",
          "ymd"
        ),
        tz = "Africa/Lagos",
        quiet = TRUE
      )
    )
}

if (any(is.na(weather_all$timestamp))) {
  stop("Some timestamps could not be parsed.")
}

weather_all <- weather_all |>
  mutate(
    date = as.Date(timestamp, tz = "Africa/Lagos")
  ) |>
  arrange(location, timestamp)

if (anyDuplicated(
  weather_all[c("location", "timestamp")]
) > 0) {
  stop(
    "Duplicate location-timestamp records detected."
  )
}

cat("\nSOURCE DATA LOADED\n")
cat("Rows:", nrow(weather_all), "\n")
cat("Locations:", n_distinct(weather_all$location), "\n")


# 4. DEFINE FEATURE ENGINEERING FUNCTIONS --------------------

# Calculate the mean of the preceding observations.
# The current observation is excluded.
#
# Example:
# A prior 3-hour mean at 15:00 uses 12:00, 13:00 and 14:00.
#
# This prevents the rolling feature from including the
# observation it is intended to help predict.

trailing_mean_prior <- function(x, window) {
  
  result <- vapply(
    seq_along(x),
    function(i) {
      
      if (i <= window) {
        return(NA_real_)
      }
      
      previous_values <- x[
        (i - window):(i - 1)
      ]
      
      if (all(is.na(previous_values))) {
        return(NA_real_)
      }
      
      mean(
        previous_values,
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  
  result
}


# 5. HOURLY TEMPORAL FEATURES -------------------------------

hourly_features <- weather_all |>
  group_by(location) |>
  mutate(
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp),
    day = lubridate::day(timestamp),
    hour = lubridate::hour(timestamp),
    
    day_of_year = lubridate::yday(timestamp),
    
    day_of_week = lubridate::wday(
      timestamp,
      label = TRUE,
      abbr = FALSE,
      week_start = 1
    ),
    
    quarter = lubridate::quarter(timestamp),
    
    is_weekend = lubridate::wday(
      timestamp,
      week_start = 1
    ) >= 6,
    
    # Cyclical hour representation.
    hour_sin = sin(2 * pi * hour / 24),
    hour_cos = cos(2 * pi * hour / 24),
    
    # Cyclical annual representation.
    day_of_year_sin = sin(
      2 * pi * day_of_year / 365.25
    ),
    
    day_of_year_cos = cos(
      2 * pi * day_of_year / 365.25
    ),
    
    # Positive radiation indicates an hour with
    # positive incoming shortwave radiation.
    positive_solar_radiation_period =
      shortwave_radiation > 0
  ) |>
  ungroup()


# 6. WIND DIRECTION FEATURES -------------------------------

# Wind direction is circular:
# 0 degrees and 360 degrees represent the same direction.
#
# Sine and cosine components allow models to represent
# directional information without treating 359 and 1
# degrees as very different values.

if ("wind_direction_10m" %in% names(hourly_features)) {
  
  hourly_features <- hourly_features |>
    mutate(
      wind_direction_radians =
        wind_direction_10m * pi / 180,
      
      wind_direction_sin = sin(
        wind_direction_radians
      ),
      
      wind_direction_cos = cos(
        wind_direction_radians
      ),
      
      # Meteorological direction indicates where wind
      # comes FROM. These components represent the
      # corresponding direction of wind movement.
      wind_u_component =
        -wind_speed_10m *
        sin(wind_direction_radians),
      
      wind_v_component =
        -wind_speed_10m *
        cos(wind_direction_radians)
    )
}


# 7. HOURLY LAG FEATURES ------------------------------------

# A lag is a previous observation.
#
# These features allow a model to learn how current
# conditions relate to recent environmental history.

hourly_lag_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

hourly_features <- hourly_features |>
  group_by(location) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    across(
      all_of(hourly_lag_variables),
      
      list(
        lag_1h = ~ lag(.x, 1),
        lag_3h = ~ lag(.x, 3),
        lag_6h = ~ lag(.x, 6),
        lag_24h = ~ lag(.x, 24)
      ),
      
      .names = "{.col}_{.fn}"
    )
  ) |>
  ungroup()


# 8. HOURLY ROLLING FEATURES -------------------------------

# Rolling averages use previous observations only.
# They describe recent environmental conditions.

hourly_rolling_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

hourly_features <- hourly_features |>
  group_by(location) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    across(
      all_of(hourly_rolling_variables),
      
      ~ trailing_mean_prior(.x, 3),
      
      .names = "{.col}_prior_3h_mean"
    ),
    
    across(
      all_of(hourly_rolling_variables),
      
      ~ trailing_mean_prior(.x, 6),
      
      .names = "{.col}_prior_6h_mean"
    ),
    
    across(
      all_of(hourly_rolling_variables),
      
      ~ trailing_mean_prior(.x, 24),
      
      .names = "{.col}_prior_24h_mean"
    )
  ) |>
  ungroup()


# 9. NEXT-HOUR PREDICTION TARGETS ----------------------------

# These are future outcomes, not predictor variables.
#
# A model can use the current observation and historical
# features to estimate the next hour's conditions.

hourly_features <- hourly_features |>
  group_by(location) |>
  arrange(timestamp, .by_group = TRUE) |>
  mutate(
    next_hour_apparent_temperature =
      lead(apparent_temperature, 1),
    
    next_hour_wet_bulb_temperature =
      lead(wet_bulb_temperature_2m, 1),
    
    next_hour_air_temperature =
      lead(temperature_2m, 1)
  ) |>
  ungroup()


# 10. SAVE HOURLY FEATURE DATASET -----------------------------

hourly_output_file <- file.path(
  processed_directory,
  "heat_exposure_hourly_features.csv"
)

write_csv(
  hourly_features,
  hourly_output_file
)

cat("\nHOURLY FEATURE DATASET CREATED\n")
cat("Rows:", nrow(hourly_features), "\n")
cat("Columns:", ncol(hourly_features), "\n")


# 11. CREATE DAILY ANALYTICAL FEATURES -----------------------

daily_features <- weather_all |>
  group_by(location, date) |>
  summarise(
    
    year = first(lubridate::year(timestamp)),
    
    month = first(lubridate::month(timestamp)),
    
    day_of_year = first(lubridate::yday(timestamp)),
    
    day_of_week = first(
      lubridate::wday(
        timestamp,
        label = TRUE,
        abbr = FALSE,
        week_start = 1
      )
    ),
    
    mean_temperature = mean(
      temperature_2m,
      na.rm = TRUE
    ),
    
    maximum_temperature = max(
      temperature_2m,
      na.rm = TRUE
    ),
    
    temperature_standard_deviation = sd(
      temperature_2m,
      na.rm = TRUE
    ),
    
    temperature_90th_percentile = quantile(
      temperature_2m,
      0.90,
      na.rm = TRUE
    ),
    
    mean_relative_humidity = mean(
      relative_humidity_2m,
      na.rm = TRUE
    ),
    
    maximum_relative_humidity = max(
      relative_humidity_2m,
      na.rm = TRUE
    ),
    
    mean_wind_speed = mean(
      wind_speed_10m,
      na.rm = TRUE
    ),
    
    maximum_wind_speed = max(
      wind_speed_10m,
      na.rm = TRUE
    ),
    
    mean_solar_radiation = mean(
      shortwave_radiation,
      na.rm = TRUE
    ),
    
    daily_solar_energy_mj = sum(
      shortwave_radiation,
      na.rm = TRUE
    ) * 3600 / 1e6,
    
    hours_with_positive_solar_radiation = sum(
      shortwave_radiation > 0,
      na.rm = TRUE
    ),
    
    mean_wet_bulb_temperature = mean(
      wet_bulb_temperature_2m,
      na.rm = TRUE
    ),
    
    maximum_wet_bulb_temperature = max(
      wet_bulb_temperature_2m,
      na.rm = TRUE
    ),
    
    wet_bulb_temperature_90th_percentile = quantile(
      wet_bulb_temperature_2m,
      0.90,
      na.rm = TRUE
    ),
    
    mean_apparent_temperature = mean(
      apparent_temperature,
      na.rm = TRUE
    ),
    
    maximum_apparent_temperature = max(
      apparent_temperature,
      na.rm = TRUE
    ),
    
    apparent_temperature_90th_percentile = quantile(
      apparent_temperature,
      0.90,
      na.rm = TRUE
    ),
    
    hourly_observations = n(),
    
    .groups = "drop"
  ) |>
  arrange(location, date)


# 12. DAILY TEMPORAL FEATURES -------------------------------

daily_features <- daily_features |>
  group_by(location) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    quarter = lubridate::quarter(date),
    
    is_weekend = lubridate::wday(
      date,
      week_start = 1
    ) >= 6,
    
    day_of_year_sin = sin(
      2 * pi * day_of_year / 365.25
    ),
    
    day_of_year_cos = cos(
      2 * pi * day_of_year / 365.25
    )
  ) |>
  ungroup()


# 13. DAILY LAG FEATURES ------------------------------------

daily_lag_variables <- c(
  "mean_temperature",
  "maximum_temperature",
  "mean_relative_humidity",
  "mean_wet_bulb_temperature",
  "maximum_wet_bulb_temperature",
  "mean_apparent_temperature",
  "maximum_apparent_temperature",
  "daily_solar_energy_mj"
)

daily_features <- daily_features |>
  group_by(location) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    across(
      all_of(daily_lag_variables),
      
      list(
        lag_1d = ~ lag(.x, 1),
        lag_3d = ~ lag(.x, 3),
        lag_7d = ~ lag(.x, 7)
      ),
      
      .names = "{.col}_{.fn}"
    )
  ) |>
  ungroup()


# 14. DAILY ROLLING FEATURES --------------------------------

daily_rolling_variables <- c(
  "mean_temperature",
  "maximum_temperature",
  "mean_relative_humidity",
  "mean_wet_bulb_temperature",
  "mean_apparent_temperature",
  "daily_solar_energy_mj"
)

daily_features <- daily_features |>
  group_by(location) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    across(
      all_of(daily_rolling_variables),
      
      ~ trailing_mean_prior(.x, 3),
      
      .names = "{.col}_prior_3d_mean"
    ),
    
    across(
      all_of(daily_rolling_variables),
      
      ~ trailing_mean_prior(.x, 7),
      
      .names = "{.col}_prior_7d_mean"
    )
  ) |>
  ungroup()


# 15. NEXT-DAY PREDICTION TARGETS ----------------------------

daily_features <- daily_features |>
  group_by(location) |>
  arrange(date, .by_group = TRUE) |>
  mutate(
    next_day_maximum_temperature =
      lead(maximum_temperature, 1),
    
    next_day_maximum_wet_bulb_temperature =
      lead(maximum_wet_bulb_temperature, 1),
    
    next_day_maximum_apparent_temperature =
      lead(maximum_apparent_temperature, 1)
  ) |>
  ungroup()


# 16. SAVE DAILY FEATURE DATASET -----------------------------

daily_output_file <- file.path(
  processed_directory,
  "heat_exposure_daily_features.csv"
)

write_csv(
  daily_features,
  daily_output_file
)

cat("\nDAILY FEATURE DATASET CREATED\n")
cat("Rows:", nrow(daily_features), "\n")
cat("Columns:", ncol(daily_features), "\n")


# 17. FEATURE DICTIONARY -------------------------------------

feature_dictionary <- tibble(
  feature_group = c(
    rep("Temporal", 8),
    rep("Wind direction", 4),
    rep("Hourly historical", 4),
    rep("Hourly rolling", 3),
    rep("Prediction targets", 3),
    rep("Daily derived", 6),
    rep("Daily historical", 3),
    rep("Daily rolling", 2),
    rep("Daily prediction targets", 3)
  ),
  
  feature_name = c(
    "hour",
    "month",
    "quarter",
    "day_of_year",
    "day_of_week",
    "is_weekend",
    "hour_sin / hour_cos",
    "day_of_year_sin / day_of_year_cos",
    
    "wind_direction_sin",
    "wind_direction_cos",
    "wind_u_component",
    "wind_v_component",
    
    "lag_1h",
    "lag_3h",
    "lag_6h",
    "lag_24h",
    
    "prior_3h_mean",
    "prior_6h_mean",
    "prior_24h_mean",
    
    "next_hour_apparent_temperature",
    "next_hour_wet_bulb_temperature",
    "next_hour_air_temperature",
    
    "daily maximum temperature",
    "daily temperature variability",
    "daily temperature percentile",
    "daily solar energy",
    "daily wet-bulb percentile",
    "daily apparent-temperature percentile",
    
    "lag_1d",
    "lag_3d",
    "lag_7d",
    
    "prior_3d_mean",
    "prior_7d_mean",
    
    "next_day_maximum_temperature",
    "next_day_maximum_wet_bulb_temperature",
    "next_day_maximum_apparent_temperature"
  ),
  
  description = c(
    "Local clock hour of observation.",
    "Calendar month.",
    "Calendar quarter.",
    "Day number within the year.",
    "Day of the week.",
    "Indicates Saturday or Sunday.",
    "Circular encoding of the hour of day.",
    "Circular encoding of annual position.",
    
    "Sine transformation of wind direction.",
    "Cosine transformation of wind direction.",
    "East-west wind movement component.",
    "North-south wind movement component.",
    
    "Value recorded one hour earlier.",
    "Value recorded three hours earlier.",
    "Value recorded six hours earlier.",
    "Value recorded 24 hours earlier.",
    
    "Mean of the preceding three hourly observations.",
    "Mean of the preceding six hourly observations.",
    "Mean of the preceding 24 hourly observations.",
    
    "Apparent temperature in the following hour.",
    "Wet-bulb temperature in the following hour.",
    "Air temperature in the following hour.",
    
    "Highest hourly air temperature of the day.",
    "Standard deviation of hourly air temperature.",
    "90th percentile of hourly air temperature.",
    "Accumulated daily incoming solar energy.",
    "90th percentile of hourly wet-bulb temperature.",
    "90th percentile of hourly apparent temperature.",
    
    "Daily value one day earlier.",
    "Daily value three days earlier.",
    "Daily value seven days earlier.",
    
    "Mean of the preceding three daily values.",
    "Mean of the preceding seven daily values.",
    
    "Maximum air temperature on the following day.",
    "Maximum wet-bulb temperature on the following day.",
    "Maximum apparent temperature on the following day."
  ),
  
  role = c(
    rep("Predictor", 8),
    rep("Predictor", 4),
    rep("Predictor", 4),
    rep("Predictor", 3),
    rep("Target", 3),
    rep("Daily analytical feature", 6),
    rep("Predictor", 3),
    rep("Predictor", 2),
    rep("Target", 3)
  )
)

write_csv(
  feature_dictionary,
  file.path(
    table_directory,
    "07a_feature_dictionary.csv"
  )
)


# 18. FEATURE QUALITY CHECKS -------------------------------

create_missing_report <- function(data, dataset_name) {
  
  tibble(
    dataset = dataset_name,
    feature = names(data),
    missing_count = vapply(
      data,
      function(x) sum(is.na(x)),
      integer(1)
    )
  ) |>
    mutate(
      total_rows = nrow(data),
      missing_percentage =
        100 * missing_count / total_rows
    )
}

hourly_missing_report <- create_missing_report(
  hourly_features,
  "Hourly"
)

daily_missing_report <- create_missing_report(
  daily_features,
  "Daily"
)

feature_quality_report <- bind_rows(
  hourly_missing_report,
  daily_missing_report
)

write_csv(
  feature_quality_report,
  file.path(
    table_directory,
    "07b_feature_engineering_quality_checks.csv"
  )
)


# 19. FEATURE ENGINEERING METHODS NOTE ----------------------

methods_note <- c(
  "HEAT EXPOSURE INTELLIGENCE",
  "SCRIPT 06: FEATURE ENGINEERING",
  "",
  "SOURCE",
  "Open-Meteo Historical Weather API - ERA5.",
  "",
  "TEMPORAL FEATURES",
  "Calendar and cyclical features represent the timing",
  "of observations, including hour, month and annual position.",
  "",
  "WIND FEATURES",
  "Wind direction is encoded using sine and cosine",
  "transformations because direction is circular.",
  "Wind components represent east-west and north-south",
  "movement based on meteorological wind direction.",
  "",
  "HOURLY HISTORICAL FEATURES",
  "Lag variables represent earlier observations.",
  "Rolling means use only observations preceding the",
  "current observation.",
  "",
  "DAILY FEATURES",
  "Daily summaries include means, maxima, variability,",
  "percentiles and accumulated incoming solar energy.",
  "",
  "PREDICTION TARGETS",
  "Next-hour and next-day environmental conditions are",
  "stored as target variables, not historical predictors.",
  "",
  "MISSING VALUES",
  "Initial lag and rolling features are naturally missing",
  "because earlier observations are unavailable.",
  "Final future-target observations are also missing.",
  "These values are not automatically imputed.",
  "",
  "MODELLING SAFEGUARD",
  "Training and testing must be separated chronologically.",
  "Future target columns must never be included as predictors.",
  "Historical features must be constructed without using",
  "future observations.",
  "",
  "SCIENTIFIC LIMITATIONS",
  "Derived environmental features do not represent",
  "direct worker exposure or measured health outcomes.",
  "Wet-bulb temperature is not WBGT."
)

writeLines(
  methods_note,
  file.path(
    report_directory,
    "02a_feature_engineering_methods.txt"
  )
)


# 20. COMPLETION REPORT --------------------------------------

cat("\n")
cat("============================================================\n")
cat("FEATURE ENGINEERING COMPLETED\n")
cat("============================================================\n")

cat("\nHOURLY DATASET:\n")
cat(hourly_output_file, "\n")

cat("\nDAILY DATASET:\n")
cat(daily_output_file, "\n")

cat("\nOUTPUT TABLES:\n")
cat("07a_feature_dictionary.csv\n")
cat("07b_feature_engineering_quality_checks.csv\n")

cat("\nOUTPUT REPORT:\n")
cat("02a_feature_engineering_methods.txt\n")

cat("\nHourly rows:", nrow(hourly_features), "\n")
cat("Hourly columns:", ncol(hourly_features), "\n")
cat("Daily rows:", nrow(daily_features), "\n")
cat("Daily columns:", ncol(daily_features), "\n")

cat("\nAll feature engineering operations completed.\n")
cat("============================================================\n")
