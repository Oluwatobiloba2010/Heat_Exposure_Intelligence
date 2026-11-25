
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 05 - STATISTICAL ANALYSIS
# ============================================================
#
# PURPOSE:
# 1. Produce descriptive statistical summaries.
# 2. Compare daily heat-related conditions across locations.
# 3. Examine associations between important environmental variables.
# 4. Examine annual changes descriptively.
# 5. Use block-bootstrap confidence intervals to account
#    approximately for temporal dependence in daily observations.
#
# DATA:
# Open-Meteo Historical Weather API - ERA5
# Period: 2021-01-01 to 2025-12-31
#
# IMPORTANT:
# These analyses describe environmental conditions.
# They do not establish actual worker exposure, illness,
# workplace safety, or causal relationships.
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
      "Install the following required packages first:",
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

table_directory <- file.path(
  "outputs",
  "tables"
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
  report_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(raw_file)) {
  stop(
    paste("The source dataset was not found:", raw_file)
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
      "The dataset is missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

# Preserve timestamps if they are already POSIXct.
# Otherwise, parse the original character values.

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
  stop(
    paste(
      "Some timestamps could not be parsed.",
      "Inspect the original timestamp values before proceeding."
    )
  )
}

weather_all <- weather_all |>
  mutate(
    date = as.Date(timestamp, tz = "Africa/Lagos"),
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp)
  ) |>
  arrange(location, timestamp)

cat("\nDATA LOADED SUCCESSFULLY\n")
cat("Timestamp class:", paste(class(weather_all$timestamp), collapse = ", "), "\n")
cat("Rows:", nrow(weather_all), "\n")
cat("Locations:", n_distinct(weather_all$location), "\n")
cat(
  "Date range:",
  as.character(min(weather_all$date)),
  "to",
  as.character(max(weather_all$date)),
  "\n"
)


# 4. DEFINE ANALYSIS VARIABLES --------------------------------

analysis_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

# Daily metrics used in the statistical comparisons.
comparison_variables <- c(
  "mean_temperature",
  "maximum_temperature",
  "mean_relative_humidity",
  "mean_wind_speed",
  "mean_wet_bulb_temperature",
  "maximum_wet_bulb_temperature",
  "mean_apparent_temperature",
  "maximum_apparent_temperature",
  "daily_solar_energy_mj"
)


# 5. OVERALL DESCRIPTIVE STATISTICS ----------------------------

descriptive_summary <- weather_all |>
  group_by(location) |>
  summarise(
    across(
      all_of(analysis_variables),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        standard_deviation = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        percentile_90 = ~ quantile(
          .x, 0.90, na.rm = TRUE
        ),
        percentile_95 = ~ quantile(
          .x, 0.95, na.rm = TRUE
        ),
        minimum = ~ min(.x, na.rm = TRUE),
        maximum = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    observations = n(),
    .groups = "drop"
  )

write_csv(
  descriptive_summary,
  file.path(
    table_directory,
    "06a_overall_descriptive_statistics.csv"
  )
)


# 6. CREATE DAILY ANALYTICAL DATASET ---------------------------

# Daily averages reduce the influence of treating every
# consecutive hourly observation as independent.

daily_summary <- weather_all |>
  group_by(location, date) |>
  summarise(
    year = first(year),
    
    mean_temperature = mean(
      temperature_2m,
      na.rm = TRUE
    ),
    
    maximum_temperature = max(
      temperature_2m,
      na.rm = TRUE
    ),
    
    mean_relative_humidity = mean(
      relative_humidity_2m,
      na.rm = TRUE
    ),
    
    mean_wind_speed = mean(
      wind_speed_10m,
      na.rm = TRUE
    ),
    
    mean_solar_radiation = mean(
      shortwave_radiation,
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
    
    mean_apparent_temperature = mean(
      apparent_temperature,
      na.rm = TRUE
    ),
    
    maximum_apparent_temperature = max(
      apparent_temperature,
      na.rm = TRUE
    ),
    
    # Hourly average radiation in W/m² is converted
    # into daily accumulated energy in MJ/m².
    daily_solar_energy_mj = sum(
      shortwave_radiation,
      na.rm = TRUE
    ) * 3600 / 1e6,
    
    .groups = "drop"
  ) |>
  arrange(location, date)

write_csv(
  daily_summary,
  file.path(
    table_directory,
    "06b_daily_statistical_dataset.csv"
  )
)


# 7. DAILY DESCRIPTIVE STATISTICS ------------------------------

daily_descriptive_summary <- daily_summary |>
  group_by(location) |>
  summarise(
    across(
      all_of(comparison_variables),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        standard_deviation = ~ sd(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        percentile_90 = ~ quantile(
          .x, 0.90, na.rm = TRUE
        ),
        percentile_95 = ~ quantile(
          .x, 0.95, na.rm = TRUE
        )
      ),
      .names = "{.col}_{.fn}"
    ),
    days_observed = n(),
    .groups = "drop"
  )

write_csv(
  daily_descriptive_summary,
  file.path(
    table_directory,
    "06c_daily_descriptive_statistics.csv"
  )
)


# 8. BLOCK BOOTSTRAP FUNCTION ---------------------------------

# Resamples consecutive blocks of days rather than individual
# days. This helps preserve short-range temporal dependence.
#
# Seven-day blocks are used as an initial analytical choice.
# Results should be interpreted as approximate confidence
# intervals, not as proof that all dependence is eliminated.

block_bootstrap_mean_ci <- function(
    values,
    B = 1000,
    block_length = 7
) {
  
  values <- values[is.finite(values)]
  
  n <- length(values)
  
  if (n < 30) {
    return(
      c(
        estimate = mean(values),
        lower_95 = NA_real_,
        upper_95 = NA_real_
      )
    )
  }
  
  block_length <- min(block_length, n)
  
  number_of_blocks <- ceiling(n / block_length)
  
  bootstrap_estimates <- numeric(B)
  
  for (i in seq_len(B)) {
    
    starts <- sample.int(
      n,
      size = number_of_blocks,
      replace = TRUE
    )
    
    sampled_indices <- unlist(
      lapply(
        starts,
        function(start) {
          ((start - 1 + 0:(block_length - 1)) %% n) + 1
        }
      )
    )
    
    sampled_indices <- sampled_indices[seq_len(n)]
    
    bootstrap_estimates[i] <- mean(
      values[sampled_indices],
      na.rm = TRUE
    )
  }
  
  c(
    estimate = mean(values),
    lower_95 = unname(
      quantile(bootstrap_estimates, 0.025, na.rm = TRUE)
    ),
    upper_95 = unname(
      quantile(bootstrap_estimates, 0.975, na.rm = TRUE)
    )
  )
}


# 9. PAIRED LOCATION COMPARISONS -------------------------------

# The three locations are observed over the same calendar
# period. Comparisons therefore use matched dates.
#
# Difference is calculated as:
# First location minus second location.
#
# Positive values mean the first location had a higher value.

set.seed(20261118)

locations <- sort(
  unique(as.character(daily_summary$location))
)

if (length(locations) < 2) {
  stop(
    "At least two locations are required for location comparisons."
  )
}

location_pairs <- combn(
  locations,
  2,
  simplify = FALSE
)

location_comparison_results <- list()

result_counter <- 1

for (variable in comparison_variables) {
  
  variable_data <- daily_summary |>
    select(
      date,
      location,
      all_of(variable)
    ) |>
    pivot_wider(
      names_from = location,
      values_from = all_of(variable)
    )
  
  for (pair in location_pairs) {
    
    first_location <- pair[1]
    second_location <- pair[2]
    
    paired_data <- variable_data |>
      filter(
        is.finite(.data[[first_location]]),
        is.finite(.data[[second_location]])
      )
    
    differences <- (
      paired_data[[first_location]] -
        paired_data[[second_location]]
    )
    
    bootstrap_result <- block_bootstrap_mean_ci(
      differences,
      B = 1000,
      block_length = 7
    )
    
    location_comparison_results[[result_counter]] <- tibble(
      variable = variable,
      first_location = first_location,
      second_location = second_location,
      comparison = paste(
        first_location,
        "minus",
        second_location
      ),
      matched_days = length(differences),
      mean_difference = unname(
        bootstrap_result["estimate"]
      ),
      ci_lower_95 = unname(
        bootstrap_result["lower_95"]
      ),
      ci_upper_95 = unname(
        bootstrap_result["upper_95"]
      ),
      median_difference = median(
        differences,
        na.rm = TRUE
      ),
      confidence_interval_excludes_zero =
        ifelse(
          is.na(bootstrap_result["lower_95"]) |
            is.na(bootstrap_result["upper_95"]),
          NA,
          bootstrap_result["lower_95"] > 0 |
            bootstrap_result["upper_95"] < 0
        )
    )
    
    result_counter <- result_counter + 1
  }
}

location_comparison_table <- bind_rows(
  location_comparison_results
)

write_csv(
  location_comparison_table,
  file.path(
    table_directory,
    "06d_paired_location_comparisons.csv"
  )
)


# 10. DAILY ENVIRONMENTAL CORRELATIONS -------------------------

# Spearman correlation is used to measure monotonic
# associations between daily environmental summaries.
#
# Block-bootstrap intervals are used instead of relying
# on ordinary correlation-test p-values that assume
# independent observations.

block_bootstrap_spearman <- function(
    x,
    y,
    B = 1000,
    block_length = 7
) {
  
  valid <- complete.cases(x, y) &
    is.finite(x) &
    is.finite(y)
  
  x <- x[valid]
  y <- y[valid]
  
  n <- length(x)
  
  if (n < 30 ||
      length(unique(x)) < 2 ||
      length(unique(y)) < 2) {
    
    return(
      c(
        estimate = NA_real_,
        lower_95 = NA_real_,
        upper_95 = NA_real_
      )
    )
  }
  
  observed_correlation <- cor(
    x,
    y,
    method = "spearman"
  )
  
  block_length <- min(block_length, n)
  
  number_of_blocks <- ceiling(n / block_length)
  
  bootstrap_estimates <- numeric(B)
  
  for (i in seq_len(B)) {
    
    starts <- sample.int(
      n,
      size = number_of_blocks,
      replace = TRUE
    )
    
    sampled_indices <- unlist(
      lapply(
        starts,
        function(start) {
          ((start - 1 + 0:(block_length - 1)) %% n) + 1
        }
      )
    )
    
    sampled_indices <- sampled_indices[seq_len(n)]
    
    bootstrap_estimates[i] <- suppressWarnings(
      cor(
        x[sampled_indices],
        y[sampled_indices],
        method = "spearman"
      )
    )
  }
  
  bootstrap_estimates <- bootstrap_estimates[
    is.finite(bootstrap_estimates)
  ]
  
  if (length(bootstrap_estimates) < 100) {
    return(
      c(
        estimate = observed_correlation,
        lower_95 = NA_real_,
        upper_95 = NA_real_
      )
    )
  }
  
  c(
    estimate = observed_correlation,
    lower_95 = unname(
      quantile(bootstrap_estimates, 0.025)
    ),
    upper_95 = unname(
      quantile(bootstrap_estimates, 0.975)
    )
  )
}


correlation_pairs <- list(
  c("mean_temperature", "mean_relative_humidity"),
  c("mean_temperature", "mean_wet_bulb_temperature"),
  c("mean_temperature", "mean_apparent_temperature"),
  c("daily_solar_energy_mj", "mean_relative_humidity"),
  c("daily_solar_energy_mj", "mean_temperature"),
  c("mean_wind_speed", "mean_apparent_temperature")
)

correlation_results <- list()

result_counter <- 1

for (location_name in locations) {
  
  location_data <- daily_summary |>
    filter(location == location_name) |>
    arrange(date)
  
  for (pair in correlation_pairs) {
    
    x_variable <- pair[1]
    y_variable <- pair[2]
    
    bootstrap_result <- block_bootstrap_spearman(
      location_data[[x_variable]],
      location_data[[y_variable]],
      B = 1000,
      block_length = 7
    )
    
    correlation_results[[result_counter]] <- tibble(
      location = location_name,
      variable_1 = x_variable,
      variable_2 = y_variable,
      observations = sum(
        complete.cases(
          location_data[[x_variable]],
          location_data[[y_variable]]
        )
      ),
      spearman_correlation = unname(
        bootstrap_result["estimate"]
      ),
      ci_lower_95 = unname(
        bootstrap_result["lower_95"]
      ),
      ci_upper_95 = unname(
        bootstrap_result["upper_95"]
      )
    )
    
    result_counter <- result_counter + 1
  }
}

correlation_table <- bind_rows(
  correlation_results
)

write_csv(
  correlation_table,
  file.path(
    table_directory,
    "06e_daily_environmental_correlations.csv"
  )
)


# 11. ANNUAL STATISTICAL SUMMARY -------------------------------

annual_statistical_summary <- daily_summary |>
  group_by(location, year) |>
  summarise(
    across(
      all_of(comparison_variables),
      list(
        annual_mean = ~ mean(.x, na.rm = TRUE),
        annual_standard_deviation = ~ sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    days_observed = n(),
    .groups = "drop"
  ) |>
  arrange(location, year)

write_csv(
  annual_statistical_summary,
  file.path(
    table_directory,
    "06f_annual_statistical_summary.csv"
  )
)


# 12. ANALYSIS QUALITY CHECKS ----------------------------------

analysis_quality_checks <- tibble(
  check = c(
    "Hourly records loaded",
    "Locations represented",
    "Daily records generated",
    "Daily records with missing mean temperature",
    "Daily records with missing relative humidity",
    "Daily records with missing solar energy",
    "First date",
    "Last date"
  ),
  
  result = c(
    as.character(nrow(weather_all)),
    as.character(n_distinct(weather_all$location)),
    as.character(nrow(daily_summary)),
    as.character(sum(is.na(daily_summary$mean_temperature))),
    as.character(sum(is.na(daily_summary$mean_relative_humidity))),
    as.character(sum(is.na(daily_summary$daily_solar_energy_mj))),
    as.character(min(daily_summary$date)),
    as.character(max(daily_summary$date))
  )
)

write_csv(
  analysis_quality_checks,
  file.path(
    table_directory,
    "06g_statistical_analysis_quality_checks.csv"
  )
)


# 13. WRITE STATISTICAL METHODS NOTE ---------------------------

methods_note <- c(
  "HEAT EXPOSURE INTELLIGENCE",
  "SCRIPT 05: STATISTICAL ANALYSIS",
  "",
  "DATA SOURCE",
  "Open-Meteo Historical Weather API, ERA5 reanalysis.",
  "",
  "ANALYTICAL APPROACH",
  "Hourly observations were aggregated into daily summaries",
  "for the main location comparisons and correlation analyses.",
  "",
  "DESCRIPTIVE STATISTICS",
  "Means, standard deviations, medians, percentiles, minima",
  "and maxima were calculated for environmental variables.",
  "",
  "LOCATION COMPARISONS",
  "Daily values were matched by calendar date across locations.",
  "Mean paired differences were calculated as the first",
  "location minus the second location.",
  "",
  "UNCERTAINTY ESTIMATION",
  "95% percentile confidence intervals were estimated using",
  "a circular moving-block bootstrap with seven-day blocks",
  "and 1,000 bootstrap replications.",
  "",
  "CORRELATION ANALYSIS",
  "Spearman rank correlations were calculated using daily",
  "environmental summaries. Confidence intervals were",
  "estimated using the same block-bootstrap approach.",
  "",
  "INTERPRETATION LIMITATIONS",
  "The block-bootstrap approach approximates temporal",
  "dependence but does not guarantee that all forms of",
  "seasonality or long-range dependence have been removed.",
  "",
  "The three locations are specific study locations and",
  "should not be treated as a representative sample of",
  "all Nigerian locations.",
  "",
  "ERA5 data represent gridded reanalysis estimates rather",
  "than direct measurements at individual workplaces.",
  "",
  "Wet-bulb temperature is not WBGT.",
  "The analysis does not measure actual worker exposure,",
  "heat illness, productivity, or workplace safety.",
  "",
  "Correlations do not establish causation.",
  "Statistical differences should be interpreted alongside",
  "their magnitude and practical relevance."
)

writeLines(
  methods_note,
  file.path(
    report_directory,
    "01a_statistical_analysis_methods.txt"
  )
)


# 14. COMPLETION REPORT ---------------------------------------

cat("\n")
cat("============================================================\n")
cat("STATISTICAL ANALYSIS COMPLETED\n")
cat("============================================================\n")

cat("\nOUTPUT TABLES:\n")

cat("06a_overall_descriptive_statistics.csv\n")
cat("06b_daily_statistical_dataset.csv\n")
cat("06c_daily_descriptive_statistics.csv\n")
cat("06d_paired_location_comparisons.csv\n")
cat("06e_daily_environmental_correlations.csv\n")
cat("06f_annual_statistical_summary.csv\n")
cat("06g_statistical_analysis_quality_checks.csv\n")

cat("\nOUTPUT REPORT:\n")
cat("01a_statistical_analysis_methods.txt\n")

cat("\nAll outputs have been saved successfully if no errors occurred.\n")
cat("============================================================\n")
