
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 08 - MODEL VALIDATION AND DIAGNOSTIC ANALYSIS
# ============================================================
#
# PURPOSE:
# 1. Validate the integrity of saved model predictions.
# 2. Verify forecast-origin and target alignment.
# 3. Compare actual and predicted environmental temperatures.
# 4. Examine residual distributions and prediction errors.
# 5. Evaluate performance by location, month, quarter and hour.
# 6. Assess the detection of unusually high temperatures.
# 7. Compare model performance across prediction tasks.
# 8. Identify the strongest model for each task and location.
# 9. Produce validation tables, charts and documentation.
#
# INPUTS:
# - outputs/tables/08a_model_performance.csv
# - outputs/tables/08b_model_test_predictions.csv
# - data/processed/heat_exposure_hourly_features.csv
# - data/processed/heat_exposure_daily_features.csv
#
# TRAINING PERIOD: 2021-2024
# TEST PERIOD:     2025
#
# IMPORTANT:
# - This script does not retrain models.
# - This script does not modify Script 07 outputs.
# - Prediction errors are calculated as predicted - actual.
# - Historical 90th-percentile thresholds are not safety limits.
# - Environmental forecasts do not establish actual worker exposure.
# ============================================================


# 1. PACKAGES -------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "scales"
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
      "Install these packages before running the script:",
      paste(missing_packages, collapse = ", ")
    )
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)


# 2. PROJECT PATHS --------------------------------------------

hourly_file <- file.path(
  "data", "processed",
  "heat_exposure_hourly_features.csv"
)

daily_file <- file.path(
  "data", "processed",
  "heat_exposure_daily_features.csv"
)

performance_file <- file.path(
  "outputs", "tables",
  "08a_model_performance.csv"
)

predictions_file <- file.path(
  "outputs", "tables",
  "08b_model_test_predictions.csv"
)

table_directory <- file.path("outputs", "tables")
figure_directory <- file.path("outputs", "figures")
report_directory <- file.path("outputs", "reports")

for (folder in c(
  table_directory,
  figure_directory,
  report_directory
)) {
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

required_files <- c(
  hourly_file,
  daily_file,
  performance_file,
  predictions_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Required input files not found:",
      paste(missing_files, collapse = "\n")
    )
  )
}


# 3. VALIDATION CONFIGURATION ---------------------------------

training_start <- as.Date("2021-01-01")
training_end <- as.Date("2024-12-31")

test_start <- as.Date("2025-01-01")
test_end <- as.Date("2025-12-31")

split_timestamp <- as.POSIXct(
  "2025-01-01 00:00:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Africa/Lagos"
)

extreme_percentile <- 0.90

minimum_group_observations <- 20

random_seed <- 20261120

set.seed(random_seed)

cat("\n")
cat("============================================================\n")
cat("MODEL VALIDATION CONFIGURATION\n")
cat("============================================================\n")

cat("Training period:", training_start, "to", training_end, "\n")
cat("Testing period:", test_start, "to", test_end, "\n")
cat("Extreme-condition percentile:", extreme_percentile, "\n")
cat("Minimum group observations:", minimum_group_observations, "\n")


# 4. LOAD INPUT DATA -------------------------------------------

hourly_features <- read_csv(
  hourly_file,
  show_col_types = FALSE
)

daily_features <- read_csv(
  daily_file,
  show_col_types = FALSE
)

model_performance <- read_csv(
  performance_file,
  show_col_types = FALSE
)

all_predictions <- read_csv(
  predictions_file,
  show_col_types = FALSE
)

cat("\nINPUT DATA LOADED\n")

cat("Hourly feature rows:", nrow(hourly_features), "\n")
cat("Daily feature rows:", nrow(daily_features), "\n")
cat("Performance records:", nrow(model_performance), "\n")
cat("Prediction records:", nrow(all_predictions), "\n")


# 5. STANDARDISE TIMESTAMPS AND DATES -------------------------

if (inherits(hourly_features$timestamp, "POSIXct")) {
  
  hourly_features <- hourly_features |>
    mutate(
      timestamp = with_tz(
        timestamp,
        tzone = "Africa/Lagos"
      )
    )
  
} else {
  
  hourly_features <- hourly_features |>
    mutate(
      timestamp = parse_date_time(
        as.character(timestamp),
        orders = c(
          "ymd HMS z",
          "ymd HM z",
          "ymd HMS",
          "ymd HM"
        ),
        tz = "Africa/Lagos",
        quiet = TRUE
      )
    )
}

if (anyNA(hourly_features$timestamp)) {
  stop("Invalid timestamps found in hourly feature data.")
}

hourly_features <- hourly_features |>
  mutate(
    date = as.Date(
      timestamp,
      tz = "Africa/Lagos"
    )
  )

daily_features <- daily_features |>
  mutate(
    date = as.Date(date)
  )

if (anyNA(daily_features$date)) {
  stop("Invalid dates found in daily feature data.")
}

# Prediction timestamps are saved as local clock-time strings.

all_predictions <- all_predictions |>
  mutate(
    observation_date = as.Date(observation_date),
    observation_timestamp = as.POSIXct(
      observation_timestamp,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Africa/Lagos"
    )
  )

if (anyNA(all_predictions$observation_date)) {
  stop("Invalid observation dates found in prediction data.")
}

if (any(
  all_predictions$frequency == "hourly" &
  is.na(all_predictions$observation_timestamp)
)) {
  stop("Hourly forecast-origin timestamps contain missing values.")
}


# 6. DEFINE PREDICTION TASKS ----------------------------------

task_dictionary <- tibble(
  task = c(
    "Hourly apparent temperature",
    "Hourly wet-bulb temperature",
    "Hourly air temperature",
    "Daily maximum apparent temperature",
    "Daily maximum wet-bulb temperature",
    "Daily maximum air temperature"
  ),
  
  frequency = c(
    "hourly",
    "hourly",
    "hourly",
    "daily",
    "daily",
    "daily"
  ),
  
  target_variable = c(
    "next_hour_apparent_temperature",
    "next_hour_wet_bulb_temperature",
    "next_hour_air_temperature",
    "next_day_maximum_apparent_temperature",
    "next_day_maximum_wet_bulb_temperature",
    "next_day_maximum_temperature"
  ),
  
  target_label = c(
    "Next-hour apparent temperature",
    "Next-hour wet-bulb temperature",
    "Next-hour air temperature",
    "Next-day maximum apparent temperature",
    "Next-day maximum wet-bulb temperature",
    "Next-day maximum air temperature"
  )
)

expected_tasks <- task_dictionary$task

if (!setequal(unique(all_predictions$task), expected_tasks)) {
  stop("Prediction data does not contain exactly the six expected tasks.")
}

if (!all(
  all_predictions$frequency %in% c("hourly", "daily")
)) {
  stop("Unexpected prediction frequency detected.")
}

if (anyNA(all_predictions$actual)) {
  stop("Missing actual values found in prediction data.")
}


# 7. VERIFY PREDICTION AND FEATURE DATA STRUCTURE -------------

required_prediction_columns <- c(
  "task",
  "frequency",
  "location",
  "observation_date",
  "observation_timestamp",
  "actual",
  "persistence_prediction",
  "linear_prediction",
  "random_forest_prediction"
)

missing_prediction_columns <- setdiff(
  required_prediction_columns,
  names(all_predictions)
)

if (length(missing_prediction_columns) > 0) {
  stop(
    paste(
      "Missing prediction columns:",
      paste(missing_prediction_columns, collapse = ", ")
    )
  )
}

if (!all(
  c("location", "timestamp") %in% names(hourly_features)
)) {
  stop("Hourly feature dataset has missing key columns.")
}

if (!all(
  c("location", "date") %in% names(daily_features)
)) {
  stop("Daily feature dataset has missing key columns.")
}

locations <- sort(unique(all_predictions$location))

if (length(locations) != 3) {
  warning(
    paste(
      "Expected three locations; found",
      length(locations)
    )
  )
}

if (any(
  !is.finite(
    as.matrix(
      all_predictions[
        ,
        c(
          "actual",
          "persistence_prediction",
          "linear_prediction",
          "random_forest_prediction"
        )
      ]
    )
  )
)) {
  stop("Non-finite actual or predicted values detected.")
}



# 8. VALIDATE FORECAST ORIGIN AND TARGET ALIGNMENT -------------

# The saved observation_timestamp represents the forecast origin.
#
# Hourly:
# Forecast origin t -> target at t + 1 hour.
# The next-hour target is stored on the feature row at time t.
#
# Daily:
# Forecast origin date t -> target on date t + 1 day.
# The next-day target is stored on the feature row for date t.
#
# IMPORTANT:
# Hourly joins use standardised local-time character keys.
# This avoids timezone-attribute mismatches during POSIXct joins.

alignment_results <- vector(
  "list",
  nrow(task_dictionary)
)

for (i in seq_len(nrow(task_dictionary))) {
  
  current_task <- task_dictionary$task[i]
  current_frequency <- task_dictionary$frequency[i]
  current_target <- task_dictionary$target_variable[i]
  
  task_predictions <- all_predictions |>
    filter(task == current_task)
  
  if (current_frequency == "hourly") {
    
    # Standardise forecast-origin timestamps.
    
    task_predictions <- task_predictions |>
      mutate(
        origin_key = format(
          observation_timestamp - hours(1),
          format = "%Y-%m-%d %H:%M:%S",
          tz = "Africa/Lagos"
        )
      )
    
    source_data <- hourly_features |>
      mutate(
        origin_key = format(
          timestamp,
          format = "%Y-%m-%d %H:%M:%S",
          tz = "Africa/Lagos"
        )
      ) |>
      select(
        location,
        origin_key,
        all_of(current_target)
      )
    
    # Confirm source keys are unique.
    
    if (anyDuplicated(
      source_data[c("location", "origin_key")]
    ) > 0) {
      stop(
        paste(
          "Duplicate hourly source keys detected for:",
          current_task
        )
      )
    }
    
    matched_data <- task_predictions |>
      left_join(
        source_data,
        by = c(
          "location",
          "origin_key"
        )
      )
    
    first_origin <- min(
      task_predictions$observation_timestamp
    )
    
    last_origin <- max(
      task_predictions$observation_timestamp
    )
    
  } else {
    
    # Daily forecast origins are matched using calendar dates.
    
    task_predictions <- task_predictions |>
      mutate(
        origin_key = as.character(observation_date)
      )
    
    source_data <- daily_features |>
      mutate(
        origin_key = as.character(date)
      ) |>
      select(
        location,
        origin_key,
        all_of(current_target)
      )
    
    if (anyDuplicated(
      source_data[c("location", "origin_key")]
    ) > 0) {
      stop(
        paste(
          "Duplicate daily source keys detected for:",
          current_task
        )
      )
    }
    
    matched_data <- task_predictions |>
      left_join(
        source_data,
        by = c(
          "location",
          "origin_key"
        )
      )
    
    first_origin <- min(
      task_predictions$observation_date
    )
    
    last_origin <- max(
      task_predictions$observation_date
    )
  }
  
  # Check that every prediction found a corresponding source row.
  
  source_target <- matched_data[[current_target]]
  
  unmatched_records <- sum(is.na(source_target))
  
  if (unmatched_records > 0) {
    
    unmatched_examples <- matched_data |>
      filter(is.na(.data[[current_target]])) |>
      select(
        location,
        origin_key
      ) |>
      head(10)
    
    print(unmatched_examples)
    
    stop(
      paste(
        "Could not match",
        unmatched_records,
        "predictions to source target values for:",
        current_task
      )
    )
  }
  
  # Compare saved actual values with the original feature targets.
  
  alignment_difference <- abs(
    as.numeric(source_target) -
      as.numeric(task_predictions$actual)
  )
  
  mismatched_values <- sum(
    alignment_difference > 1e-8
  )
  
  maximum_difference <- max(
    alignment_difference,
    na.rm = TRUE
  )
  
  if (mismatched_values > 0) {
    stop(
      paste(
        "Actual prediction values do not match source targets:",
        current_task,
        "| Mismatches:",
        mismatched_values,
        "| Maximum difference:",
        maximum_difference
      )
    )
  }
  
  alignment_results[[i]] <- tibble(
    task = current_task,
    frequency = current_frequency,
    prediction_records = nrow(task_predictions),
    source_matches = nrow(matched_data),
    unmatched_records = unmatched_records,
    mismatched_actual_values = mismatched_values,
    maximum_absolute_difference = maximum_difference,
    first_forecast_origin = as.character(first_origin),
    last_forecast_origin = as.character(last_origin),
    alignment_status = "PASS"
  )
  
  cat(
    "Alignment passed:",
    current_task,
    "| Records:",
    nrow(task_predictions),
    "| Unmatched:",
    unmatched_records,
    "| Mismatches:",
    mismatched_values,
    "\n"
  )
}

alignment_validation <- bind_rows(
  alignment_results
)

cat("\nFORECAST ORIGIN AND TARGET ALIGNMENT VALIDATION\n")

print(
  alignment_validation,
  n = Inf
)

cat("\nAll six prediction tasks passed source-target alignment.\n")


# 9. VALIDATE EXPECTED OBSERVATION COUNTS ----------------------

hourly_counts <- all_predictions |>
  filter(frequency == "hourly") |>
  count(task, location, name = "observations")

daily_counts <- all_predictions |>
  filter(frequency == "daily") |>
  count(task, location, name = "observations")

expected_hourly_per_location <- 8759
expected_daily_per_location <- 364

if (any(
  hourly_counts$observations != expected_hourly_per_location
)) {
  stop("Unexpected hourly prediction counts detected.")
}

if (any(
  daily_counts$observations != expected_daily_per_location
)) {
  stop("Unexpected daily prediction counts detected.")
}

# Verify the first hourly forecast origin is included.
#
# Saved observation_timestamp represents target time.
# The first forecast originates at 2025-01-01 00:00.
# Its target time is 2025-01-01 01:00.

first_target_timestamp <- split_timestamp + hours(1)

first_origin_check <- all_predictions |>
  filter(
    frequency == "hourly",
    observation_timestamp == first_target_timestamp
  )

if (nrow(first_origin_check) != 9) {
  stop(
    paste(
      "Expected nine predictions for the first hourly forecast origin;",
      "found", nrow(first_origin_check)
    )
  )
}

cat(
  "First hourly forecast origin verified:",
  as.character(split_timestamp),
  "| Target time:",
  as.character(first_target_timestamp),
  "| Records:",
  nrow(first_origin_check),
  "\n"
)

# Verify that no forecast origin crosses into 2026.

if (any(
  all_predictions$observation_date < test_start |
  all_predictions$observation_date > test_end
)) {
  stop("Forecast origins fall outside the 2025 test period.")
}

# Check uniqueness.

hourly_duplicates <- all_predictions |>
  filter(frequency == "hourly") |>
  count(
    task,
    location,
    observation_timestamp
  ) |>
  filter(n > 1)

daily_duplicates <- all_predictions |>
  filter(frequency == "daily") |>
  count(
    task,
    location,
    observation_date
  ) |>
  filter(n > 1)

if (nrow(hourly_duplicates) > 0 ||
    nrow(daily_duplicates) > 0) {
  stop("Duplicate forecast origins detected.")
}

cat("\nPREDICTION INTEGRITY CHECKS PASSED\n")


# 10. BUILD VALIDATION DATASET -------------------------------

prediction_long <- all_predictions |>
  select(
    task,
    frequency,
    location,
    observation_date,
    observation_timestamp,
    actual,
    persistence_prediction,
    linear_prediction,
    random_forest_prediction
  ) |>
  pivot_longer(
    cols = c(
      persistence_prediction,
      linear_prediction,
      random_forest_prediction
    ),
    names_to = "model",
    values_to = "predicted"
  ) |>
  mutate(
    model = recode(
      model,
      persistence_prediction = "Persistence baseline",
      linear_prediction = "Multiple linear regression",
      random_forest_prediction = "Random forest"
    ),
    
    residual = predicted - actual,
    
    absolute_error = abs(residual),
    
    squared_error = residual^2,
    
    forecast_origin_hour = if_else(
      frequency == "hourly",
      hour(observation_timestamp - hours(1)),
      NA_integer_
    ),
    
    target_hour = if_else(
      frequency == "hourly",
      hour(observation_timestamp),
      NA_integer_
    ),
    
    month_number = month(observation_date),
    
    month_name = factor(
      month(
        observation_date,
        label = TRUE,
        abbr = FALSE
      ),
      levels = month.abb,
      ordered = TRUE
    ),
    
    quarter = paste0(
      "Q",
      quarter(observation_date)
    ),
    
    evaluation_year = year(observation_date)
  )

if (anyNA(prediction_long$residual)) {
  stop("Residual calculation generated missing values.")
}

if (any(!is.finite(prediction_long$residual))) {
  stop("Non-finite residuals detected.")
}


# 11. OVERALL RESIDUAL SUMMARY -------------------------------

residual_summary <- prediction_long |>
  group_by(
    task,
    frequency,
    model
  ) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    mean_error = mean(residual),
    
    median_error = median(residual),
    
    error_standard_deviation = sd(residual),
    
    minimum_error = min(residual),
    
    maximum_error = max(residual),
    
    error_90th_percentile = quantile(
      absolute_error,
      0.90,
      na.rm = TRUE
    ),
    
    error_95th_percentile = quantile(
      absolute_error,
      0.95,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# 12. LOCATION-SPECIFIC VALIDATION ----------------------------

location_validation <- prediction_long |>
  group_by(
    task,
    frequency,
    location,
    model
  ) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    bias = mean(residual),
    
    median_absolute_error = median(absolute_error),
    
    error_standard_deviation = sd(residual),
    
    error_90th_percentile = quantile(
      absolute_error,
      0.90
    ),
    
    .groups = "drop"
  )


# 13. MONTHLY VALIDATION ---------------------------------------

monthly_validation <- prediction_long |>
  group_by(
    task,
    frequency,
    location,
    model,
    month_number,
    month_name
  ) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    bias = mean(residual),
    
    .groups = "drop"
  ) |>
  arrange(
    task,
    location,
    model,
    month_number
  )


# 14. QUARTERLY VALIDATION -------------------------------------

quarterly_validation <- prediction_long |>
  group_by(
    task,
    frequency,
    location,
    model,
    quarter
  ) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    bias = mean(residual),
    
    .groups = "drop"
  ) |>
  arrange(
    task,
    location,
    model,
    quarter
  )


# 15. HOUR-OF-DAY VALIDATION -----------------------------------

hourly_validation <- prediction_long |>
  filter(frequency == "hourly") |>
  group_by(
    task,
    location,
    model,
    forecast_origin_hour,
    target_hour
  ) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    bias = mean(residual),
    
    .groups = "drop"
  ) |>
  arrange(
    task,
    location,
    model,
    forecast_origin_hour
  )


# 16. EXTREME-CONDITION VALIDATION ----------------------------

# Thresholds are calculated from the training-period target
# distribution, separately for each task and location.
#
# The 90th percentile is a historical statistical benchmark.
# It is NOT a workplace safety or occupational exposure limit.
#
# The threshold is calculated using the target values already
# constructed in Script 06, restricted to training origins
# whose target period also remains within 2021-2024.

threshold_results <- vector(
  "list",
  nrow(task_dictionary)
)

for (i in seq_len(nrow(task_dictionary))) {
  
  current_task <- task_dictionary$task[i]
  current_frequency <- task_dictionary$frequency[i]
  current_target <- task_dictionary$target_variable[i]
  
  if (current_frequency == "hourly") {
    
    training_target_data <- hourly_features |>
      filter(
        timestamp < split_timestamp,
        timestamp + hours(1) < split_timestamp
      ) |>
      select(
        location,
        target_value = all_of(current_target)
      )
    
  } else {
    
    training_target_data <- daily_features |>
      filter(
        date < test_start,
        date + 1 < test_start
      ) |>
      select(
        location,
        target_value = all_of(current_target)
      )
  }
  
  current_thresholds <- training_target_data |>
    filter(
      is.finite(target_value)
    ) |>
    group_by(location) |>
    summarise(
      threshold = quantile(
        target_value,
        probs = extreme_percentile,
        na.rm = TRUE,
        names = FALSE
      ),
      
      training_observations = n(),
      
      .groups = "drop"
    ) |>
    mutate(
      task = current_task,
      frequency = current_frequency,
      percentile = extreme_percentile
    )
  
  threshold_results[[i]] <- current_thresholds
}

extreme_thresholds <- bind_rows(threshold_results)

# Attach the appropriate location-specific threshold.

extreme_evaluation <- prediction_long |>
  left_join(
    extreme_thresholds,
    by = c(
      "task",
      "frequency",
      "location"
    )
  )

if (anyNA(extreme_evaluation$threshold)) {
  stop("Unable to match all extreme-condition thresholds.")
}

extreme_evaluation <- extreme_evaluation |>
  mutate(
    actual_extreme = actual >= threshold,
    
    predicted_extreme = predicted >= threshold,
    
    true_positive = actual_extreme & predicted_extreme,
    
    false_positive = !actual_extreme & predicted_extreme,
    
    false_negative = actual_extreme & !predicted_extreme,
    
    true_negative = !actual_extreme & !predicted_extreme
  )

extreme_validation <- extreme_evaluation |>
  group_by(
    task,
    frequency,
    location,
    model
  ) |>
  summarise(
    observations = n(),
    
    threshold = first(threshold),
    
    threshold_training_observations = first(
      training_observations
    ),
    
    actual_extreme_events = sum(actual_extreme),
    
    predicted_extreme_events = sum(predicted_extreme),
    
    true_positives = sum(true_positive),
    
    false_positives = sum(false_positive),
    
    false_negatives = sum(false_negative),
    
    true_negatives = sum(true_negative),
    
    precision = ifelse(
      sum(true_positive) + sum(false_positive) > 0,
      sum(true_positive) /
        (sum(true_positive) + sum(false_positive)),
      NA_real_
    ),
    
    recall = ifelse(
      sum(true_positive) + sum(false_negative) > 0,
      sum(true_positive) /
        (sum(true_positive) + sum(false_negative)),
      NA_real_
    ),
    
    specificity = ifelse(
      sum(true_negative) + sum(false_positive) > 0,
      sum(true_negative) /
        (sum(true_negative) + sum(false_positive)),
      NA_real_
    ),
    
    F1_score = ifelse(
      !is.na(precision) &&
        !is.na(recall) &&
        precision + recall > 0,
      2 * precision * recall /
        (precision + recall),
      NA_real_
    ),
    
    event_detection_accuracy = mean(
      actual_extreme == predicted_extreme
    ),
    
    .groups = "drop"
  )


# 17. MODEL SELECTION ------------------------------------------

# Overall model ranking is based on MAE.
# A separate selection is made for each location.
#
# This prevents the overall result from concealing differences
# in model performance between Abuja, Ilorin and Lagos.

overall_model_selection <- model_performance |>
  filter(
    evaluation_group == "ALL LOCATIONS"
  ) |>
  group_by(task) |>
  arrange(MAE, .by_group = TRUE) |>
  mutate(
    overall_rank = row_number()
  ) |>
  ungroup() |>
  select(
    task,
    model,
    observations,
    MAE,
    RMSE,
    R_squared,
    bias,
    MAE_skill_vs_baseline,
    overall_rank
  )

location_model_selection <- model_performance |>
  filter(
    evaluation_group != "ALL LOCATIONS"
  ) |>
  group_by(
    task,
    evaluation_group
  ) |>
  arrange(MAE, .by_group = TRUE) |>
  mutate(
    location_rank = row_number()
  ) |>
  ungroup() |>
  rename(
    location = evaluation_group
  ) |>
  select(
    task,
    location,
    model,
    observations,
    MAE,
    RMSE,
    R_squared,
    bias,
    MAE_skill_vs_baseline,
    location_rank
  )

best_overall_models <- overall_model_selection |>
  filter(overall_rank == 1)

best_location_models <- location_model_selection |>
  filter(location_rank == 1)


# 18. ACTUAL VERSUS PREDICTED CHARTS ---------------------------

actual_predicted_plot <- ggplot(
  prediction_long,
  aes(
    x = actual,
    y = predicted,
    colour = model
  )
) +
  geom_point(
    alpha = 0.12,
    size = 0.7
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    colour = "black"
  ) +
  facet_wrap(
    ~ task,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Actual versus Predicted Environmental Temperatures",
    subtitle = "The dashed line represents perfect prediction",
    x = "Actual temperature (°C)",
    y = "Predicted temperature (°C)",
    colour = "Model",
    caption = paste(
      "Evaluation period: 2025.",
      "Points closer to the dashed line indicate smaller prediction errors."
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
    "18a_actual_vs_predicted.png"
  ),
  actual_predicted_plot,
  width = 14,
  height = 11,
  dpi = 320,
  bg = "white"
)


# 19. RESIDUAL DISTRIBUTION -----------------------------------

residual_distribution_plot <- ggplot(
  prediction_long,
  aes(
    x = residual,
    fill = model
  )
) +
  geom_density(
    alpha = 0.30
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ task,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Prediction Residual Distributions",
    subtitle = "Residual = predicted temperature - actual temperature",
    x = "Prediction error (°C)",
    y = "Density",
    fill = "Model",
    caption = paste(
      "Positive residuals indicate overprediction;",
      "negative residuals indicate underprediction."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "18b_residual_distributions.png"
  ),
  residual_distribution_plot,
  width = 14,
  height = 11,
  dpi = 320,
  bg = "white"
)


# 20. RESIDUALS AGAINST ACTUAL VALUES -------------------------

residual_scatter_plot <- ggplot(
  prediction_long,
  aes(
    x = actual,
    y = residual,
    colour = model
  )
) +
  geom_point(
    alpha = 0.12,
    size = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    colour = "black"
  ) +
  facet_wrap(
    ~ task,
    scales = "free",
    ncol = 2
  ) +
  labs(
    title = "Prediction Residuals versus Actual Temperatures",
    subtitle = "Used to identify systematic prediction errors",
    x = "Actual temperature (°C)",
    y = "Residual (°C)",
    colour = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "18c_residuals_vs_actual.png"
  ),
  residual_scatter_plot,
  width = 14,
  height = 11,
  dpi = 320,
  bg = "white"
)


# 21. LOCATION-SPECIFIC PERFORMANCE CHART ---------------------

location_performance_plot <- location_validation |>
  ggplot(
    aes(
      x = location,
      y = MAE,
      fill = model
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  facet_wrap(
    ~ task,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Prediction Error Across Study Locations",
    subtitle = "Lower MAE indicates better predictive performance",
    x = "Study location",
    y = "Mean absolute error (°C)",
    fill = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "18d_location_validation.png"
  ),
  location_performance_plot,
  width = 14,
  height = 11,
  dpi = 320,
  bg = "white"
)


# 22. MONTHLY PERFORMANCE CHART -------------------------------

monthly_performance_plot <- ggplot(
  monthly_validation,
  aes(
    x = month_number,
    y = MAE,
    colour = model,
    group = model
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  facet_grid(
    task ~ location,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Model Performance",
    subtitle = "Changes in prediction error across 2025",
    x = "Month",
    y = "Mean absolute error (°C)",
    colour = "Model"
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
    "18e_monthly_model_performance.png"
  ),
  monthly_performance_plot,
  width = 16,
  height = 14,
  dpi = 320,
  bg = "white"
)


# 23. HOUR-OF-DAY PERFORMANCE CHART ---------------------------

hourly_performance_plot <- ggplot(
  hourly_validation,
  aes(
    x = forecast_origin_hour,
    y = MAE,
    colour = model,
    group = model
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.5
  ) +
  facet_grid(
    task ~ location,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = 0:23,
    labels = function(x) {
      sprintf("%02d:00", x)
    }
  ) +
  labs(
    title = "Hourly Forecast Performance",
    subtitle = paste(
      "Errors are grouped by forecast-origin hour;",
      "the target occurs one hour later."
    ),
    x = "Forecast-origin hour (local time)",
    y = "Mean absolute error (°C)",
    colour = "Model"
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
    "18f_hourly_model_performance.png"
  ),
  hourly_performance_plot,
  width = 16,
  height = 14,
  dpi = 320,
  bg = "white"
)


# 24. EXTREME-CONDITION DETECTION CHART -----------------------

extreme_detection_plot <- extreme_validation |>
  select(
    task,
    location,
    model,
    precision,
    recall
  ) |>
  pivot_longer(
    cols = c(
      precision,
      recall
    ),
    names_to = "metric",
    values_to = "score"
  ) |>
  ggplot(
    aes(
      x = model,
      y = score,
      fill = model
    )
  ) +
  geom_col(
    width = 0.7,
    na.rm = TRUE
  ) +
  facet_grid(
    task ~ location
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format()
  ) +
  labs(
    title = "Detection of Historically High-Temperature Conditions",
    subtitle = paste(
      "Precision and recall using location-specific",
      "training-period 90th-percentile thresholds"
    ),
    x = "Model",
    y = "Detection score",
    fill = "Model"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  ) +
  facet_grid(
    metric ~ task + location
  )

ggsave(
  file.path(
    figure_directory,
    "18g_extreme_condition_detection.png"
  ),
  extreme_detection_plot,
  width = 17,
  height = 9,
  dpi = 320,
  bg = "white"
)


# 25. PREDICTION BIAS CHART -----------------------------------

bias_plot <- location_validation |>
  ggplot(
    aes(
      x = location,
      y = bias,
      fill = model
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ task,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "Prediction Bias Across Study Locations",
    subtitle = "Positive bias indicates overprediction",
    x = "Location",
    y = "Mean prediction error (°C)",
    fill = "Model"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )

ggsave(
  file.path(
    figure_directory,
    "18h_prediction_bias.png"
  ),
  bias_plot,
  width = 14,
  height = 11,
  dpi = 320,
  bg = "white"
)


# 26. SAVE VALIDATION TABLES ----------------------------------

write_csv(
  alignment_validation,
  file.path(
    table_directory,
    "09a_prediction_alignment_validation.csv"
  )
)

write_csv(
  residual_summary,
  file.path(
    table_directory,
    "09b_residual_diagnostic_summary.csv"
  )
)

write_csv(
  location_validation,
  file.path(
    table_directory,
    "09c_location_model_validation.csv"
  )
)

write_csv(
  monthly_validation,
  file.path(
    table_directory,
    "09d_monthly_model_validation.csv"
  )
)

write_csv(
  quarterly_validation,
  file.path(
    table_directory,
    "09e_quarterly_model_validation.csv"
  )
)

write_csv(
  hourly_validation,
  file.path(
    table_directory,
    "09f_hourly_model_validation.csv"
  )
)

write_csv(
  extreme_thresholds,
  file.path(
    table_directory,
    "09g_extreme_condition_thresholds.csv"
  )
)

write_csv(
  extreme_validation,
  file.path(
    table_directory,
    "09h_extreme_condition_validation.csv"
  )
)

write_csv(
  overall_model_selection,
  file.path(
    table_directory,
    "09i_overall_model_selection.csv"
  )
)

write_csv(
  location_model_selection,
  file.path(
    table_directory,
    "09j_location_model_selection.csv"
  )
)


# 27. SAVE COMPLETE VALIDATION METHODS NOTE -------------------

methods_note <- c(
  
  "HEAT EXPOSURE INTELLIGENCE",
  
  "SCRIPT 08: MODEL VALIDATION AND DIAGNOSTIC ANALYSIS",
  
  "",
  
  "PURPOSE",
  
  "Evaluate the predictive performance and reliability of",
  "the six forecasting tasks developed in Script 07.",
  
  "",
  
  "DATA SOURCE",
  
  "Open-Meteo Historical Weather API - ERA5 reanalysis.",
  
  "",
  
  "TRAINING PERIOD",
  
  "2021-2024.",
  
  "",
  
  "TEST PERIOD",
  
  "2025.",
  
  "",
  
  "VALIDATION APPROACH",
  
  "Existing predictions from Script 07 were evaluated.",
  "Models were not retrained.",
  "The chronological training/testing boundary was preserved.",
  "Actual target values were checked against the original",
  "feature datasets using their forecast-origin keys.",
  
  "",
  
  "RESIDUAL DEFINITION",
  
  "Residual = predicted value - actual value.",
  "Positive residuals indicate overprediction.",
  "Negative residuals indicate underprediction.",
  
  "",
  
  "DIAGNOSTIC EVALUATION",
  
  "Actual versus predicted values.",
  "Residual distributions.",
  "Residuals versus actual temperatures.",
  "Location-specific performance.",
  "Monthly and quarterly performance.",
  "Hour-of-day performance.",
  "Prediction bias.",
  
  "",
  
  "EXTREME-CONDITION EVALUATION",
  
  "Historical 90th-percentile thresholds were calculated",
  "separately by location and prediction task.",
  "Thresholds were derived from training-period targets.",
  "Precision, recall, specificity and F1-score were calculated.",
  "These thresholds are historical statistical benchmarks,",
  "not occupational safety or exposure limits.",
  
  "",
  
  "MODEL SELECTION",
  
  "Models were ranked by mean absolute error.",
  "Overall rankings and location-specific rankings were",
  "evaluated separately.",
  
  "",
  
  "IMPORTANT LIMITATIONS",
  
  "The predictions concern environmental temperatures,",
  "not individual worker exposure or health outcomes.",
  "ERA5 is gridded reanalysis, not direct workplace measurement.",
  "Three study locations do not represent all of Nigeria.",
  "Historical model performance does not guarantee future",
  "forecast performance.",
  "Prediction errors may be temporally dependent.",
  "Extreme-condition detection does not establish workplace safety.",
  
  "",
  
  "SCRIPT 07 OUTPUTS WERE NOT MODIFIED.",
  
  "VALIDATION RESULTS ARE STORED IN OUTPUTS/TABLES.",
  "DIAGNOSTIC FIGURES ARE STORED IN OUTPUTS/FIGURES."
)

writeLines(
  methods_note,
  file.path(
    report_directory,
    "04a_model_validation_methods.txt"
  )
)


# 28. FINAL VALIDATION REPORT ---------------------------------

cat("\n")
cat("============================================================\n")
cat("MODEL VALIDATION COMPLETED\n")
cat("============================================================\n")

cat("\nPREDICTION ALIGNMENT VALIDATION\n")

print(
  alignment_validation,
  n = Inf
)

cat("\nOVERALL RESIDUAL SUMMARY\n")

print(
  residual_summary,
  n = Inf
)

cat("\nLOCATION-SPECIFIC MODEL PERFORMANCE\n")

print(
  location_validation,
  n = Inf
)

cat("\nBEST OVERALL MODEL BY TASK\n")

print(
  best_overall_models,
  n = Inf
)

cat("\nBEST MODEL BY TASK AND LOCATION\n")

print(
  best_location_models,
  n = Inf
)

cat("\nEXTREME-CONDITION DETECTION\n")

print(
  extreme_validation |>
    select(
      task,
      location,
      model,
      threshold,
      actual_extreme_events,
      predicted_extreme_events,
      precision,
      recall,
      F1_score
    ),
  n = Inf
)

cat("\nOUTPUT TABLES\n")

cat("09a_prediction_alignment_validation.csv\n")
cat("09b_residual_diagnostic_summary.csv\n")
cat("09c_location_model_validation.csv\n")
cat("09d_monthly_model_validation.csv\n")
cat("09e_quarterly_model_validation.csv\n")
cat("09f_hourly_model_validation.csv\n")
cat("09g_extreme_condition_thresholds.csv\n")
cat("09h_extreme_condition_validation.csv\n")
cat("09i_overall_model_selection.csv\n")
cat("09j_location_model_selection.csv\n")

cat("\nOUTPUT FIGURES\n")

cat("18a_actual_vs_predicted.png\n")
cat("18b_residual_distributions.png\n")
cat("18c_residuals_vs_actual.png\n")
cat("18d_location_validation.png\n")
cat("18e_monthly_model_performance.png\n")
cat("18f_hourly_model_performance.png\n")
cat("18g_extreme_condition_detection.png\n")
cat("18h_prediction_bias.png\n")

cat("\nOUTPUT REPORT\n")

cat("04a_model_validation_methods.txt\n")

cat("\n============================================================\n")
cat("SCRIPT 08 COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")

gc()
