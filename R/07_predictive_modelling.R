
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 07 - PREDICTIVE MODELLING
# ============================================================
#
# PURPOSE:
# 1. Forecast environmental temperature conditions.
# 2. Compare persistence, linear regression and random forest.
# 3. Use a chronological training/testing strategy.
# 4. Evaluate overall and location-specific performance.
# 5. Examine predictive importance.
# 6. Save fitted models, predictions and performance outputs.
#
# TRAINING PERIOD: 2021-2024
# TEST PERIOD:     2025
#
# FORECAST HORIZONS:
# - Next-hour environmental temperatures.
# - Next-day maximum environmental temperatures.
#
# IMPORTANT:
# - No random train/test splitting.
# - Future targets are never used as predictors.
# - Test observations are not used for model fitting.
# - Forecast origins and target periods are kept distinct.
# - Predictions concern environmental conditions, not worker
#   illness, individual exposure or workplace safety.
# ============================================================


# 1. PACKAGES -------------------------------------------------

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "lubridate",
  "ggplot2",
  "ranger"
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


# 2. PROJECT PATHS --------------------------------------------

hourly_file <- file.path(
  "data", "processed",
  "heat_exposure_hourly_features.csv"
)

daily_file <- file.path(
  "data", "processed",
  "heat_exposure_daily_features.csv"
)

model_directory <- file.path("outputs", "models")
table_directory <- file.path("outputs", "tables")
figure_directory <- file.path("outputs", "figures")
report_directory <- file.path("outputs", "reports")

for (folder in c(
  model_directory,
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

if (!file.exists(hourly_file)) {
  stop(paste("Hourly feature dataset not found:", hourly_file))
}

if (!file.exists(daily_file)) {
  stop(paste("Daily feature dataset not found:", daily_file))
}


# 3. MODELLING CONFIGURATION ---------------------------------

split_date <- as.Date("2025-01-01")

# Construct the chronological boundary explicitly in local time.
# This prevents ambiguity when converting Date to POSIXct.

split_timestamp <- as.POSIXct(
  paste0(
    as.character(split_date),
    " 00:00:00"
  ),
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Africa/Lagos"
)

if (is.na(split_timestamp)) {
  stop("Could not construct the local test-period boundary.")
}

number_of_trees <- 250
random_seed <- 20261118
number_of_threads <- 2

set.seed(random_seed)

cat("\nMODELLING CONFIGURATION\n")
cat("Training period: 2021-2024\n")
cat("Testing period: 2025\n")
cat(
  "Local test boundary:",
  format(
    split_timestamp,
    "%Y-%m-%d %H:%M:%S",
    tz = "Africa/Lagos"
  ),
  "\n"
)


# 4. LOAD AND VALIDATE DATA -----------------------------------

hourly_features <- read_csv(
  hourly_file,
  show_col_types = FALSE
)

daily_features <- read_csv(
  daily_file,
  show_col_types = FALSE
)

# Parse and standardise hourly timestamps.

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
  stop("Hourly timestamps contain values that could not be parsed.")
}

if (!all(c("location", "timestamp") %in% names(hourly_features))) {
  stop("Hourly dataset must contain location and timestamp.")
}

if (!all(c("location", "date") %in% names(daily_features))) {
  stop("Daily dataset must contain location and date.")
}

hourly_features <- hourly_features |>
  mutate(
    date = as.Date(timestamp, tz = "Africa/Lagos")
  ) |>
  arrange(location, timestamp)

daily_features <- daily_features |>
  mutate(
    date = as.Date(date)
  ) |>
  arrange(location, date)

if (anyNA(daily_features$date)) {
  stop("Daily dataset contains invalid dates.")
}

if (anyDuplicated(
  hourly_features[c("location", "timestamp")]
) > 0) {
  stop("Duplicate location-timestamp keys found in hourly data.")
}

if (anyDuplicated(
  daily_features[c("location", "date")]
) > 0) {
  stop("Duplicate location-date keys found in daily data.")
}

locations <- sort(unique(hourly_features$location))

if (length(locations) != 3) {
  warning(
    paste(
      "Expected three study locations;",
      "found", length(locations), "."
    )
  )
}

cat("\nMODELLING DATA LOADED\n")
cat("Hourly rows:", nrow(hourly_features), "\n")
cat("Daily rows:", nrow(daily_features), "\n")
cat("Locations:", paste(locations, collapse = ", "), "\n")


# 5. DEFINE HOURLY PREDICTORS -------------------------------

hourly_base_predictors <- c(
  "location",
  "latitude",
  "longitude",
  "year",
  "month",
  "day",
  "hour",
  "day_of_year",
  "day_of_week",
  "quarter",
  "is_weekend",
  "hour_sin",
  "hour_cos",
  "day_of_year_sin",
  "day_of_year_cos",
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature",
  "wind_direction_sin",
  "wind_direction_cos",
  "wind_u_component",
  "wind_v_component"
)

hourly_history_predictors <- grep(
  pattern = "(_lag_(1|3|6|24)h$|_prior_(3|6|24)h_mean$)",
  x = names(hourly_features),
  value = TRUE
)

hourly_predictors <- unique(
  c(
    hourly_base_predictors,
    hourly_history_predictors
  )
)


# 6. DEFINE DAILY PREDICTORS ---------------------------------

daily_base_predictors <- c(
  "location",
  "year",
  "month",
  "quarter",
  "day_of_year",
  "day_of_week",
  "is_weekend",
  "day_of_year_sin",
  "day_of_year_cos",
  "mean_temperature",
  "maximum_temperature",
  "temperature_standard_deviation",
  "temperature_90th_percentile",
  "mean_relative_humidity",
  "maximum_relative_humidity",
  "mean_wind_speed",
  "maximum_wind_speed",
  "mean_solar_radiation",
  "daily_solar_energy_mj",
  "hours_with_positive_solar_radiation",
  "mean_wet_bulb_temperature",
  "maximum_wet_bulb_temperature",
  "wet_bulb_temperature_90th_percentile",
  "mean_apparent_temperature",
  "maximum_apparent_temperature",
  "apparent_temperature_90th_percentile"
)

daily_history_predictors <- grep(
  pattern = "(_lag_(1|3|7)d$|_prior_(3|7)d_mean$)",
  x = names(daily_features),
  value = TRUE
)

daily_predictors <- unique(
  c(
    daily_base_predictors,
    daily_history_predictors
  )
)


# 7. VALIDATE REQUIRED PREDICTORS -----------------------------

missing_hourly_predictors <- setdiff(
  hourly_base_predictors,
  names(hourly_features)
)

missing_daily_predictors <- setdiff(
  daily_base_predictors,
  names(daily_features)
)

if (length(missing_hourly_predictors) > 0) {
  stop(
    paste(
      "Required hourly predictors missing:",
      paste(missing_hourly_predictors, collapse = ", ")
    )
  )
}

if (length(missing_daily_predictors) > 0) {
  stop(
    paste(
      "Required daily predictors missing:",
      paste(missing_daily_predictors, collapse = ", ")
    )
  )
}

cat("\nPREDICTOR CONFIGURATION\n")
cat("Hourly predictors:", length(hourly_predictors), "\n")
cat("Daily predictors:", length(daily_predictors), "\n")


# 8. DEFINE PREDICTION TASKS ---------------------------------

hourly_tasks <- tibble(
  task = c(
    "Hourly apparent temperature",
    "Hourly wet-bulb temperature",
    "Hourly air temperature"
  ),
  target = c(
    "next_hour_apparent_temperature",
    "next_hour_wet_bulb_temperature",
    "next_hour_air_temperature"
  ),
  baseline = c(
    "apparent_temperature",
    "wet_bulb_temperature_2m",
    "temperature_2m"
  )
)

daily_tasks <- tibble(
  task = c(
    "Daily maximum apparent temperature",
    "Daily maximum wet-bulb temperature",
    "Daily maximum air temperature"
  ),
  target = c(
    "next_day_maximum_apparent_temperature",
    "next_day_maximum_wet_bulb_temperature",
    "next_day_maximum_temperature"
  ),
  baseline = c(
    "maximum_apparent_temperature",
    "maximum_wet_bulb_temperature",
    "maximum_temperature"
  )
)


# 9. PERFORMANCE METRICS FUNCTION ----------------------------

calculate_metrics <- function(
    actual,
    predicted,
    location,
    task,
    model_name
) {
  
  prediction_data <- tibble(
    location = as.character(location),
    actual = as.numeric(actual),
    predicted = as.numeric(predicted)
  ) |>
    filter(
      is.finite(actual),
      is.finite(predicted)
    )
  
  if (nrow(prediction_data) == 0) {
    return(tibble())
  }
  
  combined <- bind_rows(
    prediction_data |>
      mutate(evaluation_group = "ALL LOCATIONS"),
    
    prediction_data |>
      mutate(evaluation_group = location)
  )
  
  combined |>
    group_by(evaluation_group) |>
    summarise(
      task = task,
      model = model_name,
      observations = n(),
      
      MAE = mean(abs(actual - predicted)),
      
      RMSE = sqrt(
        mean((actual - predicted)^2)
      ),
      
      bias = mean(predicted - actual),
      
      R_squared = {
        total_variation <- sum(
          (actual - mean(actual))^2
        )
        
        if (total_variation > 0) {
          1 - sum((actual - predicted)^2) /
            total_variation
        } else {
          NA_real_
        }
      },
      
      .groups = "drop"
    )
}


# 10. MODEL TRAINING FUNCTION --------------------------------

run_model_task <- function(
    data,
    task_name,
    target_variable,
    baseline_variable,
    predictor_variables,
    frequency
) {
  
  cat("\n")
  cat("============================================================\n")
  cat("MODELLING TASK:", task_name, "\n")
  cat("============================================================\n")
  
  if (!frequency %in% c("hourly", "daily")) {
    stop("Frequency must be hourly or daily.")
  }
  
  required_variables <- unique(
    c(
      predictor_variables,
      target_variable,
      baseline_variable
    )
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(data)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      paste(
        "Missing variables for", task_name, ":",
        paste(missing_variables, collapse = ", ")
      )
    )
  }
  
  # Prepare model data.
  
  if (frequency == "hourly") {
    
    model_data <- data |>
      select(
        timestamp,
        date,
        all_of(required_variables)
      )
    
  } else {
    
    model_data <- data |>
      select(
        date,
        all_of(required_variables)
      )
  }
  
  # Remove records with missing values in required fields.
  
  model_data <- model_data |>
    filter(
      if_all(
        all_of(required_variables),
        ~ !is.na(.x)
      )
    )
  
  # Remove non-finite numeric values.
  # This is a safeguard against Inf and -Inf.
  
  numeric_required <- required_variables[
    vapply(
      model_data[required_variables],
      is.numeric,
      logical(1)
    )
  ]
  
  if (length(numeric_required) > 0) {
    
    model_data <- model_data |>
      filter(
        if_all(
          all_of(numeric_required),
          ~ is.finite(.x)
        )
      )
  }
  
  # Convert character predictors to factors.
  # Logical predictors are converted to integers.
  
  model_data <- model_data |>
    mutate(
      across(
        where(is.character),
        as.factor
      ),
      across(
        where(is.logical),
        as.integer
      )
    )
  
  # Chronological split.
  
  if (frequency == "hourly") {
    
    # Training targets must remain strictly before
    # the first test-period timestamp.
    
    training_data <- model_data |>
      filter(
        timestamp < split_timestamp,
        timestamp + hours(1) < split_timestamp
      )
    
    # Include every eligible forecast origin from local
    # midnight on 1 January 2025 onwards.
    
    testing_data <- model_data |>
      filter(
        timestamp >= split_timestamp
      )
    
  } else {
    
    training_data <- model_data |>
      filter(
        date < split_date,
        date + 1 < split_date
      )
    
    testing_data <- model_data |>
      filter(
        date >= split_date
      )
  }
  
  if (nrow(training_data) == 0 ||
      nrow(testing_data) == 0) {
    
    stop(
      paste(
        "Insufficient training or testing observations for:",
        task_name
      )
    )
  }
  
  cat("Training observations:", nrow(training_data), "\n")
  cat("Testing observations:", nrow(testing_data), "\n")
  cat("Predictor variables:", length(predictor_variables), "\n")
  
  # Check that the training and testing periods do not overlap.
  
  if (frequency == "hourly") {
    
    if (max(training_data$timestamp) >= split_timestamp) {
      stop("Hourly training data crosses the test boundary.")
    }
    
    if (min(testing_data$timestamp) < split_timestamp) {
      stop("Hourly testing data starts before the test boundary.")
    }
    
  } else {
    
    if (max(training_data$date) >= split_date) {
      stop("Daily training data crosses the test boundary.")
    }
    
    if (min(testing_data$date) < split_date) {
      stop("Daily testing data starts before the test boundary.")
    }
  }
  
  # Construct modelling frames.
  
  training_model <- as.data.frame(
    training_data[
      ,
      c(predictor_variables, target_variable),
      drop = FALSE
    ]
  )
  
  testing_model <- as.data.frame(
    testing_data[
      ,
      c(predictor_variables, target_variable),
      drop = FALSE
    ]
  )
  
  # Confirm the response is numeric.
  
  if (!is.numeric(training_model[[target_variable]])) {
    stop(
      paste("Target is not numeric:", target_variable)
    )
  }
  
  # Construct model formula.
  
  model_formula <- reformulate(
    predictor_variables,
    response = target_variable
  )
  
  # MODEL 1: MULTIPLE LINEAR REGRESSION -----------------------
  
  cat("\nFitting multiple linear regression...\n")
  
  linear_model <- lm(
    formula = model_formula,
    data = training_model
  )
  
  linear_predictions <- as.numeric(
    predict(
      linear_model,
      newdata = testing_model
    )
  )
  
  # MODEL 2: RANDOM FOREST -----------------------------------
  
  cat("Fitting random forest...\n")
  
  random_forest_model <- ranger::ranger(
    formula = model_formula,
    data = training_model,
    num.trees = number_of_trees,
    mtry = max(
      1,
      floor(sqrt(length(predictor_variables)))
    ),
    min.node.size = 5,
    importance = "permutation",
    seed = random_seed,
    num.threads = number_of_threads,
    respect.unordered.factors = "order"
  )
  
  forest_predictions <- as.numeric(
    predict(
      random_forest_model,
      data = testing_model[
        ,
        predictor_variables,
        drop = FALSE
      ]
    )$predictions
  )
  
  # MODEL 3: PERSISTENCE BASELINE ----------------------------
  
  baseline_predictions <- as.numeric(
    testing_model[[baseline_variable]]
  )
  
  # VALIDATE PREDICTIONS -------------------------------------
  
  prediction_lengths <- c(
    length(linear_predictions),
    length(forest_predictions),
    length(baseline_predictions),
    nrow(testing_data)
  )
  
  if (length(unique(prediction_lengths)) != 1) {
    stop(
      paste(
        "Prediction length mismatch for:",
        task_name
      )
    )
  }
  
  if (any(!is.finite(linear_predictions)) ||
      any(!is.finite(forest_predictions)) ||
      any(!is.finite(baseline_predictions))) {
    
    stop(
      paste(
        "Non-finite predictions detected for:",
        task_name
      )
    )
  }
  
  # SAVE PREDICTIONS -----------------------------------------
  
  # Store forecast-origin time, not target time.
  # The target is one hour after this timestamp for hourly
  # tasks and the following day for daily tasks.
  
  if (frequency == "hourly") {
    
    timestamp_output <- format(
      testing_data$timestamp,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Africa/Lagos"
    )
    
  } else {
    
    timestamp_output <- rep(
      NA_character_,
      nrow(testing_data)
    )
  }
  
  prediction_output <- tibble(
    task = task_name,
    frequency = frequency,
    location = as.character(testing_data$location),
    observation_date = as.character(testing_data$date),
    observation_timestamp = timestamp_output,
    actual = as.numeric(testing_model[[target_variable]]),
    persistence_prediction = baseline_predictions,
    linear_prediction = linear_predictions,
    random_forest_prediction = forest_predictions
  )
  
  if (anyNA(prediction_output$actual) ||
      anyNA(prediction_output$persistence_prediction) ||
      anyNA(prediction_output$linear_prediction) ||
      anyNA(prediction_output$random_forest_prediction)) {
    
    stop(
      paste(
        "Missing prediction values detected for:",
        task_name
      )
    )
  }
  
  # PERFORMANCE METRICS --------------------------------------
  
  baseline_metrics <- calculate_metrics(
    prediction_output$actual,
    prediction_output$persistence_prediction,
    prediction_output$location,
    task_name,
    "Persistence baseline"
  )
  
  linear_metrics <- calculate_metrics(
    prediction_output$actual,
    prediction_output$linear_prediction,
    prediction_output$location,
    task_name,
    "Multiple linear regression"
  )
  
  forest_metrics <- calculate_metrics(
    prediction_output$actual,
    prediction_output$random_forest_prediction,
    prediction_output$location,
    task_name,
    "Random forest"
  )
  
  task_metrics <- bind_rows(
    baseline_metrics,
    linear_metrics,
    forest_metrics
  )
  
  # RANDOM FOREST FEATURE IMPORTANCE -------------------------
  
  importance_output <- tibble(
    task = task_name,
    feature = names(
      random_forest_model$variable.importance
    ),
    importance = as.numeric(
      random_forest_model$variable.importance
    )
  ) |>
    arrange(desc(importance))
  
  # SAVE FITTED MODEL OBJECTS --------------------------------
  
  safe_task_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(task_name)
  )
  
  saveRDS(
    list(
      linear_regression = linear_model,
      random_forest = random_forest_model,
      target = target_variable,
      baseline = baseline_variable,
      predictors = predictor_variables,
      frequency = frequency,
      training_period = "2021-2024",
      testing_period = "2025",
      split_date = split_date,
      split_timestamp = split_timestamp,
      random_seed = random_seed,
      number_of_trees = number_of_trees
    ),
    file.path(
      model_directory,
      paste0(safe_task_name, "_models.rds")
    )
  )
  
  cat("Predictions saved in memory:", nrow(prediction_output), "\n")
  cat("Completed:", task_name, "\n")
  
  list(
    metrics = task_metrics,
    predictions = prediction_output,
    importance = importance_output
  )
}


# 11. RUN HOURLY MODELS ---------------------------------------

hourly_results <- vector(
  "list",
  nrow(hourly_tasks)
)

for (i in seq_len(nrow(hourly_tasks))) {
  
  hourly_results[[i]] <- run_model_task(
    data = hourly_features,
    task_name = hourly_tasks$task[i],
    target_variable = hourly_tasks$target[i],
    baseline_variable = hourly_tasks$baseline[i],
    predictor_variables = hourly_predictors,
    frequency = "hourly"
  )
}


# 12. RUN DAILY MODELS ----------------------------------------

daily_results <- vector(
  "list",
  nrow(daily_tasks)
)

for (i in seq_len(nrow(daily_tasks))) {
  
  daily_results[[i]] <- run_model_task(
    data = daily_features,
    task_name = daily_tasks$task[i],
    target_variable = daily_tasks$target[i],
    baseline_variable = daily_tasks$baseline[i],
    predictor_variables = daily_predictors,
    frequency = "daily"
  )
}


# 13. COMBINE ALL RESULTS -------------------------------------

all_results <- c(
  hourly_results,
  daily_results
)

model_performance <- bind_rows(
  lapply(
    all_results,
    function(x) x$metrics
  )
)

all_predictions <- bind_rows(
  lapply(
    all_results,
    function(x) x$predictions
  )
)

all_feature_importance <- bind_rows(
  lapply(
    all_results,
    function(x) x$importance
  )
)


# 14. CALCULATE SKILL AGAINST BASELINE ------------------------

baseline_performance <- model_performance |>
  filter(
    model == "Persistence baseline"
  ) |>
  select(
    task,
    evaluation_group,
    baseline_MAE = MAE,
    baseline_RMSE = RMSE
  )

model_performance <- model_performance |>
  left_join(
    baseline_performance,
    by = c("task", "evaluation_group")
  ) |>
  mutate(
    MAE_skill_vs_baseline = ifelse(
      baseline_MAE > 0,
      1 - MAE / baseline_MAE,
      NA_real_
    ),
    RMSE_skill_vs_baseline = ifelse(
      baseline_RMSE > 0,
      1 - RMSE / baseline_RMSE,
      NA_real_
    )
  )


# 15. VALIDATE FINAL PREDICTION DATA --------------------------

cat("\nFINAL PREDICTION DATA VALIDATION\n")

if (anyNA(all_predictions$actual) ||
    anyNA(all_predictions$persistence_prediction) ||
    anyNA(all_predictions$linear_prediction) ||
    anyNA(all_predictions$random_forest_prediction)) {
  
  stop("Final prediction dataset contains missing prediction values.")
}

hourly_prediction_counts <- all_predictions |>
  filter(frequency == "hourly") |>
  count(task, location, name = "observations")

daily_prediction_counts <- all_predictions |>
  filter(frequency == "daily") |>
  count(task, location, name = "observations")

cat("\nHourly prediction counts:\n")
print(hourly_prediction_counts, n = Inf)

cat("\nDaily prediction counts:\n")
print(daily_prediction_counts, n = Inf)

# Check duplicate forecast origins.

hourly_duplicate_check <- all_predictions |>
  filter(frequency == "hourly") |>
  count(task, location, observation_timestamp) |>
  filter(n > 1)

daily_duplicate_check <- all_predictions |>
  filter(frequency == "daily") |>
  count(task, location, observation_date) |>
  filter(n > 1)

if (nrow(hourly_duplicate_check) > 0) {
  stop("Duplicate hourly prediction origins detected.")
}

if (nrow(daily_duplicate_check) > 0) {
  stop("Duplicate daily prediction origins detected.")
}

# Check that the first hourly test origin is included.

first_hourly_origins <- all_predictions |>
  filter(
    frequency == "hourly",
    observation_timestamp == "2025-01-01 00:00:00"
  )

if (nrow(first_hourly_origins) != 9) {
  stop(
    paste(
      "Expected nine first-hour predictions",
      "(three tasks x three locations), but found",
      nrow(first_hourly_origins),
      ". Check the test boundary and data filtering."
    )
  )
}

cat(
  "\nFirst hourly test-origin records:",
  nrow(first_hourly_origins),
  "\n"
)


# 16. SAVE OUTPUT TABLES --------------------------------------

write_csv(
  model_performance,
  file.path(
    table_directory,
    "08a_model_performance.csv"
  )
)

write_csv(
  all_predictions,
  file.path(
    table_directory,
    "08b_model_test_predictions.csv"
  )
)

write_csv(
  all_feature_importance,
  file.path(
    table_directory,
    "08c_random_forest_feature_importance.csv"
  )
)


# 17. PERFORMANCE COMPARISON CHART ----------------------------

performance_plot <- model_performance |>
  filter(
    evaluation_group == "ALL LOCATIONS"
  ) |>
  ggplot(
    aes(
      x = task,
      y = MAE,
      fill = model
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "Predictive Model Performance Comparison",
    subtitle = paste(
      "Lower mean absolute error indicates better predictions;",
      "evaluation period: 2025"
    ),
    x = "Prediction task",
    y = "Mean absolute error (°C)",
    fill = "Model",
    caption = paste(
      "Test data: ERA5 environmental data.",
      "Performance is evaluated on the held-out chronological period."
    )
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(
    figure_directory,
    "16a_predictive_model_performance.png"
  ),
  plot = performance_plot,
  width = 13,
  height = 8,
  dpi = 320,
  bg = "white"
)


# 18. RANDOM FOREST FEATURE IMPORTANCE CHARTS ---------------

for (task_name in unique(all_feature_importance$task)) {
  
  importance_plot_data <- all_feature_importance |>
    filter(task == task_name) |>
    slice_max(
      order_by = importance,
      n = 15,
      with_ties = FALSE
    ) |>
    mutate(
      feature = reorder(feature, importance)
    )
  
  feature_figure_id <- dplyr::case_when(
    task_name == "Daily maximum air temperature" ~ "17a",
    task_name == "Daily maximum apparent temperature" ~ "17b",
    task_name == "Daily maximum wet-bulb temperature" ~ "17c",
    task_name == "Hourly air temperature" ~ "17d",
    task_name == "Hourly apparent temperature" ~ "17e",
    task_name == "Hourly wet-bulb temperature" ~ "17f",
    TRUE ~ NA_character_
  )

  stopifnot(!is.na(feature_figure_id))

  importance_plot <- ggplot(
    importance_plot_data,
    aes(
      x = feature,
      y = importance
    )
  ) +
    geom_col(
      fill = "#2878B5"
    ) +
    coord_flip() +
    labs(
      title = paste0(
        feature_figure_id,
        " | Most Important Predictors: ",
        task_name
      ),
      x = "Predictor",
      y = "Permutation importance",
      caption = paste(
        "Importance describes model reliance on predictors;",
        "it does not establish causation."
      )
    ) +
    theme_minimal(base_size = 11)
  
  safe_task_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(task_name)
  )
  
  ggsave(
    filename = file.path(
      figure_directory,
      paste0(
        feature_figure_id,
        "_feature_importance_",
        safe_task_name,
        ".png"
      )
    ),
    plot = importance_plot,
    width = 12,
    height = 8,
    dpi = 320,
    bg = "white"
  )
}


# 19. SAVE MODELLING METHODS NOTE -----------------------------

methods_note <- c(
  "HEAT EXPOSURE INTELLIGENCE",
  "SCRIPT 07: PREDICTIVE MODELLING",
  "",
  "DATA SOURCE",
  "Open-Meteo Historical Weather API - ERA5.",
  "",
  "TRAINING PERIOD",
  "2021-2024.",
  "",
  "TEST PERIOD",
  "2025.",
  "",
  "TIME STANDARD",
  "Africa/Lagos local time.",
  "",
  "MODELS",
  "Persistence baseline.",
  "Multiple linear regression.",
  "Random forest.",
  "",
  "PREDICTION TASKS",
  "Next-hour apparent temperature.",
  "Next-hour wet-bulb temperature.",
  "Next-hour air temperature.",
  "Next-day maximum apparent temperature.",
  "Next-day maximum wet-bulb temperature.",
  "Next-day maximum air temperature.",
  "",
  "PREDICTION ALIGNMENT",
  "Hourly predictions use the current hour's available",
  "features to forecast the following hour.",
  "Daily predictions use completed daily summaries to",
  "forecast the following day's maximum temperature.",
  "Saved timestamps identify forecast origins, not target times.",
  "",
  "PERFORMANCE METRICS",
  "Mean Absolute Error (MAE).",
  "Root Mean Squared Error (RMSE).",
  "R-squared.",
  "Prediction bias.",
  "MAE and RMSE skill relative to persistence.",
  "",
  "VALIDATION",
  "A chronological split reserves 2025 for testing.",
  "Training records whose future targets cross the",
  "training/testing boundary are excluded.",
  "Missing required modelling fields are not imputed.",
  "Prediction completeness and duplicate forecast origins",
  "are checked before outputs are saved.",
  "",
  "LIMITATIONS",
  "The models predict environmental conditions, not",
  "individual worker exposure, illness or productivity.",
  "The three study locations do not represent every",
  "Nigerian location.",
  "Performance on this historical test period does not",
  "guarantee performance under future climate conditions.",
  "Feature importance does not establish causation."
)

writeLines(
  methods_note,
  file.path(
    report_directory,
    "03a_predictive_modelling_methods.txt"
  )
)


# 20. FINAL COMPLETION REPORT -------------------------------

cat("\n")
cat("============================================================\n")
cat("PREDICTIVE MODELLING COMPLETED\n")
cat("============================================================\n")

cat("\nMODEL PERFORMANCE - ALL LOCATIONS\n")

print(
  model_performance |>
    filter(
      evaluation_group == "ALL LOCATIONS"
    ) |>
    select(
      task,
      model,
      observations,
      MAE,
      RMSE,
      R_squared,
      bias,
      MAE_skill_vs_baseline
    ),
  n = Inf
)

cat("\nPREDICTION COUNTS BY FREQUENCY\n")

print(
  all_predictions |>
    count(frequency, task, name = "observations"),
  n = Inf
)

cat("\nOUTPUT TABLES:\n")
cat("08a_model_performance.csv\n")
cat("08b_model_test_predictions.csv\n")
cat("08c_random_forest_feature_importance.csv\n")

cat("\nOUTPUT FIGURES:\n")
cat("16a_predictive_model_performance.png\n")
cat("17a-17f feature-importance figures\n")

cat("\nOUTPUT MODELS:\n")
cat("Six fitted model collections saved as RDS files.\n")

cat("\nOUTPUT REPORT:\n")
cat("03a_predictive_modelling_methods.txt\n")

cat("\nTraining period: 2021-2024\n")
cat("Testing period: 2025\n")

cat(
  "\nFirst hourly test origin:",
  format(
    split_timestamp,
    "%Y-%m-%d %H:%M:%S",
    tz = "Africa/Lagos"
  ),
  "\n"
)

cat("\nPREDICTIVE MODELLING COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")
