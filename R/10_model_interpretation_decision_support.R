# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 10 - MODEL INTERPRETATION AND DECISION SUPPORT
# ============================================================
#
# PURPOSE:
# 1. Interpret the six completed forecasting tasks.
# 2. Assess model performance by location and season.
# 3. Examine forecast errors and systematic bias.
# 4. Summarise the most influential model predictors.
# 5. Compare predictive models with the persistence baseline.
# 6. Translate findings into cautious operational intelligence.
#
# IMPORTANT:
# - This script does not train or modify models.
# - It uses the existing 2025 test predictions.
# - Historical P90/P95 values are descriptive benchmarks,
#   NOT occupational heat-safety thresholds.
# - Environmental forecasts are NOT worker health predictions.
#
# INPUTS:
# outputs/tables/08a_model_performance.csv
# outputs/tables/08b_model_test_predictions.csv
# outputs/tables/08c_random_forest_feature_importance.csv
#
# OUTPUTS:
# outputs/tables/12a-12i
# outputs/figures/20a-20e
# outputs/reports/06a_model_interpretation_methods.txt
#
# ============================================================


# 1. INITIAL SETUP --------------------------------------------

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(forcats)


cat("\n")
cat("============================================================\n")
cat("MODEL INTERPRETATION AND DECISION SUPPORT\n")
cat("============================================================\n")


# 2. DEFINE DIRECTORIES ---------------------------------------

table_directory <- "outputs/tables"
figure_directory <- "outputs/figures"
report_directory <- "outputs/reports"

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


# 3. LOAD EXISTING PROJECT OUTPUTS ----------------------------

performance_file <- file.path(
  table_directory,
  "08a_model_performance.csv"
)

predictions_file <- file.path(
  table_directory,
  "08b_model_test_predictions.csv"
)

importance_file <- file.path(
  table_directory,
  "08c_random_forest_feature_importance.csv"
)

required_files <- c(
  performance_file,
  predictions_file,
  importance_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste(
      "Required project files are missing:",
      paste(missing_files, collapse = "\n"),
      sep = "\n"
    )
  )
}

model_performance <- read_csv(
  performance_file,
  show_col_types = FALSE
)

all_predictions <- read_csv(
  predictions_file,
  show_col_types = FALSE
)

feature_importance <- read_csv(
  importance_file,
  show_col_types = FALSE
)


# 4. STANDARDISE COLUMN NAMES -------------------------------

standardise_names <- function(data) {
  
  names(data) <- names(data) |>
    str_trim() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
  
  data
}

model_performance <- standardise_names(
  model_performance
)

all_predictions <- standardise_names(
  all_predictions
)

feature_importance <- standardise_names(
  feature_importance
)


# 5. CHECK REQUIRED PREDICTION FIELDS -------------------------

cat("\nPREDICTION DATA STRUCTURE\n")

print(names(all_predictions))

required_prediction_fields <- c(
  "task",
  "location"
)

missing_prediction_fields <- setdiff(
  required_prediction_fields,
  names(all_predictions)
)

if (length(missing_prediction_fields) > 0) {
  stop(
    paste(
      "Missing required prediction fields:",
      paste(missing_prediction_fields, collapse = ", ")
    )
  )
}


# Identify actual observation column.

actual_candidates <- c(
  "actual",
  "actual_value",
  "observed",
  "observed_value"
)

actual_column <- intersect(
  actual_candidates,
  names(all_predictions)
)

if (length(actual_column) != 1) {
  stop(
    paste(
      "Could not identify exactly one actual-value column.",
      "Available columns:",
      paste(names(all_predictions), collapse = ", ")
    )
  )
}

actual_column <- actual_column[1]


# Identify prediction columns.

prediction_columns <- intersect(
  c(
    "persistence_prediction",
    "linear_prediction",
    "random_forest_prediction"
  ),
  names(all_predictions)
)

if (length(prediction_columns) != 3) {
  stop(
    paste(
      "Expected all three prediction columns.",
      "Available prediction columns:",
      paste(prediction_columns, collapse = ", ")
    )
  )
}


# 6. PREPARE LONG-FORM PREDICTION DATA ------------------------

prediction_long <- all_predictions |>
  pivot_longer(
    cols = all_of(prediction_columns),
    names_to = "model",
    values_to = "predicted"
  ) |>
  mutate(
    model = recode(
      model,
      persistence_prediction = "Persistence",
      linear_prediction = "Linear regression",
      random_forest_prediction = "Random forest"
    ),
    
    actual = .data[[actual_column]],
    
    residual = predicted - actual,
    
    absolute_error = abs(residual),
    
    squared_error = residual^2
  ) |>
  filter(
    is.finite(actual),
    is.finite(predicted),
    is.finite(residual)
  )

if (nrow(prediction_long) == 0) {
  stop("No valid prediction records remain after preparation.")
}

cat("\nPREDICTION RECORDS\n")

cat(
  "Long-format records:",
  nrow(prediction_long),
  "\n"
)

cat(
  "Forecasting tasks:",
  n_distinct(prediction_long$task),
  "\n"
)

cat(
  "Locations:",
  n_distinct(prediction_long$location),
  "\n"
)


# 7. CALCULATE MODEL PERFORMANCE -----------------------------

performance_summary <- prediction_long |>
  group_by(task, model) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    mean_bias = mean(residual),
    
    median_absolute_error = median(absolute_error),
    
    error_sd = sd(residual),
    
    .groups = "drop"
  ) |>
  group_by(task) |>
  mutate(
    persistence_MAE = MAE[
      model == "Persistence"
    ][1],
    
    MAE_skill_vs_persistence =
      1 - MAE / persistence_MAE,
    
    model_rank = rank(
      MAE,
      ties.method = "min"
    )
  ) |>
  ungroup() |>
  arrange(task, model_rank)


write_csv(
  performance_summary,
  file.path(
    table_directory,
    "12a_interpreted_model_performance.csv"
  )
)


# 8. LOCATION-SPECIFIC MODEL PERFORMANCE ---------------------

location_performance <- prediction_long |>
  group_by(task, location, model) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    mean_bias = mean(residual),
    
    median_absolute_error = median(absolute_error),
    
    .groups = "drop"
  ) |>
  group_by(task, location) |>
  mutate(
    persistence_MAE = MAE[
      model == "Persistence"
    ][1],
    
    MAE_skill_vs_persistence =
      1 - MAE / persistence_MAE
  ) |>
  ungroup() |>
  arrange(task, location, MAE)


write_csv(
  location_performance,
  file.path(
    table_directory,
    "12b_location_specific_model_performance.csv"
  )
)



# 9. SEASONAL FORECAST PERFORMANCE ---------------------------

# Hourly predictions use their target observation timestamp.
# Daily predictions use their observation date.
# The daily date represents the day on which the next-day
# forecast was issued.

prediction_long <- prediction_long |>
  mutate(
    forecast_date = case_when(
      frequency == "hourly" ~ as.Date(
        observation_timestamp,
        tz = "Africa/Lagos"
      ),
      
      frequency == "daily" ~ as.Date(
        observation_date
      ),
      
      TRUE ~ as.Date(NA)
    )
  )

# Stop if any prediction could not be assigned a date.

if (any(is.na(prediction_long$forecast_date))) {
  stop(
    "Some predictions have missing or unrecognised forecast dates. ",
    "Check observation_timestamp and observation_date."
  )
}

# Create calendar-quarter groupings in chronological order.

prediction_long <- prediction_long |>
  mutate(
    month_number = month(forecast_date),
    
    month_name = factor(
      month.abb[month_number],
      levels = month.abb,
      ordered = TRUE
    ),
    
    quarter = paste0(
      "Q",
      quarter(forecast_date)
    ),
    
    season = case_when(
      month_number %in% c(1, 2, 3) ~ "JFM",
      month_number %in% c(4, 5, 6) ~ "AMJ",
      month_number %in% c(7, 8, 9) ~ "JAS",
      month_number %in% c(10, 11, 12) ~ "OND"
    ),
    
    season = factor(
      season,
      levels = c("JFM", "AMJ", "JAS", "OND"),
      ordered = TRUE
    )
  )

# Confirm all four quarters are represented.

cat("\nQUARTERLY PREDICTION COUNTS\n")

print(
  prediction_long |>
    count(frequency, season),
  n = Inf
)

seasonal_performance <- prediction_long |>
  group_by(task, location, model, season) |>
  summarise(
    observations = n(),
    
    MAE = mean(absolute_error),
    
    RMSE = sqrt(mean(squared_error)),
    
    mean_bias = mean(residual),
    
    .groups = "drop"
  ) |>
  group_by(task, location, season) |>
  mutate(
    persistence_MAE = MAE[
      model == "Persistence"
    ][1],
    
    MAE_skill_vs_persistence =
      1 - MAE / persistence_MAE
  ) |>
  ungroup() |>
  arrange(task, location, season, MAE)

write_csv(
  seasonal_performance,
  file.path(
    table_directory,
    "12c_seasonal_model_performance.csv"
  )
)


# 10. FORECAST BIAS AND ERROR DIAGNOSTICS ---------------------

bias_summary <- prediction_long |>
  group_by(task, location, model) |>
  summarise(
    observations = n(),
    
    mean_bias = mean(residual),
    
    median_bias = median(residual),
    
    bias_sd = sd(residual),
    
    mean_absolute_error = mean(absolute_error),
    
    percentage_overpredicted = mean(
      residual > 0
    ) * 100,
    
    percentage_underpredicted = mean(
      residual < 0
    ) * 100,
    
    percentage_exact = mean(
      abs(residual) < 1e-8
    ) * 100,
    
    .groups = "drop"
  )


write_csv(
  bias_summary,
  file.path(
    table_directory,
    "12d_forecast_bias_diagnostics.csv"
  )
)


# 11. RANDOM FOREST FEATURE IMPORTANCE -----------------------

cat("\nFEATURE IMPORTANCE STRUCTURE\n")

print(names(feature_importance))

importance_required <- c(
  "task",
  "feature",
  "importance"
)

missing_importance_fields <- setdiff(
  importance_required,
  names(feature_importance)
)

if (length(missing_importance_fields) > 0) {
  stop(
    paste(
      "Missing feature-importance fields:",
      paste(missing_importance_fields, collapse = ", ")
    )
  )
}


top_features <- feature_importance |>
  filter(
    is.finite(importance)
  ) |>
  group_by(task) |>
  arrange(
    desc(importance),
    .by_group = TRUE
  ) |>
  mutate(
    importance_rank = row_number()
  ) |>
  filter(
    importance_rank <= 10
  ) |>
  ungroup()


write_csv(
  top_features,
  file.path(
    table_directory,
    "12e_top_random_forest_predictors.csv"
  )
)


# 12. SUMMARISE MODEL SELECTION -------------------------------

model_selection <- performance_summary |>
  group_by(task) |>
  slice_min(
    order_by = MAE,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  select(
    task,
    selected_model = model,
    selected_MAE = MAE,
    selected_RMSE = RMSE,
    selected_bias = mean_bias,
    MAE_skill_vs_persistence
  )


write_csv(
  model_selection,
  file.path(
    table_directory,
    "12f_selected_model_summary.csv"
  )
)


# 13. IDENTIFY LOCATION-SPECIFIC PERFORMANCE VARIATION -------

location_variation <- location_performance |>
  group_by(task, location) |>
  slice_min(
    order_by = MAE,
    n = 1,
    with_ties = FALSE
  ) |>
  ungroup() |>
  group_by(task) |>
  summarise(
    best_location_MAE = min(MAE),
    
    worst_location_MAE = max(MAE),
    
    MAE_range = worst_location_MAE -
      best_location_MAE,
    
    location_count = n(),
    
    .groups = "drop"
  )


write_csv(
  location_variation,
  file.path(
    table_directory,
    "12g_location_performance_variation.csv"
  )
)


# 14. CREATE INTERPRETATION FLAGS -----------------------------

decision_support <- model_selection |>
  mutate(
    performance_interpretation = case_when(
      MAE_skill_vs_persistence > 0 ~
        "Selected model improves on persistence",
      
      MAE_skill_vs_persistence == 0 ~
        "Selected model matches persistence",
      
      TRUE ~
        "Selected model underperforms persistence"
    ),
    
    bias_interpretation = case_when(
      abs(selected_bias) < 0.1 ~
        "Small average bias",
      
      selected_bias >= 0.1 ~
        "Average overprediction",
      
      selected_bias <= -0.1 ~
        "Average underprediction",
      
      TRUE ~
        "Review forecast bias"
    ),
    
    operational_interpretation =
      "Use forecasts as environmental decision-support information; verify local conditions."
  )


write_csv(
  decision_support,
  file.path(
    table_directory,
    "12h_model_decision_support_summary.csv"
  )
)


# 15. CREATE MODEL PERFORMANCE CHART --------------------------

performance_plot <- ggplot(
  performance_summary,
  aes(
    x = model,
    y = MAE,
    fill = model
  )
) +
  geom_col(
    show.legend = FALSE
  ) +
  facet_wrap(
    ~ task,
    scales = "free_y"
  ) +
  labs(
    title = "Forecast Error Across the Six Prediction Tasks",
    subtitle = "Lower mean absolute error indicates better average accuracy",
    x = NULL,
    y = "Mean absolute error (°C)"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold"
    )
  )


ggsave(
  file.path(
    figure_directory,
    "20a_interpreted_model_performance.png"
  ),
  performance_plot,
  width = 15,
  height = 10,
  dpi = 320,
  bg = "white"
)


# 16. CREATE LOCATION PERFORMANCE CHART -----------------------

location_plot <- ggplot(
  location_performance,
  aes(
    x = location,
    y = MAE,
    fill = model
  )
) +
  geom_col(
    position = "dodge"
  ) +
  facet_wrap(
    ~ task,
    scales = "free_y"
  ) +
  labs(
    title = "Forecast Performance Across the Three Locations",
    subtitle = "Model errors are evaluated separately for Abuja, Ilorin and Lagos",
    x = NULL,
    y = "Mean absolute error (°C)",
    fill = "Model"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    strip.text = element_text(
      face = "bold"
    )
  )


ggsave(
  file.path(
    figure_directory,
    "20b_location_model_performance.png"
  ),
  location_plot,
  width = 15,
  height = 10,
  dpi = 320,
  bg = "white"
)



# 17. CREATE SEASONAL PERFORMANCE CHART -----------------------

seasonal_plot <- ggplot(
  seasonal_performance,
  aes(
    x = season,
    y = MAE,
    colour = model,
    group = model
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 2
  ) +
  facet_grid(
    task ~ location,
    scales = "free_y"
  ) +
  scale_x_discrete(
    limits = c("JFM", "AMJ", "JAS", "OND"),
    drop = FALSE
  ) +
  labs(
    title = "Quarterly Variation in Forecast Error",
    subtitle = paste(
      "Model performance across calendar quarters",
      "in the 2025 test period"
    ),
    x = "Calendar quarter",
    y = "Mean absolute error (°C)",
    colour = "Model"
  ) +
  theme_minimal(
    base_size = 10
  ) +
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    panel.spacing = grid::unit(
      0.8,
      "lines"
    )
  )

ggsave(
  file.path(
    figure_directory,
    "20c_seasonal_model_performance.png"
  ),
  seasonal_plot,
  width = 16,
  height = 13,
  dpi = 320,
  bg = "white"
)


# 18. CREATE FORECAST BIAS CHART ------------------------------

bias_plot <- ggplot(
  bias_summary,
  aes(
    x = location,
    y = mean_bias,
    fill = model
  )
) +
  geom_col(
    position = "dodge"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  facet_wrap(
    ~ task,
    scales = "free_y"
  ) +
  labs(
    title = "Average Forecast Bias by Location",
    subtitle = "Positive values indicate overprediction; negative values indicate underprediction",
    x = NULL,
    y = "Mean forecast bias (°C)",
    fill = "Model"
  ) +
  theme_minimal(
    base_size = 11
  )


ggsave(
  file.path(
    figure_directory,
    "20d_forecast_bias_by_location.png"
  ),
  bias_plot,
  width = 15,
  height = 10,
  dpi = 320,
  bg = "white"
)


# 19. CREATE FEATURE IMPORTANCE CHART -------------------------

importance_plot <- ggplot(
  top_features,
  aes(
    x = reorder_within(
      feature,
      importance,
      task
    ),
    y = importance
  )
) +
  geom_col() +
  coord_flip() +
  facet_wrap(
    ~ task,
    scales = "free"
  ) +
  scale_x_reordered() +
  labs(
    title = "Most Influential Predictors in the Random Forest Models",
    subtitle = "Permutation importance; higher values indicate greater model reliance",
    x = NULL,
    y = "Permutation importance"
  ) +
  theme_minimal(
    base_size = 10
  )


ggsave(
  file.path(
    figure_directory,
    "20e_random_forest_feature_importance.png"
  ),
  importance_plot,
  width = 16,
  height = 13,
  dpi = 320,
  bg = "white"
)


# 20. QUALITY CHECKS ------------------------------------------

quality_checks <- tibble(
  check = c(
    "Prediction records available",
    "Six forecasting tasks available",
    "Three model types available",
    "Three locations available",
    "No missing actual values",
    "No missing predictions",
    "All model performance metrics finite",
    "All model tasks have persistence baseline",
    "All model tasks have selected models"
  ),
  
  result = c(
    nrow(prediction_long) > 0,
    
    n_distinct(prediction_long$task) == 6,
    
    n_distinct(prediction_long$model) == 3,
    
    n_distinct(prediction_long$location) == 3,
    
    all(is.finite(prediction_long$actual)),
    
    all(is.finite(prediction_long$predicted)),
    
    all(is.finite(performance_summary$MAE)) &&
      all(is.finite(performance_summary$RMSE)),
    
    all(
      prediction_long |>
        distinct(task, model) |>
        count(task) |>
        pull(n) == 3
    ),
    
    nrow(model_selection) == 6
  )
)


write_csv(
  quality_checks,
  file.path(
    table_directory,
    "12i_model_interpretation_quality_checks.csv"
  )
)


cat("\nQUALITY CHECK RESULTS\n")

print(
  quality_checks,
  n = Inf
)

if (any(!quality_checks$result)) {
  stop(
    "One or more model interpretation quality checks failed."
  )
}


# 21. SAVE METHODS AND INTERPRETATION REPORT ------------------

report_lines <- c(
  
  "HEAT EXPOSURE INTELLIGENCE",
  
  "SCRIPT 10: MODEL INTERPRETATION AND DECISION SUPPORT",
  
  "",
  
  "1. PURPOSE",
  
  "This analysis interprets the existing predictive models using",
  "their held-out 2025 test predictions.",
  
  "",
  
  "2. MODEL DESIGN",
  
  "Three forecasting approaches were evaluated:",
  "- Persistence baseline",
  "- Multiple linear regression",
  "- Random forest",
  
  "The six prediction tasks cover hourly and daily temperature",
  "forecasting for apparent temperature, wet-bulb temperature",
  "and air temperature.",
  
  "",
  
  "3. PERFORMANCE INTERPRETATION",
  
  "Mean absolute error measures average absolute forecast error.",
  "Root mean squared error gives greater weight to larger errors.",
  "Mean bias measures systematic overprediction or underprediction.",
  "Positive bias indicates overprediction.",
  "Negative bias indicates underprediction.",
  
  "MAE skill relative to persistence is calculated as:",
  "1 - (model MAE / persistence MAE).",
  
  "Positive values indicate improvement over persistence.",
  "Negative values indicate worse MAE than persistence.",
  
  "",
  
  "4. SEASONAL AND LOCATION-SPECIFIC ASSESSMENT",
  
  "Performance is assessed separately by location and by four",
  "three-month calendar-quarter groupings using the 2025 test period.",
  "These results describe the evaluated test year and should not",
  "be interpreted as definitive long-term seasonal performance.",
  
  "",
  
  "5. FEATURE IMPORTANCE",
  
  "Random forest permutation importance describes the relative",
  "predictive contribution of variables within each fitted model.",
  "It does not establish causation.",
  "Correlated predictors may share or redistribute importance.",
  "Importance values should not be compared directly across tasks.",
  
  "",
  
  "6. OPERATIONAL INTERPRETATION",
  
  "Forecasts may help organisations identify periods and locations",
  "requiring closer environmental monitoring and operational review.",
  "Potential responses may include reviewing work schedules,",
  "hydration provision, rest opportunities and access to shade.",
  
  "These outputs do not independently determine whether a worksite",
  "is safe or unsafe.",
  
  "",
  
  "7. SCIENTIFIC LIMITATIONS",
  
  "The environmental dataset is based on ERA5 reanalysis obtained",
  "through the Open-Meteo Historical Weather API.",
  "It is not direct measurement of conditions at individual worksites.",
  
  "Wet-bulb temperature is not Wet-Bulb Globe Temperature (WBGT).",
  
  "No worker health, workload, clothing, acclimatisation or",
  "individual exposure data were available.",
  
  "Consequently, the models do not predict heat illness,",
  "worker productivity or individual occupational risk.",
  
  "Historical P90 and P95 benchmarks are statistical reference",
  "values, not occupational safety limits.",
  
  "The three study locations do not represent all of Nigeria.",
  
  "",
  
  "8. TEST-PERIOD LIMITATION",
  
  "The seasonal assessment uses the 2025 holdout period.",
  "It provides an initial assessment of seasonal variation,",
  "not a multi-year independent seasonal validation.",
  
  "",
  
  "9. OUTPUTS",
  
  "Tables 12a-12i contain interpreted model performance,",
  "location-specific and seasonal performance, bias diagnostics,",
  "feature importance, model selection, decision-support summaries",
  "and quality checks.",
  
  "Figures 20a-20e visualise performance, location differences,",
  "seasonal errors, forecast bias and predictor importance.",
  
  "",
  
  "END OF REPORT"
)


writeLines(
  report_lines,
  file.path(
    report_directory,
    "06a_model_interpretation_methods.txt"
  )
)


# 22. FINAL SUMMARY -------------------------------------------

cat("\n")
cat("============================================================\n")
cat("SCRIPT 10 COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")

cat(
  "\nPrediction records analysed:",
  nrow(prediction_long),
  "\n"
)

cat(
  "Forecasting tasks:",
  n_distinct(prediction_long$task),
  "\n"
)

cat(
  "Locations:",
  n_distinct(prediction_long$location),
  "\n"
)

cat("\nSELECTED MODELS\n")

print(
  model_selection,
  n = Inf
)

cat("\nQUALITY CHECKS\n")

print(
  quality_checks,
  n = Inf
)

cat("\nOUTPUTS SAVED TO:\n")

cat("- outputs/tables/12a-12i\n")
cat("- outputs/figures/20a-20e\n")
cat("- outputs/reports/06a_model_interpretation_methods.txt\n")

cat("\n============================================================\n")
