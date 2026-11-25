
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 04 - EXPLORATORY DATA ANALYSIS
# Revised visualisation and reporting edition
# ============================================================

library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. PROJECT PATHS AND OUTPUT DIRECTORIES
# ============================================================

processed_file <- "data/processed/Nigeria_Weather_Cleaned.csv"

table_directory <- "outputs/tables"
figure_directory <- "outputs/figures"

dir.create(table_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_directory, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. LOAD AND VALIDATE PREPARED DATA
# ============================================================

weather_all <- read_csv(
  processed_file,
  show_col_types = FALSE
)

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
  "apparent_temperature"
)

missing_columns <- setdiff(
  required_columns,
  names(weather_all)
)

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Required columns missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

if (!inherits(weather_all$timestamp, "POSIXct")) {
  stop(
    "Timestamp is not POSIXct. Check Script 03 before proceeding."
  )
}

weather_all <- weather_all |>
  mutate(
    timestamp = with_tz(
      timestamp,
      tzone = "Africa/Lagos"
    )
  ) |>
  arrange(location, timestamp)

if (anyNA(weather_all$timestamp)) {
  stop("Missing or invalid timestamps detected.")
}

# Do not silently continue with missing or non-finite
# measurements in the main numerical analysis.

numeric_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "wind_direction_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

non_finite_summary <- weather_all |>
  summarise(
    across(
      all_of(numeric_variables),
      ~sum(!is.finite(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "non_finite_records"
  )

write_csv(
  non_finite_summary,
  file.path(
    table_directory,
    "01a_non_finite_values_check.csv"
  )
)

if (any(non_finite_summary$non_finite_records > 0)) {
  stop(
    paste(
      "Non-finite values detected.",
      "Inspect 01a_non_finite_values_check.csv before analysis."
    )
  )
}

# ============================================================
# 3. HUMAN-READABLE VARIABLE LABELS
# ============================================================

variable_labels <- c(
  temperature_2m = "Air temperature (°C)",
  relative_humidity_2m = "Relative humidity (%)",
  wind_speed_10m = "Wind speed (km/h)",
  wind_direction_10m = "Wind direction (degrees)",
  shortwave_radiation = "Incoming solar radiation (W/m²)",
  wet_bulb_temperature_2m = "Wet-bulb temperature (°C)",
  apparent_temperature = "Apparent temperature (°C)"
)

thermal_variables <- c(
  "temperature_2m",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

supporting_variables <- c(
  "relative_humidity_2m",
  "wind_speed_10m"
)

# ============================================================
# 4. CREATE TIME VARIABLES
# ============================================================

weather_all <- weather_all |>
  mutate(
    date = as.Date(timestamp),
    year = year(timestamp),
    month_number = month(timestamp),
    month_label = factor(
      month(timestamp, label = TRUE, abbr = TRUE),
      levels = month.abb
    ),
    hour = hour(timestamp)
  )

cat("\n--- DATA DIMENSIONS ---\n")
print(dim(weather_all))

cat("\n--- RECORDS BY LOCATION ---\n")
print(count(weather_all, location))

# ============================================================
# 5. LONG-FORMAT DATA FOR MULTI-VARIABLE ANALYSIS
# ============================================================

weather_long <- weather_all |>
  select(
    location,
    timestamp,
    date,
    year,
    month_number,
    month_label,
    hour,
    all_of(numeric_variables)
  ) |>
  pivot_longer(
    cols = all_of(numeric_variables),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    variable_label = unname(
      variable_labels[variable]
    )
  )

# ============================================================
# 6. DESCRIPTIVE STATISTICS
# ============================================================

descriptive_statistics <- weather_long |>
  group_by(
    location,
    variable,
    variable_label
  ) |>
  summarise(
    observations = sum(!is.na(value)),
    missing_values = sum(is.na(value)),
    mean = mean(value, na.rm = TRUE),
    standard_deviation = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    minimum = min(value, na.rm = TRUE),
    first_quartile = quantile(
      value,
      0.25,
      na.rm = TRUE
    ),
    third_quartile = quantile(
      value,
      0.75,
      na.rm = TRUE
    ),
    ninetieth_percentile = quantile(
      value,
      0.90,
      na.rm = TRUE
    ),
    ninety_fifth_percentile = quantile(
      value,
      0.95,
      na.rm = TRUE
    ),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  descriptive_statistics,
  file.path(
    table_directory,
    "01b_descriptive_statistics.csv"
  )
)

cat("\n--- DESCRIPTIVE STATISTICS ---\n")
print(descriptive_statistics, n = Inf)

# ============================================================
# 7. COMMON VISUALISATION THEME
# ============================================================

heat_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 17,
      face = "bold",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 10,
      colour = "grey35",
      hjust = 0
    ),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(
      size = 12,
      face = "bold"
    ),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 18, 14, 18)
  )

# ============================================================
# 8. WEATHER VARIABLE DISTRIBUTIONS
# ============================================================

# Thermal variables are shown together because they are
# all measured in degrees Celsius.

thermal_long <- weather_long |>
  filter(variable %in% thermal_variables) |>
  mutate(
    variable_label = factor(
      variable_label,
      levels = unname(variable_labels[thermal_variables])
    )
  )

distribution_plot <- ggplot(
  thermal_long,
  aes(
    x = value,
    colour = location
  )
) +
  geom_density(
    linewidth = 0.9,
    adjust = 1
  ) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Distribution of Temperature-Related Conditions",
    subtitle = "How frequently different temperature values occurred in each location, 2021–2025",
    x = "Measured temperature (°C)",
    y = "Density",
    colour = "Location",
    caption = paste(
      "Density curves show the relative concentration of observations.",
      "They do not represent the number of hours directly."
    )
  ) +
  heat_theme

print(distribution_plot)

ggsave(
  file.path(
    figure_directory,
    "01a_weather_variable_distributions.png"
  ),
  distribution_plot,
  width = 12,
  height = 13,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# Supporting environmental distributions

supporting_distribution <- weather_long |>
  filter(variable %in% supporting_variables) |>
  mutate(
    variable_label = factor(
      variable_label,
      levels = unname(variable_labels[supporting_variables])
    )
  )

supporting_distribution_plot <- ggplot(
  supporting_distribution,
  aes(
    x = value,
    colour = location
  )
) +
  geom_density(
    linewidth = 0.9,
    adjust = 1
  ) +
  facet_wrap(
    ~variable_label,
    scales = "free",
    ncol = 1
  ) +
  labs(
    title = "Distribution of Humidity and Wind Speed",
    subtitle = "Historical hourly observations by location",
    x = "Measured value",
    y = "Density",
    colour = "Location"
  ) +
  heat_theme

print(supporting_distribution_plot)

ggsave(
  file.path(
    figure_directory,
    "01b_humidity_and_wind_distributions.png"
  ),
  supporting_distribution_plot,
  width = 12,
  height = 9,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 9. LOCATION COMPARISON BOXPLOTS
# ============================================================

# Wind direction is deliberately excluded:
# it is circular data and ordinary boxplots can mislead.
#
# Solar radiation is also kept separate because it has
# many zero/night-time observations.


# ============================================================
# 9. LOCATION COMPARISON BOXPLOTS
# ============================================================

boxplot_variables <- c(
  "temperature_2m",
  "wet_bulb_temperature_2m",
  "apparent_temperature",
  "relative_humidity_2m",
  "wind_speed_10m"
)

boxplot_data <- weather_long |>
  filter(variable %in% boxplot_variables) |>
  mutate(
    variable_label = factor(
      variable_label,
      levels = unname(variable_labels[boxplot_variables])
    )
  )

location_boxplot <- ggplot(
  boxplot_data,
  aes(
    x = location,
    y = value,
    fill = location
  )
) +
  geom_boxplot(
    width = 0.65,
    outlier.alpha = 0.12,
    outlier.size = 0.35,
    show.legend = FALSE
  ) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title = "How Environmental Conditions Differ Across Locations",
    subtitle = "Comparison of the median, middle 50% and potential outliers",
    x = "Location",
    y = "Measured value",
    caption = paste(
      "Units differ by panel and are stated in each variable label.",
      "Potential outliers are observations requiring investigation, not automatically errors."
    )
  ) +
  heat_theme

print(location_boxplot)

ggsave(
  file.path(
    figure_directory,
    "02a_location_distribution_boxplots.png"
  ),
  location_boxplot,
  width = 14,
  height = 13,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 10. HOURLY ENVIRONMENTAL PATTERNS
# ============================================================

hourly_profile <- weather_long |>
  group_by(
    location,
    variable,
    variable_label,
    hour
  ) |>
  summarise(
    observations = sum(!is.na(value)),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    ninetieth_percentile = quantile(
      value,
      0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  hourly_profile,
  file.path(
    table_directory,
    "01c_hourly_profiles.csv"
  )
)

# Thermal profile

hourly_thermal <- hourly_profile |>
  filter(variable %in% thermal_variables)

hourly_thermal_plot <- ggplot(
  hourly_thermal,
  aes(
    x = hour,
    y = mean_value,
    colour = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  labs(
    title = "Hourly Changes in Temperature-Related Conditions",
    subtitle = "Average conditions for each hour, based on 2021–2025 observations",
    x = "Hour of day (Nigeria local time)",
    y = "Average temperature (°C)",
    colour = "Location",
    caption = "Each point represents the average of observations recorded at that hour across the five-year period."
  ) +
  heat_theme

print(hourly_thermal_plot)

ggsave(
  file.path(
    figure_directory,
    "03a_hourly_environmental_profiles.png"
  ),
  hourly_thermal_plot,
  width = 12,
  height = 13,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# Humidity and wind speed profile

hourly_supporting <- hourly_profile |>
  filter(variable %in% supporting_variables)

hourly_supporting_plot <- ggplot(
  hourly_supporting,
  aes(
    x = hour,
    y = mean_value,
    colour = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  labs(
    title = "Hourly Changes in Humidity and Wind Speed",
    subtitle = "Average hourly conditions across the study period",
    x = "Hour of day (Nigeria local time)",
    y = "Average measured value",
    colour = "Location"
  ) +
  heat_theme

print(hourly_supporting_plot)

ggsave(
  file.path(
    figure_directory,
    "03b_hourly_humidity_and_wind.png"
  ),
  hourly_supporting_plot,
  width = 12,
  height = 9,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# Solar radiation profile

hourly_radiation <- hourly_profile |>
  filter(variable == "shortwave_radiation")

hourly_radiation_plot <- ggplot(
  hourly_radiation,
  aes(
    x = hour,
    y = mean_value,
    colour = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    limits = c(0, 23)
  ) +
  labs(
    title = "Hourly Changes in Solar Radiation",
    subtitle = "Average hourly incoming shortwave radiation",
    x = "Hour of day (Nigeria local time)",
    y = "Average solar radiation (W/m²)",
    colour = "Location",
    caption = "Night-time values are expected to be zero or close to zero."
  ) +
  heat_theme

print(hourly_radiation_plot)

ggsave(
  file.path(
    figure_directory,
    "03c_hourly_solar_radiation.png"
  ),
  hourly_radiation_plot,
  width = 12,
  height = 6,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 11. MONTHLY ENVIRONMENTAL PATTERNS
# ============================================================

monthly_profile <- weather_long |>
  group_by(
    location,
    variable,
    variable_label,
    month_number,
    month_label
  ) |>
  summarise(
    observations = sum(!is.na(value)),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    ninetieth_percentile = quantile(
      value,
      0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(location, variable, month_number)

write_csv(
  monthly_profile,
  file.path(
    table_directory,
    "01d_monthly_profiles.csv"
  )
)

# Thermal monthly patterns

monthly_thermal <- monthly_profile |>
  filter(variable %in% thermal_variables)

monthly_thermal_plot <- ggplot(
  monthly_thermal,
  aes(
    x = month_number,
    y = mean_value,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Changes in Temperature-Related Conditions",
    subtitle = "Average monthly conditions across 2021–2025",
    x = "Month",
    y = "Average temperature (°C)",
    colour = "Location",
    caption = "Values combine observations from the same calendar month across the five-year study period."
  ) +
  heat_theme

print(monthly_thermal_plot)

ggsave(
  file.path(
    figure_directory,
    "04a_monthly_environmental_profiles.png"
  ),
  monthly_thermal_plot,
  width = 12,
  height = 13,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# Supporting monthly patterns

monthly_supporting <- monthly_profile |>
  filter(variable %in% supporting_variables)

monthly_supporting_plot <- ggplot(
  monthly_supporting,
  aes(
    x = month_number,
    y = mean_value,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Changes in Humidity and Wind Speed",
    subtitle = "Average monthly conditions across 2021–2025",
    x = "Month",
    y = "Average measured value",
    colour = "Location"
  ) +
  heat_theme

print(monthly_supporting_plot)

ggsave(
  file.path(
    figure_directory,
    "04b_monthly_humidity_and_wind.png"
  ),
  monthly_supporting_plot,
  width = 12,
  height = 9,
  dpi = 320,
  bg = "white"
)

# Solar radiation by month

monthly_radiation <- monthly_profile |>
  filter(variable == "shortwave_radiation")

monthly_radiation_plot <- ggplot(
  monthly_radiation,
  aes(
    x = month_number,
    y = mean_value,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Monthly Changes in Incoming Solar Radiation",
    subtitle = "Average monthly incoming shortwave radiation",
    x = "Month",
    y = "Average solar radiation (W/m²)",
    colour = "Location"
  ) +
  heat_theme

print(monthly_radiation_plot)

ggsave(
  file.path(
    figure_directory,
    "04c_monthly_solar_radiation.png"
  ),
  monthly_radiation_plot,
  width = 12,
  height = 6,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 12. ANNUAL ENVIRONMENTAL PATTERNS
# ============================================================

annual_profile <- weather_long |>
  group_by(
    location,
    variable,
    variable_label,
    year
  ) |>
  summarise(
    observations = sum(!is.na(value)),
    mean_value = mean(value, na.rm = TRUE),
    median_value = median(value, na.rm = TRUE),
    ninetieth_percentile = quantile(
      value,
      0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(location, variable, year)

write_csv(
  annual_profile,
  file.path(
    table_directory,
    "01e_annual_profiles.csv"
  )
)

# Thermal annual patterns

annual_thermal <- annual_profile |>
  filter(variable %in% thermal_variables)

annual_thermal_plot <- ggplot(
  annual_thermal,
  aes(
    x = year,
    y = mean_value,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = sort(unique(weather_all$year))
  ) +
  labs(
    title = "Annual Changes in Temperature-Related Conditions",
    subtitle = "Annual average conditions from 2021 to 2025",
    x = "Year",
    y = "Annual average temperature (°C)",
    colour = "Location",
    caption = paste(
      "Five years of data describe this study period.",
      "They are not sufficient on their own to establish a long-term climate trend."
    )
  ) +
  heat_theme

print(annual_thermal_plot)

ggsave(
  file.path(
    figure_directory,
    "05a_annual_environmental_profiles.png"
  ),
  annual_thermal_plot,
  width = 12,
  height = 13,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# Supporting annual patterns

annual_supporting <- annual_profile |>
  filter(variable %in% supporting_variables)

annual_supporting_plot <- ggplot(
  annual_supporting,
  aes(
    x = year,
    y = mean_value,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  facet_wrap(
    ~variable_label,
    scales = "free_y",
    ncol = 1
  ) +
  scale_x_continuous(
    breaks = sort(unique(weather_all$year))
  ) +
  labs(
    title = "Annual Changes in Humidity and Wind Speed",
    subtitle = "Annual average conditions from 2021 to 2025",
    x = "Year",
    y = "Annual average",
    colour = "Location"
  ) +
  heat_theme

print(annual_supporting_plot)

ggsave(
  file.path(
    figure_directory,
    "05b_annual_humidity_and_wind.png"
  ),
  annual_supporting_plot,
  width = 12,
  height = 9,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 13. DAILY MAXIMUM ENVIRONMENTAL CONDITIONS
# ============================================================

daily_maximums <- weather_all |>
  group_by(location, date) |>
  summarise(
    maximum_air_temperature =
      max(temperature_2m, na.rm = TRUE),
    
    maximum_wet_bulb_temperature =
      max(wet_bulb_temperature_2m, na.rm = TRUE),
    
    maximum_apparent_temperature =
      max(apparent_temperature, na.rm = TRUE),
    
    maximum_relative_humidity =
      max(relative_humidity_2m, na.rm = TRUE),
    
    maximum_solar_radiation =
      max(shortwave_radiation, na.rm = TRUE),
    
    .groups = "drop"
  )

write_csv(
  daily_maximums,
  file.path(
    table_directory,
    "01f_daily_maximum_conditions.csv"
  )
)

# ============================================================
# 14. HISTORICALLY HIGH TEMPERATURE HOURS
# ============================================================

# P90 is a location-specific historical percentile.
# It is NOT a health or occupational safety threshold.

temperature_thresholds <- weather_all |>
  group_by(location) |>
  summarise(
    historical_p90_temperature = quantile(
      temperature_2m,
      0.90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  temperature_thresholds,
  file.path(
    table_directory,
    "02a_location_temperature_thresholds.csv"
  )
)

temperature_exceedances <- weather_all |>
  left_join(
    temperature_thresholds,
    by = "location"
  ) |>
  mutate(
    above_location_p90 =
      temperature_2m > historical_p90_temperature
  )

monthly_exceedances <- temperature_exceedances |>
  group_by(
    location,
    month_number,
    month_label
  ) |>
  summarise(
    total_observed_hours = n(),
    hours_above_historical_threshold = sum(
      above_location_p90,
      na.rm = TRUE
    ),
    percentage_of_hours_above_threshold =
      100 * mean(
        above_location_p90,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) |>
  arrange(location, month_number)

write_csv(
  monthly_exceedances,
  file.path(
    table_directory,
    "02b_monthly_temperature_exceedances.csv"
  )
)

# Percentage chart

exceedance_percentage_plot <- ggplot(
  monthly_exceedances,
  aes(
    x = month_number,
    y = percentage_of_hours_above_threshold,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "How Often Temperatures Exceeded Historical High-Temperature Levels",
    subtitle = "Percentage of observed hours above each location's own historical 90th-percentile temperature",
    x = "Month",
    y = "Hours above historical threshold (%)",
    colour = "Location",
    caption = paste(
      "Each month's percentage combines the same calendar month across 2021–2025.",
      "This is a relative historical measure, not a safety classification."
    )
  ) +
  heat_theme

print(exceedance_percentage_plot)

ggsave(
  file.path(
    figure_directory,
    "06a_monthly_temperature_exceedance_percentage.png"
  ),
  exceedance_percentage_plot,
  width = 12,
  height = 7,
  dpi = 320,
  bg = "white"
)

# Number of hours chart

exceedance_count_plot <- ggplot(
  monthly_exceedances,
  aes(
    x = month_number,
    y = hours_above_historical_threshold,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  labs(
    title = "Number of Hours Above Historical High-Temperature Levels",
    subtitle = "Total hours across the five-year study period, grouped by calendar month",
    x = "Month",
    y = "Number of hours",
    colour = "Location"
  ) +
  heat_theme

print(exceedance_count_plot)

ggsave(
  file.path(
    figure_directory,
    "06b_monthly_temperature_exceedance_hours.png"
  ),
  exceedance_count_plot,
  width = 12,
  height = 7,
  dpi = 320,
  bg = "white"
)


# ============================================================
# 14B. ANNUAL HISTORICAL HIGH-TEMPERATURE FREQUENCY
# ============================================================

annual_exceedances <- temperature_exceedances |>
  group_by(location, year) |>
  summarise(
    total_observed_hours = n(),
    hours_above_historical_threshold = sum(
      above_location_p90,
      na.rm = TRUE
    ),
    percentage_above_threshold = 100 * mean(
      above_location_p90,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  annual_exceedances,
  file.path(
    table_directory,
    "02c_annual_temperature_exceedances.csv"
  )
)

annual_exceedance_plot <- ggplot(
  annual_exceedances,
  aes(
    x = year,
    y = percentage_above_threshold,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = sort(unique(annual_exceedances$year))
  ) +
  labs(
    title = "Annual Frequency of Hours Above Historical High-Temperature Levels",
    subtitle = "Percentage of hourly observations exceeding each location's 2021–2025 historical 90th-percentile threshold",
    x = "Year",
    y = "Hours above historical threshold (%)",
    colour = "Location",
    caption = paste(
      "The threshold is calculated separately for each location using the full 2021–2025 period.",
      "This is a relative historical measure, not an occupational safety threshold."
    )
  ) +
  heat_theme

print(annual_exceedance_plot)

ggsave(
  file.path(
    figure_directory,
    "06c_annual_temperature_exceedance_percentage.png"
  ),
  annual_exceedance_plot,
  width = 11,
  height = 7,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 15. CORRELATION ANALYSIS
# ============================================================

# Pearson correlation is used here for exploratory analysis.
# Wind direction is excluded because it is circular data.
# Correlation does not establish causation.

correlation_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

correlation_labels <- unname(
  variable_labels[correlation_variables]
)

correlation_results <- lapply(
  unique(weather_all$location),
  function(current_location) {
    
    location_data <- weather_all |>
      filter(location == current_location) |>
      select(all_of(correlation_variables))
    
    correlation_matrix <- cor(
      location_data,
      use = "pairwise.complete.obs",
      method = "pearson"
    )
    
    as.data.frame(
      as.table(correlation_matrix)
    ) |>
      mutate(
        location = current_location,
        variable_1 = factor(
          as.character(Var1),
          levels = correlation_variables,
          labels = correlation_labels
        ),
        variable_2 = factor(
          as.character(Var2),
          levels = correlation_variables,
          labels = correlation_labels
        )
      ) |>
      transmute(
        location,
        variable_1,
        variable_2,
        correlation = Freq
      )
  }
) |>
  bind_rows()

write_csv(
  correlation_results,
  file.path(
    table_directory,
    "03a_correlation_results.csv"
  )
)

correlation_plot <- ggplot(
  correlation_results,
  aes(
    x = variable_1,
    y = variable_2,
    fill = correlation
  )
) +
  geom_tile(
    colour = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(label = sprintf("%.2f", correlation)),
    size = 3.2
  ) +
  facet_wrap(~location) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  labs(
    title = "How Environmental Variables Are Related",
    subtitle = "Pearson correlation coefficients calculated separately for each location",
    x = NULL,
    y = NULL,
    caption = paste(
      "Values close to +1 indicate positive association;",
      "values close to -1 indicate negative association;",
      "values near zero indicate weak linear association."
    )
  ) +
  heat_theme +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 9)
  )

print(correlation_plot)

ggsave(
  file.path(
    figure_directory,
    "07a_correlation_matrices.png"
  ),
  correlation_plot,
  width = 15,
  height = 8,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 16. WIND ANALYSIS
# ============================================================

# Wind direction is circular:
# 0° and 360° both represent north.
#
# Wind direction represents the direction FROM which
# the wind blows.
#
# Calm observations are separated from directional observations.

calm_wind_threshold <- 0.5

wind_data <- weather_all |>
  mutate(
    wind_condition = if_else(
      wind_speed_10m < calm_wind_threshold,
      "Calm",
      "Directional wind"
    )
  )

calm_wind_summary <- wind_data |>
  group_by(location) |>
  summarise(
    total_observations = n(),
    calm_hours = sum(
      wind_speed_10m < calm_wind_threshold,
      na.rm = TRUE
    ),
    directional_wind_hours = sum(
      wind_speed_10m >= calm_wind_threshold,
      na.rm = TRUE
    ),
    calm_percentage = 100 * mean(
      wind_speed_10m < calm_wind_threshold,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

write_csv(
  calm_wind_summary,
  file.path(
    table_directory,
    "04a_calm_wind_summary.csv"
  )
)

# Sixteen compass sectors

compass_sectors <- c(
  "N", "NNE", "NE", "ENE",
  "E", "ESE", "SE", "SSE",
  "S", "SSW", "SW", "WSW",
  "W", "WNW", "NW", "NNW"
)

wind_rose_data <- wind_data |>
  filter(
    wind_speed_10m >= calm_wind_threshold,
    !is.na(wind_direction_10m)
  ) |>
  mutate(
    sector_index = (
      floor(
        ((wind_direction_10m %% 360) + 11.25) / 22.5
      ) %% 16
    ) + 1,
    
    wind_sector = factor(
      compass_sectors[sector_index],
      levels = compass_sectors
    ),
    
    wind_speed_category = cut(
      wind_speed_10m,
      breaks = c(
        0.5, 5, 10, 15, 20, Inf
      ),
      labels = c(
        "0.5–5 km/h",
        "5–10 km/h",
        "10–15 km/h",
        "15–20 km/h",
        "Above 20 km/h"
      ),
      right = FALSE
    )
  ) |>
  count(
    location,
    wind_sector,
    wind_speed_category,
    name = "hours"
  ) |>
  group_by(location) |>
  mutate(
    percentage = 100 * hours / sum(hours)
  ) |>
  ungroup()

write_csv(
  wind_rose_data,
  file.path(
    table_directory,
    "04b_wind_rose_data.csv"
  )
)

wind_rose_plot <- ggplot(
  wind_rose_data,
  aes(
    x = wind_sector,
    y = percentage,
    fill = wind_speed_category
  )
) +
  geom_col(
    width = 0.95,
    colour = "white",
    linewidth = 0.2
  ) +
  coord_polar(
    start = -pi / 16,
    direction = 1
  ) +
  facet_wrap(~location) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Where the Wind Comes From and How Strong It Is",
    subtitle = "Wind direction and speed distribution for each location",
    x = NULL,
    y = "Share of directional wind observations",
    fill = "Wind speed",
    caption = paste(
      "Wind direction indicates where the wind comes FROM.",
      "Calm hours are excluded from the wind rose and reported separately."
    )
  ) +
  heat_theme +
  theme(
    axis.text.x = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

print(wind_rose_plot)

ggsave(
  file.path(
    figure_directory,
    "08a_wind_rose_by_location.png"
  ),
  wind_rose_plot,
  width = 13,
  height = 9,
  dpi = 320,
  bg = "white"
)


# ============================================================
# 16B. WIND DIRECTION AND STRENGTH OVER TIME
# ============================================================

# Calm observations are excluded because their direction
# is not meaningful.

wind_temporal <- weather_all |>
  filter(
    wind_speed_10m >= 0.5,
    !is.na(wind_direction_10m)
  ) |>
  mutate(
    sector_index = (
      floor(
        ((wind_direction_10m %% 360) + 11.25) / 22.5
      ) %% 16
    ) + 1,
    
    wind_sector = factor(
      compass_sectors[sector_index],
      levels = compass_sectors
    ),
    
    hour_label = factor(
      sprintf("%02d:00", hour(timestamp)),
      levels = sprintf("%02d:00", 0:23)
    ),
    
    month_label = factor(
      month(timestamp, label = TRUE, abbr = TRUE),
      levels = month.abb
    ),
    
    year_label = factor(
      year(timestamp),
      levels = sort(unique(year(timestamp)))
    )
  )

# Summarise observations by time period and compass sector

summarise_wind_period <- function(data, period_column) {
  
  data |>
    group_by(
      location,
      .data[[period_column]],
      wind_sector
    ) |>
    summarise(
      observations = n(),
      mean_wind_speed = mean(
        wind_speed_10m,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    group_by(
      location,
      .data[[period_column]]
    ) |>
    mutate(
      direction_share = 100 *
        observations / sum(observations)
    ) |>
    ungroup()
}

wind_hourly_summary <- summarise_wind_period(
  wind_temporal,
  "hour_label"
)

wind_monthly_summary <- summarise_wind_period(
  wind_temporal,
  "month_label"
)

wind_annual_summary <- summarise_wind_period(
  wind_temporal,
  "year_label"
)

write_csv(
  wind_hourly_summary,
  file.path(table_directory, "04c_hourly_wind_direction_strength.csv")
)

write_csv(
  wind_monthly_summary,
  file.path(table_directory, "04d_monthly_wind_direction_strength.csv")
)

write_csv(
  wind_annual_summary,
  file.path(table_directory, "04e_annual_wind_direction_strength.csv")
)

# Reusable heatmap function

make_wind_heatmap <- function(
    data,
    period_column,
    metric_column,
    period_name,
    metric_name,
    filename,
    metric_description
) {
  
  p <- ggplot(
    data,
    aes(
      x = .data[[period_column]],
      y = wind_sector,
      fill = .data[[metric_column]]
    )
  ) +
    geom_tile(
      colour = "white",
      linewidth = 0.25
    ) +
    facet_wrap(~location, nrow = 1) +
    scale_fill_viridis_c(
      option = "C",
      name = metric_name
    ) +
    labs(
      title = paste(
        metric_description,
        "by Wind Direction and",
        period_name
      ),
      subtitle = paste(
        "Wind directions are grouped into 16 compass sectors.",
        "Calm observations are excluded."
      ),
      x = period_name,
      y = "Direction FROM which wind blows",
      caption = paste(
        "A compass sector represents the direction from which",
        "the wind originates, not the direction towards which it travels."
      )
    ) +
    heat_theme +
    theme(
      axis.text.x = element_text(
        angle = ifelse(period_column == "hour_label", 45, 0),
        hjust = 1
      ),
      axis.text.y = element_text(size = 9)
    )
  
  print(p)
  
  ggsave(
    file.path(figure_directory, filename),
    p,
    width = 15,
    height = 7,
    dpi = 320,
    bg = "white",
    limitsize = FALSE
  )
}

# HOURLY: direction frequency

make_wind_heatmap(
  wind_hourly_summary,
  "hour_label",
  "direction_share",
  "Hour of day",
  "Share (%)",
  "12a_hourly_wind_direction_frequency.png",
  "Frequency of Wind Directions"
)

# HOURLY: average wind strength

make_wind_heatmap(
  wind_hourly_summary,
  "hour_label",
  "mean_wind_speed",
  "Hour of day",
  "Mean speed (km/h)",
  "12b_hourly_wind_strength_by_direction.png",
  "Average Wind Strength"
)

# MONTHLY: direction frequency

make_wind_heatmap(
  wind_monthly_summary,
  "month_label",
  "direction_share",
  "Month",
  "Share (%)",
  "13a_monthly_wind_direction_frequency.png",
  "Frequency of Wind Directions"
)

# MONTHLY: average wind strength

make_wind_heatmap(
  wind_monthly_summary,
  "month_label",
  "mean_wind_speed",
  "Month",
  "Mean speed (km/h)",
  "13b_monthly_wind_strength_by_direction.png",
  "Average Wind Strength"
)

# ANNUAL: direction frequency

make_wind_heatmap(
  wind_annual_summary,
  "year_label",
  "direction_share",
  "Year",
  "Share (%)",
  "14a_annual_wind_direction_frequency.png",
  "Frequency of Wind Directions"
)

# ANNUAL: average wind strength

make_wind_heatmap(
  wind_annual_summary,
  "year_label",
  "mean_wind_speed",
  "Year",
  "Mean speed (km/h)",
  "14b_annual_wind_strength_by_direction.png",
  "Average Wind Strength"
)



# ============================================================
# 17. SOLAR RADIATION DISTRIBUTION
# ============================================================

# A. Calculate the proportion of zero-radiation observations

radiation_zero_summary <- weather_all |>
  group_by(location) |>
  summarise(
    total_hours = n(),
    zero_radiation_hours = sum(
      shortwave_radiation <= 0,
      na.rm = TRUE
    ),
    zero_radiation_percentage = 100 *
      zero_radiation_hours / total_hours,
    .groups = "drop"
  )

# B. Keep positive-radiation observations for the distribution.
# Night-time zero readings are reported separately above.

positive_radiation <- weather_all |>
  filter(shortwave_radiation > 0)

zero_summary_text <- paste(
  paste0(
    radiation_zero_summary$location,
    ": ",
    round(
      radiation_zero_summary$zero_radiation_percentage,
      1
    ),
    "% zero readings"
  ),
  collapse = " | "
)

# C. Plot the distribution

solar_radiation_distribution <- ggplot(
  positive_radiation,
  aes(x = shortwave_radiation)
) +
  geom_histogram(
    bins = 50,
    fill = "#E5A000",
    colour = "white",
    linewidth = 0.2
  ) +
  facet_wrap(
    ~location,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    title = "Distribution of Incoming Solar Radiation",
    subtitle = paste(
      "Positive-radiation observations only.",
      zero_summary_text
    ),
    x = "Incoming solar radiation (W/m²)",
    y = "Number of hourly observations",
    caption = paste(
      "Zero-radiation observations, mostly occurring at night,",
      "are excluded from the histograms but reported in the subtitle."
    )
  ) +
  heat_theme

print(solar_radiation_distribution)

ggsave(
  filename = file.path(
    figure_directory,
    "15a_solar_radiation_distribution.png"
  ),
  plot = solar_radiation_distribution,
  width = 12,
  height = 10,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)

# D. Save the zero-radiation summary

write_csv(
  radiation_zero_summary,
  file.path(
    table_directory,
    "05a_solar_radiation_zero_observations.csv"
  )
)


# ============================================================
# 17b. ANNUAL CHANGES IN DAILY SOLAR ENERGY
# ============================================================

# A. Calculate daily incoming solar energy.
# Hourly mean radiation (W/m²) × 3,600 seconds,
# converted to MJ/m².

daily_solar_energy <- weather_all |>
  mutate(
    date = as.Date(timestamp),
    year = lubridate::year(timestamp)
  ) |>
  group_by(
    location,
    year,
    date
  ) |>
  summarise(
    daily_solar_energy_mj = sum(
      shortwave_radiation,
      na.rm = TRUE
    ) * 3600 / 1e6,
    .groups = "drop"
  )

# B. Calculate annual average daily solar energy

annual_solar_summary <- daily_solar_energy |>
  group_by(location, year) |>
  summarise(
    average_daily_solar_energy = mean(
      daily_solar_energy_mj,
      na.rm = TRUE
    ),
    daily_standard_deviation = sd(
      daily_solar_energy_mj,
      na.rm = TRUE
    ),
    days_observed = n(),
    .groups = "drop"
  )

# C. Save annual summary

write_csv(
  annual_solar_summary,
  file.path(
    table_directory,
    "05b_annual_solar_radiation_summary.csv"
  )
)

# D. Visualise annual changes

annual_solar_plot <- ggplot(
  annual_solar_summary,
  aes(
    x = year,
    y = average_daily_solar_energy,
    colour = location,
    group = location
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = sort(unique(annual_solar_summary$year))
  ) +
  labs(
    title = "Annual Changes in Incoming Solar Energy",
    subtitle = paste(
      "Average daily incoming solar energy",
      "compared across 2021–2025"
    ),
    x = "Year",
    y = "Average daily solar energy (MJ/m²/day)",
    colour = "Location",
    caption = paste(
      "Daily energy is calculated from hourly average incoming",
      "shortwave radiation. Annual averages include all hours,",
      "including night-time zero-radiation periods."
    )
  ) +
  heat_theme

print(annual_solar_plot)

ggsave(
  filename = file.path(
    figure_directory,
    "15b_annual_changes_in_solar_energy.png"
  ),
  plot = annual_solar_plot,
  width = 12,
  height = 7,
  dpi = 320,
  bg = "white",
  limitsize = FALSE
)


# ============================================================
# 17c. RELATIONSHIP BETWEEN SOLAR RADIATION AND HUMIDITY
# ============================================================

solar_humidity_plot <- ggplot(
  scatter_sample,
  aes(
    x = shortwave_radiation,
    y = relative_humidity_2m,
    colour = location
  )
) +
  geom_point(
    alpha = 0.16,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Solar Radiation and Relative Humidity",
    subtitle = "Sample of hourly observations from each location",
    x = "Incoming solar radiation (W/m²)",
    y = "Relative humidity (%)",
    colour = "Location"
  ) +
  heat_theme

print(solar_humidity_plot)

ggsave(
  file.path(
    figure_directory,
    "11c_solar_radiation_vs_relative_humidity.png"
  ),
  solar_humidity_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 18. ENVIRONMENTAL RELATIONSHIPS
# ============================================================

set.seed(2026)

scatter_sample <- weather_all |>
  group_by(location) |>
  group_modify(
    ~slice_sample(
      .x,
      n = min(5000, nrow(.x))
    )
  ) |>
  ungroup()

# Air temperature and relative humidity

temperature_humidity_plot <- ggplot(
  scatter_sample,
  aes(
    x = temperature_2m,
    y = relative_humidity_2m,
    colour = location
  )
) +
  geom_point(
    alpha = 0.16,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Air Temperature and Humidity",
    subtitle = "Sample of hourly observations from each location",
    x = "Air temperature (°C)",
    y = "Relative humidity (%)",
    colour = "Location"
  ) +
  heat_theme

print(temperature_humidity_plot)

ggsave(
  file.path(
    figure_directory,
    "10a_temperature_vs_humidity.png"
  ),
  temperature_humidity_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)

# Air temperature and wet-bulb temperature

temperature_wetbulb_plot <- ggplot(
  scatter_sample,
  aes(
    x = temperature_2m,
    y = wet_bulb_temperature_2m,
    colour = location
  )
) +
  geom_point(
    alpha = 0.16,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Air and Wet-Bulb Temperature",
    subtitle = "Sample of hourly observations from each location",
    x = "Air temperature (°C)",
    y = "Wet-bulb temperature (°C)",
    colour = "Location"
  ) +
  heat_theme

print(temperature_wetbulb_plot)

ggsave(
  file.path(
    figure_directory,
    "10b_temperature_vs_wetbulb.png"
  ),
  temperature_wetbulb_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)

# Air temperature and apparent temperature

temperature_apparent_plot <- ggplot(
  scatter_sample,
  aes(
    x = temperature_2m,
    y = apparent_temperature,
    colour = location
  )
) +
  geom_point(
    alpha = 0.16,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Air and Apparent Temperature",
    subtitle = "Sample of hourly observations from each location",
    x = "Air temperature (°C)",
    y = "Apparent temperature (°C)",
    colour = "Location"
  ) +
  heat_theme

print(temperature_apparent_plot)

ggsave(
  file.path(
    figure_directory,
    "10c_temperature_vs_apparent_temperature.png"
  ),
  temperature_apparent_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)

# Air temperature and solar radiation

temperature_radiation_plot <- ggplot(
  scatter_sample,
  aes(
    x = shortwave_radiation,
    y = temperature_2m,
    colour = location
  )
) +
  geom_point(
    alpha = 0.16,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Solar Radiation and Air Temperature",
    subtitle = "Sample of hourly observations from each location",
    x = "Incoming solar radiation (W/m²)",
    y = "Air temperature (°C)",
    colour = "Location"
  ) +
  heat_theme

print(temperature_radiation_plot)

ggsave(
  file.path(
    figure_directory,
    "11a_temperature_vs_solar_radiation.png"
  ),
  temperature_radiation_plot,
  width = 10,
  height = 7,
  dpi = 320,
  bg = "white"
)


# ============================================================
# 18B. WIND SPEED AND SOLAR RADIATION RELATIONSHIP
# ============================================================

wind_radiation_plot <- ggplot(
  scatter_sample,
  aes(
    x = shortwave_radiation,
    y = wind_speed_10m,
    colour = location
  )
) +
  geom_point(
    alpha = 0.15,
    size = 0.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Solar Radiation and Wind Speed",
    subtitle = "Hourly environmental observations across the three locations",
    x = "Incoming solar radiation (W/m²)",
    y = "Wind speed (km/h)",
    colour = "Location",
    caption = paste(
      "This is an exploratory association.",
      "It does not establish a causal relationship."
    )
  ) +
  heat_theme

print(wind_radiation_plot)

ggsave(
  file.path(
    figure_directory,
    "11b_wind_speed_vs_solar_radiation.png"
  ),
  wind_radiation_plot,
  width = 11,
  height = 7,
  dpi = 320,
  bg = "white"
)

# ============================================================
# 19. EXTREME OBSERVATIONS FOR FURTHER INVESTIGATION
# ============================================================

extreme_observations <- weather_all |>
  group_by(location) |>
  mutate(
    temperature_p99 = quantile(
      temperature_2m,
      0.99,
      na.rm = TRUE
    ),
    
    apparent_temperature_p99 = quantile(
      apparent_temperature,
      0.99,
      na.rm = TRUE
    ),
    
    wet_bulb_temperature_p99 = quantile(
      wet_bulb_temperature_2m,
      0.99,
      na.rm = TRUE
    )
  ) |>
  filter(
    temperature_2m >= temperature_p99 |
      apparent_temperature >= apparent_temperature_p99 |
      wet_bulb_temperature_2m >= wet_bulb_temperature_p99
  ) |>
  ungroup() |>
  select(
    location,
    timestamp,
    temperature_2m,
    relative_humidity_2m,
    wind_speed_10m,
    wind_direction_10m,
    shortwave_radiation,
    wet_bulb_temperature_2m,
    apparent_temperature
  )

write_csv(
  extreme_observations,
  file.path(
    table_directory,
    "03c_extreme_observations_for_review.csv"
  )
)

# ============================================================
# 20. EDA OUTPUT REGISTER
# ============================================================

eda_output_register <- tibble(
  output_type = c(
    rep("Table", 18),
    rep("Figure", 31)
  ),
  filename = c(
    "01a_non_finite_values_check.csv",
    "01b_descriptive_statistics.csv",
    "01c_hourly_profiles.csv",
    "01d_monthly_profiles.csv",
    "01e_annual_profiles.csv",
    "01f_daily_maximum_conditions.csv",
    "02a_location_temperature_thresholds.csv",
    "02b_monthly_temperature_exceedances.csv",
    "02c_annual_temperature_exceedances.csv",
    "03a_correlation_results.csv",
    "04a_calm_wind_summary.csv",
    "04b_wind_rose_data.csv",
    "03c_extreme_observations_for_review.csv",
    "04c_hourly_wind_direction_strength.csv",
    "04d_monthly_wind_direction_strength.csv",
    "04e_annual_wind_direction_strength.csv",
    "05a_solar_radiation_zero_observations.csv",
    "05b_annual_solar_radiation_summary.csv",
    "01a_weather_variable_distributions.png",
    "01b_humidity_and_wind_distributions.png",
    "02a_location_distribution_boxplots.png",
    "03a_hourly_environmental_profiles.png",
    "03b_hourly_humidity_and_wind.png",
    "03c_hourly_solar_radiation.png",
    "04a_monthly_environmental_profiles.png",
    "04b_monthly_humidity_and_wind.png",
    "04c_monthly_solar_radiation.png",
    "05a_annual_environmental_profiles.png",
    "05b_annual_humidity_and_wind.png",
    "06a_monthly_temperature_exceedance_percentage.png",
    "06b_monthly_temperature_exceedance_hours.png",
    "06c_annual_temperature_exceedance_percentage.png",
    "07a_correlation_matrices.png",
    "08a_wind_rose_by_location.png",
    "09a_daylight_solar_radiation_distribution.png",
    "10a_temperature_vs_humidity.png",
    "10b_temperature_vs_wetbulb.png",
    "10c_temperature_vs_apparent_temperature.png",
    "11a_temperature_vs_solar_radiation.png",
    "11b_wind_speed_vs_solar_radiation.png",
    "11c_solar_radiation_vs_relative_humidity.png",
    "12a_hourly_wind_direction_frequency.png",
    "12b_hourly_wind_strength_by_direction.png",
    "13a_monthly_wind_direction_frequency.png",
    "13b_monthly_wind_strength_by_direction.png",
    "14a_annual_wind_direction_frequency.png",
    "14b_annual_wind_strength_by_direction.png",
    "15a_solar_radiation_distribution.png",
    "15b_annual_changes_in_solar_energy.png"
  )
 )

write_csv(
  eda_output_register,
  "outputs/audit/eda_output_register.csv"
)

# ============================================================
# 21. COMPLETION REPORT
# ============================================================

cat("\n")
cat("====================================================\n")
cat("HEAT EXPOSURE INTELLIGENCE - EDA COMPLETED\n")
cat("====================================================\n")

cat(
  "\nLocations analysed:",
  paste(unique(weather_all$location), collapse = ", "),
  "\n"
)

cat(
  "Observations analysed:",
  nrow(weather_all),
  "\n"
)

cat(
  "Study period:",
  as.character(min(weather_all$date)),
  "to",
  as.character(max(weather_all$date)),
  "\n"
)

cat(
  "\nTables directory:",
  table_directory,
  "\n"
)

cat(
  "Figures directory:",
  figure_directory,
  "\n"
)

cat(
  "Output register:",
  "outputs/audit/eda_output_register.csv",
  "\n"
)

cat("\nAll EDA sections have finished executing.\n")
cat("Review the output register and inspect the saved figures.\n")
cat("====================================================\n")