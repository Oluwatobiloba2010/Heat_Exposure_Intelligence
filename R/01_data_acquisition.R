
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# 01 - WEATHER DATA ACQUISITION
# Source: Open-Meteo Historical Weather API
# Model: ERA5
# Period: 2021-01-01 to 2025-12-31
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(lubridate)
library(readr)

# 1. PROJECT SETTINGS -----------------------------------------

start_date <- "2021-01-01"
end_date   <- "2025-12-31"

timezone <- "Africa/Lagos"

base_url <- "https://archive-api.open-meteo.com/v1/archive"

# 2. LOCATION SPECIFICATIONS ----------------------------------

locations <- tibble(
  location = c("Lagos", "Ilorin", "Abuja"),
  latitude = c(6.5244, 8.4799, 9.0765),
  longitude = c(3.3792, 4.5418, 7.3986)
)

# 3. WEATHER VARIABLES ----------------------------------------

weather_variables <- c(
  "temperature_2m",
  "relative_humidity_2m",
  "wind_speed_10m",
  "wind_direction_10m",
  "shortwave_radiation",
  "wet_bulb_temperature_2m",
  "apparent_temperature"
)

# 4. CREATE OUTPUT DIRECTORIES -------------------------------

dir.create(
  "data/raw/api_responses",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

# 5. FUNCTION TO DOWNLOAD ONE LOCATION -----------------------

download_weather <- function(location, latitude, longitude) {
  
  message("Downloading data for: ", location)
  
  response <- request(base_url) |>
    req_url_query(
      latitude = latitude,
      longitude = longitude,
      start_date = start_date,
      end_date = end_date,
      hourly = paste(weather_variables, collapse = ","),
      timezone = timezone,
      models = "era5"
    ) |>
    req_perform()
  
  # Check that the request succeeded
  resp_check_status(response)
  
  # Read the API response
  weather_json <- resp_body_json(
    response,
    simplifyVector = FALSE
  )
  
  # Check for an API error
  if (isTRUE(weather_json$error)) {
    stop(weather_json$reason)
  }
  
  # Preserve the original structured response
  raw_file <- paste0(
    "data/raw/api_responses/",
    location,
    "_ERA5_2021_2025.json"
  )
  
  write_json(
    weather_json,
    raw_file,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  
  # Extract hourly observations
  hourly_data <- as_tibble(
    lapply(weather_json$hourly, unlist)
  )
  
  # Add location and source information
  hourly_data <- hourly_data |>
    mutate(
      location = location,
      latitude = latitude,
      longitude = longitude,
      model = "ERA5",
      data_source = "Open-Meteo Historical Weather API"
    ) |>
    rename(timestamp = time) |>
    mutate(
      timestamp = ymd_hm(
        timestamp,
        tz = timezone
      )
    ) |>
    relocate(
      location,
      latitude,
      longitude,
      timestamp
    )
  
  # Save location-specific CSV
  csv_file <- paste0(
    "data/raw/",
    location,
    "_Weather_2021_2025.csv"
  )
  
  write_csv(hourly_data, csv_file)
  
  message(
    "Completed: ", location,
    " | Records: ", nrow(hourly_data)
  )
  
  return(hourly_data)
}

# 6. DOWNLOAD ALL THREE LOCATIONS ----------------------------

weather_list <- vector(
  "list",
  nrow(locations)
)

for (i in seq_len(nrow(locations))) {
  
  weather_list[[i]] <- download_weather(
    location = locations$location[i],
    latitude = locations$latitude[i],
    longitude = locations$longitude[i]
  )
  
}

# 7. COMBINE ALL LOCATIONS -----------------------------------

weather_all <- bind_rows(weather_list)

write_csv(
  weather_all,
  "data/raw/Nigeria_Weather_2021_2025.csv"
)

# 8. BASIC ACQUISITION SUMMARY -------------------------------

cat("\nDATA ACQUISITION COMPLETED\n")

cat("Total records:", nrow(weather_all), "\n")

cat(
  "Locations:",
  paste(unique(weather_all$location), collapse = ", "),
  "\n"
)

cat(
  "First timestamp:",
  as.character(min(weather_all$timestamp)),
  "\n"
)

cat(
  "Last timestamp:",
  as.character(max(weather_all$timestamp)),
  "\n"
)

cat("\nRecords by location:\n")

print(
  weather_all |>
    count(location)
)

# ============================================================
# END OF SCRIPT
# ============================================================

