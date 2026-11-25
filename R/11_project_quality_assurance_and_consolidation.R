
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# SCRIPT 11: PROJECT-WIDE QUALITY ASSURANCE AND CONSOLIDATION
# ============================================================
#
# Purpose:
#   Perform final structural and output QA without rerunning
#   Scripts 00-10 or retraining any models.
#
# Outputs:
#   outputs/audit/qa_project_checks.csv
#   outputs/audit/qa_project_summary.txt
#
# ============================================================


# 1. PROJECT CONFIGURATION ------------------------------------

project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

output_root <- file.path(project_root, "outputs")
r_directory <- file.path(project_root, "R")
audit_directory <- file.path(output_root, "audit")

dir.create(
  audit_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

cat("\nHEAT EXPOSURE INTELLIGENCE\n")
cat("SCRIPT 11: PROJECT-WIDE QUALITY ASSURANCE\n\n")
cat("Project root:", project_root, "\n")


# 2. EXPECTED OUTPUT INVENTORY --------------------------------

expected_counts <- c(
  tables = 68L,
  figures = 57L,
  reports = 6L
)

output_directories <- c(
  tables = file.path(output_root, "tables"),
  figures = file.path(output_root, "figures"),
  reports = file.path(output_root, "reports")
)

extensions <- c(
  tables = "\\.csv$",
  figures = "\\.png$",
  reports = "\\.txt$"
)

inventory <- list()

for (type in names(output_directories)) {
  
  folder <- output_directories[[type]]
  
  if (!dir.exists(folder)) {
    stop("Required output directory is missing: ", folder)
  }
  
  files <- list.files(
    folder,
    pattern = extensions[[type]],
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  inventory[[type]] <- files
}


# 3. QA CHECK REGISTER ----------------------------------------

qa_results <- data.frame(
  check_id = character(),
  category = character(),
  check_description = character(),
  expected = character(),
  observed = character(),
  status = character(),
  notes = character(),
  stringsAsFactors = FALSE
)

add_check <- function(
    id,
    category,
    description,
    expected,
    observed,
    passed,
    notes = ""
) {
  
  qa_results <<- rbind(
    qa_results,
    data.frame(
      check_id = id,
      category = category,
      check_description = description,
      expected = as.character(expected),
      observed = as.character(observed),
      status = ifelse(passed, "PASS", "REVIEW"),
      notes = notes,
      stringsAsFactors = FALSE
    )
  )
}


# 4. OUTPUT COUNT VALIDATION ----------------------------------

for (type in names(expected_counts)) {
  
  observed <- length(inventory[[type]])
  expected <- expected_counts[[type]]
  
  add_check(
    paste0("OUT_", toupper(type)),
    "Output inventory",
    paste("Number of", type, "files"),
    expected,
    observed,
    observed == expected
  )
}


# 5. OUTPUT FILENAME VALIDATION -------------------------------

for (type in names(inventory)) {
  
  files <- inventory[[type]]
  
  filenames <- basename(files)
  
  duplicate_names <- anyDuplicated(tolower(filenames)) > 0
  
  add_check(
    paste0("NAME_", toupper(type)),
    "Filename validation",
    paste("Unique filenames for", type),
    "No duplicate filenames",
    ifelse(duplicate_names, "Duplicates detected", "Unique"),
    !duplicate_names
  )
  
  numbered <- grepl(
    "^[0-9]{2}[a-z]_",
    filenames
  )
  
  add_check(
    paste0("PREFIX_", toupper(type)),
    "Filename validation",
    paste("Two-digit and letter-suffix numbering for", type),
    "All files numbered",
    paste(sum(numbered), "of", length(filenames)),
    all(numbered)
  )
}


# 6. CSV READABILITY ------------------------------------------

csv_files <- inventory$tables

csv_readability <- data.frame(
  filename = basename(csv_files),
  readable = FALSE,
  columns = NA_integer_,
  rows_sampled = NA_integer_,
  stringsAsFactors = FALSE
)

for (i in seq_along(csv_files)) {
  
  result <- tryCatch({
    
    sample_data <- read.csv(
      csv_files[i],
      nrows = 5,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    list(
      readable = TRUE,
      columns = ncol(sample_data),
      rows = nrow(sample_data)
    )
    
  }, error = function(e) {
    
    list(
      readable = FALSE,
      columns = NA_integer_,
      rows = NA_integer_
    )
    
  })
  
  csv_readability$readable[i] <- result$readable
  csv_readability$columns[i] <- result$columns
  csv_readability$rows_sampled[i] <- result$rows
}

add_check(
  "CSV_READ",
  "Table integrity",
  "CSV files readable",
  length(csv_files),
  sum(csv_readability$readable),
  all(csv_readability$readable)
)

add_check(
  "CSV_COLUMNS",
  "Table integrity",
  "CSV files have column names",
  "All files",
  sum(csv_readability$columns > 0, na.rm = TRUE),
  all(csv_readability$columns > 0, na.rm = TRUE)
)


# 7. FIGURE FILE VALIDATION -----------------------------------

png_files <- inventory$figures

png_validity <- data.frame(
  filename = basename(png_files),
  non_empty = FALSE,
  valid_signature = FALSE,
  size_bytes = NA_real_,
  stringsAsFactors = FALSE
)

png_signature <- as.raw(
  c(137, 80, 78, 71, 13, 10, 26, 10)
)

for (i in seq_along(png_files)) {
  
  file_size <- file.info(png_files[i])$size
  
  png_validity$size_bytes[i] <- file_size
  
  png_validity$non_empty[i] <- (
    !is.na(file_size) && file_size > 0
  )
  
  signature <- tryCatch(
    readBin(
      png_files[i],
      what = "raw",
      n = 8
    ),
    error = function(e) raw(0)
  )
  
  png_validity$valid_signature[i] <- (
    length(signature) == 8 &&
      identical(signature, png_signature)
  )
}

add_check(
  "PNG_SIZE",
  "Figure integrity",
  "PNG files are non-empty",
  length(png_files),
  sum(png_validity$non_empty),
  all(png_validity$non_empty)
)

add_check(
  "PNG_SIGNATURE",
  "Figure integrity",
  "PNG file signatures are valid",
  length(png_files),
  sum(png_validity$valid_signature),
  all(png_validity$valid_signature)
)


# 8. METHOD REPORT VALIDATION ---------------------------------

report_files <- inventory$reports

report_validity <- data.frame(
  filename = basename(report_files),
  readable = FALSE,
  characters = NA_integer_,
  stringsAsFactors = FALSE
)

for (i in seq_along(report_files)) {
  
  content <- tryCatch(
    readLines(
      report_files[i],
      warn = FALSE
    ),
    error = function(e) NULL
  )
  
  if (!is.null(content)) {
    
    report_validity$readable[i] <- TRUE
    
    report_validity$characters[i] <- nchar(
      paste(content, collapse = "\n")
    )
  }
}

add_check(
  "REPORT_READ",
  "Report integrity",
  "Method reports readable",
  length(report_files),
  sum(report_validity$readable),
  all(report_validity$readable)
)

add_check(
  "REPORT_CONTENT",
  "Report integrity",
  "Method reports contain text",
  length(report_files),
  sum(
    report_validity$characters > 0,
    na.rm = TRUE
  ),
  all(report_validity$characters > 0, na.rm = TRUE)
)


# 9. R SCRIPT INVENTORY ---------------------------------------

expected_scripts <- c(
  "00_project_setup.R",
  "01_data_acquisition.R",
  "02_data_audit.R",
  "03_data_cleaning.R",
  "04_exploratory_analysis.R",
  "05_statistical_analysis.R",
  "06_feature_engineering.R",
  "07_predictive_modelling.R",
  "08_model_validation.R",
  "09_historical_heat_exposure.R",
  "10_model_interpretation_decision_support.R",
  "11_project_quality_assurance_and_consolidation.R",
  "supp_assistance_codes.R"
)

available_scripts <- list.files(
  r_directory,
  pattern = "\\.R$",
  full.names = FALSE
)

missing_scripts <- setdiff(
  expected_scripts,
  available_scripts
)

add_check(
  "R_SCRIPTS",
  "Project structure",
  "Expected R scripts present",
  length(expected_scripts),
  length(expected_scripts) - length(missing_scripts),
  length(missing_scripts) == 0,
  ifelse(
    length(missing_scripts) == 0,
    "All expected scripts present",
    paste(missing_scripts, collapse = "; ")
  )
)


# 10. AUDIT AND RAW DATA INVENTORY ----------------------------

audit_files <- list.files(
  audit_directory,
  pattern = "\\.csv$",
  full.names = TRUE
)

add_check(
  "AUDIT_FILES",
  "Project structure",
  "Audit CSV files present",
  "At least 14",
  length(audit_files),
  length(audit_files) >= 14
)

raw_api_directory <- file.path(
  project_root,
  "data",
  "raw",
  "api_responses"
)

if (dir.exists(raw_api_directory)) {
  
  api_files <- list.files(
    raw_api_directory,
    pattern = "\\.json$",
    full.names = TRUE
  )
  
} else {
  
  api_files <- character()
}

add_check(
  "API_RESPONSES",
  "Data provenance",
  "Raw API response files present",
  3,
  length(api_files),
  length(api_files) >= 3
)


# 11. MODEL FILE INVENTORY ------------------------------------

model_directory <- file.path(
  output_root,
  "models"
)

if (dir.exists(model_directory)) {
  
  model_files <- list.files(
    model_directory,
    pattern = "\\.rds$",
    full.names = TRUE,
    recursive = TRUE
  )
  
} else {
  
  model_files <- character()
}

add_check(
  "MODEL_FILES",
  "Model inventory",
  "Saved RDS model files",
  6,
  length(model_files),
  length(model_files) == 6
)


# 12. OUTPUT REFERENCE SCAN ------------------------------------

all_source_files <- c(
  list.files(
    r_directory,
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  ),
  report_files
)

available_output_names <- tolower(
  basename(
    unlist(inventory, use.names = FALSE)
  )
)

reference_results <- data.frame(
  source_file = character(),
  referenced_file = character(),
  exists_in_inventory = logical(),
  stringsAsFactors = FALSE
)

for (source_file in all_source_files) {
  
  lines <- tryCatch(
    readLines(source_file, warn = FALSE),
    error = function(e) character()
  )
  
  if (length(lines) == 0) next
  
  references <- unlist(
    regmatches(
      lines,
      gregexpr(
        "[A-Za-z0-9_.-]+\\.(csv|png|txt)",
        lines,
        ignore.case = TRUE
      )
    ),
    use.names = FALSE
  )
  
  references <- unique(references)
  
  if (length(references) == 0) next
  
  for (reference in references) {
    
    reference_results <- rbind(
      reference_results,
      data.frame(
        source_file = basename(source_file),
        referenced_file = reference,
        exists_in_inventory = (
          tolower(reference) %in% available_output_names
        ),
        stringsAsFactors = FALSE
      )
    )
  }
}

unresolved_references <- reference_results[
  !reference_results$exists_in_inventory,
  ,
  drop = FALSE
]

add_check(
  "OUTPUT_REFERENCES",
  "Reference integrity",
  "Literal output references resolved",
  "No unresolved references",
  nrow(unresolved_references),
  nrow(unresolved_references) == 0,
  "Review flagged references: some may be historical comments or dynamic filenames."
)


# 13. SAVE DETAILED QA RESULTS -------------------------------

qa_csv <- file.path(
  audit_directory,
  "qa_project_checks.csv"
)

write.csv(
  qa_results,
  qa_csv,
  row.names = FALSE
)

write.csv(
  csv_readability,
  file.path(
    audit_directory,
    "qa_csv_readability.csv"
  ),
  row.names = FALSE
)

write.csv(
  png_validity,
  file.path(
    audit_directory,
    "qa_png_integrity.csv"
  ),
  row.names = FALSE
)

write.csv(
  report_validity,
  file.path(
    audit_directory,
    "qa_report_integrity.csv"
  ),
  row.names = FALSE
)

write.csv(
  reference_results,
  file.path(
    audit_directory,
    "qa_output_reference_scan.csv"
  ),
  row.names = FALSE
)


# 14. FINAL QA SUMMARY ----------------------------------------

passed_checks <- sum(
  qa_results$status == "PASS"
)

review_checks <- sum(
  qa_results$status == "REVIEW"
)

overall_status <- ifelse(
  review_checks == 0,
  "PASS",
  "REVIEW REQUIRED"
)

summary_lines <- c(
  "HEAT EXPOSURE INTELLIGENCE",
  "SCRIPT 11: PROJECT-WIDE QUALITY ASSURANCE",
  "==========================================",
  "",
  paste("Project root:", project_root),
  paste("Date:", Sys.Date()),
  "",
  paste("Overall status:", overall_status),
  paste("Checks passed:", passed_checks),
  paste("Checks requiring review:", review_checks),
  "",
  "OUTPUT INVENTORY",
  "----------------",
  paste("Tables:", length(inventory$tables)),
  paste("Figures:", length(inventory$figures)),
  paste("Method reports:", length(inventory$reports)),
  "",
  "PROJECT COMPONENTS",
  "------------------",
  paste("R scripts:", length(available_scripts)),
  paste("Audit CSV files:", length(audit_files)),
  paste("Raw API JSON files:", length(api_files)),
  paste("Saved model RDS files:", length(model_files)),
  "",
  "CHECK RESULTS",
  "-------------",
  paste(
    qa_results$check_id,
    qa_results$status,
    qa_results$check_description,
    sep = " | "
  ),
  "",
  "IMPORTANT LIMITATIONS",
  "---------------------",
  "This script performs structural and file-integrity QA.",
  "It does not retrain models or rerun completed analyses.",
  "It does not independently establish scientific validity.",
  "Flagged output references require manual contextual review.",
  ""
)

summary_file <- file.path(
  audit_directory,
  "qa_project_summary.txt"
)

writeLines(
  summary_lines,
  summary_file
)


# 15. DISPLAY RESULTS -----------------------------------------

cat("\n\nPROJECT-WIDE QA RESULTS\n")
cat("=======================\n\n")

print(qa_results)

cat("\nOverall status:", overall_status, "\n")
cat("Passed checks:", passed_checks, "\n")
cat("Checks requiring review:", review_checks, "\n")

cat("\nSaved files:\n")
cat(qa_csv, "\n")
cat(summary_file, "\n")

cat(
  "\nScript 11 completed. Review the QA register before final sign-off.\n"
)
