# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# POST-INSTALL RENUMBERING VALIDATION
# ============================================================
# Run from the project root after installing the final patch.
# This script validates files and key references; it does not
# rename files or modify analytical calculations.

project_root <- getwd()
required_dirs <- c("R", "outputs/tables", "outputs/figures", "outputs/reports", "outputs/audit")
missing_dirs <- required_dirs[!dir.exists(file.path(project_root, required_dirs))]
if (length(missing_dirs)) stop("Missing required directories: ", paste(missing_dirs, collapse = ", "))

expected <- c(tables = 68L, figures = 57L, reports = 6L)
observed <- c(
  tables = length(list.files(file.path(project_root, "outputs/tables"), pattern = "\\.csv$", full.names = TRUE)),
  figures = length(list.files(file.path(project_root, "outputs/figures"), pattern = "\\.png$", full.names = TRUE)),
  reports = length(list.files(file.path(project_root, "outputs/reports"), pattern = "\\.txt$", full.names = TRUE))
)
print(data.frame(output_type = names(expected), expected = as.integer(expected), observed = as.integer(observed)))
if (!identical(as.integer(observed), as.integer(expected))) stop("Output counts do not match the expected inventory.")

register_path <- file.path(project_root, "outputs/master_output_register.csv")
map_path <- file.path(project_root, "outputs/output_renaming_map.csv")
if (!file.exists(register_path) || !file.exists(map_path)) stop("Master register or renaming map is missing.")
register <- read.csv(register_path, stringsAsFactors = FALSE)
map <- read.csv(map_path, stringsAsFactors = FALSE)

if (anyDuplicated(paste(register$output_type, register$output_id, sep = "::"))) {
  stop("Duplicate output IDs detected within an output category.")
}

subdir <- ifelse(register$output_type == "Table", "tables",
          ifelse(register$output_type == "Figure", "figures", "reports"))
registered_paths <- file.path(project_root, "outputs", subdir, register$filename)
missing_outputs <- registered_paths[!file.exists(registered_paths)]
if (length(missing_outputs)) stop("Missing registered outputs:\n", paste(missing_outputs, collapse = "\n"))

r_files <- list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)
r_text <- unlist(lapply(r_files, readLines, warn = FALSE), use.names = FALSE)
script_text <- paste(r_text, collapse = "\n")
changed_old_names <- map$old_filename[map$old_filename != map$new_filename]
legacy_refs <- changed_old_names[vapply(changed_old_names, function(x) grepl(x, script_text, fixed = TRUE), logical(1))]
if (length(legacy_refs)) stop("Legacy output filename references remain in R scripts:\n", paste(unique(legacy_refs), collapse = "\n"))

figure_ids <- register$output_id[register$output_type == "Figure"]
required_figure_ids <- c("16a", paste0("17", letters[1:6]), paste0("18", letters[1:8]),
                         paste0("19", letters[1:6]), paste0("20", letters[1:5]))
if (!all(required_figure_ids %in% figure_ids)) stop("One or more required figure IDs in groups 16-20 are missing.")
report_ids <- register$output_id[register$output_type == "Method report"]
if (!all(paste0(sprintf("%02d", 1:6), "a") %in% report_ids)) stop("Method report IDs must be 01a-06a.")

cat("\nPASS: Renumbered outputs exist, registers resolve, and legacy output references are absent.\n")
cat("Next: proceed to Script 11 project-wide QA and consolidation.\n")
