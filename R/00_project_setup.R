
# ============================================================
# HEAT EXPOSURE INTELLIGENCE
# PROJECT DIRECTORY SETUP
# ============================================================

# 1. Define the folders we need

project_directories <- c(
  "data/raw/api_responses",
  "data/interim",
  "data/processed",
  
  "R/functions",
  
  "python/functions",
  
  "app/R",
  "app/python",
  "app/shared",
  
  "outputs/audit",
  "outputs/figures",
  "outputs/tables",
  "outputs/models",
  "outputs/reports",
  
  "tests/R",
  "tests/python",
  
  "docs/methodology",
  "docs/tutorials",
  "docs/project_notes"
)

# 2. Create the directories

for (folder in project_directories) {
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

# 3. Confirm that the directories exist

directory_status <- data.frame(
  directory = project_directories,
  exists = dir.exists(project_directories)
)

print(directory_status)

# 4. Report the result

if (all(directory_status$exists)) {
  message("Project directory structure created successfully.")
} else {
  warning("Some project directories could not be created.")
}
