# Evaluation Split Configuration Helper
# This script provides split-mode-aware paths for evaluation scripts
# Usage: source("scripts/evaluation/evaluation_split_config.R")

# Load CLI parser if not already loaded
if (!exists("get_split_mode")) {
  source("scripts/utils/cli_parser.R")
}

# Get split mode from command line or use default
EVAL_SPLIT_MODE <- tryCatch({
  get_split_mode()
}, error = function(e) {
  # Default to chronological if parsing fails
  "chronological"
})

cat("=== Evaluation Split Configuration ===\n")
cat("Split Mode:", EVAL_SPLIT_MODE, "\n")

# Load split-specific configuration
if (EVAL_SPLIT_MODE == "chronological") {
  if (file.exists("scripts/config/chrono_split_config.R")) {
    suppressMessages(source("scripts/config/chrono_split_config.R"))
  } else {
    # Defaults
    OUTPUT_BASE <- "outputs/chronological"
    RESULTS_BASE <- "results/chronological"
  }
} else if (EVAL_SPLIT_MODE == "tscv") {
  if (file.exists("scripts/config/tscv_split_config.R")) {
    suppressMessages(source("scripts/config/tscv_split_config.R"))
  } else {
    # Defaults
    OUTPUT_BASE <- "outputs/tscv"
    RESULTS_BASE <- "results/tscv"
  }
} else {
  warning("Unknown split mode: ", EVAL_SPLIT_MODE, ". Using chronological.")
  OUTPUT_BASE <- "outputs/chronological"
  RESULTS_BASE <- "results/chronological"
}

# Define split-aware paths for evaluation
EVAL_PATHS <- list(
  # Input paths (from simulation)
  nf_garch_results = file.path(RESULTS_BASE, "consolidated", paste0("NF_GARCH_Results_", EVAL_SPLIT_MODE, ".xlsx")),
  garch_fitting = file.path(OUTPUT_BASE, "garch_fitting"),
  residuals = file.path(OUTPUT_BASE, "residuals_by_model"),
  nf_models = file.path(OUTPUT_BASE, "nf_models"),
  
  # Output paths
  consolidated = file.path(RESULTS_BASE, "consolidated"),
  tables = file.path(RESULTS_BASE, "dissertation_tables"),
  figures = file.path(RESULTS_BASE, "figures"),
  diagnostics = file.path(RESULTS_BASE, "diagnostics")
)

# Create output directories if they don't exist
for (path_name in names(EVAL_PATHS)) {
  path <- EVAL_PATHS[[path_name]]
  if (!dir.exists(path) && !grepl("\\.xlsx$", path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

# Helper function to get split-specific output filename
get_eval_output_file <- function(base_name, extension = ".xlsx") {
  # Add split mode to filename
  file.path(
    EVAL_PATHS$consolidated,
    paste0(base_name, "_", EVAL_SPLIT_MODE, extension)
  )
}

cat("Output Base:", OUTPUT_BASE, "\n")
cat("Results Base:", RESULTS_BASE, "\n")
cat("Consolidated Results:", EVAL_PATHS$consolidated, "\n")
cat("======================================\n\n")
