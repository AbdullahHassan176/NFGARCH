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
    OUTPUT_BASE <- "outputs/tscv"
    RESULTS_BASE <- "results/tscv"
  }
} else if (EVAL_SPLIT_MODE == "manual") {
  OUTPUT_BASE <- "outputs/manual"
  RESULTS_BASE <- "results/manual"
} else {
  warning("Unknown split mode: ", EVAL_SPLIT_MODE, ". Using chronological.")
  OUTPUT_BASE <- "outputs/chronological"
  RESULTS_BASE <- "results/chronological"
}

# Define split-aware paths for evaluation
EVAL_PATHS <- list(
  # Input paths (from simulation)
  nf_garch_results = paste(RESULTS_BASE, "consolidated", paste0("NF_GARCH_Results_", EVAL_SPLIT_MODE, ".xlsx"), sep="/"),
  garch_fitting = paste(OUTPUT_BASE, "garch_fitting", sep="/"),
  residuals = paste(OUTPUT_BASE, "residuals_by_model", sep="/"),
  nf_models = paste(OUTPUT_BASE, "nf_models", sep="/"),
  
  # Output paths
  consolidated = paste(RESULTS_BASE, "consolidated", sep="/"),
  tables = paste(RESULTS_BASE, "dissertation_tables", sep="/"),
  figures = paste(RESULTS_BASE, "figures", sep="/"),
  diagnostics = paste(RESULTS_BASE, "diagnostics", sep="/")
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
  paste(
    EVAL_PATHS$consolidated,
    paste0(base_name, "_", EVAL_SPLIT_MODE, extension),
    sep="/"
  )
}

cat("Output Base:", OUTPUT_BASE, "\n")
cat("Results Base:", RESULTS_BASE, "\n")
cat("Consolidated Results:", EVAL_PATHS$consolidated, "\n")
cat("======================================\n\n")
