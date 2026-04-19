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

# Reviewer-3 / robustness: results under outputs/reviewer3/<run_id>/results;
# GARCH inputs stay under outputs/manual (or REVIEWER3_MANUAL_OUTPUT_BASE).
r3_root <- Sys.getenv("REVIEWER3_RUN_ROOT", unset = "")
if (nzchar(r3_root)) {
  manual_out <- trimws(Sys.getenv("REVIEWER3_MANUAL_OUTPUT_BASE", unset = "outputs/manual"))
  if (!nzchar(manual_out)) manual_out <- "outputs/manual"
  RESULTS_BASE <- file.path(r3_root, "results")
  OUTPUT_BASE <- manual_out
  nf_models_dir <- r3_root
  cat("REVIEWER3_RUN_ROOT active:", r3_root, "\n")
  cat("  RESULTS_BASE ->", RESULTS_BASE, "\n")
  cat("  OUTPUT_BASE (GARCH/residuals) ->", OUTPUT_BASE, "\n")
  cat("  nf_models (synthetic CSVs) ->", nf_models_dir, "\n")
} else {
  nf_models_dir <- file.path(OUTPUT_BASE, "nf_models")
}

# Chronological pipeline writes GARCH/residuals/NF outputs under outputs/manual via manual_garch_fitting.R;
# redirect paths accordingly so evaluation steps find those files.
garch_residual_nf_base <- OUTPUT_BASE
if (!nzchar(r3_root) && EVAL_SPLIT_MODE == "chronological" &&
    dir.exists(file.path("outputs/manual/residuals_by_model"))) {
  garch_residual_nf_base <- "outputs/manual"
  nf_models_dir <- file.path("outputs/manual", "nf_models")
  cat(
    "Chronological + manual engine: GARCH, residuals, NF checkpoints -> ",
    garch_residual_nf_base, "\n",
    sep = ""
  )
}

# Define split-aware paths for evaluation
EVAL_PATHS <- list(
  nf_garch_results = file.path(RESULTS_BASE, "consolidated", paste0("NF_GARCH_Results_", EVAL_SPLIT_MODE, ".xlsx")),
  garch_fitting = file.path(garch_residual_nf_base, "garch_fitting"),
  residuals = file.path(garch_residual_nf_base, "residuals_by_model"),
  nf_models = nf_models_dir,
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

# Align R RNG with NF run when full-chain driver sets REVIEWER3_REPRODUCIBILITY_SEED
r3seed_env <- Sys.getenv("REVIEWER3_REPRODUCIBILITY_SEED", unset = "")
if (nzchar(r3seed_env)) {
  v <- suppressWarnings(as.integer(r3seed_env))
  if (!is.na(v)) {
    REPRODUCIBILITY_SEED <<- v
    set.seed(v)
    cat("REVIEWER3_REPRODUCIBILITY_SEED applied:", v, "\n\n")
  }
}
