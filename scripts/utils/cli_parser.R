# CLI Argument Parser for Engine Selection
# Manual engine only - simplified parser
# Supports split mode parameter for chronological vs TS CV pipelines

parse_cli_args <- function() {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default values - always use manual engine
  config <- list(
    engine = "manual",
    config_file = NULL,
    split = "chronological"  # Default to chronological split
  )
  
  # Parse arguments
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--engine") {
      if (i + 1 <= length(args)) {
        engine <- args[i + 1]
        if (engine == "manual") {
          config$engine <- engine
        } else {
          warning("Invalid engine: ", engine, ". Only 'manual' engine is supported. Using 'manual'.")
          config$engine <- "manual"
        }
        i <- i + 2
      } else {
        stop("--engine requires a value")
      }
    } else if (args[i] == "--config") {
      if (i + 1 <= length(args)) {
        config$config_file <- args[i + 1]
        i <- i + 2
      } else {
        stop("--config requires a file path")
      }
    } else if (args[i] == "--split") {
      if (i + 1 <= length(args)) {
        split_mode <- args[i + 1]
        if (split_mode %in% c("chronological", "tscv")) {
          config$split <- split_mode
        } else {
          warning("Invalid split mode: ", split_mode, ". Using 'chronological'.")
          config$split <- "chronological"
        }
        i <- i + 2
      } else {
        stop("--split requires a value (chronological or tscv)")
      }
    } else if (args[i] == "--help" || args[i] == "-h") {
      cat("Usage: Rscript script.R [options]\n")
      cat("Options:\n")
      cat("  --engine ENGINE     GARCH engine to use (manual only) [default: manual]\n")
      cat("  --split SPLIT       Data split mode: chronological or tscv [default: chronological]\n")
      cat("  --config FILE       Configuration file path\n")
      cat("  --help, -h          Show this help message\n")
      cat("\n")
      cat("Examples:\n")
      cat("  Rscript simulate_nf_garch.R --engine manual --split chronological\n")
      cat("  Rscript simulate_nf_garch.R --engine manual --split tscv\n")
      cat("  Rscript simulate_nf_garch.R --config config.yaml\n")
      quit(status = 0)
    } else {
      warning("Unknown argument: ", args[i])
      i <- i + 1
    }
  }
  
  # Load config file if specified
  if (!is.null(config$config_file) && file.exists(config$config_file)) {
    config_from_file <- load_config_file(config$config_file)
    # CLI arguments override config file
    if (!is.null(config_from_file$engine)) {
      if (config_from_file$engine == "manual") {
        config$engine <- config_from_file$engine
      } else {
        warning("Config file specified invalid engine. Using 'manual'.")
      }
    }
  }
  
  return(config)
}

load_config_file <- function(config_file) {
  # Load configuration from YAML file
  if (!requireNamespace("yaml", quietly = TRUE)) {
    warning("yaml package not available, skipping config file")
    return(list())
  }
  
  tryCatch({
    config <- yaml::read_yaml(config_file)
    return(config)
  }, error = function(e) {
    warning("Failed to load config file: ", conditionMessage(e))
    return(list())
  })
}

# Function to get current engine setting
get_engine <- function() {
  config <- parse_cli_args()
  return(config$engine)
}

# Function to get current split mode
get_split_mode <- function() {
  config <- parse_cli_args()
  return(config$split)
}

# Function to print current configuration
print_config <- function() {
  config <- parse_cli_args()
  cat("=== Pipeline Configuration ===\n")
  cat("Engine:", config$engine, "\n")
  cat("Split mode:", config$split, "\n")
  if (!is.null(config$config_file)) {
    cat("Config file:", config$config_file, "\n")
  }
  cat("==============================\n\n")
}
