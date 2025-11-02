#!/usr/bin/env Rscript
# Checkpoint Management Utility
# Provides functions for saving and loading pipeline checkpoints

library(jsonlite)

# Save checkpoint
save_checkpoint <- function(step_num, step_name, status = "completed", error = NULL) {
  checkpoint_file <- "checkpoints/pipeline_status.json"
  
  # Ensure checkpoint directory exists
  if (!dir.exists("checkpoints")) {
    dir.create("checkpoints", recursive = TRUE)
  }
  
  # Load existing checkpoints
  if (file.exists(checkpoint_file)) {
    checkpoints <- fromJSON(checkpoint_file)
  } else {
    checkpoints <- list()
  }
  
  # Update checkpoint
  checkpoints[[paste0("step_", step_num)]] <- list(
    step_num = step_num,
    name = step_name,
    status = status,
    timestamp = as.character(Sys.time()),
    error = error
  )
  
  # Save checkpoints
  writeLines(toJSON(checkpoints, auto_unbox = TRUE, pretty = TRUE), checkpoint_file)
  
  cat("[CHECKPOINT] Step", step_num, "(", step_name, ") saved:", status, "\n")
}

# Load checkpoint
load_checkpoint <- function(step_num) {
  checkpoint_file <- "checkpoints/pipeline_status.json"
  
  if (!file.exists(checkpoint_file)) {
    return(NULL)
  }
  
  checkpoints <- fromJSON(checkpoint_file)
  step_key <- paste0("step_", step_num)
  
  if (step_key %in% names(checkpoints)) {
    return(checkpoints[[step_key]])
  }
  
  return(NULL)
}

# Check if step is completed
is_step_completed <- function(step_num) {
  checkpoint <- load_checkpoint(step_num)
  if (is.null(checkpoint)) {
    return(FALSE)
  }
  return(checkpoint$status == "completed")
}

# Get all checkpoints
get_all_checkpoints <- function() {
  checkpoint_file <- "checkpoints/pipeline_status.json"
  
  if (!file.exists(checkpoint_file)) {
    return(list())
  }
  
  checkpoints <- fromJSON(checkpoint_file)
  return(checkpoints)
}

# Reset checkpoint
reset_checkpoint <- function(step_num) {
  checkpoint_file <- "checkpoints/pipeline_status.json"
  
  if (!file.exists(checkpoint_file)) {
    return()
  }
  
  checkpoints <- fromJSON(checkpoint_file)
  step_key <- paste0("step_", step_num)
  
  if (step_key %in% names(checkpoints)) {
    checkpoints[[step_key]] <- NULL
    writeLines(toJSON(checkpoints, auto_unbox = TRUE, pretty = TRUE), checkpoint_file)
    cat("[CHECKPOINT] Step", step_num, "reset\n")
  }
}

# Clear all checkpoints
clear_all_checkpoints <- function() {
  checkpoint_file <- "checkpoints/pipeline_status.json"
  if (file.exists(checkpoint_file)) {
    file.remove(checkpoint_file)
    cat("[CHECKPOINT] All checkpoints cleared\n")
  }
}

# Print status
print_status <- function() {
  checkpoints <- get_all_checkpoints()
  
  if (length(checkpoints) == 0) {
    cat("No checkpoints found.\n")
    return()
  }
  
  cat("\n=== PIPELINE STATUS ===\n\n")
  
  # Sort by step number
  step_nums <- sapply(checkpoints, function(x) x$step_num)
  checkpoints <- checkpoints[order(as.numeric(step_nums))]
  
  for (step_key in names(checkpoints)) {
    cp <- checkpoints[[step_key]]
    status_symbol <- if (cp$status == "completed") "[OK]" else if (cp$status == "failed") "[FAILED]" else "[?]"
    cat(sprintf("%-3s Step %2s: %-35s [%s] %s\n", 
                status_symbol,
                cp$step_num,
                cp$name,
                cp$status,
                cp$timestamp))
  }
  cat("\n")
}

# Command line interface
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  print_status()
} else if (args[1] == "save" && length(args) >= 3) {
  step_num <- args[2]
  step_name <- args[3]
  status <- if (length(args) >= 4) args[4] else "completed"
  error <- if (length(args) >= 5) args[5] else NULL
  save_checkpoint(step_num, step_name, status, error)
} else if (args[1] == "check" && length(args) >= 2) {
  step_num <- args[2]
  if (is_step_completed(step_num)) {
    cat("COMPLETED\n")
  } else {
    cat("NOT_COMPLETED\n")
  }
} else if (args[1] == "status") {
  print_status()
} else if (args[1] == "reset" && length(args) >= 2) {
  step_num <- args[2]
  reset_checkpoint(step_num)
} else if (args[1] == "clear") {
  clear_all_checkpoints()
} else {
  cat("Usage:\n")
  cat("  Rscript checkpoint_manager.R status              - Show status\n")
  cat("  Rscript checkpoint_manager.R save <num> <name>  - Save checkpoint\n")
  cat("  Rscript checkpoint_manager.R check <num>        - Check if step completed\n")
  cat("  Rscript checkpoint_manager.R reset <num>        - Reset step\n")
  cat("  Rscript checkpoint_manager.R clear              - Clear all\n")
}

