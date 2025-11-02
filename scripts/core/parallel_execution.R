# Parallel Execution for Optimized Pipeline
# Reduces execution time while maintaining statistical rigor

library(parallel)
library(doParallel)
library(foreach)

# Setup parallel processing
setup_parallel <- function(n_cores = 4) {
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  return(cl)
}

# Parallel asset processing
process_assets_parallel <- function(assets, process_function, ...) {
  results <- foreach(asset = assets, .packages = c("xts", "dplyr")) %dopar% {
    tryCatch({
      process_function(asset, ...)
    }, error = function(e) {
      cat("Error processing", asset, ":", e$message, "\n")
      return(NULL)
    })
  }
  
  # Filter out NULL results
  results <- results[!sapply(results, is.null)]
  return(results)
}

# Parallel model fitting
fit_models_parallel <- function(assets, models, data) {
  # Create combinations
  combinations <- expand.grid(asset = assets, model = models, stringsAsFactors = FALSE)
  
  results <- foreach(i = 1:nrow(combinations), .packages = c("xts", "dplyr")) %dopar% {
    asset <- combinations$asset[i]
    model <- combinations$model[i]
    
    tryCatch({
      fit_single_model(asset, model, data)
    }, error = function(e) {
      cat("Error fitting", model, "for", asset, ":", e$message, "\n")
      return(NULL)
    })
  }
  
  return(results)
}

# Parallel NF training
train_nf_parallel <- function(residual_files) {
  results <- foreach(file = residual_files, .packages = c("torch", "numpy")) %dopar% {
    tryCatch({
      train_single_nf(file)
    }, error = function(e) {
      cat("Error training NF for", file, ":", e$message, "\n")
      return(NULL)
    })
  }
  
  return(results)
}

# Parallel evaluation
evaluate_parallel <- function(model_results, metrics) {
  results <- foreach(result = model_results, .packages = c("PerformanceAnalytics")) %dopar% {
    tryCatch({
      evaluate_single_model(result, metrics)
    }, error = function(e) {
      cat("Error evaluating model:", e$message, "\n")
      return(NULL)
    })
  }
  
  return(results)
}

# Cleanup parallel processing
cleanup_parallel <- function(cl) {
  stopCluster(cl)
  registerDoSEQ()
}

# Main parallel execution function
run_parallel_pipeline <- function(config) {
  # Setup
  cl <- setup_parallel(config$parallel$n_cores)
  
  tryCatch({
    # Parallel asset processing
    cat("Processing assets in parallel...\n")
    asset_results <- process_assets_parallel(
      config$assets, 
      process_asset_data,
      config$quick
    )
    
    # Parallel model fitting
    cat("Fitting models in parallel...\n")
    model_results <- fit_models_parallel(
      config$assets,
      config$models,
      asset_results
    )
    
    # Parallel NF training
    cat("Training NFs in parallel...\n")
    nf_results <- train_nf_parallel(
      extract_residual_files(model_results)
    )
    
    # Parallel evaluation
    cat("Evaluating models in parallel...\n")
    eval_results <- evaluate_parallel(
      model_results,
      config$metrics
    )
    
    return(list(
      assets = asset_results,
      models = model_results,
      nfs = nf_results,
      evaluation = eval_results
    ))
    
  }, finally = {
    cleanup_parallel(cl)
  })
}


