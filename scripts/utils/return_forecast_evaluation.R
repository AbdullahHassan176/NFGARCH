# Return Forecast Evaluation Utilities
# Functions for proper return forecast evaluation using multiple simulation paths

#' Generate multiple simulation paths and calculate point forecasts
#' 
#' @param fit Fitted GARCH model
#' @param nf_residuals Standardized NF residuals (or standard residuals for standard GARCH)
#' @param horizon Forecast horizon
#' @param model_type GARCH model type
#' @param submodel Submodel type (if applicable)
#' @param engine Engine to use ("manual" or "rugarch")
#' @param n_paths Number of simulation paths to generate (default: 1000)
#' @return List with:
#'   - point_forecast: Mean across all paths (expected return)
#'   - all_paths: Matrix of all simulation paths (horizon x n_paths)
#'   - sigma_forecast: Mean volatility forecast across paths
#'   - sigma_paths: Matrix of all volatility paths
generate_multiple_paths <- function(fit, nf_residuals, horizon, model_type, submodel = NULL, 
                                    engine = "manual", n_paths = 1000) {  if (n_paths < 1) stop("n_paths must be >= 1")
  
  # Initialize storage
  all_return_paths <- matrix(NA, nrow = horizon, ncol = n_paths)
  all_sigma_paths <- matrix(NA, nrow = horizon, ncol = n_paths)
  
  # Generate multiple paths
  path_errors <- 0
  for (i in 1:n_paths) {    tryCatch({
      # Sample NF residuals for this path (with replacement if needed)
      # For each path, we need to sample residuals independently to get different paths
      if (length(nf_residuals) < horizon) {
        # Sample with replacement if not enough residuals
        path_residuals <- sample(nf_residuals, size = horizon, replace = TRUE)
      } else {
        # Sample without replacement, then shuffle to get different path each time
        # This ensures each path uses different residual sequence
        path_residuals <- sample(nf_residuals, size = horizon, replace = FALSE)
      }
      
      # Use residuals as-is: nf_residuals are already standardized (mean 0, SD 1).
      # Re-standardizing the sample would rescale innovations and is incorrect for the
      # GARCH recursion, which expects z_t with E[z]=0, Var[z]=1 in the population.
      path_residuals[is.na(path_residuals)] <- 0
      
      # Generate one simulation path
      sim_result <- engine_path(
        fit,
        path_residuals,
        horizon,
        model_type,
        submodel,
        engine
      )
      
      if (!is.null(sim_result) && !is.null(sim_result$returns) && !is.null(sim_result$sigma)) {
        if (length(sim_result$returns) == horizon && length(sim_result$sigma) == horizon) {
          all_return_paths[, i] <- sim_result$returns
          all_sigma_paths[, i] <- sim_result$sigma
        } else {
          path_errors <- path_errors + 1
          if (path_errors <= 3) {  # Log first 3 errors only
            warning("Path ", i, ": Length mismatch - returns=", length(sim_result$returns), 
                   ", sigma=", length(sim_result$sigma), ", expected=", horizon)
          }
        }
      } else {
        path_errors <- path_errors + 1
        if (path_errors <= 3) {  # Log first 3 errors only
          warning("Path ", i, ": sim_result is NULL or missing returns/sigma")
        }
      }
    }, error = function(e) {
      # If path generation fails, leave as NA
      path_errors <- path_errors + 1
      if (path_errors <= 3) {  # Log first 3 errors only
        warning("Path generation failed for path ", i, ": ", e$message)
      }
    })
  }
  
  if (path_errors > 0 && path_errors < n_paths) {
    message("Warning: ", path_errors, " out of ", n_paths, " paths failed (", 
            round(path_errors/n_paths*100, 1), "%)")
  }
  
  # Calculate point forecasts (mean across paths)
  point_forecast <- rowMeans(all_return_paths, na.rm = TRUE)
  sigma_forecast <- rowMeans(all_sigma_paths, na.rm = TRUE)
  
  # Count valid paths
  valid_paths <- sum(!is.na(all_return_paths[1, ]))
  
  # #region agent log
  log_entry <- list(location="return_forecast_evaluation.R:90",message="End generate_multiple_paths",data=list(valid_paths=valid_paths,total_paths=n_paths,errors=path_errors),timestamp=as.numeric(Sys.time())*1000,sessionId="debug-session",hypothesisId="H1_H3")
  tryCatch(write(jsonlite::toJSON(log_entry,auto_unbox=TRUE),file="c:\\Experimentation\\NFGARCH\\.cursor\\debug.log",append=TRUE),error=function(e){})
  # #endregion
  
  if (valid_paths == 0) {
    warning("No valid paths generated")
    return(NULL)
  }
  
  return(list(
    point_forecast = point_forecast,
    all_paths = all_return_paths,
    sigma_forecast = sigma_forecast,
    sigma_paths = all_sigma_paths,
    n_valid_paths = valid_paths
  ))
}

#' Calculate predictive log-likelihood for density forecasts
#' 
#' @param actual_returns Actual returns to evaluate
#' @param all_paths Matrix of simulated return paths (horizon x n_paths)
#' @param method Method for density estimation: "kernel" (default) or "empirical"
#' @return Log-likelihood value
calculate_predictive_loglik <- function(actual_returns, all_paths, method = "kernel") {
  if (length(actual_returns) != nrow(all_paths)) {
    stop("Length of actual_returns must match nrow(all_paths)")
  }
  
  loglik <- 0
  
  for (t in seq_along(actual_returns)) {
    # Get all simulated returns at time t
    sim_returns_t <- all_paths[t, ]
    sim_returns_t <- sim_returns_t[!is.na(sim_returns_t)]
    
    if (length(sim_returns_t) < 10) {
      # Not enough samples, skip or use default
      next
    }
    
    if (method == "kernel") {
      # Use kernel density estimation
      tryCatch({
        kde <- density(sim_returns_t, na.rm = TRUE)
        # Interpolate density at actual return
        dens <- approx(kde$x, kde$y, xout = actual_returns[t], rule = 2)$y
        if (dens > 0) {
          loglik <- loglik + log(dens)
        }
      }, error = function(e) {
        # Fallback to empirical
        dens <- mean(abs(sim_returns_t - actual_returns[t]) < 0.001, na.rm = TRUE)
        if (dens > 0) {
          loglik <- loglik + log(dens)
        }
      })
    } else {
      # Empirical density (histogram-based)
      # Use small bin width
      bin_width <- diff(range(sim_returns_t, na.rm = TRUE)) / 50
      if (bin_width == 0) bin_width <- 0.001
      
      # Count how many simulated returns are in bin around actual return
      dens <- mean(abs(sim_returns_t - actual_returns[t]) < bin_width / 2, na.rm = TRUE)
      if (dens > 0) {
        loglik <- loglik + log(dens)
      }
    }
  }
  
  return(loglik)
}

#' Evaluate return forecasts using multiple paths
#' 
#' @param fit Fitted GARCH model
#' @param nf_residuals Standardized NF residuals
#' @param actual_returns Actual returns to compare against
#' @param horizon Forecast horizon
#' @param model_type GARCH model type
#' @param submodel Submodel type
#' @param engine Engine to use
#' @param n_paths Number of simulation paths (default: 1000)
#' @return List with evaluation metrics:
#'   - mse: Mean squared error on point forecast
#'   - mae: Mean absolute error on point forecast
#'   - loglik: Predictive log-likelihood
#'   - n_valid_paths: Number of valid paths generated
evaluate_return_forecasts <- function(fit, nf_residuals, actual_returns, horizon, 
                                      model_type, submodel = NULL, engine = "manual", 
                                      n_paths = 1000) {
  # Validate inputs
  if (is.null(fit)) {
    warning("evaluate_return_forecasts: fit is NULL")
    return(list(mse = NA, mae = NA, loglik = NA, n_valid_paths = 0))
  }
  
  if (is.null(fit$manual_fit)) {
    warning("evaluate_return_forecasts: fit$manual_fit is NULL")
    return(list(mse = NA, mae = NA, loglik = NA, n_valid_paths = 0))
  }
  
  if (is.null(nf_residuals) || length(nf_residuals) == 0) {
    warning("evaluate_return_forecasts: nf_residuals is NULL or empty")
    return(list(mse = NA, mae = NA, loglik = NA, n_valid_paths = 0))
  }
  
  if (is.null(actual_returns) || length(actual_returns) == 0) {
    warning("evaluate_return_forecasts: actual_returns is NULL or empty")
    return(list(mse = NA, mae = NA, loglik = NA, n_valid_paths = 0))
  }
  
  # Generate multiple paths
  path_results <- tryCatch({
    generate_multiple_paths(
      fit, nf_residuals, horizon, model_type, submodel, engine, n_paths
    )
  }, error = function(e) {
    warning("evaluate_return_forecasts: generate_multiple_paths failed: ", e$message)
    return(NULL)
  })
  
  if (is.null(path_results)) {
    warning("evaluate_return_forecasts: path_results is NULL - no valid paths generated")
    return(list(
      mse = NA,
      mae = NA,
      loglik = NA,
      n_valid_paths = 0
    ))
  }
  
  # Ensure same length
  min_len <- min(length(actual_returns), length(path_results$point_forecast))
  actual <- actual_returns[1:min_len]
  point_forecast <- path_results$point_forecast[1:min_len]
  all_paths_subset <- path_results$all_paths[1:min_len, , drop = FALSE]
  
  # Calculate point forecast metrics
  mse <- mean((actual - point_forecast)^2, na.rm = TRUE)
  mae <- mean(abs(actual - point_forecast), na.rm = TRUE)
  
  # #region agent log
  log_entry <- list(location="return_forecast_evaluation.R:234",message="Before density loglik calc",data=list(actual_len=length(actual),paths_dim=dim(all_paths_subset)),timestamp=as.numeric(Sys.time())*1000,sessionId="debug-session",hypothesisId="H1")
  tryCatch(write(jsonlite::toJSON(log_entry,auto_unbox=TRUE),file="c:\\Experimentation\\NFGARCH\\.cursor\\debug.log",append=TRUE),error=function(e){})
  # #endregion
  
  # Calculate density forecast log-likelihood
  loglik <- calculate_predictive_loglik(actual, all_paths_subset)
  
  # #region agent log
  log_entry <- list(location="return_forecast_evaluation.R:242",message="After density loglik calc",data=list(loglik=loglik),timestamp=as.numeric(Sys.time())*1000,sessionId="debug-session",hypothesisId="H1")
  tryCatch(write(jsonlite::toJSON(log_entry,auto_unbox=TRUE),file="c:\\Experimentation\\NFGARCH\\.cursor\\debug.log",append=TRUE),error=function(e){})
  # #endregion
  
  return(list(
    mse = mse,
    mae = mae,
    loglik = loglik,
    n_valid_paths = path_results$n_valid_paths,
    point_forecast = point_forecast,
    sigma_forecast = path_results$sigma_forecast[1:min_len]
  ))
}
