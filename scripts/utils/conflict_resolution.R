# Shim for pipelines that source this file before initialize_pipeline().
# Keeps simulate_nf_garch_engine.R working without pulling in scripts/core/utils.R (ggplot2, etc.).
initialize_pipeline <- function() {
  options(xts.warn_dplyr_breaks_lag = FALSE)
  invisible(TRUE)
}
