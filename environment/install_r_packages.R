# Install R packages used by run_chronological.bat and reviewer3 full chain into
# environment/R_library (same layout as main pipeline). Run from repo root.
repo_root <- normalizePath(getwd(), winslash = "/")
if (!file.exists(file.path(repo_root, "scripts", "core", "config.R"))) {
  stop("Run from repository root (folder that contains scripts/core/config.R).")
}
lib <- normalizePath(file.path(repo_root, "environment", "R_library"), winslash = "/")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
message("R library: ", lib)

pkgs <- sort(unique(c(
  "xts", "zoo", "PerformanceAnalytics", "dplyr", "tidyr", "stringr", "lubridate",
  "openxlsx", "doParallel", "foreach", "iterators",
  "jsonlite", "yaml", "transport", "moments", "forecast", "tseries", "FinTS", "lmtest",
  "ggplot2", "scales", "viridis", "gridExtra", "quantmod", "readxl", "xtable",
  "rugarch"
)))

have <- rownames(installed.packages(lib.loc = lib))
need <- setdiff(pkgs, have)
if (!length(need)) {
  message("All packages already present in project library.")
  quit(save = "no", status = 0)
}
message("Installing: ", paste(need, collapse = ", "))
install.packages(
  need,
  lib = lib,
  repos = "https://cloud.r-project.org",
  dependencies = TRUE
)
still <- need[!vapply(need, function(p) file.exists(file.path(lib, p)), logical(1))]
if (length(still)) {
  stop("Failed to install: ", paste(still, collapse = ", "))
}
message("[OK] R packages installed into environment/R_library")
