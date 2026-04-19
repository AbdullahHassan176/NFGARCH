#!/usr/bin/env Rscript
# Export scripts/core/nf_config.json from scripts/core/config.R (cwd-safe for Windows batch).
args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) args[[1L]] else getwd()
repo <- normalizePath(repo, winslash = "/", mustWork = TRUE)
setwd(repo)
if (!file.exists("scripts/core/config.R")) {
  cat("ERROR: scripts/core/config.R not found under:", repo, "\n", sep = "")
  quit(status = 1L)
}
source("scripts/core/config.R")
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  cat("ERROR: package 'jsonlite' required. Run: run_chronological.bat /InstallRPackages\n")
  quit(status = 1L)
}
export_config_to_json()
out <- file.path(repo, "scripts/core/nf_config.json")
if (!file.exists(out)) {
  cat("ERROR: nf_config.json was not written to", out, "\n", sep = "")
  quit(status = 1L)
}
cat(
  "[OK] nf_config.json <- config.R | flow_family=", NF_CONFIG$flow_family,
  " seed=", REPRODUCIBILITY_SEED, " mode=", PIPELINE_MODE, "\n",
  sep = ""
)
quit(status = 0L)
