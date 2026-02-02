# Master Script: Run All Feasible Analyses
# Executes Analyses 1, 3, 4 (others require data not saved during pipeline)

cat("================================================================\n")
cat("ADDITIONAL INVESTIGATION: DEEP DIVE INTO NF-GARCH FAILURE\n")
cat("================================================================\n\n")

cat("This investigation runs 3 analyses on available data:\n")
cat("  1. Residual Diagnostics (ACF, ARCH effects)\n")
cat("  3. Information Loss (Entropy, KL divergence)\n")
cat("  4. Temporal Dynamics (Runs tests, variance ratios)\n\n")

cat("Note: Analyses 2, 5, 6, 7 require forecast paths not saved during pipeline.\n")
cat("See ANALYSES_NOTE.md for details.\n\n")

cat("================================================================\n\n")

# Record start time
start_time <- Sys.time()

# Create results directory if needed
if(!dir.exists("analyses/results")) {
  dir.create("analyses/results", recursive=TRUE)
}

# Track which analyses succeed
analyses_status <- data.frame(
  Analysis = character(),
  Status = character(),
  Duration_sec = numeric(),
  stringsAsFactors = FALSE
)

# Analysis 1: Residual Diagnostics
cat("\n>>> RUNNING ANALYSIS 1: Residual Diagnostics <<<\n")
cat("================================================================\n")
a1_start <- Sys.time()
a1_success <- tryCatch({
  source("analyses/analysis_1_residual_diagnostics.R")
  TRUE
}, error = function(e) {
  cat("\n[ERROR] Analysis 1 failed:", e$message, "\n")
  FALSE
})
a1_duration <- as.numeric(difftime(Sys.time(), a1_start, units="secs"))
analyses_status <- rbind(analyses_status, data.frame(
  Analysis = "Analysis 1: Residual Diagnostics",
  Status = ifelse(a1_success, "SUCCESS", "FAILED"),
  Duration_sec = a1_duration
))

# Analysis 3: Information Loss
cat("\n>>> RUNNING ANALYSIS 3: Information Loss <<<\n")
cat("================================================================\n")
a3_start <- Sys.time()
a3_success <- tryCatch({
  source("analyses/analysis_3_information_loss.R")
  TRUE
}, error = function(e) {
  cat("\n[ERROR] Analysis 3 failed:", e$message, "\n")
  FALSE
})
a3_duration <- as.numeric(difftime(Sys.time(), a3_start, units="secs"))
analyses_status <- rbind(analyses_status, data.frame(
  Analysis = "Analysis 3: Information Loss",
  Status = ifelse(a3_success, "SUCCESS", "FAILED"),
  Duration_sec = a3_duration
))

# Analysis 4: Temporal Dynamics
cat("\n>>> RUNNING ANALYSIS 4: Temporal Dynamics <<<\n")
cat("================================================================\n")
a4_start <- Sys.time()
a4_success <- tryCatch({
  source("analyses/analysis_4_temporal_dynamics.R")
  TRUE
}, error = function(e) {
  cat("\n[ERROR] Analysis 4 failed:", e$message, "\n")
  FALSE
})
a4_duration <- as.numeric(difftime(Sys.time(), a4_start, units="secs"))
analyses_status <- rbind(analyses_status, data.frame(
  Analysis = "Analysis 4: Temporal Dynamics",
  Status = ifelse(a4_success, "SUCCESS", "FAILED"),
  Duration_sec = a4_duration
))

# Total duration
total_duration <- as.numeric(difftime(Sys.time(), start_time, units="mins"))

# Final summary
cat("\n================================================================\n")
cat("INVESTIGATION COMPLETE\n")
cat("================================================================\n\n")

cat("Analysis Status:\n")
print(analyses_status)

cat(paste0("\nTotal Duration: ", round(total_duration, 2), " minutes\n"))
cat(paste0("Successful: ", sum(analyses_status$Status == "SUCCESS"), " of ", 
           nrow(analyses_status), "\n"))

# Save status
write.csv(analyses_status, "analyses/results/investigation_status.csv", row.names=FALSE)

cat("\nResults saved to: analyses/results/\n")

# Create summary report
cat("\n>>> GENERATING SUMMARY REPORT <<<\n\n")

summary_report <- paste0(
  "# Additional Investigation: Deep Dive into NF-GARCH Failure\n\n",
  "## Execution Summary\n\n",
  "**Date:** ", Sys.time(), "\n",
  "**Duration:** ", round(total_duration, 2), " minutes\n",
  "**Branch:** additional_investigation\n\n",
  "## Analyses Executed\n\n"
)

for(i in 1:nrow(analyses_status)) {
  status_icon <- ifelse(analyses_status$Status[i] == "SUCCESS", "✅", "❌")
  summary_report <- paste0(summary_report,
    status_icon, " **", analyses_status$Analysis[i], "** - ",
    analyses_status$Status[i], " (",
    round(analyses_status$Duration_sec[i], 1), "s)\n"
  )
}

summary_report <- paste0(summary_report, "\n## Key Files Generated\n\n")

# List all result files
result_files <- list.files("analyses/results", pattern="*.csv", full.names=FALSE)
for(file in result_files) {
  summary_report <- paste0(summary_report, "- `analyses/results/", file, "`\n")
}

summary_report <- paste0(summary_report, "\n## Next Steps\n\n",
  "1. Review detailed CSVs in `analyses/results/`\n",
  "2. Compare sGARCH_norm vs sGARCH_sstd findings\n",
  "3. Document key insights in dissertation\n",
  "4. Consider re-running pipeline with saved paths for Analyses 2, 5-7\n"
)

# Save summary
writeLines(summary_report, "analyses/INVESTIGATION_SUMMARY.md")

cat("Summary report: analyses/INVESTIGATION_SUMMARY.md\n\n")
cat("================================================================\n")
cat("DONE!\n")
cat("================================================================\n")
