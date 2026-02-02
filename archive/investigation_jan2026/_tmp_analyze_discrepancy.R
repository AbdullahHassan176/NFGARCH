#!/usr/bin/env Rscript
# Investigate equity vs FX discrepancy in Standard GARCH

library(openxlsx)

cat("=== INVESTIGATING EQUITY VS FX DISCREPANCY ===\n\n")

# Load combined results
df <- read.xlsx('results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx', 
                sheet='Combined_Results')

# Focus on Standard GARCH
std <- df[df$Source == 'Standard', c('Model', 'Distribution', 'Asset', 'MSE', 'MAE', 'NPaths')]

cat("=== ALL STANDARD GARCH RESULTS (sorted by MSE) ===\n")
print(std[order(std$MSE, decreasing=TRUE), ], row.names=FALSE)

cat("\n\n=== EQUITY ASSETS (NVDA, MSFT, AMZN) ===\n")
equity <- std[std$Asset %in% c('NVDA', 'MSFT', 'AMZN'), ]
cat("Mean MSE:", mean(equity$MSE, na.rm=TRUE), "\n")
cat("Median MSE:", median(equity$MSE, na.rm=TRUE), "\n")
cat("Range:", range(equity$MSE, na.rm=TRUE), "\n\n")
print(equity[order(equity$MSE, decreasing=TRUE), ], row.names=FALSE)

cat("\n\n=== FX ASSETS (EURUSD, GBPUSD, USDZAR) ===\n")
fx <- std[std$Asset %in% c('EURUSD', 'GBPUSD', 'USDZAR'), ]
cat("Mean MSE:", mean(fx$MSE, na.rm=TRUE), "\n")
cat("Median MSE:", median(fx$MSE, na.rm=TRUE), "\n")
cat("Range:", range(fx$MSE, na.rm=TRUE), "\n\n")
print(fx[order(fx$MSE, decreasing=TRUE), ], row.names=FALSE)

cat("\n\n=== BREAKDOWN BY MODEL TYPE ===\n")
cat("\n--- Equity by Model ---\n")
print(aggregate(MSE ~ Model, data=equity, FUN=function(x) c(mean=mean(x), median=median(x))))

cat("\n--- FX by Model ---\n")
print(aggregate(MSE ~ Model, data=fx, FUN=function(x) c(mean=mean(x), median=median(x))))

cat("\n\n=== NF-GARCH FOR COMPARISON ===\n")
nf <- df[df$Source == 'NF_GARCH', c('Model', 'Asset', 'MSE', 'MAE')]
cat("\nEquity NF-GARCH:\n")
print(nf[nf$Asset %in% c('NVDA', 'MSFT', 'AMZN'), ], row.names=FALSE)
cat("\nFX NF-GARCH:\n")
print(nf[nf$Asset %in% c('EURUSD', 'GBPUSD', 'USDZAR'), ], row.names=FALSE)
