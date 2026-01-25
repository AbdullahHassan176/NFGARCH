# NF-GARCH Simulation: Consolidated Results Summary

**Purpose:** One document for any reader to understand what was done and what the results were.

---

## 1. What Was Done

### 1.1 Project
- **NF-GARCH** = Normalizing Flow–enhanced GARCH models for financial return forecasting
- **Goal:** Compare NF-GARCH to standard GARCH on point forecasts (MSE, MAE), density forecasts (Predictive Log-Likelihood), and in-sample fit (AIC, BIC, LogLikelihood)

### 1.2 Data
- **Assets:** 6 (3 FX: EURUSD, GBPUSD, USDZAR; 3 Equity: NVDA, MSFT, AMZN)
- **Source:** `data/processed/raw (FX + EQ).csv`
- **Returns:** Log-returns from prices

### 1.3 GARCH Models
- **sGARCH (norm)** – standard GARCH, normal errors  
- **sGARCH (sstd)** – standard GARCH, skewed Student-t  
- **gjrGARCH (sstd)** – GJR-GARCH, skewed Student-t  
- **eGARCH (sstd)** – exponential GARCH, skewed Student-t  
- **TGARCH (sstd)** – threshold GARCH, skewed Student-t  

### 1.4 Pipeline Steps
1. **Train GARCH** on ~65% of returns (chronological)
2. **Train Normalizing Flows** on standardized GARCH residuals → synthetic residuals
3. **Forecast** on test ~35%:  
   - **NF-GARCH:** 1,000 simulation paths using synthetic residuals → point forecast (mean) + full predictive distribution  
   - **Standard GARCH:** single-path forecast using fitted residuals → point forecast only
4. **Evaluate:** MSE, MAE, Predictive Log-Likelihood (NF-GARCH only), AIC, BIC, LogLikelihood
5. **Compare** NF-GARCH vs Standard GARCH where both exist

### 1.5 Main Scripts
- `scripts/simulation_forecasting/simulate_nf_garch_engine.R` – NF-GARCH simulation and evaluation  
- `scripts/evaluation/compare_nf_vs_standard_garch.R` – NF-GARCH vs Standard GARCH  
- `scripts/complete_analysis.R` – analysis and summary reports  
- `scripts/analyze_all_metrics.R` – point and in-sample metrics  
- `scripts/analyze_density_metrics.R` – density (PredictiveLogLik) metrics  

---

## 2. Run Summary

- **Models attempted:** 30 (5 GARCH types × 6 assets)  
- **Successful NF-GARCH fits:** 25 (83.3%)  
- **Failed:** 5 (all **eGARCH** on EURUSD, GBPUSD, USDZAR, NVDA, MSFT – optimization convergence code 52)  
- **eGARCH:** 1/6 (AMZN only)  
- **Simulation paths per NF-GARCH forecast:** 1,000  
- **Split:** Chronological 65% train / 35% test  

---

## 3. Results: Point Forecasts (MSE, MAE)

### 3.1 NF-GARCH vs Standard GARCH (Overall)

| Metric | NF-GARCH | Standard GARCH | Improvement |
|--------|----------|----------------|-------------|
| **MSE** | 2.08 × 10⁻⁶ | 3.50 | **~100%** (NF-GARCH ~1.68M× smaller) |
| **MAE** | 3.56 × 10⁻⁴ | 0.0952 | **~99.6%** (NF-GARCH ~267× smaller) |

### 3.2 Win Rate (NF-GARCH vs Standard, by MSE)
- **NF-GARCH wins:** 18/18 (100%)  
- **Standard GARCH wins:** 0  
- **Wilcoxon (MSE, NF < Standard):** p = 0.0156 for sGARCH, TGARCH, gjrGARCH → statistically significant  

### 3.3 By Model (when comparable)

| Model | MSE improvement | MAE improvement |
|-------|-----------------|-----------------|
| TGARCH | 99.65% | 97.42% |
| gjrGARCH | 99.67% | 97.65% |
| sGARCH | Similar | Similar |

### 3.4 By Asset Class

| Class | MSE improvement | MAE improvement |
|-------|-----------------|-----------------|
| FX | 99.96% | 99.01% |
| Equity | 100% | 99.62% |

### 3.5 Best Point Forecasts (NF-GARCH, by MSE)
1. EURUSD – sGARCH (sstd): 2.21×10⁻⁵  
2. EURUSD – TGARCH (sstd): 2.21×10⁻⁵  
3. EURUSD – gjrGARCH (sstd): 2.21×10⁻⁵  
4. EURUSD – sGARCH (norm): 2.22×10⁻⁵  
5. GBPUSD – sGARCH (norm): 3.43×10⁻⁵  

---

## 4. Results: Density Forecasts (Predictive Log-Likelihood)

### 4.1 Availability
- **NF-GARCH:** Has Predictive Log-Likelihood (1,000 paths → full predictive distribution)  
- **Standard GARCH:** No density metric (single-path, point forecast only)  
- **Interpretation:** Higher PredictiveLogLik = predictive distribution assigns more probability to realized returns  

### 4.2 NF-GARCH Density Metrics (25 models)

| Statistic | Value |
|-----------|-------|
| Mean Predictive Log-Likelihood | 2,337.81 |
| Median | 3,455.28 |
| Min | -5,724.75 |
| Max | 6,230.30 |
| Mean NPaths | 1,000 |

### 4.3 By Model Type (NF-GARCH)

| Model | Mean PredictiveLogLik |
|-------|------------------------|
| eGARCH (sstd) | 5,659.42 (n=1) |
| sGARCH (sstd) | 4,307.76 |
| sGARCH (norm) | 4,190.73 |
| gjrGARCH (sstd) | 3,601.57 |
| TGARCH (sstd) | -3,302.41 |

### 4.4 By Asset (NF-GARCH)

| Asset | Mean PredictiveLogLik |
|-------|------------------------|
| EURUSD | 4,327.01 |
| GBPUSD | 4,071.37 |
| USDZAR | 2,682.96 |
| AMZN | 2,152.76 |
| MSFT | 1,567.41 |
| NVDA | -728.38 |

### 4.5 Best Density Forecasts (NF-GARCH, top 5)
1. EURUSD – gjrGARCH (sstd): 6,230.30  
2. EURUSD – sGARCH (sstd): 6,118.37  
3. EURUSD – sGARCH (norm): 6,069.99  
4. GBPUSD – gjrGARCH (sstd): 5,925.89  
5. AMZN – eGARCH (sstd): 5,659.42  

---

## 5. Results: In-Sample Fit (AIC, BIC, LogLikelihood)

### 5.1 Comparison

| Metric | NF-GARCH | Standard GARCH | Better |
|--------|----------|----------------|--------|
| **AIC** (lower better) | -16,711.98 | -23,419.17 | Standard |
| **BIC** (lower better) | -16,680.57 | -23,382.71 | Standard |
| **LogLikelihood** (higher better) | 8,361.24 | 11,715.27 | Standard |

### 5.2 Interpretation
- **In-sample:** Standard GARCH fits the *training* data better (lower AIC/BIC, higher LogLikelihood).  
- **Out-of-sample:** NF-GARCH has much better MSE and MAE and provides density forecasts.  
- For forecasting, out-of-sample and density performance are the relevant criteria; NF-GARCH is preferred there.

---

## 6. NF-GARCH vs Standard GARCH: Summary

| Dimension | NF-GARCH | Standard GARCH |
|-----------|----------|----------------|
| **Point forecasts (MSE, MAE)** | Much better (99.6%+ improvement, 100% win rate) | Baseline |
| **Density forecasts** | Full predictive distribution + PredictiveLogLik | Not available (single path) |
| **In-sample fit (AIC/BIC/LogLik)** | Weaker | Stronger |
| **Uncertainty / risk** | 1,000 paths, full distribution | Single path, no distribution |

**Conclusion:** NF-GARCH outperforms Standard GARCH on *forecast accuracy* (MSE, MAE) and is the only one with *density* forecasts; Standard GARCH wins only on in-sample fit.

---

## 7. Failed Models and Limitations

### 7.1 Failed Fits (5)
- **Model:** eGARCH (sstd)  
- **Assets:** EURUSD, GBPUSD, USDZAR, NVDA, MSFT  
- **Reason:** Optimizer convergence failure (code 52)  
- **eGARCH success:** 1/6 (AMZN only)

### 7.2 Omitted from Comparison
- Standard GARCH: no PredictiveLogLik (by design: single-path)  
- NF-GARCH: no direct density comparison with Standard GARCH, because Standard GARCH does not produce a density forecast in this setup  

---

## 8. Output Files

| File | Content |
|------|---------|
| `results/consolidated/NF_GARCH_Results_manual.xlsx` | NF-GARCH: Chrono_Split_NF_GARCH (25 rows), Chrono_Summary |
| `results/consolidated/NF_vs_Standard_GARCH_Comparison.xlsx` | NF vs Standard: Summary, Model_Comparison, Overall_Comparison, Win_Rate_Analysis, Combined_Results, Wilcoxon_Test, Asset_Class_Summary, Best_Model_By_Class |
| `results/consolidated/Analysis_Summary.xlsx` | Overall_Summary, Performance_by_Asset, Performance_by_Model, Failed_Models, Best_Performing_Models |
| `results/consolidated/FINAL_ANALYSIS_REPORT.md` | Detailed analysis report |
| `results/consolidated/ANALYSIS_COMPLETE.txt` | Short status and file list |
| `results/consolidated/CONSOLIDATED_RESULTS_SUMMARY.md` | This document |

---

## 9. One-Paragraph Summary

NF-GARCH (Normalizing Flow–GARCH) was run on 6 assets (3 FX, 3 equity) with 5 GARCH specs (sGARCH norm/sstd, gjrGARCH, eGARCH, TGARCH). Each NF-GARCH forecast used 1,000 paths; standard GARCH used one path. **Point forecasts:** NF-GARCH strongly outperforms: ~100% MSE improvement, ~99.6% MAE improvement, 18/18 wins, significant Wilcoxon tests. **Density forecasts:** Only NF-GARCH has Predictive Log-Likelihood (mean 2,338, max 6,230); standard GARCH has no density metric. **In-sample fit:** Standard GARCH has better AIC, BIC, and LogLikelihood. Five eGARCH fits failed (EURUSD, GBPUSD, USDZAR, NVDA, MSFT); 25/30 NF-GARCH models succeeded. **Bottom line:** NF-GARCH is better for out-of-sample point and density forecasting; standard GARCH fits in-sample better but forecasts worse and does not produce density forecasts.
