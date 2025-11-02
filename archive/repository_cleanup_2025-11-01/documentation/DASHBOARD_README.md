# Interactive HTML Dashboard - Quick Reference

## Overview

An interactive HTML dashboard (`dashboard_visualizations.html`) has been created to provide a visual walkthrough of all research results with explanations, citations, and links.

## How to View

1. **Open in Browser:**
   - Navigate to `results/dashboard_visualizations.html`
   - Double-click the file to open in your default web browser
   - Or right-click → "Open with" → choose your browser

2. **Features:**
   - Sticky navigation menu for easy section access
   - Interactive sections with embedded plots
   - Clickable links to detailed Excel files
   - Comprehensive citations and references
   - Professional styling with responsive design

## Dashboard Sections

1. **Research Overview**
   - Methodology explanation
   - Dataset description
   - Evaluation framework summary

2. **Model Performance**
   - MSE comparison by model
   - AIC comparison by model
   - Links to detailed results

3. **Distributional Metrics**
   - Kolmogorov-Smirnov distance
   - Wasserstein distance
   - Tail index (Hill estimator)
   - Detailed explanations of each metric

4. **Stylized Facts**
   - Volatility clustering by asset class
   - Leverage effect by asset class
   - Theoretical background

5. **VaR Backtesting**
   - Exceedance rates
   - Kupiec test results
   - Risk calibration validation

6. **Stress Testing**
   - Historical crisis analysis (2008 GFC, 2020 COVID)
   - Hypothetical shock scenarios
   - Model robustness assessment

7. **NF vs Standard Comparison**
   - Side-by-side performance comparison
   - Asset-class dependent findings
   - Statistical significance tests

8. **References**
   - Complete bibliography
   - Links to original papers
   - Further reading suggestions

## Generated Files

### Visualizations (13 plots)
Location: `results/dashboard_plots/`

- `mse_by_model_chrono.png` - Forecasting accuracy comparison
- `aic_by_model.png` - Model fit comparison
- `ks_distance_by_model.png` - Distributional fit (KS distance)
- `wasserstein_distance_by_model.png` - Distributional fit (Wasserstein)
- `tail_index_by_model.png` - Tail heaviness analysis
- `volatility_clustering_by_class.png` - Stylized fact: volatility clustering
- `leverage_effect_by_class.png` - Stylized fact: leverage effect
- `var_exceedance_rates.png` - Risk calibration validation
- `kupiec_pvalues.png` - Statistical backtesting results
- `historical_crisis_volatility.png` - Crisis period analysis
- `hypothetical_shock_impact.png` - Scenario stress testing
- `nf_vs_standard_mse.png` - NF vs Standard performance
- `nf_vs_standard_by_class.png` - Asset class analysis

### HTML Dashboard
Location: `results/dashboard_visualizations.html`

- Self-contained HTML file
- All plots embedded as images
- Links to Excel files for detailed data
- Complete citations and references

### Data Files
Location: `results/consolidated/`

- `Final_Dashboard.xlsx` - Comprehensive Excel dashboard
- `NF_GARCH_Results_manual.xlsx` - NF-GARCH simulation results
- `Distributional_Metrics.xlsx` - Distributional analysis
- `Stylized_Facts.xlsx` - Stylized facts analysis
- `VaR_Backtesting.xlsx` - Risk calibration results
- `Stress_Testing.xlsx` - Stress testing scenarios
- `NF_vs_Standard_GARCH_Comparison.xlsx` - Comparison analysis

## Quick Start

1. Open `results/dashboard_visualizations.html` in a web browser
2. Use the navigation menu to jump to specific sections
3. Click on plot captions for explanations
4. Use file links to access detailed Excel data
5. Reference section provides citations and further reading

## Technical Details

- **Visualization Generation:** `scripts/evaluation/generate_dashboard_visualizations.R`
- **Plot Format:** PNG (300 DPI)
- **HTML Format:** Self-contained (no external dependencies)
- **Browser Compatibility:** Modern browsers (Chrome, Firefox, Edge, Safari)

## Citation Information

All plots and analyses can be cited using:
- Original research references (provided in HTML dashboard)
- Repository link: https://github.com/AIHawking187/Financial-SDG-GARCH
- Research project: MSc Mathematical Statistics, University of the Witwatersrand

## Notes

- Plots are generated automatically from consolidated results
- HTML dashboard is static (no server required)
- All visualizations use consistent color schemes and styling
- Excel files provide detailed numerical results for further analysis

