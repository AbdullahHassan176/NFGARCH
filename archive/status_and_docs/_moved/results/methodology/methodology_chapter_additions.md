# Methodology Chapter Additions

This document contains text sections for inclusion in Chapter 3 (Methodology) of the dissertation.

## 3.X.1 Hyperparameter Selection Methodology

Hyperparameters for Normalizing Flow models were selected through sensitivity analysis, varying each parameter one at a time while keeping others constant at base values. This approach provides clear insight into each parameter's impact while being computationally efficient compared to full grid search.

Four key hyperparameters were tested: (1) num_layers: [3, 4, 5, 6] - controls model depth, (2) hidden_features: [32, 64, 128] - controls model width, (3) learning_rate: [0.0005, 0.001, 0.002] - controls optimization step size, (4) batch_size: [256, 512, 1024] - controls training batch size. For each hyperparameter, we evaluated model performance using validation loss, KS statistic, and Wasserstein distance. The final configuration (num_layers=4, hidden_features=64, learning_rate=0.001, batch_size=512) was selected to minimize validation loss while maintaining reasonable training time and preventing overfitting.

## 3.X.2 Residual Stationarity Validation

To validate the assumption of residual stationarity after GARCH filtering, we performed comprehensive diagnostic tests on all GARCH residuals. The tests included: (1) Augmented Dickey-Fuller (ADF) test for unit roots (null: non-stationary, alternative: stationary), (2) KPSS test for trend stationarity (null: stationary, alternative: non-stationary), (3) Ljung-Box test for serial correlation (null: no serial correlation), and (4) ARCH LM test for remaining heteroskedasticity (null: no ARCH effects).

Results from these tests (see Table X.X) show that the majority of GARCH residuals pass stationarity tests, with ADF tests rejecting the null hypothesis of non-stationarity (p < 0.05) for most model-asset combinations, and KPSS tests failing to reject the null hypothesis of stationarity (p >= 0.05). However, some residuals show remaining ARCH effects, indicating that complete heteroskedasticity removal may not be achieved in all cases. This finding is acknowledged as a limitation of the two-stage approach.

## 3.X.3 Conditional Heterogeneity Analysis

The two-stage pipeline structure (GARCH fitting followed by NF training on residuals) assumes that residual distributions are unconditional. However, financial return innovations often exhibit conditional heterogeneity that could affect flow stability. To assess this, we performed several tests: (1) rolling window variance analysis to detect time-varying volatility in residuals, (2) structural break tests (CUSUM) to identify regime changes, (3) time-varying distribution analysis using rolling window statistics, and (4) enhanced ARCH effects testing at multiple lags.

Results indicate that while GARCH filtering removes most conditional heteroskedasticity, some residual conditional heterogeneity may remain in certain model-asset combinations. This is particularly evident in the rolling variance analysis, where some residuals show time-varying characteristics. We acknowledge this limitation and note that the NF model stability may be affected by such conditional heterogeneity. However, the impact appears to be limited based on our analysis of distribution stability across different time periods.

## 3.X.4 Methodological Limitations and Assumptions

Several methodological limitations should be acknowledged: (1) The two-stage approach assumes unconditional residual distributions, which may not hold if conditional heterogeneity persists after GARCH filtering. (2) Hyperparameter selection used one-at-a-time sensitivity analysis, assuming parameter independence; joint optimization could potentially yield better results. (3) The analysis was performed on a subset of model-asset combinations for computational efficiency. (4) NF model stability across different market regimes requires further investigation. These limitations are explicitly acknowledged to maintain methodological transparency and rigor.

## References to Results Tables

- Hyperparameter sensitivity results: `Methodology_Hyperparameter_Sensitivity.xlsx`
- Residual stationarity tests: `Methodology_Residual_Stationarity.xlsx`
- Conditional heterogeneity analysis: `Methodology_Conditional_Heterogeneity.xlsx`
- Consolidated results: `Methodology_Consolidated.xlsx`

