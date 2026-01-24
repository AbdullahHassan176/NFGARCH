# Multi-Seed Synthetic Distribution Recovery Experiment - Results

**Date**: 2026-01-09
**Number of seeds**: 10

## Overview

This report summarizes results from running the synthetic distribution recovery experiment across multiple random seeds to assess stability and robustness.

## Evaluation Modes

### RAW Mode

Metrics computed on distributions as produced by each pipeline (including scale drift).
This answers: **"What distribution does each full pipeline output?"**

### SHAPE Mode

Metrics computed on standardized distributions (mean=0, SD=1).
This answers: **"How well does each method recover the innovation SHAPE ignoring scale/mean drift?"**

## Aggregated Recovery Metrics (RAW Mode)

| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |
|--------|-------------------|----------------------|---------------------|---------------------|
| Gaussian_GARCH | 0.2888±0.0605 | 1.2979±0.0147 | 0.2906±0.3402 | 3.6839±4.0731 |
| NF_GARCH | 0.1643±0.0177 | 0.5662±0.0845 | 1.5285±0.4618 | 6.6169±6.5508 |
| Student_t_GARCH | 0.0964±0.0095 | 1.3196±0.0143 | 0.4112±0.3291 | 5.4505±5.0550 |

## Aggregated Recovery Metrics (SHAPE Mode)

| Method | KS Stat (mean±sd) | Wasserstein (mean±sd) | Skew Diff (mean±sd) | Kurt Diff (mean±sd) |
|--------|-------------------|----------------------|---------------------|---------------------|
| Gaussian_GARCH | 0.0805±0.0151 | 1.0448±0.0138 | 0.2906±0.3402 | 3.6839±4.0731 |
| NF_GARCH | 0.1144±0.0078 | 0.2209±0.0144 | 1.5285±0.4618 | 6.6169±6.5508 |
| Student_t_GARCH | 0.0514±0.0060 | 1.0276±0.0211 | 0.4112±0.3291 | 5.4505±5.0550 |

## Scale Drift Analysis (RAW Mode)

| Method | Mean (mean±sd) | SD (mean±sd) |
|--------|---------------|--------------|
| Gaussian GARCH | -0.4560±0.1564 | 1.3240±0.0718 |
| NF-GARCH | 0.0919±0.0733 | 1.5286±0.0893 |
| Student-t GARCH | 0.0769±0.0224 | 1.5485±0.0459 |
| True | 0.0014±0.0047 | 0.9992±0.0066 |

## NF-GARCH Skewness Sign Match Rate

**Rate**: 40%

This is the proportion of seeds where NF-GARCH recovered the correct sign of skewness.

## Winner Summary

### RAW Mode Winners

| Metric | Winner |
|--------|--------|
| KS Statistic | Student_t_GARCH |
| Wasserstein Distance | NF_GARCH |
| Skewness Difference | Gaussian_GARCH |
| Kurtosis Difference | Gaussian_GARCH |

### SHAPE Mode Winners

| Metric | Winner |
|--------|--------|
| KS Statistic | Student_t_GARCH |
| Wasserstein Distance | NF_GARCH |
| Skewness Difference | Gaussian_GARCH |
| Kurtosis Difference | Gaussian_GARCH |

## Files Generated

- `recovery_metrics_raw_aggregate.csv`: Aggregated RAW metrics
- `recovery_metrics_shape_aggregate.csv`: Aggregated SHAPE metrics
- `summary_statistics_raw_aggregate.csv`: Aggregated RAW summary statistics
- `summary_statistics_shape_aggregate.csv`: Aggregated SHAPE summary statistics
- `seed_*/`: Per-seed results directories

## Notes

- RAW metrics include scale drift effects
- SHAPE metrics isolate distributional shape recovery
- Lower values indicate better recovery
- Standard deviations indicate stability across seeds

