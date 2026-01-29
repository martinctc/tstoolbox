# tstoolbox

[![R build status](https://github.com/martinctc/tstoolbox/workflows/R-CMD-check/badge.svg)](https://github.com/martinctc/tstoolbox/actions) [![CodeFactor](https://www.codefactor.io/repository/github/martinctc/tstoolbox/badge)](https://www.codefactor.io/repository/github/martinctc/tstoolbox)

Tools for time series co-movement analysis and diagnostics.

## Overview

**tstoolbox** provides a comprehensive toolkit for analysing how time series move together. The package focuses on **directional co-movement** — measuring when series rise and fall in sync — with functions for:

- **Co-movement Analysis** — Measure how often two series move in the same direction
- **Rolling Analysis** — Track how relationships change over time
- **Lead-Lag Detection** — Identify which series leads or follows another
- **Statistical Testing** — Test if co-movement is significant
- **Asymmetry Detection** — Check if series co-move differently in upturns vs downturns

These tools work as diagnostic/exploratory aids for time series modelling or as standalone analysis methods.

---

## Installation

Install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("martinctc/tstoolbox")
```

---

## Quick Start

```r
library(tstoolbox)

# Simulate two related time series
set.seed(123)
x <- cumsum(rnorm(100))
y <- x + rnorm(100, sd = 0.5)

# Basic co-movement analysis
analyse_direction(data.frame(x, y), x, y)
#> There are 72 out of 99 instance(s) (73%) where values move in the same direction.

# Is this statistically significant?
direction_test(x, y)
#> Co-movement proportion: 0.7273
#> p-value: 1.189e-05

# How does co-movement change over time?
plot_rolling_direction(x, y, window = 20)

# Does one series lead the other?
direction_leadlag(x, y, max_lag = 5)
#> Contemporaneous relationship (no lead-lag) with 72.7% co-movement
```

---
## Function Reference

### Co-movement Analysis

| Function | Description |
|----------|-------------|
| `analyse_direction()` | Analyse co-movement between two variables with diagnostic output |
| `direction()` | Get direction of change ("Positive", "Negative", "Equal") for each observation |
| `concordance()` | Calculate Harding-Pagan concordance index |

### Temporal Analysis

| Function | Description |
|----------|-------------|
| `rolling_direction()` | Calculate co-movement over rolling windows |
| `plot_rolling_direction()` | Visualize rolling co-movement over time |
| `direction_leadlag()` | Detect lead-lag relationships |

### Statistical Testing

| Function | Description |
|----------|-------------|
| `direction_test()` | Test significance of co-movement (binomial, permutation, bootstrap) |
| `asymmetric_direction()` | Test if co-movement differs in upturns vs downturns |

### Cross-Correlation

| Function | Description |
|----------|-------------|
| `xcf()` | Create cross-correlation table |
| `plot_xcf()` | Generate cross-correlation plot |

### Adstock Transformations

| Function | Description |
|----------|-------------|
| `adstock()` | Apply adstock (decay) transformation |
| `reverse_adstock()` | Reverse adstock transformation |

### Utilities

| Function | Description |
|----------|-------------|
| `ts_summarise()` | Aggregate time series by time period |
| `return_k_date()` | Get k-th most recent/oldest date |
| `pc_change()` | Calculate percentage change |
| `stend_line()` | Linear interpolation between start and end |
| `sumlagdiff()` | Sum of absolute differences (fluctuation score) |

---

## Examples

### Detecting Lead-Lag Relationships

```r
# x leads y by 2 periods
x <- cumsum(rnorm(100))
y <- dplyr::lag(x, 2) + rnorm(100, sd = 0.3)

result <- direction_leadlag(x, y, max_lag = 5)
print(result)
#> Optimal lag: -2
#> Interpretation: x leads y by 2 period(s) with 85.6% co-movement

plot(result)
```

### Asymmetric Co-movement

```r
# Do series co-move more during downturns?
asymmetric_direction(x, y)
#> During upturns: 71.2% (n = 52)
#> During downturns: 78.3% (n = 46)
#> Interpretation: Stronger co-movement during downturns
```

### Rolling Analysis with Dates
```r
dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 100)
plot_rolling_direction(x, y, window = 12, time = dates)
```

---

## Contact

Please submit suggestions and report bugs: <https://github.com/martinctc/tstoolbox/issues>

Also check out my [website](https://martinctc.github.io) for my other work and packages.
