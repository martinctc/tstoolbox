# tstoolbox

[![R build status](https://github.com/martinctc/tstoolbox/workflows/R-CMD-check/badge.svg)](https://github.com/martinctc/tstoolbox/actions)

Useful tools for time series analysis

This is a package built on functions that I've created in time-series analysis that I have produced in the past. Like many packages, not all of this would be 100% original work - some of them would be built upon the work of others, or are convenient wrappers around functions from other packages that perform more of the heavy-lifting work. I hope you would find this package useful!

---

This package includes functions for:
* Direction Analysis - What is the proportion of data points where two time series move in the same direction?
* Cross-correlation analysis - exploring lagged correlations
* Calculating adstock (and "reverse" calculate the pre-transformed values using adstocked values)

The functions in this package can work as supplementary tools to validate or support hypotheses which come out of time series modelling. They can also work as early diagnostic / exploratory tools pre-modelling.

---

### Installation

surveytoolbox is not release on CRAN (yet). 
You can install the latest development version from GitHub with:

```
install.packages("devtools")
devtools::install_github("martinctc/tstoolbox")
```
---

This package is currently still under development, so it does come with a health advice: if you do wish to use them - have a check and run through the examples before assimilating them into your analysis. 

---
### Function Overview

- `analyse_direction()` analyses co-movement between two numeric variables, returning a diagnostic explanation.
- `return_k_date()` returns the _kth_ most recent or oldest date-time from a date-time vector.
- `plot_xcf()` generates a cross-correlation plot as a 'pretty' ggplot object.
(More to come!)

---
### Contact me
---
Please feel free to submit suggestions and report bugs: <https://github.com/martinctc/tstoolbox/issues>

Also check out my [website](https://martinctc.github.io) for my other work and packages.
