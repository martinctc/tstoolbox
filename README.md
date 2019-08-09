# tstoolbox
Useful tools for time series analysis

---

This package includes functions for:
* Direction Analysis - What is the proportion of data points where two time series move in the same direction?
* Cross-correlation analysis

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
(More to come!)

---
### Contact me
---
Please feel free to submit suggestions and report bugs: <https://github.com/martinctc/tstoolbox/issues>

Also check out my [website](https://martinctc.github.io) for my other work and packages.
