# tstoolbox 0.1.0

* Initial CRAN submission

## Co-movement Analysis (New)
* `rolling_direction()` - Calculate co-movement proportion over rolling windows
* `direction_test()` - Statistical significance testing for co-movement (binomial, permutation, bootstrap)
* `plot_rolling_direction()` - Visualize rolling co-movement over time
* `direction_leadlag()` - Detect lead-lag relationships in directional co-movement
* `concordance()` - Harding-Pagan concordance index for formal co-movement measurement
* `asymmetric_direction()` - Analyse if co-movement differs during upturns vs downturns

## Direction Analysis
* `analyse_direction()` - Analyse co-movement between two numeric variables
* `direction()` - Return direction of change relative to previous value

## Cross-correlation
* `xcf()` - Create cross-correlation table
* `plot_xcf()` - Create pretty cross-correlation plot

## Adstock Transformations
* `adstock()` - Calculate adstock (decay) transformation
* `reverse_adstock()` - Convert adstocked values back to original

## Time Series Utilities
* `ts_summarise()` - Group-summarise a time series by time interval
* `return_k_date()` - Return k-th most recent or oldest date from a vector
* `pc_change()` - Calculate percentage change relative to lag k
* `stend_line()` - Generate linear vector between start and end values
* `sumlagdiff()` - Sum of absolute differences (fluctuation score)
