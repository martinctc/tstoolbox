#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' tstoolbox: Tools for Time Series Analysis and Diagnostics
#'
#' @description
#' The tstoolbox package provides a comprehensive toolkit for analysing how time 
#' series move together. The package focuses on directional co-movement — measuring 
#' when series rise and fall in sync — with functions for statistical testing, 
#' lead-lag detection, and asymmetry analysis.
#'
#' @section Co-movement Analysis:
#' Core functions for measuring directional co-movement:
#' \itemize{
#'   \item \code{\link{analyse_direction}} - Analyse co-movement between two variables with diagnostic output
#'   \item \code{\link{direction}} - Get direction of change for each observation
#'   \item \code{\link{concordance}} - Calculate Harding-Pagan concordance index
#' }
#'
#' @section Temporal Analysis:
#' Track relationships over time:
#' \itemize{
#'   \item \code{\link{rolling_direction}} - Calculate co-movement over rolling windows
#'   \item \code{\link{plot_rolling_direction}} - Visualize rolling co-movement
#'   \item \code{\link{direction_leadlag}} - Detect lead-lag relationships
#' }
#'
#' @section Statistical Testing:
#' Test significance and asymmetry:
#' \itemize{
#'   \item \code{\link{direction_test}} - Test significance of co-movement
#'   \item \code{\link{asymmetric_direction}} - Test if co-movement differs in upturns vs downturns
#' }
#'
#' @section Cross-Correlation:
#' Traditional cross-correlation analysis:
#' \itemize{
#'   \item \code{\link{xcf}} - Create cross-correlation table
#'   \item \code{\link{plot_xcf}} - Generate cross-correlation plot
#' }
#'
#' @section Adstock Transformations:
#' Marketing mix modeling utilities:
#' \itemize{
#'   \item \code{\link{adstock}} - Apply adstock (decay) transformation
#'   \item \code{\link{reverse_adstock}} - Reverse adstock transformation
#' }
#'
#' @section Time Series Utilities:
#' Additional helper functions:
#' \itemize{
#'   \item \code{\link{ts_summarise}} - Group-summarise a time series by interval
#'   \item \code{\link{pc_change}} - Calculate percentage change
#'   \item \code{\link{stend_line}} - Generate linear vector between values
#'   \item \code{\link{sumlagdiff}} - Sum of absolute differences (fluctuation score)
#'   \item \code{\link{return_k_date}} - Return k-th most recent or oldest date
#' }
#'
#' @author Martin Chan \email{martinchan53@@gmail.com}
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/martinctc/tstoolbox}
#'   \item Report bugs at \url{https://github.com/martinctc/tstoolbox/issues}
#' }
#'
"_PACKAGE"
