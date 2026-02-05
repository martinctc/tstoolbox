#' Plot rolling co-movement proportion over time
#'
#' Creates a ggplot2 visualization of rolling co-movement between two time
#' series, with a reference line at 0.5 (random/no relationship).
#'
#' @param x Numeric vector for the first time series.
#' @param y Numeric vector for the second time series (same length as `x`).
#' @param window Integer specifying the rolling window size. Defaults to 12.
#' @param time Optional vector of time/date values for the x-axis. If NULL,
#'   uses sequence indices.
#' @param align Character string specifying window alignment: "right" (default),
#'   "center", or "left".
#' @param title Character string for the plot title.
#' @param show_bands Logical. If TRUE, shows colored bands indicating
#'   co-movement strength. Defaults to TRUE.
#'
#' @return A ggplot object.
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item A line showing rolling co-movement proportion
#'   \item A horizontal reference line at 0.5 (chance level)
#'   \item Optional colored bands: green (>0.5, positive co-movement),
#'     red (<0.5, inverse movement)
#' }
#'
#' @examples
#' # Simulated data with changing relationship
#' set.seed(123)
#' n <- 200
#' x <- cumsum(rnorm(n))
#' # First half: strong co-movement, second half: weak
#' y <- c(x[1:100] + rnorm(100, sd = 0.3), 
#'        rnorm(100))
#' 
#' plot_rolling_direction(x, y, window = 20)
#'
#' # With dates
#' dates <- seq(as.Date("2020-01-01"), by = "month", length.out = n)
#' plot_rolling_direction(x, y, window = 20, time = dates)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_ribbon
#'   scale_y_continuous labs theme_minimal theme element_text annotate
#' @importFrom scales percent_format
#' @export
plot_rolling_direction <- function(x, y, 
                                   window = 12, 
                                   time = NULL,
                                   align = "right",
                                   title = "Rolling Co-movement",
                                   show_bands = TRUE) {
  
  # Calculate rolling direction
  rolling_prop <- rolling_direction(x, y, window = window, align = align)
  
  # Create time index if not provided
  if (is.null(time)) {
    time <- seq_along(x)
  }
  
  # Build data frame
  df <- data.frame(
    time = time,
    comovement = rolling_prop
  )
  
  # Remove NAs for cleaner plotting
  df_clean <- df[!is.na(df$comovement), ]
  
  # Avoid R CMD check notes
  comovement <- NULL
  
  # Base plot
  p <- ggplot2::ggplot(df_clean, ggplot2::aes(x = time, y = comovement))
  
  # Add bands if requested
  if (show_bands) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = min(df_clean$time), xmax = max(df_clean$time),
        ymin = 0.5, ymax = 1,
        fill = "#339933", alpha = 0.1
      ) +
      ggplot2::annotate(
        "rect",
        xmin = min(df_clean$time), xmax = max(df_clean$time),
        ymin = 0, ymax = 0.5,
        fill = "#cc0000", alpha = 0.1
      )
  }
  
  # Add line and reference
  p <- p +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", 
                        color = "gray40", linewidth = 0.5) +
    ggplot2::geom_line(color = "#2c3e50", linewidth = 0.8) +
    ggplot2::scale_y_continuous(limits = c(0, 1), 
                                labels = scales::percent_format()) +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Window size: ", window),
      x = NULL,
      y = "Co-movement proportion"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  p
}
