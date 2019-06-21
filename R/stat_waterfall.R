#' Transforms data for a horizon plot
StatWaterfall <- ggproto(
  "StatWaterfall",
  Stat,
  required_aes = c("x", "y", "group"),
  compute_group = function(data, scales) {
    data <- data[order(data$x), ]
    data$ymax <- c(0, head(data$y, -1))
    data$ymin <- data$y
    data$xmin <- c(NA, head(data$x, -1))
    data$xmax <- data$x
    data$fill2 <- data$ymax < data$ymin
    data$fill <- "#a50026"
    data$fill[data$fill2 == T] <- "#006837"
    data$fill2 <- data$ymax == data$ymin
    data$fill[data$ymax == data$ymin] <- "#000000"
    data
  }
)

#' Plot a time series as a waterfall plot
#'
#' @param mapping mapping
#' @param data data
#' @param show.legend legend
#' @param inherit.aes logical
#' @param na.rm logical
#' @param bandwidth bandwidth
#' @param ... more functions
#'
#' A waterfall plot highlights the change in the time series rather than the
#' value of the time series itself.
#'
#' @section Aesthetics: stat_waterfall needs x, y
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
#' \code{
#' +
#' xlab(NULL) +
#' ylab(NULL) +
#' theme(
#'    axis.text = element_blank(),
#'    axis.ticks = element_blank(),
#'    legend.position = 'none',
#'    strip.background = element_blank(),
#'    plot.background = element_blank(),
#'    panel.background = element_blank(),
#'    panel.border = element_blank(),
#'    panel.grid = element_blank(),
#'    panel.border = element_blank()
#' )
#' }
#' @section Also See: \code{\link{ggplot_waterfall}}, a
#' less flexible but more polished alternative.
#' @export
#' @import ggplot2
#' @examples {
#' library(ggplot2)
#' set.seed(1)
#' dfData = data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' ggplot(dfData, aes(x =x, y = y) )+
#'    stat_waterfall()}
stat_waterfall <- function(mapping = NULL,
                           data = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = T,
                           bandwidth = NULL,
                           ...) {
  list(
    layer(
      stat = StatWaterfall,
      data = data,
      mapping = mapping,
      geom = "rect",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )
}
