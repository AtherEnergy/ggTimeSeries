#' Transforms data for a horizon plot
StatWaterfall <- ggproto("StatWaterfall", Stat,
  required_aes = c("x", "y", "group"),
  compute_group = function(data, scales) {

      # rolling over previous values to the next row
      # the rectangle of each waterfall unit will be drawn between these values
      data = data[order(data$x),]
      data$ymax = c(0,head(data$y,-1))
      data$ymin = data$y
      data$xmin = c(NA,head(data$x, -1))
      data$xmax = data$x
      data$fill2 = data$ymax < data$ymin
      data$fill = '#a50026'
      data$fill[data$fill2 == T] = '#006837'
      data$fill2 = data$ymax == data$ymin
      data$fill[data$ymax == data$ymin] = '#000000'

      data

  }
)


#' Plot a time series as a waterfall plot
#'
#' A waterfall plot highlights the change in the time series rather than the
#' value of the time series itself.
#'
#' @section Aesthetics: stat_waterfall needs x, y
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
#' \code{ \cr
#' + \cr
#' xlab(NULL) + \cr
#' ylab(NULL) + \cr
#' theme( \cr
#'    axis.text = element_blank(), \cr
#'    axis.ticks = element_blank(), \cr
#'    legend.position = 'none', \cr
#'    strip.background = element_blank(), \cr
#'    plot.background = element_blank(), \cr
#'    panel.background = element_blank(), \cr
#'    panel.border = element_blank(), \cr
#'    panel.grid = element_blank(), \cr
#'    panel.border = element_blank() \cr
#' ) \cr
#' }
#' @section Also See: \code{\link{ggplot_waterfall}}, a
#' flexible but less polished alternative.
#' @export
#' @import ggplot2
#' @examples
#' set.seed(1)
#' dfData = data.frame(x = 1:20, y = cumsum(rnorm(20)))
#' ggplot(dfData, aes(x =x, y = y) )+
#'    stat_waterfall()
stat_waterfall = function(
   mapping = NULL,
   data = NULL,
   show.legend = NA,
   inherit.aes = TRUE,
   na.rm = T,
   bandwidth = NULL,
   ...
) {

   list(
      layer(
         stat = StatWaterfall, data = data, mapping = mapping, geom = 'rect',
         position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, ...)
      )

   )

}

