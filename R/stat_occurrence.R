#' Transforms data for an occurrence plot
StatOccurrence <- ggproto("StatOccurrence", Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {

      data = do.call(
         rbind,
         lapply(
            seq(nrow(data)),
            function(iRow) {
               data.frame(
                  x = data$x[iRow],
                  y = 0:round(data$y[iRow])
               )
            }
         )
      )

      data = data[data$y != 0,]

      data

  }
)


#' Plots a time series as a dot plot
#'
#' For rare events, it's convenient to have the count of events encoded in
#' the chart itself. A bar chart requires the user to perceive the y axis which
#' this does not.
#'
#' @section Aesthetics: x, y
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
#' \code{ \cr +
#'    xlab(NULL) + \cr
#'    ylab(NULL) + \cr
#'    scale_fill_continuous(low = 'green', high = 'red') + \cr
#'    theme( \cr
#'       axis.text.y = element_blank(), \cr
#'       axis.ticks.y = element_blank(), \cr
#'       legend.position = 'none', \cr
#'       strip.background = element_blank(), \cr
#'       panel.background = element_blank(), \cr
#'       panel.border = element_blank(), \cr
#'       panel.grid = element_blank(), \cr
#'       panel.border = element_blank() \cr
#'    ) + \cr
#'    coord_fixed(ylim = c(0,1 + max(dfData$y))) \cr
#' } \cr
#' \code{\link{ggplot2::coord_fixed()}} can provide a balance to the aspect
#' ratio of the chart.
#' @export
#' @import ggplot2
#' @examples
#' set.seed(1)
#' dfData = data.table(x = 1:100, y = floor(4 * abs(rnorm(100, 0 , 0.4))))
#' ggplot(dfData, aes(x =x, y = y) )+
#'    stat_occurrence() +
#'    coord_fixed()
stat_occurrence = function(
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
         stat = StatOccurrence, data = data, mapping = mapping, geom = 'point',
         position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, ...)
      )

   )

}

