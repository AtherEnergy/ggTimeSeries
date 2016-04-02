#' Plot a time series as a horizon plot
#'
#' A horizon plot breaks the Y dimension down using colours. This is useful
#' when visualising y values spanning a vast range and / or trying to highlight
#' outliers without losing context of the rest of the data.\cr \cr Horizon
#' plots are best viewed in an apsect ratio of very low vertical length.
#'
#' @param dtDateValue Data set which may include other columns apart from date
#' and values.
#' @param cXColumnName Column name of dates.
#' @param cYColumnName Column name of values.
#' @param bandwidth the width of one band of Y values.
#' easier to differentiate between the bands.
#' @param vcGroupingColumnNames The set of columns which together define the group
#' for the chart to operate within If you plan to facet your plot,
#' you should specify the same column names to this argument.
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the example output object:
#' \code{ \cr
#' + \cr
#' xlab(NULL) + \cr
#' ylab(NULL) + \cr
#' scale_fill_continuous(low = 'green', high = 'red') + \cr
#' theme( \cr
#'    axis.text = element_blank(), \cr
#'    axis.ticks = element_blank(), \cr
#'    legend.position = 'none', \cr
#'    strip.background = element_blank(), \cr
#'    # strip.text = element_blank(), # useful if only one year of data \cr
#'    plot.background = element_blank(), \cr
#'    panel.border = element_blank(), \cr
#'    panel.background  = element_blank(), \cr
#'    panel.grid = element_blank(), \cr
#'    panel.border = element_blank() \cr
#' ) +
#' coord_fixed( 0.5 * diff(range(dfData$x)) / diff(range(dfData$y)))\cr
#' }
#' @return Returns a gpplot friendly object which means the user can use
#' ggplot scales, etc. to modify the look.
#' @section Also See: \code{\link{stat_horizon}}, a less polished but more
#' flexible alternative.
#' @import data.table
#' @import ggplot2
#' @export
#' @examples
#' set.seed(1)
#' dfData = data.frame(x = 1:1000, y = cumsum(rnorm(1000)))
#' p1 = ggplot_horizon(dfData, 'x', 'y')
#' p1
#' # add new geoms or colours
#' p1 +
#' geom_text(label = '!!!') +
#' scale_colour_continuous(low = 'red', high = 'green')
ggplot_horizon = function(
   dtData,
   cXColumnName,
   cYColumnName,
   bandwidth = NULL,
   vcGroupingColumnNames = NULL
) {

   setDT(dtData)
   setkeyv(dtData, cXColumnName)

   # calculating a default bandwidth
   if (is.null(bandwidth)) {

      bandwidth = diff(range(dtData[, cYColumnName, with = F])) / 4

   }

   # calculating the lowest value, all the horizon bracketed y values will
   # need to be offset by this
   dtData[, nMinY := min(get(cYColumnName), na.rm = T), vcGroupingColumnNames]

   # Calculating the horizon bands and the new Y values
   dtData[, HorizonBracket := ((get(cYColumnName) - nMinY) %/% bandwidth)]
   dtData[, HorizonY := get(cYColumnName) - (bandwidth * HorizonBracket) - nMinY]
   dtData[, HorizonBracketGroup := cumsum(c(0, diff(HorizonBracket) != 0))]

   # adding the horizon bands
   # need to use bars because areas show a blank region if there is only one
   # data point in that band
   ggplot_horizon = ggplot(
      dtData,
      aes_string(x = cXColumnName, y = 'HorizonY', fill = 'HorizonBracket')
   ) +
      geom_bar(aes( y = (HorizonBracket > 0) * bandwidth, fill = HorizonBracket - 1), stat = 'identity') +
      geom_bar(stat = 'identity') +
      geom_line(aes(group = HorizonBracketGroup), color = 'black')
      # geom_area(aes(group = HorizonBracketGroup, y = (HorizonBracket > 0) * bandwidth, fill = HorizonBracket - 1), stat = 'identity') +
      # geom_area(aes(group = HorizonBracketGroup), stat = 'identity', color = 'black')

   return (ggplot_horizon)

}
