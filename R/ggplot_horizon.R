#' Plot a time series as a horizon plot
#'
#' A horizon plot breaks the Y dimension down using colours. This is useful
#' when visualising y values spanning a vast range and / or trying to highlight
#' outliers without losing context of the rest of the data.  Horizon
#' plots are best viewed in an apsect ratio of very low vertical length.
#'
#' @param dtData Data set which may include other columns apart from date
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
#' \code{
#' +
#' xlab(NULL) +
#' ylab(NULL) +
#' scale_fill_continuous(low = 'green', high = 'red') +
#' theme(
#'    axis.text = element_blank(),
#'    axis.ticks = element_blank(),
#'    legend.position = 'none',
#'    strip.background = element_blank(),
#'    # strip.text = element_blank(), # useful if only one year of data
#'    plot.background = element_blank(),
#'    panel.border = element_blank(),
#'    panel.background  = element_blank(),
#'    panel.grid = element_blank(),
#'    panel.border = element_blank()
#' ) +
#' coord_fixed( 0.5 * diff(range(dfData$x)) / diff(range(dfData$y)))
#' }
#' @return Returns a gpplot friendly object which means the user can use
#' ggplot scales, etc. to modify the look.
#' @section Also See: \code{\link{stat_horizon}}, a less polished but more
#' flexible alternative.
#' @import data.table
#' @import ggplot2
#' @export
#' @examples {
#' library(ggplot2)
#' set.seed(1)
#' dfData = data.frame(x = 1:1000, y = cumsum(rnorm(1000)))
#' p1 = ggplot_horizon(dfData, 'x', 'y')
#' p1
#' # add new geoms or colours
#' p1 +
#' geom_text(label = '!!!') +
#' scale_colour_continuous(low = 'red', high = 'green')
#' }
ggplot_horizon <- function(
                           dtData,
                           cXColumnName,
                           cYColumnName,
                           bandwidth = NULL,
                           vcGroupingColumnNames = NULL) {
  nMinY <- ""
  HorizonBracket <- ""
  HorizonY <- ""
  HorizonBracketGroup <- ""

  dtData <- copy(data.table(dtData))
  setkeyv(dtData, cXColumnName)
  if (is.null(bandwidth)) {
    bandwidth <- diff(range(dtData[, cYColumnName, with = F])) / 4
  }

  dtData[, nMinY := min(get(cYColumnName), na.rm = T), vcGroupingColumnNames]

  dtData[, HorizonBracket := ((get(cYColumnName) - nMinY) %/% bandwidth)]
  dtData[, HorizonY := get(cYColumnName) - (bandwidth * HorizonBracket) - nMinY]
  dtData[, HorizonBracketGroup := cumsum(c(0, diff(HorizonBracket) != 0))]

  ggplot_horizon <- ggplot(
    dtData,
    aes_string(x = cXColumnName, y = "HorizonY", fill = "HorizonBracket")
  ) +
    geom_bar(aes(y = (HorizonBracket > 0) * bandwidth, fill = HorizonBracket - 1), stat = "identity") +
    geom_bar(stat = "identity") +
    geom_line(aes(group = HorizonBracketGroup), color = "black")
  return(ggplot_horizon)
}
