#' Plots a water fall plot
#'
#' A waterfall plot highlights the change in the time series rather than the
#' value of the time series itself.
#'
#' @param dtData Data set which may include other columns apart from the
#' columns mapped to x and y .
#' @param cXColumnName Column name of the x mapping.
#' @param cYColumnName Column name of the y mapping.
#' @param nArrowSize the size of the arrow head on the plot in cm
#' @param vcGroupingColumnNames The set of columns which together define the group
#' for the chart to operate between. If you plan to facet your plot,
#' you should specify the same column names to this argument.
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
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
#'    panel.background = element_blank(),
#'    panel.border = element_blank(),
#'    panel.grid = element_blank(),
#'    panel.border = element_blank()
#' )
#' }
#' @section Also See: \code{\link{stat_waterfall}}, a
#' flexible but less polished alternative.
#' @return Returns a gpplot friendly object which means the user can use
#' ggplot scales to modify the look, add more geoms, etc.
#' @import data.table
#' @import ggplot2
#' @export
#' @examples {
#' library(ggplot2)
#' set.seed(1)
#' dfData = data.frame(x = 1:100, y = cumsum(rnorm(100)))
#' ggplot_waterfall(
#'    dtData = dfData,
#'    'x',
#'    'y'
#' )}
ggplot_waterfall <- function(dtData,
                             cXColumnName,
                             cYColumnName,
                             nArrowSize = 0.25,
                             vcGroupingColumnNames = NULL) {
  NextY <- ""
  tail <- ""
  Change <- ""

  dtData <- copy(data.table(dtData))

  dtData[
    ,
    NextY := c(tail(get(cYColumnName), -1), NA),
    unique(vcGroupingColumnNames)
  ]

  dtData[, Change := "+"]
  dtData[NextY > get(cYColumnName), Change := "-"]
  dtData[NextY == get(cYColumnName), Change := ""]
  ggplotWaterfall <- ggplot() +
    geom_segment(
      data = dtData[get(cYColumnName) != NextY],
      aes_string(
        x = cXColumnName,
        y = cYColumnName,
        xend = cXColumnName,
        yend = "NextY",
        color = "Change"
      ),
      arrow = arrow(length = unit(nArrowSize, "cm"))
    ) +
    geom_point(
      data = dtData[get(cYColumnName) == NextY],
      aes_string(
        x = cXColumnName,
        y = cYColumnName,
        color = "Change"
      )
    ) +
    scale_color_manual(
      breaks = c("+", "-", ""),
      values = c("green", "red", "black")
    )
  ggplotWaterfall
}
