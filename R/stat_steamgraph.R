#' Transforms data for a steam graph
#' @import data.table
StatSteamgraph <- ggproto(
   "StatSteamgraph",
   Stat,
   required_aes = c("x", "y", "group"),
   setup_params = function(data, params) {

      # converting dataset to data.table because it's just more convenient
      setDT(data)


      # deciding the order of the streams such that the high sd streams fall
      # on the outside and low sd streams fall on the inside
      datasdorder = data[,
         list(
            sdy = sd(y)
         ),
         by = list(group)
      ][,
         ranksdy := rank(sdy, ties.method = 'random')
      ][
         ranksdy %%2 == 0,
         ranksdy := -ranksdy
      ]

      data = merge(
         data,
         datasdorder,
         'group',
         all = T
     )


      # offsetting values to make the stream
      setkey(data, x, ranksdy)
      data[, ymax := cumsum(y) - (sum(y)/2), by = list(x, PANEL)]
      data[, ymin := ymax - y, by = list(x, PANEL)]

      list(
         overalldata = data,
         na.rm = T
      )

  },
  compute_group = function(data, scales, overalldata) {

      # picking out the values in this group
      # all the calculation already happened in setup_parms
      data = overalldata[group %in% data$group]
      data.frame(
         x = data[, x],
         ymin = data[, ymin],
         ymax = data[, ymax]
      )

  }
)


#' Plot multiple time series as a steamgraph
#'
#' Plots \code{\link{ggplot2::geom_ribbon}}s  for each time series and stacks
#' them one on top of the other. It's a more aesthetically appealing version
#' of a stacked area chart. The groups with the most variance are placed on the
#' outside, and the groups with the least variance are placed on the inside.
#'
#' @section Aesthetics: geom_steamgraph needs x, y, group, fill.
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the example output object:
#' \code{ \cr
#' + \cr
#' xlab(NULL) + \cr
#' ylab(NULL) + \cr
#' theme( \cr
#'    axis.text = element_blank(), \cr
#'    axis.ticks = element_blank(), \cr
#'    legend.position = 'none', \cr
#'    strip.background = element_blank(), \cr
#'    # strip.text = element_blank(), # useful if only one year of data \cr
#'    plot.background = element_blank(), \cr
#'    panel.background = element_blank(), \cr
#'    panel.border = element_blank(), \cr
#'    panel.grid = element_blank(), \cr
#'    panel.border = element_blank() \cr
#' ) + \cr
#' coord_fixed( 0.2 * diff(range(df$Time)) / diff(range(df$Signal))) \cr
#' }
#' @export
#' @import ggplot2
#' @examples
#' set.seed(10)
#' df = data.frame(
#'    Time = 1:1000,
#'    Signal = abs(c(cumsum(rnorm(1000, 0, 3)), cumsum(rnorm(1000, 0, 4)), cumsum(rnorm(1000, 0, 1)), cumsum(rnorm(1000, 0, 2)))),
#'    Variable = c(rep('a', 1000), rep('b', 1000), rep('c', 1000), rep('d', 1000)),
#'    VariableLabel = c(rep('Class A', 1000), rep('Class B', 1000), rep('Class C', 1000), rep('Class D', 1000))
#' )
#'
#' ggplot(df, aes(x = Time, y = Signal, group = Variable, fill = VariableLabel)) +
#'   stat_steamgraph()
stat_steamgraph = function(
   mapping = NULL,
   data = NULL,
   show.legend = NA,
   inherit.aes = TRUE,
   na.rm = T,
   ...
) {


   layer(
      stat = StatSteamgraph, data = data, mapping = mapping, geom = 'ribbon',
      position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
   )

}
