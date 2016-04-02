#' Transforms data for the month breaks of the calendar heatmap
#' @import data.table
StatCalendarMonthBreaks <- ggproto(
   "StatCalendarMonthBreaks",
   Stat,
   required_aes = c("date", "fill"),
   compute_group = function(data, scales) {

      # pre processing
      data2 = data.table(date = data[, 'date'])
      data2[, Year := as.integer(strftime(date, '%Y'))]

      data2 = data2[,
            list(
               date = seq(
                  as.Date(paste0("1/1/",Year), "%d/%m/%Y"),
                  as.Date(paste0("31/12/",Year), "%d/%m/%Y"),
                  'days'
               )
            ),
            Year
         ]
      data2[, MonthOfYear := as.integer(strftime(date, '%m'))]
      data2[, x := 1 + as.integer(strftime(date, '%W'))]
      data2[, y := as.integer(strftime(date, '%w'))]
      data2[y == 0, y := 7L]
      data2[, y := 8 - y]

      # identifying last weeks for each month
      setkey(data2, Year, y, x, MonthOfYear)
      data2[, MonthChange := c(1,diff(MonthOfYear)), list(Year,y)]
      MonthChangeDatasetBetweenWeeks = data2[MonthChange == 1]
      MonthChangeDatasetBetweenWeeks[, x := x - 0.5]
      MonthChangeDatasetBetweenWeeks[, y := y - 0.5]
      MonthChangeDatasetBetweenWeeks[, xend := x]
      MonthChangeDatasetBetweenWeeks[, yend := y + 1]

      # identifying last weekday of the last week for each month
      setkey(data2, Year, x, y, MonthOfYear)
      data2[, MonthChange := c(1,abs(diff(MonthOfYear))), list(Year, x)]
      MonthChangeDatasetWithinWeek = data2[MonthChange == 1]
      MonthChangeDatasetWithinWeek[, y := y - 0.5]
      MonthChangeDatasetWithinWeek[, x := x - 0.5]
      MonthChangeDatasetWithinWeek[, xend := x + 1]
      MonthChangeDatasetWithinWeek[, yend := y]

      # the line on top before all Mondays
      RightBorder = data2[, list(x = max(x), date = date[which.max(x)]), y]
      RightBorder[, y := y - 0.5]
      RightBorder[, x := x + 0.5]
      RightBorder[, xend := x]
      RightBorder[, yend := y + 1]

      # the line on the right after the last week of the year
      TopBorder = data2[, list(y = max(y), date = date[which.max(y)]), x]
      TopBorder[, y := y + 0.5]
      TopBorder[, x := x - 0.5]
      TopBorder[, xend := x + 1]
      TopBorder[, yend := y]

      # consolidating data
      data2 = rbind(
         MonthChangeDatasetWithinWeek[,c('date','x','y','xend','yend'), with = F],
         MonthChangeDatasetBetweenWeeks[,c('date','x','y','xend','yend'), with = F]
      )

      data2 = rbind(
         data2,
         TopBorder[,c('date','x','y','xend','yend'), with = F]
      )

      data2 = rbind(
         data2,
         RightBorder[,c('date','x','y','xend','yend'), with = F]
      )

      data2$group = data$group[1]
      data2$PANEL = data$PANEL[1]
      data = merge(data, data2, c('date','PANEL','group'), all = T)

      data

  }
)

#' Transforms data for the tiles of the heatmap
StatCalendarTiles <- ggproto(
   "StatCalendarTiles",
   Stat,
   required_aes = c("date", "fill"),
   compute_group = function(data, scales) {

      # calculating the transformed x and y for the heatmap
      data$x = 1 + as.integer(strftime(data$date, '%W'))
      data$y = as.integer(strftime(data$date, '%w'))
      data$y[data$y == 0L] = 7
      data$y = 8 - data$y
      data

  }
)


#' Plots a calendar heatmap
#'
#' A calendar heatmap provides context for weeks, and day of week and
#' is a better way to visualise daily data than line charts.
#'
#' @section Aesthetics: date, fill.
#' @section Data Tips: \code{\link{base::strftime}} can help extract the value
#' of the year, week of year, and day of week from the date column. You might
#' need to extract the year to facet multiple years as demonstrated in the
#' example. \cr\cr
#' This stat uses the following transformation to obtain the x and  y
#' coordinate to be used in the heatmap -
#' \code{ \cr
#'    data$x = 1 + as.integer(strftime(data$date, "\%W")) \cr
#'    data$y = as.integer(strftime(data$date, "\%w")) \cr
#'    data$y[data$y == 0L] = 7 \cr
#'    data$y = 8 - data$y \cr
#' }
#' @section Cosmetic Tips: The minimalist look can be achieved by appending the
#' following chunk of code to the output object:
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
#'    panel.background = element_blank(), \cr
#'    panel.border = element_blank() \cr
#' ) \cr
#' }
#' @section Also See: \code{\link{ggplot_calendar_heatmap}}, a
#' polished but less flexible alternative.
#' @export
#' @import ggplot2
#' @examples
#' DailyData = data.frame(
#'       DateCol = seq(
#'          as.Date("1/01/2014", "%d/%m/%Y"),
#'          as.Date("31/12/2015", "%d/%m/%Y"),
#'          "days"
#'       ),
#'       ValueCol = runif(730)
#'    )
#'    DailyData$Year = strftime(DailyData$DateCol, "%Y")
#' ggplot(
#'    DailyData,
#'    aes(
#'       date = DateCol,
#'       fill = ValueCol
#'    )
#' ) +
#'    stat_calendar_heatmap() +
#'    facet_wrap(~Year, ncol = 1)
stat_calendar_heatmap = function(
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
         stat = StatCalendarTiles, data = data, mapping = mapping, geom = 'tile',
         position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, ...)
      ),
      layer(
         stat = StatCalendarMonthBreaks, data = data, mapping = mapping, geom = 'segment',
         position = 'identity', show.legend = show.legend, inherit.aes = inherit.aes,
         params = list(na.rm = na.rm, ...)
      ),
      coord_fixed()

   )

}
