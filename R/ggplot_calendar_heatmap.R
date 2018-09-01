#' Plots a calendar heatmap
#'
#' A calendar heatmap provides context for weeks, and day of week which makes
#' it a better way to visualise daily data than line charts. Largely uses
#' Codoremifa's code from
#' stackoverflow.com/questions/22815688/calendar-time-series-with-r.
#'
#'
#' @param dtDateValue Data set which may include other columns apart from date
#' and values.
#' @param cDateColumnName Column name of the dates.
#' @param cValueColumnName Column name of the data.
#' @param vcGroupingColumnNames The set of columns which together define the group
#' for the chart to operate within If you plan to facet your plot,
#' you should specify the same column names to this argument. The function
#' will automatically add the veriable for the year to the facet.
#' @param dayBorderSize Size of the border around each day
#' @param dayBorderColour Colour of the border around each day
#' @param monthBorderSize Size of the border around each month
#' @param monthBorderColour Colour of the border around each month
#' @param monthBorderLineEnd Line end for the border around each month
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
#'    panel.border = element_blank(),
#'    panel.background  = element_blank(),
#'    panel.grid = element_blank(),
#'    panel.border = element_blank()
#' )
#' }
#' @section Also See: \code{\link{stat_calendar_heatmap}}, a
#' flexible but less polished alternative.
#' @return Returns a gpplot friendly object which means the user can use
#' ggplot scales to modify the look, add more geoms, etc.
#' @import data.table
#' @import ggplot2
#' @import stats
#'
#' @importFrom utils "globalVariables"
#'
#' @export
#' @examples {
#' library(data.table)
#' library(ggplot2)
#' set.seed(1)
#' dtData = data.table(
#'       DateCol = seq(
#'          as.Date("1/01/2014", "%d/%m/%Y"),
#'          as.Date("31/12/2015", "%d/%m/%Y"),
#'          "days"
#'       ),
#'       ValueCol = runif(730)
#'    )
#' # you could also try categorical data with
#' # ValueCol = sample(c('a','b','c'), 730, replace = T)
#' p1 = ggplot_calendar_heatmap(
#'    dtData,
#'    'DateCol',
#'    'ValueCol'
#' )
#' p1
#' # add new geoms
#' p1 +
#' geom_text(label = '!!!') +
#' scale_colour_continuous(low = 'red', high = 'green')
#' }
ggplot_calendar_heatmap <- function(dtDateValue,
                                    cDateColumnName = "",
                                    cValueColumnName = "",
                                    vcGroupingColumnNames = "Year",
                                    dayBorderSize = 0.25,
                                    dayBorderColour = "black",
                                    monthBorderSize = 2,
                                    monthBorderColour = "black",
                                    monthBorderLineEnd = "round") {
  Year <- ""
  MonthOfYear <- ""
  WeekOfYear <- ""
  DayOfWeek <- ""
  as.formula <- ""
  MonthChange <- ""
  meanWeekOfYear <- ""

  dtDateValue <- copy(data.table(dtDateValue))
  dtDateValue[, Year := as.integer(strftime(get(cDateColumnName), "%Y"))]
  vcGroupingColumnNames <- unique(c(vcGroupingColumnNames, "Year"))
  dtDateValue <- merge(
    dtDateValue,
    setnames(
      dtDateValue[
        ,
        list(DateCol = seq(
          min(get(cDateColumnName)),
          max(get(cDateColumnName)),
          "days"
        )),
        vcGroupingColumnNames
      ],
      "DateCol",
      cDateColumnName
    ),
    c(vcGroupingColumnNames, cDateColumnName),
    all = T
  )

  dtDateValue[, MonthOfYear := as.integer(strftime(get(cDateColumnName), "%m"))]
  dtDateValue[, WeekOfYear := 1 + as.integer(strftime(get(cDateColumnName), "%W"))]
  dtDateValue[, DayOfWeek := as.integer(strftime(get(cDateColumnName), "%w"))]
  dtDateValue[DayOfWeek == 0L, DayOfWeek := 7L]

  ggplotcalendar_heatmap <-
    ggplot(
      data = dtDateValue[, list(WeekOfYear, DayOfWeek)],
      aes(
        x = WeekOfYear,
        y = DayOfWeek
      )
    ) +
    geom_tile(
      data = dtDateValue,
      aes_string(fill = cValueColumnName),
      color = dayBorderColour,
      size = dayBorderSize
    ) +
    coord_fixed() +
    xlab("Month") +
    ylab("DoW") +
    facet_wrap(as.formula(paste(
      "~", paste(vcGroupingColumnNames, collapse = "+")
    )))

  setkeyv(
    dtDateValue,
    c(
      vcGroupingColumnNames,
      "DayOfWeek",
      "WeekOfYear",
      "MonthOfYear"
    )
  )
  dtDateValue[, MonthChange := c(1, diff(MonthOfYear)), c(vcGroupingColumnNames, "DayOfWeek")]
  dtMonthChangeDatasetBetweenWeeks <- dtDateValue[MonthChange == 1]
  dtMonthChangeDatasetBetweenWeeks[, WeekOfYear := WeekOfYear - 0.5]
  dtMonthChangeDatasetBetweenWeeks <- rbind(
    dtMonthChangeDatasetBetweenWeeks[, c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), with = F],
    dtDateValue[, list(WeekOfYear = 0.5 + max(WeekOfYear)), c(vcGroupingColumnNames, "DayOfWeek")]
  )
  if (nrow(dtMonthChangeDatasetBetweenWeeks) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
      geom_segment(
        data = dtMonthChangeDatasetBetweenWeeks,
        aes(
          x = WeekOfYear,
          xend = WeekOfYear,
          y = DayOfWeek - 0.5,
          yend = DayOfWeek + 0.5
        ),
        size = monthBorderSize,
        colour = monthBorderColour,
        lineend = monthBorderLineEnd
      )
  }
  setkeyv(
    dtDateValue,
    c(
      vcGroupingColumnNames,
      "WeekOfYear",
      "DayOfWeek",
      "MonthOfYear"
    )
  )
  dtDateValue[, MonthChange := c(1, diff(MonthOfYear)), vcGroupingColumnNames]
  MonthChangeDatasetWithinWeek <- dtDateValue[MonthChange == 1 &
    (DayOfWeek != 1)]
  MonthChangeDatasetWithinWeek[, DayOfWeek := DayOfWeek - 0.5]
  MonthChangeDatasetWithinWeek <- rbind(
    MonthChangeDatasetWithinWeek[, c("DayOfWeek", "WeekOfYear", vcGroupingColumnNames), with = F],
    dtDateValue[, list(DayOfWeek = c(min(DayOfWeek) - 0.5, max(DayOfWeek) + 0.5)), c(vcGroupingColumnNames, "WeekOfYear")]
  )
  if (nrow(MonthChangeDatasetWithinWeek) > 0) {
    ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
      geom_segment(
        data = MonthChangeDatasetWithinWeek,
        aes(
          x = WeekOfYear - 0.5,
          xend = WeekOfYear + 0.5,
          y = DayOfWeek,
          yend = DayOfWeek
        ),
        size = monthBorderSize,
        colour = monthBorderColour,
        lineend = monthBorderLineEnd
      )
  }
  dtMonthLabels <- dtDateValue[,
    list(meanWeekOfYear = mean(WeekOfYear)),
    by = c("MonthOfYear")
  ]
  dtMonthLabels[, MonthOfYear := month.abb[MonthOfYear]]
  ggplotcalendar_heatmap <- ggplotcalendar_heatmap +
    scale_x_continuous(
      breaks = dtMonthLabels[, meanWeekOfYear],
      labels = dtMonthLabels[, MonthOfYear],
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      trans = "reverse",
      breaks = c(1:7),
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
      expand = c(0, 0)
    )
  return(ggplotcalendar_heatmap)
}
