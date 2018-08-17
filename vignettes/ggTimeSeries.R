## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 3
)

## ----initialisation, echo = FALSE, message = F, warning = F--------------
library(ggthemes)
library(ggplot2)
library(data.table)
library(ggTimeSeries)

## ----excel97_line, ext = 'png', fig.align = 'center', echo = FALSE, message = F, warning = F----
set.seed(10)
dfData = data.frame(
   Time = 1:100,
   Signal = abs(
      c(
         cumsum(rnorm(100, 0, 3)), 
         cumsum(rnorm(100, 0, 4)), 
         cumsum(rnorm(100, 0, 1)), 
         cumsum(rnorm(100, 0, 2))
      )
   ),
   Variable = c(rep('a', 100), rep('b', 100), rep('c', 100), rep('d', 100)),
   VariableLabel = c(rep('Class A', 100), rep('Class B', 100), rep('Class C', 100), rep('Class D', 100))
)

Excel97Plot = ggplot(dfData, aes(x = Time, y = Signal, color = VariableLabel)) +
   geom_line() +
   geom_point() +
   theme_excel() + 
   scale_colour_excel()

print("Excel 97 look recreated in R with the ggthemes package")
plot(Excel97Plot)


## ----minimalTheme--------------------------------------------------------
minimalTheme = theme_set(theme_bw(12))
minimalTheme = theme_update(
   axis.ticks = element_blank(), 
   legend.position = 'none',
   strip.background = element_blank(), 
   panel.border = element_blank(), 
   panel.background = element_blank(), 
   panel.grid = element_blank()
)


## ----calendar_heatmap, fig.align = 'center', echo = TRUE, message = F, warning = F----

# creating some data
set.seed(1)
dtData = data.table(
      DateCol = seq(
         as.Date("1/01/2014", "%d/%m/%Y"),
         as.Date("31/12/2015", "%d/%m/%Y"),
         "days"
      ),
      ValueCol = runif(730)
   )
dtData[, ValueCol := ValueCol + (strftime(DateCol,"%u") %in% c(6,7) * runif(1) * 0.75), .I]
dtData[, ValueCol := ValueCol + (abs(as.numeric(strftime(DateCol,"%m")) - 6.5)) * runif(1) * 0.75, .I]

# base plot
p1 = ggplot_calendar_heatmap(
   dtData,
   'DateCol',
   'ValueCol'
)

# adding some formatting
p1 + 
   xlab(NULL) + 
   ylab(NULL) + 
   scale_fill_continuous(low = 'green', high = 'red') + 
   facet_wrap(~Year, ncol = 1)


# creating some categorical data
dtData[, CategCol := letters[1 + round(ValueCol * 7)]]

# base plot
p2 = ggplot_calendar_heatmap(
   dtData,
   'DateCol',
   'CategCol'
)

# adding some formatting
p2 + 
   xlab(NULL) + 
   ylab(NULL) + 
   facet_wrap(~Year, ncol = 1)



## ----horizon, fig.align = 'center', echo = TRUE, message = F, warning = F----

# creating some data
set.seed(1)
dfData = data.frame(x = 1:1000, y = cumsum(rnorm(1000)))

# base plot
p1 = ggplot_horizon(dfData, 'x', 'y')


print("If you're seeing any vertical white stripes, it's a display thing.")
# adding some formatting
p1 + 
   xlab(NULL) + 
   ylab(NULL) + 
   scale_fill_continuous(low = 'green', high = 'red') + 
   coord_fixed( 0.5 * diff(range(dfData$x)) / diff(range(dfData$y)))

## ----steamgraph, fig.align = 'center', echo = TRUE, message = F, warning = F----
# creating some data
set.seed(10)
dfData = data.frame(
   Time = 1:1000,
   Signal = abs(
      c(
         cumsum(rnorm(1000, 0, 3)), 
         cumsum(rnorm(1000, 0, 4)), 
         cumsum(rnorm(1000, 0, 1)),
         cumsum(rnorm(1000, 0, 2))
      )
   ),
   VariableLabel = c(rep('Class A', 1000), rep('Class B', 1000), rep('Class C', 1000), rep('Class D', 1000))
)

# base plot
p1 = ggplot(dfData, aes(x = Time, y = Signal, group = VariableLabel, fill = VariableLabel)) +
  stat_steamgraph()


# adding some formatting
p1 + 
   xlab(NULL) + 
   ylab(NULL) + 
   coord_fixed( 0.2 * diff(range(dfData$Time)) / diff(range(dfData$Signal)))


## ----waterfall, fig.align = 'center', echo = TRUE, message = F, warning = F----
# creating some data
set.seed(1)
dfData = data.frame(x = 1:100, y = cumsum(rnorm(100)))

# base plot
p1 = ggplot_waterfall(
   dtData = dfData,
   'x',
   'y'
)

# adding some formatting
p1 + 
   xlab(NULL) + 
   ylab(NULL)

## ----occurrence_dotplot, fig.align = 'center', echo = TRUE, message = F, warning = F----
# creating some data
set.seed(1)
dfData = data.table(x = 1:100, y = floor(4 * abs(rnorm(100, 0 , 0.4))))

# base plot
p1 = ggplot(dfData, aes(x =x, y = y) )+
   stat_occurrence()

# adding some formatting   
p1 +
   xlab(NULL) + 
   ylab(NULL) + 
   coord_fixed(ylim = c(0,1 + max(dfData$y)))

## ----marimekko, fig.align = 'center', echo = TRUE, message = F, warning = F----
# creating some data
set.seed(1)

dfData = data.frame(Signal = pmax(pmin(rnorm(10000), 3), -3))

dfData2 = data.frame(
   Signal = round(head(dfData$Signal, -1),0),
   NextSignal = round(tail(dfData$Signal, -1),0),
   Weight = 1
)

# base plot
p1 = ggplot(dfData2, aes(xbucket = Signal, ybucket = NextSignal, fill = NextSignal, weight = Weight) )+
   stat_marimekko(color = 'black', xlabelyposition = -0.1)

# adding some formatting   
p1 +
   xlab('Signal occurrence %') + 
   ylab('Signal | Next signal occurrence %') +
   scale_x_continuous(breaks = 0:10/10) +
   scale_y_continuous(breaks = 0:10/10)

