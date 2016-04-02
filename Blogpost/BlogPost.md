Line Charts Legacy
------------------

IoT devices generate a lot of sequential data over time, also called
time series data. Legacy portrayals of such data would centre around
line charts. Line charts have reportedly been around since the early
1700s (source: Wikipedia) and we have nothing against them. They
facilitate trend detection and comparison, are simple to draw, and easy
to understand; all in all a very well behaved visualisation. In modern
times, their use is wIDespread from the heartbeat monitor at a hospital
to the multiple-monitor display at a trader's desk.

We all remember these days -

    ## [1] "Excel 97 look recreated in R with the ggplot2 and ggthemes package"

<img src="BlogPost_files/figure-markdown_strict/excel97_line-1.png" title="" alt="" style="display: block; margin: auto;" />

Alternatives
------------

However there are cases when the data scientist becomes more demanding
and specific. Five alternatives available to such a data scientist are
listed below. We're a clean energy company so we used temperature data
from a random weather station somewhere in the USA. Climate change is a
real threat ... oh what's the point, we're never going to make it out of
Leo's Oscar speech's shadow.

Calendar Heatmap
----------------

A calendar heatmap is a great way to visualise daily data. Its structure
makes it easy to detect weekly, monthly, or seasonal patterns. The
smallest box is a day, the thicker borders demarcate months, and a year
forms one entire box.

The below chart plots the maximum temperature recorded on the day over
two years. Can you make out summer from winter? Does the winter of 2015
look warmer than the winter of 2014? Do weekends look slightly different
from weekdays?

    p1 = ggplot_calendar_heatmap(
       dtDataset[Variable %in% 'TMAX'],
       'Date',
       'Value'
    )

    # adding some formatting
    p1 + 
       xlab(NULL) + 
       ylab(NULL) + 
       scale_fill_continuous(low = 'green', high = 'red') + 
       facet_wrap(~Year, ncol = 1)

<img src="BlogPost_files/figure-markdown_strict/calendar_heatmap_continuous-1.png" title="" alt="" style="display: block; margin: auto;" />

### Horizon Plots

Available as `stat_horizon` and `ggplot_horizon`.

Imagine an area chart which has been chopped into multiple chunks of
equal height. If you overlay these chunks one on top of the the other,
and colour them to indicate which chunk it is, you get a horizon plot.
Horizon plots are useful when vertical space is constrained, and/or when
visualising y values spanning a vast range but with a skewed
distribution, and / or trying to highlight outliers without losing
context of variation in the rest of the data.

    # base plot
    p1 = ggplot(dtDataset[Variable == 'TMAX'][, Year := strftime(Date, '%Y')], aes(x = Date, y = Value, fill = Value)) + 
        # geom_bar(stat = 'identity') +
        stat_horizon() +
        facet_wrap(~Year, scale = 'free_x', ncol = 1)


    print("If you're seeing any vertical white stripes, it's a display thing.")

    ## [1] "If you're seeing any vertical white stripes, it's a display thing."

    # adding some formatting
    p1 + 
       xlab(NULL) + 
       ylab(NULL) + 
       scale_fill_continuous(low = 'green', high = 'red') 

<img src="BlogPost_files/figure-markdown_strict/horizon-1.png" title="" alt="" style="display: block; margin: auto;" />

### Steamgraphs

Available as `stat_steamgraph`.

A steamgraph is a more aesthetically appealing version of a stacked area
chart. It tries to highlight the changes in the data by placing the
groups with the most variance on the edges, and the groups with the
least variance towards the centre. This feature, in conjunction with the
centred alignment of each of the individual parts, makes it easier for
the viewer to compare the contribution of the individual components
across time.

    # base plot
    # the data looks created because we need to ensure the completeness of data
    p1 =  ggplot(
       merge(
          setnames(
             setDT(
                dtDataset[
                   Date >= as.POSIXct('20140401', format = '%Y%m%d', origin = '1960-01-01', tz = "GMT") &
                   Date < as.POSIXct('20150401', format = '%Y%m%d', origin = '1960-01-01', tz = "GMT") &
                   ID %in% c("CA005012961", "USC00456898", "US1COGN0018", "USC00051660", "USC00420072", "CA00117CA90", "CA001108906", "US1COJK0023"), 
                  expand.grid(unique(ID), unique(Date))
                ]
             ), 
             c('ID','Date')
          ),
          dtDataset[Variable %in% 'SNOW'],
          c('ID','Date'),
          all.x= T
       )[, 
          CumValue := CumValue - min(CumValue, na.rm = T), 
          list(ID)
       ][, 
          CumValue := na.locf(CumValue), 
          list(ID)
       ], 
       aes(x = Date, y = CumValue, group = ID, fill = ID)
       ) +
       stat_steamgraph()



    # adding some formatting
    p1 + 
       xlab(NULL) + 
       ylab(NULL)

<img src="BlogPost_files/figure-markdown_strict/steamgraph-1.png" title="" alt="" style="display: block; margin: auto;" />

### Waterfall

In some cases, instead of the values itself, you might want to see the
changes in the values.

    # base plot
    p1 = ggplot_waterfall(
       dtData = dtDataset[
          Variable %in% c('SNWD') & 
          ID %in% c("CA005012961") &
          Date >= as.POSIXct('20140401', format = '%Y%m%d', origin = '1960-01-01', tz = "GMT") &
          Date < as.POSIXct('20150401', format = '%Y%m%d', origin = '1960-01-01', tz = "GMT")
       ],
       'Date',
       'Value'
    )

    # adding some formatting
    p1 + 
       xlab(NULL) + 
       ylab(NULL)

<img src="BlogPost_files/figure-markdown_strict/waterfall-1.png" title="" alt="" style="display: block; margin: auto;" />

### Occurrence Dot Plot

This one is a favourite in infographics. For rare events, the reader
would find it convenient to have the count of events encoded in the
chart itself instead of having to map the value back to the Y axis.

    # base plot
    p1 = ggplot(
       dtDataset[ID %in% 'USC00051660' & Variable %in% 'PRCP'], 
       aes(
          x = Date, 
          y = Value/10)
       ) +
       stat_occurrence()

    # adding some formatting   
    p1 +
       xlab(NULL) + 
       ylab(NULL)

<img src="BlogPost_files/figure-markdown_strict/occurrence_dotplot-1.png" title="" alt="" style="display: block; margin: auto;" />

Data Attribution
----------------

<http://doi.org/10.7289/V5D21VHZ>

Downloaded from <ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/>

Menne, M.J., I. Durre, B. Korzeniewski, S. McNeal, K. Thomas, X. Yin, S.
Anthony, R. Ray, R.S. Vose, B.E.Gleason, and T.G. Houston, 2012: Global
Historical Climatology Network - Daily (GHCN-Daily), Version 3.12
