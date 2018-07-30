#' Marimekkofy
#'
#' @param data dataframe
#' @param xbucket x value
#' @param ybucket y value
#' @param weight weight value
#'
#' @return df
#' @export
Marimekkofy <- function(data,
                        xbucket = "xbucket",
                        ybucket = "ybucket",
                        weight = NULL) {
  xmax <- ""
  xmin <- ""
  ymax <- ""
  ymin <- ""
  fill <- ""
  data2 <- as.data.table(data)

  if (is.null(weight) | !weight %in% colnames(data2)) {
    warning("weight aesthetic not defined. Weight defaulted to 1 for every row.")
    data2[, weight := as.numeric(1)]
    weight <- "weight"
  }
  setnames(
    data2,
    c(xbucket, ybucket, weight),
    c("xbucket", "ybucket", "weight")
  )
  dtX <- data2[
    ,
    list(weight = as.numeric(sum(weight))),
    xbucket
  ]
  setkey(dtX, xbucket)

  dtX[
    ,
    xmax := cumsum(weight / sum(weight))
  ]
  dtX[
    ,
    xmin := xmax - (weight / sum(weight))
  ][
    ,
    weight := NULL
  ]

  dtY <- data2[
    ,
    list(weight = as.numeric(sum(weight))),
    list(
      xbucket,
      ybucket
    )
  ]

  setkey(dtY, xbucket, ybucket)

  dtY[
    ,
    ymax := cumsum(weight / sum(weight)),
    xbucket
  ]

  dtY[
    ,
    ymin := ymax - (weight / sum(weight)),
    xbucket
  ][
    ,
    weight := NULL
  ]

  data <- merge(dtX,
    dtY,
    "xbucket",
    allow.cartesian = T
  )

  data <- merge(
    data,
    data2[, list(xbucket, ybucket, fill)],
    c("xbucket", "ybucket")
  )
  setnames(
    data,
    c("xbucket", "ybucket"),
    c(xbucket, ybucket)
  )
  return(data)
}

StatMarimekkoLabels <- ggproto(
  "StatMarimekko",
  Stat,
  required_aes = c("xbucket", "ybucket"),
  setup_params = function(data, params) {
    if (is.null(params$xlabelyposition)) {
      params$xlabelyposition <- 1.2
    }
    overalldata <- rbindlist(lapply(
      unique(data$PANEL),
      function(cPANEL) {
        Marimekkofy(
          data[cPANEL == data$PANEL, ],
          "xbucket",
          "ybucket",
          "weight"
        )[
          ,
          PANEL := cPANEL
        ]
      }
    ))
    overalldata <- overalldata[
      ,
      list(
        x = (xmin[1] + xmax[1]) / 2,
        y = params$xlabelyposition
      ),
      list(
        label = xbucket,
        PANEL
      )
    ]
    return(list(
      overalldata = overalldata,
      na.rm = T
    ))
  },
  compute_group = function(data,
                             scales,
                             overalldata,
                             xlabelyposition) {
    data <- overalldata
    return(data)
  }
)

#' Transforms data for the tiles of the heatmap
StatMarimekko <- ggproto(
  "StatMarimekko",
  Stat,
  required_aes = c("xbucket", "ybucket"),
  setup_params = function(data, params) {
    overalldata2 <- rbindlist(lapply(
      unique(data$PANEL),
      function(cPANEL) {
        Marimekkofy(
          data[cPANEL == data$PANEL, ],
          "xbucket",
          "ybucket",
          "weight"
        )[
          ,
          PANEL := cPANEL
        ]
      }
    ))
    return(list(
      overalldata2 = overalldata2,
      na.rm = T
    ))
  },
  compute_group = function(data,
                             scales,
                             overalldata2,
                             xlabelyposition) {
    data <- overalldata2
    return(data)
  }
)
#' Plot two categorical variables as marimekko
#'
#' @param mapping mapping
#' @param data data
#' @param show.legend logical
#' @param inherit.aes logical
#' @param na.rm logical
#' @param xlabelyposition position
#' @param ... other functions
#' A marimekko plot, or a mosaic plot, visualises the co-occurrence of two
#' categorical / ordinal variables. In a time series, it could be used to
#' visualise the transitions from one state to another by considering each
#' state to be a category and plotting current category vs. next category.
#'
#' @section Aesthetics: xbucket, ybucket, fill. Fill argument needs to be
#' assigned to ybucket., or some other column which is a one to one mapping of ybucket.
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
#' @export
#' @import ggplot2
#' @examples {
#' library(ggplot2)
#' ggplot(
#'    data.frame(
#'       x1 = round(3 * runif(10000), 0),
#'       y1 = pmax(pmin(round(3 * rnorm(10000), 0), 3), -3),
#'       weight = 1:10000
#'    )
#' ) +
#'    stat_marimekko(
#'       aes(
#'          xbucket = x1,
#'          ybucket = y1,
#'          fill = factor(y1),
#'          weight = weight
#'       ),
#'       xlabelyposition = 1.1,
#'       color = 'black'
#'    )}
stat_marimekko <- function(mapping = NULL,
                           data = NULL,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = T,
                           xlabelyposition = NULL,
                           ...) {
  list(
    layer(
      stat = StatMarimekko,
      data = data,
      mapping = mapping,
      geom = "rect",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(xlabelyposition = xlabelyposition, na.rm = na.rm, ...)
    ),
    layer(
      stat = StatMarimekkoLabels,
      data = data,
      mapping = mapping,
      geom = "text",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(xlabelyposition = xlabelyposition, na.rm = na.rm, ...)
    ),
    coord_fixed()
  )
}
