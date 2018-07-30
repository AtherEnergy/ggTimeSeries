#' Transforms data for a horizon plot
StatHorizon <- ggproto(
  "StatHorizon",
  Stat,
  required_aes = c("x", "y"),
  setup_params = function(data, params) {
    if (is.null(params$bandwidth)) {
      params$bandwidth <- diff(range(data$y)) / 4
    }
    params$nMinY <- min(data$y, na.rm = T)
    params
  },
  compute_group = function(data, scales, bandwidth, nMinY) {
    data$fill <- ((data$y - nMinY) %/% bandwidth) + 1

    data$y <- data$y - (bandwidth * (data$fill - 1)) - nMinY
    lBandedData <- lapply(
      sort(unique(data$fill)),
      function(iFillBand) {
        dfBandedData <- data[data$fill == iFillBand, ]
        dfBandedDataHigh <- data[data$fill > iFillBand, ]
        if (nrow(dfBandedDataHigh) > 0) {
          dfBandedDataHigh$y <- bandwidth
          dfBandedDataHigh$fill <- iFillBand
        }
        dfBandedDataLow <- data[data$fill < iFillBand, ]
        if (nrow(dfBandedDataLow) > 0) {
          dfBandedDataLow$y <- 0
          dfBandedDataLow$fill <- iFillBand
        }

        data <- rbind(
          rbind(
            dfBandedData,
            dfBandedDataLow
          ),
          dfBandedDataHigh
        )
        data$fill <- data$fill * bandwidth
        data
      }
    )
    data <- do.call(rbind, lBandedData)
    viRemovalCandidates <- c(
      F,
      diff(data$fill) == 0 &
        diff(data$y) == 0 & head(data$y, -1) %in% c(0, bandwidth)
    )
    viRemovalCandidates <- viRemovalCandidates[(diff(viRemovalCandidates) != 1)]
    data$group <- data$fill
    data
  }
)

#' Plot a time series as a horizon plot
#'
#' @param mapping mapping
#' @param data dataframe
#' @param show.legend logical
#' @param inherit.aes logical
#' @param na.rm logical
#' @param bandwidth bandwith
#' @param ... other functions
#'
#' A horizon plot breaks the Y dimension down using colours. This is useful
#' when visualising y values spanning a vast range and / or trying to highlight
#' outliers without losing context of the rest of the data.  Horizon
#' plots are best viewed in an apsect ratio of very low vertical length.
#'
#' @section Aesthetics: x, y, fill. Fill argument is overridden internally but
#' is required for ggplot to assign a colour / fill scale.
#' @section Other parameters: bandwidth, to dictate the span of a band.
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
#' @section Also See: \code{\link{ggplot_horizon}}, a more polished but less
#' flexible alternative.
#' @export
#' @import ggplot2
#' @examples {
#' library(ggplot2)
#' ggplot(data.frame(x = 1:89, y = as.numeric(unlist(austres))), aes(x =x, y = y, fill = y) )+
#'    stat_horizon() +
#'    scale_fill_continuous(low = 'white', high = 'red')
#'
#' set.seed(10)
#' ggplot(data.frame(x = 1:1000, y = cumsum(rnorm(1000))), aes(x =x, y = y, fill = y) )+
#'    stat_horizon() +
#'    scale_fill_continuous(low = 'white', high = 'red')}
stat_horizon <- function(mapping = NULL,
                         data = NULL,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         na.rm = T,
                         bandwidth = NULL,
                         ...) {
  list(
    layer(
      stat = StatHorizon,
      data = data,
      mapping = mapping,
      geom = "bar",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
    ),
    layer(
      stat = StatHorizon,
      data = data,
      mapping = mapping,
      geom = "line",
      position = "identity",
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
    )
  )
}
