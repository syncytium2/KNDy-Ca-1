#' Add a jitter geom to a ggplot
#' 
#' Defaults to a circle with fill and outline (shape 21). Provides defaults for
#' size, alpha (fill density), jitter width, and jitter height
#' 
#' https://stackoverflow.com/questions/77055966/shapes-border-color-is-darker-than-the-fill-after-setting-alpha-in-ggplot2
#' https://github.com/tidyverse/ggplot2/issues/2152
#' https://stackoverflow.com/questions/39661304/ggplot2-different-alpha-values-for-border-and-filling-of-geom-point
#' 
#' Issue in output if using a low alpha where the inner fill and outer stroke overlap for a portion of the stroke
#' and give a composite color. Switching to define the alpha when defining the color and fill scales
#' and always using 1 for the color alpha and just changing the inner fill
#'
#' @param size a number. Size of the shape. Default is 1.5
#' @param alpha a number. density of fill. Default is 1
#' @param width a number. jitter width - spread of the data horizontally. Default is 0.35
#' @param height a number. jitter height - spread of the data vertically. Default is 0
#'
#' @return a geom_jitter with selected values
#' @export
#'
#' @examples
jitterGeom <- function(
  size = 1.5,
  alpha = 1,
  width = 0.42,
  height = 0
  , forManuscript = isManuscript
  , dodgeWidth = NULL
){
  # geom_jitter(
  #   shape = 21,
  #   size = size,
  #   alpha = alpha,
  #   width = width,
  #   height = height
  # )
  geom_quasirandom(
    shape = ifelse(forManuscript, 16, 21)
    , size = size
    , alpha = alpha
    , width = width
    , dodge.width = dodgeWidth
  )
  # geom_beeswarm(
  #   shape = 21
  #   , size = size
  #   , alpha = alpha
  #   # , width = width
  # )
}

jitterGeom_shapeAes <- function(
  size = 1.5,
  alpha = 1,
  width = 0.42,
  height = 0
){
  # geom_jitter(
  #   size = size,
  #   alpha = alpha,
  #   width = width,
  #   height = height
  # )
  
  #shows the distribution of the data
  geom_quasirandom(
    size = size
    # , alpha = alpha
    , width = width
  )
  
  # tries to align everything in the center, then distribute so no overlap
  
  # geom_beeswarm(
  #   size = size
  #   , alpha = alpha
  #   # , width = width
  # )
}

addMeanHorizontalBar <- function(
  width = 0.9,
  size = ifelse(isManuscript, 0.6, 1),
  addLineType = FALSE,
  lineTypeName = "early life trt",
  lineTypeGuide = c("STD" = "dotted", "LBN" = "solid"),
  typeVar = earlyLifeTrt,
  barPosition = "identity",
  meanColor = "black", # added 2023-11-22, possible that this will cause problems where the mean color is based on a group
  ... # Into aes
){
  if(!addLineType){
    geom <- stat_summary(
        geom = "meanbar", 
        fun.y = mean,
        width = width,
        linewidth = size,
        position = barPosition,
        color = meanColor,
        aes(...)
      )
  } else{
    geom <- list(
      stat_summary(
        geom = "meanbar",
        fun.y = mean,
        width = width,
        linewidth = size,
        position = barPosition,
        color = meanColor,
        aes(linetype = {{ typeVar }}, ...)
      ),
      scale_linetype_manual(lineTypeName, values = lineTypeGuide)
    )
  }
}

addMeanSE_vertBar <- function(
  size = ifelse(isManuscript, 0.6, 1),
  barPosition = "identity",
  barColor = "black", # added 2023-11-22, possible that this will cause problems where the bar color is based on a group
  ... # into aes
){
  stat_summary(
    geom = "linerange", 
    fun.data = mean_se,
    # size = size,
    linewidth = size,
    position = barPosition,
    color = barColor,
    aes(...),
    show.legend = FALSE
  )
}

addMedianHorizontalBar <- function(
  width = 0.9,
  size = ifelse(isManuscript, 0.6, 1),
  color = "black",
  alpha = 1
){
  stat_summary(
    geom = "meanbar",
    fun.y = median,
    width = width,
    linewidth = size,
    color = color,
    alpha = alpha
  )
}



# Lines -------------------------------------------------------------------

my_geom_line <- function(
  useLineType = TRUE,
  lineTypeVar,
  lineGroupVar,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8
){
  # aesList <- list(
  #   if(useLineType) linetype = {{ lineTypeVar }},
  #   group = {{ lineGroupVar }}
  # )
  list(
    geom_line(
      alpha = indivLineAlpha, # semi-transparent
      if(useLineType){
        aes(linetype = {{ lineTypeVar }},
            group = {{ lineGroupVar }})
      } else{
        aes(group = {{ lineGroupVar }})
      },
      # aes(
      #   if(useLineType){linetype = {{ lineTypeVar }}},
      #   group = {{ lineGroupVar }}
      #   # aesList
      # ),
      linewidth = indivLineSize
    )
  )
}

mean_geom_line <- function(
  useLineType, #TRUE/FALSE
  lineTypeVar,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6
){
  list(
    stat_summary(
      fun = mean, 
      geom = "line", 
      if(useLineType){aes(linetype = {{ lineTypeVar }})}, 
      # size = meanLineSize, 
      linewidth = meanLineSize,
      alpha = meanAlpha
    ),
    stat_summary(
      geom = "errorbar", 
      fun.data = mean_se, 
      if(useLineType){aes(group = {{ lineTypeVar }})}, 
      # size = errorBarSize, 
      linewidth = errorBarSize, 
      width = errorBarWidth, 
      # color = errorBarColor, 
      alpha = errorBarAlpha
    )
  )
}

plot_line_geom_layers <- function(
  useLineType, # TRUE/FALSE
  lineTypeVar,
  lineGroupVar,
  xtitle, #x axis label
  ytitle, #y axis label
  title = NULL, # plot title
  individualLines = TRUE, # plot individual lines
  meanLines = TRUE, # plot mean lines with SE
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  indivLineAlpha = 0.5,
  indivLineSize = 0.8,
  errorBarWidth = 1,
  meanLineSize = 1.4,
  meanAlpha = 1,
  errorBarSize = 1,
  # errorBarColor = "grey10",
  errorBarAlpha = 0.6,
  textSize = 11,
  axisSize = 0.5
  , clipVal = "on"
){
  list(
    labs(
      x = xtitle,
      y = ytitle,
      title = title
    ),
    if(individualLines)
      my_geom_line(
        # Will always plot based on lineTypeVar
        useLineType = useLineType,
        lineTypeVar = {{ lineTypeVar }},
        lineGroupVar = {{ lineGroupVar }},
        indivLineAlpha = indivLineAlpha,
        indivLineSize = indivLineSize
      ),
    if(meanLines)
      mean_geom_line(
        useLineType = useLineType,
        lineTypeVar = {{ lineTypeVar }},
        errorBarWidth = errorBarWidth,
        meanLineSize = meanLineSize,
        meanAlpha = meanAlpha,
        errorBarSize = errorBarSize,
        # errorBarColor = errorBarColor,
        errorBarAlpha = errorBarAlpha
      ),
    expand_limits(y = 0),
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}, clip = clipVal),
    textTheme(size = textSize),
    boxTheme(axisSize = axisSize)
  )
}
