getErrorDF_LMM <- function(
    mixedModel
    , xVarAsChar
    , errorType = "model"
    , ... # other things to afex_plot
) {
  lmmData <- afex_plot(
    mixedModel
    , xVarAsChar
    , error_ci = FALSE
    , return = "data"
    , error = errorType
    , ...
  )
  return(
    lmmData$means %>%
      as.data.frame()
  )
}

makeManualErrorDF_LMM <- function(
    emmModel
    , makeGroupCols = TRUE
){
  df <- emmModel %>%
    as.data.frame() %>%
    rename(
      y = response
    ) %>%
    mutate(
      lower = y - SE
      , upper = y + SE
    )
  
  if(makeGroupCols){
    df <- df %>%    
      combineGroups()
  }
  return(df)
}

plotError_LMM <- function(
    lmmData
    , xVar
    , meanBarWidth = 0.9
    , barSize = ifelse(isManuscript, 0.6, 1)
    , color = "black" 
    , nudgeErrorLine = 0
    , nudgeMeanLine = 0
) {
  geoms <- list(
    geom_meanbar(
      aes(
        x = {{xVar}}
        , y = y
        # , ymax = y
        # , ymin = NA
      )
      , data = lmmData
      , inherit.aes = FALSE
      , width = meanBarWidth
      # , size = barSize
      , linewidth = barSize
      , color = color
      , position = position_nudge(x = nudgeMeanLine)
    )
    , geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
      , color = color
      , position = position_nudge(x = nudgeErrorLine)
    )
  )
  return(geoms)
}

plotError_LMM_aes <- function(
    lmmData
    , xVar
    , meanBarWidth = 0.9
    , barSize = ifelse(isManuscript, 0.6, 1)
    , nudgeErrorLine = 0.1 # to offset from current error bar. Remove
    , nudgeMeanLine = 0
    , ...
) {
  geoms <- list(
    geom_meanbar(
      aes(
        x = {{xVar}}
        , y = y
        , ...
      )
      , data = lmmData
      , inherit.aes = FALSE
      , width = meanBarWidth
      # , size = barSize
      , linewidth = barSize
      , position = position_nudge(x = nudgeMeanLine)
    )
    , geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , ...
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
      , position = position_nudge(x = nudgeErrorLine)
    )
  )
  return(geoms)
}

plotError_LMM_aes_xAxis <- function(
    lmmData
    , yVal = 50
    , barSize = 1
    , dodgeWidth = 4
    , ...
) {
  geoms <- list(
    geom_point(
      aes(
        y = yVal
        , x = y
        , color = comboTrt
      )
      , size = 2
      , data = lmmData
      , inherit.aes = FALSE
      # can't position_dodge with the continuous y variable. Would have to program into lmmData
    ), 
    geom_linerange(
      aes(
        y = yVal
        , xmin = lower
        , xmax = upper
        , color = comboTrt
      )
      , data = lmmData
      , inherit.aes = FALSE
      , linewidth = barSize
    )
  )
  return(geoms)
}

plotError_LMM_meanLine <- function(
    lmmData
    , xVar
    , barSize = 1.2
    , nudgeErrorLine = 0 # to offset from current error bar. Remove
    , ... # into aes
) {
  geoms <- list(
    geom_line(
      aes(
        x = PND
        , y = y
        , ...
      )
      , data = lmmData
      # , size = barSize
      , linewidth = barSize
      , inherit.aes = FALSE # 2023-07-30
    ),
    geom_linerange(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , ... 
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      , linewidth = barSize
      , position = position_nudge(x = nudgeErrorLine)
      , show.legend = FALSE
    )
  )
  return(geoms)
}

plotError_LMM_meanLine_mass <- function(
    lmmData
    , xVar
    , barSize = 1.2
    , nudgeErrorLine = 0 # to offset from current error bar. Remove
    , ribbonAlpha = 0.2
    , ... # into aes
) {
  geoms <- list(
    geom_line(
      aes(
        x = {{ xVar }}
        , y = y
        , color = earlyLifeTrt
        # , ...
      )
      , data = lmmData
      # , size = barSize
      # , linewidth = barSize
      , linewidth = .2
      , inherit.aes = FALSE # 2023-07-30
    ),
    geom_ribbon(
      aes(
        x = {{xVar}}
        , ymin = lower
        , ymax = upper
        , ... 
      )
      , data = lmmData
      , inherit.aes = FALSE
      # , size = barSize
      # , linewidth = barSize
      , position = position_nudge(x = nudgeErrorLine)
      , show.legend = FALSE
      , alpha = ribbonAlpha
    )
  )
  return(geoms)
}
