getAD_Pval <- function(result, version = 1, fromPaired = FALSE){
  if(fromPaired){
    result <- result$test_result
  }
  tbl <- result$ad
  tbl_tibble <- tbl %>% as.tibble()
  p_val <- tbl_tibble$` asympt. P-value`[[version]]
  return(p_val)
}

getAD_vals <- function(result, version = 1, fromPaired = FALSE){
  if(fromPaired){
    result <- result$test_result
  }
  tbl <- result$ad
  tbl_tibble <- tbl %>% as.tibble()
  p_val <- tbl_tibble$` asympt. P-value`[[version]]
  AD <- tbl_tibble$AD[[version]]
  T.AD <- tbl_tibble$T.AD[[version]]
  n1 <- result$ns[1]
  n2 <- result$ns[2]
  return(list(
    "AD" = AD
    , "T.AD" = T.AD
    , "p" = p_val
    , "n1" = n1
    , "n2" = n2
  ))
}

calcCumFreq <- function(
    df
    , varToSum
){
  df %>%
    arrange(
      {{ varToSum }}
    ) %>%
    mutate(
      cumFreq = ecdf({{ varToSum }})({{ varToSum }})
    )
}

plotCumulativeFreqDist <- function(
    df
    , xVar
    , xLab
    , yLab = "cumulative probability"
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 1
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , scaleLog10 = FALSE
    , lineSize = 0.3
) {
  plot <- df %>%
    combineStress() %>%
    group_by(earlyLifeTrt, adultTrt) %>%
    calcCumFreq({{ xVar }}) %>%
    ggplot(
      aes(
        x = {{ xVar }}
        , y = cumFreq
        , group = comboTrt
        , color = comboTrt
      )
    ) +
    geom_line(
      linewidth = lineSize
    ) +
    boxTheme() +
    textTheme(size = textSize)
  
  if(scaleLog10){
    plot <- plot + scale_x_log10()
  } else {
    plot <- plot + 
      expand_limits(x = 0) +
      coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  }
  
  plot <- plot +
    expand_limits(y = 0) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    xlab(xLab) +
    ylab(yLab) +
    comboTrtLineColor() +
    theme(
      legend.position = legendPosition 
      , legend.key = element_blank()
    )
  return(plot)
}

plotCumulativeFreqDist_defVar <- function(
    df
    , xVar
    , xLab
    , yLab = "cumulative probability"
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 1
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , scaleLog10 = FALSE
    , lineSize = 0.3
    , groupVar = adultTrt
) {
  plot <- df %>%
    group_by( {{ groupVar }} ) %>%
    calcCumFreq({{ xVar }}) %>%
    ggplot(
      aes(
        x = {{ xVar }}
        , y = cumFreq
        , group = {{ groupVar}}
        , color = {{ groupVar}}
      )
    ) +
    geom_line(
      linewidth = lineSize
    ) +
    boxTheme() +
    textTheme(size = textSize)
  
  if(scaleLog10){
    plot <- plot + scale_x_log10()
  } else {
    plot <- plot + 
      expand_limits(x = 0) +
      coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  }
  
  plot <- plot +
    expand_limits(y = 0) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    xlab(xLab) +
    ylab(yLab) +
    theme(
      legend.position = legendPosition 
      , legend.key = element_blank()
    )
  
  plot <- plot + labs(color = "treatment")
  
  return(plot)
}

plotDensityDistribution <- function(
    df
    , xVar
    , xLab
    , yLab = "density"
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = FALSE
    , ymin = NA
    , ymax = NA
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , STD_CON_fill = "lightgrey"
    , STD_CON_color = "grey30"
    , fillAlpha = 0.8
    , scaleLog10 = FALSE
) {
  plot <- df %>%
    combineStress() %>%
    ggplot(
      aes(
        x = {{ xVar }}
        , color = comboTrt
        # , fill = comboTrt
      )
    ) +
    geom_density(alpha = fillAlpha)
  
  if(scaleLog10){
    plot <- plot + scale_x_log10()
  } else {
    plot <- plot +
      expand_limits(x = 0) +
      coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})
  }
  plot <- plot +
    xlab(xLab) +
    ylab(yLab) +
    comboTrtFillShape(STD_CON_fill = STD_CON_fill, STD_CON_color = STD_CON_color) +
    theme_pubr() +
    boxTheme() +
    textTheme(textSize)
  
  return(plot)
}

plotPSCProp_log <- function(
    df
    , yVar
    , yLab
    , logBreaks = waiver()
    , logLabels = waiver()
    , byCell = TRUE
    , dotSize = 0.75
    , dotAlpha = 0.6
    , jitterWidth = 0.35
    , byQuartiles = TRUE
    , sortByQuartile = TRUE
    , textSize = textSize
){
  if(byCell){
    if(sortByQuartile){
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          quartile50 = quantile({{ yVar }}, probs = 0.50, na.rm = TRUE)
          , .groups = "drop"
        ) %>%
        arrange(
          desc(quartile50)
        )
      
    } else {
      df_counts <- df %>%
        group_by(cellID) %>%
        summarize(
          count = n()
          , .groups = "drop"
        ) %>%
        arrange(
          desc(count)
        )
      
    }
    
    df <- df %>%
      mutate(
        cellID = factor(
          cellID
          , levels = df_counts$cellID
        )
      )
  }
  
  df <- df %>%
    combineStress()
  
  if(byQuartiles){
    df <- df %>%
      add_quartile_colors(
        if(byCell){cellID} else {comboTrt}
        , value_col = {{ yVar }}
      )
  }
  
  viz <- df %>%
    ggplot(
      aes(
        x = if(byCell){cellID} else{comboTrt}
        , y = {{ yVar }}
        , color = if(byQuartiles){Color} else {log({{ yVar }})}
        , fill = if(byQuartiles){Color} else {log({{ yVar }})}
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = dotAlpha,
      width = jitterWidth,
      height = 0
    ) +
    theme_pubr()+
    theme(
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme() +
    theme(
      axis.text.x = element_blank()
    ) + 
    labs(y = yLab, x = ifelse(byCell, "cell", ""))
  
  if(byQuartiles){
    viz <- viz +
      scale_color_identity() +
      scale_fill_identity()
  } else {
    viz <- viz +
      scale_color_viridis() +
      scale_fill_viridis()
  }
  
  viz <- viz +
    facet_wrap(
      ~ comboTrt
      , nrow = 1
      , scales = "free_x"
      , strip.position = "bottom"
    ) +
    scale_y_log10(
      breaks = logBreaks
      , labels = logLabels
    )
  
  return(viz)
}

add_quartile_colors <- function(
    data
    , group_col
    , value_col
    , reverse_order = FALSE
) {
  data <- data %>%
    group_by({{ group_col }}) %>%
    mutate(
      QuartileID = cut({{ value_col }},
                       breaks = quantile({{ value_col }}, probs = 0:4/4, na.rm = TRUE),
                       include.lowest = TRUE,
                       labels = FALSE)
    ) %>% 
    ungroup()
  
  # quartile_colors <- viridis(4, option = "D")
  quartile_colors <- c("1" = "#2c7bb6", "2" = "#abd9e9", "3" = "#fdae61", "4" = "#d7191c")
  if (reverse_order) {
    color_assignment <- (4 - as.numeric(data$QuartileID)) +1
  } else {
    color_assignment <- as.numeric(data$QuartileID)
  }
  
  data$Color <- quartile_colors[color_assignment]
  
  data <- data %>%
    select(-QuartileID)
  
  # mutate(
  #   Color = viridis(4, option = "D")[as.numeric(QuartileID)]
  # ) %>%
  # select(-QuartileID)
  
  return(data)
}


# 4-way anderson darling --------------------------------------------------

fourWayAD <- function(
    df
    , feature
    , xLab
    , testVersion = 1
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 1
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , STD_CON_fill = "lightgrey" # for density plot
    , STD_CON_color = "grey30"
    , colorCellByQuartiles = TRUE
){
  SC <- df %>%
    filter(
      earlyLifeTrt == "STD"
      , adultTrt == "CON"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  SA <- df %>%
    filter(
      earlyLifeTrt == "STD"
      , adultTrt == "ALPS"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  LC <- df %>%
    filter(
      earlyLifeTrt == "LBN"
      , adultTrt == "CON"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  LA <- df %>%
    filter(
      earlyLifeTrt == "LBN"
      , adultTrt == "ALPS"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  adTestResult <- ad.test(
    SC
    , SA
    , LC
    , LA
  )
  
  pVal <- getAD_Pval(adTestResult, version = testVersion)
  
  cumFreqPlot <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , zoom_y = zoom_y
    , ymin = ymin
    , ymax = ymax
    , textSize = textSize
    , legendPosition = legendPosition
  )
  
  fullDistCumFreqPlot <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
  )
  
  cumFreqPlot_log <- plotCumulativeFreqDist(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = c(0.1, 0.6)
    , scaleLog10 = TRUE
  )
  
  densityPlot <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
  )
  
  fullDensityPlot <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
  )
  
  densityPlot_log <- plotDensityDistribution(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , legendPosition = legendPosition
    , STD_CON_fill = STD_CON_fill
    , STD_CON_color = STD_CON_color
    , scaleLog10 = TRUE
  )
  
  plotByCell_log <- plotPSCProp_log(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , byQuartiles = colorCellByQuartiles
    , sortByQuartile = colorCellByQuartiles
  )
  
  plotQuartiles_log <- plotPSCProp_log(
    df
    , {{ feature }}
    , xLab
    , textSize = textSize
    , byQuartiles = colorCellByQuartiles
    , sortByQuartile = colorCellByQuartiles
    , byCell = FALSE
  )
  
  return(
    list(
      "adTest" = adTestResult
      , "pVal" = pVal
      , "plots" = list(
        "cumulativeFreqPlot" = cumFreqPlot
        , "fullCumulativeFreqPlot" = fullDistCumFreqPlot
        , "cumulativeFreqPlot_log" = cumFreqPlot_log
        , "densityPlot" = densityPlot
        , "fullDensityPlot" = fullDensityPlot
        , "densityPlot_log" = densityPlot_log
        , "plotByCell_log" = plotByCell_log
        , "plotsQuartiles_log" = plotQuartiles_log
      )
      , "vectors" = list(
        "SC" = SC
        , "SA" = SA
        , "LC" = LC
        , "LA" = LA
      )
    )
  )
}

mainEffectsAD <- function(
    df
    , feature
    , xLab
    , testVersion = 1
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , zoom_y = TRUE
    , ymin = 0
    , ymax = 1
    , textSize = 11
    , legendPosition = c(0.6, 0.3)
    , STD_CON_fill = "lightgrey" # for density plot
    , STD_CON_color = "grey30"
    , colorCellByQuartiles = TRUE
){
  STD <- df %>%
    filter(
      earlyLifeTrt == "STD"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  LBN <- df %>%
    filter(
      earlyLifeTrt == "LBN"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  CON <- df %>%
    filter(
      adultTrt == "CON"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  ALPS <- df %>%
    filter(
      adultTrt == "ALPS"
    ) %>%
    select(
      {{ feature }}
    ) %>% 
    pull()
  
  earlyLifeAdTestResult <- ad.test(
    STD
    , LBN
  )
 
   adultAdTestResult <- ad.test(
    CON
    , ALPS
  )
  
  earlyLifePVal <- getAD_Pval(earlyLifeAdTestResult, version = testVersion)
  adultPVal <- getAD_Pval(adultAdTestResult, version = testVersion)
  
  earlyLifeCumFreqPlot <- plotCumulativeFreqDist_defVar(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , zoom_y = zoom_y
    , ymin = ymin
    , ymax = ymax
    , textSize = textSize
    , legendPosition = legendPosition
    , groupVar = earlyLifeTrt
  ) +
    earlyLifeColor(STDColor = "#CCCCCC")
  
  adultCumFreqPlot <- plotCumulativeFreqDist_defVar(
    df
    , {{ feature }}
    , xLab
    , zoom_x = zoom_x
    , xmin = xmin
    , xmax = xmax
    , zoom_y = zoom_y
    , ymin = ymin
    , ymax = ymax
    , textSize = textSize
    , legendPosition = legendPosition
    , groupVar = adultTrt
  ) +
    adultTrtColor(CONColor = "#CCCCCC")
  
  # fullDistCumFreqPlot <- plotCumulativeFreqDist(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , legendPosition = legendPosition
  # )
  # 
  # cumFreqPlot_log <- plotCumulativeFreqDist(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , legendPosition = c(0.1, 0.6)
  #   , scaleLog10 = TRUE
  # )
  # 
  # densityPlot <- plotDensityDistribution(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , zoom_x = zoom_x
  #   , xmin = xmin
  #   , xmax = xmax
  #   , textSize = textSize
  #   , legendPosition = legendPosition
  #   , STD_CON_fill = STD_CON_fill
  #   , STD_CON_color = STD_CON_color
  # )
  # 
  # fullDensityPlot <- plotDensityDistribution(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , legendPosition = legendPosition
  #   , STD_CON_fill = STD_CON_fill
  #   , STD_CON_color = STD_CON_color
  # )
  # 
  # densityPlot_log <- plotDensityDistribution(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , legendPosition = legendPosition
  #   , STD_CON_fill = STD_CON_fill
  #   , STD_CON_color = STD_CON_color
  #   , scaleLog10 = TRUE
  # )
  # 
  # plotByCell_log <- plotPSCProp_log(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , byQuartiles = colorCellByQuartiles
  #   , sortByQuartile = colorCellByQuartiles
  # )
  # 
  # plotQuartiles_log <- plotPSCProp_log(
  #   df
  #   , {{ feature }}
  #   , xLab
  #   , textSize = textSize
  #   , byQuartiles = colorCellByQuartiles
  #   , sortByQuartile = colorCellByQuartiles
  #   , byCell = FALSE
  # )
  
  return(
    list(
      "earlyLifeAdTest" = earlyLifeAdTestResult
      , "adultAdTest" = adultAdTestResult
      , "earlyLifePVal" = earlyLifePVal
      , "adultPVal" = adultPVal
      , "plots" = list(
        "earlyLifeCumulativeFreqPlot" = earlyLifeCumFreqPlot
        , "adultCumulativeFreqPlot" = adultCumFreqPlot
        # , "fullCumulativeFreqPlot" = fullDistCumFreqPlot
        # , "cumulativeFreqPlot_log" = cumFreqPlot_log
        # , "densityPlot" = densityPlot
        # , "fullDensityPlot" = fullDensityPlot
        # , "densityPlot_log" = densityPlot_log
        # , "plotByCell_log" = plotByCell_log
        # , "plotsQuartiles_log" = plotQuartiles_log
      )
      , "vectors" = list(
        "STD" = STD
        , "LBN" = LBN
        , "CON" = CON
        , "ALPS" = ALPS
      )
    )
  )
}

runFourWayAD_allProps <- function(df, colorCellByQuartiles = TRUE, appendToPCol = NULL){
  amplitudeRes <- fourWayAD(df, amplitude, "amplitude (pA)", zoom_x = TRUE, xmin = 0, xmax = 125, colorCellByQuartiles = colorCellByQuartiles)
  riseTimeRes <- fourWayAD(df, riseTime, "rise time (ms)", zoom_x = TRUE, xmin = 0, xmax = 2.2, colorCellByQuartiles = colorCellByQuartiles)
  decayTimeRes <- fourWayAD(df, decay9010, "decay time (ms)", zoom_x = TRUE, xmin = 0, xmax = 80, colorCellByQuartiles = colorCellByQuartiles)
  fwhmRes <- fourWayAD(df, fwhm, "fwhm (ms)", zoom_x = TRUE, xmin = 0, xmax = 20, colorCellByQuartiles = colorCellByQuartiles)
  
  pValDF <- data.frame(
    "feature" = c("amplitude", "rise time", "decay time", "fwhm")
    , "p.value" = c(amplitudeRes$pVal, riseTimeRes$pVal, decayTimeRes$pVal, fwhmRes$pVal)
  )
  
  if(!is.null(appendToPCol)){
    newColName = paste0("p.value", appendToPCol)
    pValDF <- pValDF %>%
      rename(
        !!newColName := p.value
      )
  }
  return(
    list(
      "amplitude" = amplitudeRes
      , "riseTime" = riseTimeRes
      , "decayTime" = decayTimeRes
      , "fwhm" = fwhmRes
      , "pVals" = pValDF
    )
  )
}

## Pairwise KS/AD -----------------

dist_pairwise <- function(adRes, variable, singleRes = TRUE){
  if(!singleRes){
    vectorList <- adRes[[variable]]$vectors
  } else{
    vectorList <- adRes$vectors
  }
  ks_SCvSA <- ks.test(vectorList$SC, vectorList$SA)
  ks_SCvLC <- ks.test(vectorList$SC, vectorList$LC)
  # ks_SCvLA <- ks.test(vectorList$SC, vectorList$LA)
  # ks_LCvSA <- ks.test(vectorList$LC, vectorList$SA)
  ks_LCvLA <- ks.test(vectorList$LC, vectorList$LA)
  ks_SAvLA <- ks.test(vectorList$SA, vectorList$LA)
  
  KS_res <- adjust_pvalue(
    data.frame(
      "comp" = c(
        "STD-CON vs STD-ALPS"
        , "STD-CON vs LBN-CON"
        # , "STD-CON vs LBN-ALPS"
        # , "STD-ALPS vs LBN-CON"
        , "LBN-CON vs LBN-ALPS"
        , "STD-ALPS vs LBN-ALPS"
      )
      , "D" = c(
        ks_SCvSA$statistic
        , ks_SCvLC$statistic
        # , ks_SCvLA$statistic
        # , ks_LCvSA$statistic
        , ks_LCvLA$statistic
        , ks_SAvLA$statistic
      )
      , "p" = c(
        ks_SCvSA$p.value
        , ks_SCvLC$p.value
        # , ks_SCvLA$p.value
        # , ks_LCvSA$p.value
        , ks_LCvLA$p.value
        , ks_SAvLA$p.value
      )
    )
    , p.col = "p"
    , method = "holm"
  ) %>%
    formatPCol() %>%
    rename(
      orgP = p
      , p = p.adj
    ) %>%
    formatPCol() %>%
    rename(
      p = orgP
      , `holm adj p` = p
    )
  
  ad_SCvSA <- ad.test(vectorList$SC, vectorList$SA)
  ad_SCvLC <- ad.test(vectorList$SC, vectorList$LC)
  # ad_SCvLA <- ad.test(vectorList$SC, vectorList$LA)
  # ad_LCvSA <- ad.test(vectorList$LC, vectorList$SA)
  ad_LCvLA <- ad.test(vectorList$LC, vectorList$LA)
  ad_SAvLA <- ad.test(vectorList$SA, vectorList$LA)
  
  ad_SCvSA_res <- getAD_vals(ad_SCvSA)
  ad_SCvLC_res <- getAD_vals(ad_SCvLC)
  ad_LCvLA_res <- getAD_vals(ad_LCvLA)
  ad_SAvLA_res <- getAD_vals(ad_SAvLA)
  
  AD_res <- adjust_pvalue(
    data.frame(
      "comp" = c(
        "STD-CON vs STD-ALPS"
        , "STD-CON vs LBN-CON"
        , "LBN-CON vs LBN-ALPS"
        , "STD-ALPS vs LBN-ALPS"
      )
      , "n group 1" = c(
        ad_SCvSA_res$n1
        , ad_SCvLC_res$n1
        , ad_LCvLA_res$n1
        , ad_SAvLA_res$n1
      )
      , "n group 2" = c(
        ad_SCvSA_res$n2
        , ad_SCvLC_res$n2
        , ad_LCvLA_res$n2
        , ad_SAvLA_res$n2
      )
      , "AD" = c(
        ad_SCvSA_res$AD
        , ad_SCvLC_res$AD
        , ad_LCvLA_res$AD
        , ad_SAvLA_res$AD
      )
      , "T.AD" = c(
        ad_SCvSA_res$T.AD
        , ad_SCvLC_res$T.AD
        , ad_LCvLA_res$T.AD
        , ad_SAvLA_res$T.AD
      )
      , "p" = c(
        ad_SCvSA_res$p
        , ad_SCvLC_res$p
        , ad_LCvLA_res$p
        , ad_SAvLA_res$p
      )
    )
    , p.col = "p"
    , method = "holm"
  ) %>%
    formatPCol() %>%
    rename(
      orgP = p
      , p = p.adj
    ) %>%
    formatPCol() %>%
    rename(
      p = orgP
      , `holm adj p` = p
    )
  
  joinedRes <- KS_res %>%
    left_join(
      AD_res
      , by = "comp"
      , suffix = c(".KS", ".AD")
    )
  
  return(list(
    "KS" = KS_res
    , "AD" = AD_res
    , "joinedRes" = joinedRes
    , "AD_out" = list(
      "ad_SCvSA" = ad_SCvSA
      , "ad_SCvLC" = ad_SCvLC
      , "ad_LCvLA" = ad_LCvLA
      , "ad_SAvLA" = ad_SAvLA
    )
  ))
}

# Bootstrap -----------------------

## summarize column means from vector of cols ------------

summarize_means <- function(df, colsAsText) {
  # Create a named list of expressions to be evaluated by summarize
  summary_expressions <- sapply(colsAsText, function(col) {
    expr(mean(!!sym(col), na.rm = TRUE))
  }, simplify = FALSE)
  names(summary_expressions) <- colsAsText
  
  # Use !!! to splice the list of expressions into summarize
  df %>% summarize(!!!summary_expressions, .groups = "drop")
}


## main bootstrapping functions --------------------

getOneBootstrapMeans <- function(
    df
    , maxPerCell = NULL
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , propColsAsText = c("amplitude", "interval")
    , replace = TRUE
    , bootstrapIt = NA
){
  if(!is.null(maxPerCell)){
    df <- df %>%
      group_by(cellID) %>%
      slice_sample(n = maxPerCell) %>%
      ungroup()
  }
  
  df <- slice_sample(
    df %>%
      group_by(!!! groupingVars)
    , prop = 1
    , replace = replace
  )
  
  if(!is.na(bootstrapIt)){
    df <- df %>%
      mutate(
        bootIt = bootstrapIt
      )
    
    propColsAsText <- c(propColsAsText, "bootIt")
  }
  
  sumDF <- df %>%
    summarize_means(
      propColsAsText
    )
  
  return(list(
    "mean" = sumDF
    , "all" = df
    )
  )
}

bootstrapGroup <- function(df
                           , nBootstrap = 2000
                           , maxPerCell = 100 # set to NULL if don't want to limit per cell
                           , groupingVars = exprs(earlyLifeTrt, adultTrt)
                           , replace = TRUE # only for sampling after picking events from each cell
                           , setSeed = NULL
                           , propColsAsText = c("amplitude", "interval")
) {
  if (!is.null(setSeed)) {
    set.seed(setSeed)
  }
  
  bootstrapRes <- bind_rows(
    map(
      1:nBootstrap
      , ~ getOneBootstrapMeans(df, replace = replace, bootstrapIt = .x, maxPerCell = maxPerCell, groupingVars = groupingVars, propColsAsText = propColsAsText)$all
      , .progress = "bootstrap mean prog"
    )
  )
  
  return(bootstrapRes)
}

## Plots errors on x axis ---------------

plotBootstrapErrors_xAxis <- function(
    df
    , xLab
    , zoom_x = FALSE
    , xmin = NA
    , xmax = NA
    , textSize = 11
    , dotSize = 0.5
    , meanBarWidth = 0.75
    , barSize = 0.5
) {
  df %>%
    ggplot(
      aes(
        y = comboTrt
        , x = y
        , color = comboTrt
      )
    ) +
    # geom_point(size = dotSize) +
    geom_errorbar( # not sure that meanbar is working with the flippped aes. Just delete the extra
      aes(
        y = comboTrt
        , xmin = y
        , xmax = y
      )
      , width = meanBarWidth
      , linewidth = barSize
    ) +
    geom_linerange(
      aes(
        y = comboTrt
        , xmin = lower
        , xmax = upper
      )
    ) +
    comboTrtLineColor() +
    expand_limits(x = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}) +
    labs(x = xLab) +
    scale_y_discrete(
      limits = rev
      , labels = c(
        "STD-CON" = "SC"
        , "LBN-CON" = "LC"
        , "STD-ALPS" = "SA"
        , "LBN-ALPS" = "LA"
        # "STD-CON" = "STD\nCON"
        # , "LBN-CON" = "LBN\nCON"
        # , "STD-ALPS" = "STD\nALPS"
        # , "LBN-ALPS" = "LBN\nALPS"
      )
      # , position = "right"
    )+
    theme_pubr()+
    boxTheme() +
    textTheme(size = textSize) +
    theme(
      axis.title.y = element_blank()
      , axis.text.y = element_text(face = "bold")
      , legend.position = "none"
    )
}

## format results ----------

formatBootstrapPvaluesCI <- function(bootDiffCI){
  bootDiffCI %>%
    group_by(
      variable
    ) %>%
    mutate(
      mult2.p.value = p.value * 2
    ) %>%
    adjust_pvalue(p.col = "mult2.p.value", method = "holm") %>%
    formatPCol(p.value) %>%
    formatPCol(mult2.p.value) %>%
    formatPCol(mult2.p.value.adj) %>%
    rename(
      p = mult2.p.value
      , p.adj = mult2.p.value.adj
    ) %>%
    mutate(
      CI = paste0("[", roundDecimals(lowerCIDiff, 2), ",", roundDecimals(upperCIDiff, 2), "]")
      , .after = meanDiff
    ) %>%
    select(
      - c(bothSameDir, p.value, lowerCIDiff, upperCIDiff, minDiff, maxDiff)
    )
}

renameBootstrapComparisonGroups <- function(df, removeComparison = TRUE, twoLineLabels = FALSE){
  df <- df %>%
    mutate(
      comp = case_when(
        comparison == "STD_ALPSvLBN_CON" ~ "STD-ALPS vs LBN-CON"
        , comparison == "STD_CONvLBN_ALPS" ~ "STD-CON vs LBN-ALPS"
        , comparison == "alpsSTDvLBN" ~ "STD-ALPS vs LBN-ALPS"
        , comparison == "conSTDvLBN" ~ "STD-CON vs LBN-CON"
        , comparison == "lbnCONvALPS" ~ "LBN-CON vs LBN-ALPS"
        , comparison == "stdCONvALPS" ~ "STD-CON vs STD-ALPS"
      )
      , comp = factor(
        comp
        , levels = c(
          "STD-CON vs STD-ALPS"
          , "LBN-CON vs LBN-ALPS"
          , "STD-CON vs LBN-CON"
          , "STD-ALPS vs LBN-ALPS"
          , "STD-CON vs LBN-ALPS"
          , "STD-ALPS vs LBN-CON"
        )
      )
      , .after = comparison
    ) %>%
    arrange(
      variable, comp
    )
  
  if(twoLineLabels){
    df <- df %>%
      makeCompTwoLines()
  }
  
  if(removeComparison) {
    df <- df %>%
      select(
        -comparison
      )
  }
  return(df)
}

makeCompTwoLines <- function(df){
  df %>%
    mutate(
      comp = gsub(" vs ", "\n", comp, fixed = TRUE)
      , comp = factor(
        comp
        , levels = c(
          "STD-CON\nSTD-ALPS"
          , "LBN-CON\nLBN-ALPS"
          , "STD-CON\nLBN-CON"
          , "STD-ALPS\nLBN-ALPS"
          , "STD-CON\nLBN-ALPS"
          , "STD-ALPS\nLBN-CON"
        )
      )
    )
}

## Plot pairwise -------------

plotBootstrapPairwiseDiffs <- function(bootRes, variableAsString, yLab, compProp = 0.15, specMeanDiffsDF = NULL){
  diffCItoPlot <- bootRes$diffsCI %>%
    makeCompTwoLines() %>%
    rename(
      y = meanDiff
      , lower = lowerCIDiff
      , upper = upperCIDiff
    )
  
  percOfSTDCON = bootRes$meansCI %>% 
    filter(variable == variableAsString
           , earlyLifeTrt == "STD"
           , adultTrt == "CON") %>%
    pull(mean) * compProp
  
  if(is.null(specMeanDiffsDF)){
    meanDiffs <- bootRes$extra$meanDiffs
  } else {
    meanDiffs <- specMeanDiffsDF
  }
  
  plot <- meanDiffs %>%
    renameBootstrapComparisonGroups(twoLineLabels = TRUE) %>%
    filter(
      variable == variableAsString
    ) %>%
    ggplot(
      aes(
        x = comp
        , y = difference
      )
    ) +
    jitterGeom() +
    theme_pubr()+
    boxTheme() +
    textTheme() +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = percOfSTDCON, color = "blue") +
    geom_hline(yintercept = - percOfSTDCON, color = "blue") +
    labs(y = yLab, x = "pairwise comparisons between groups") +
    theme(
      axis.title.x = element_text(margin = margin(t = 0))
    )
  
  if(is.null(specMeanDiffsDF)){
    plot <- plot +
      plotError_LMM(
        diffCItoPlot %>%
          filter(
            variable == variableAsString
          )
        , xVar = comp
      )
  }
  return(plot)
}

## Run bootstrap -------------
analyzeBootstrapResults <- function(
    df
    , nBootstrap = 2000
    , maxPerCell = 100
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , replace = TRUE
    , setSeed = NULL
    , dotSize = 1
    , textSize = 11
    , propDifferent = 0.15
    , CIprop = 0.05
    , legendPosition = c(0.7, 0.3)
    , propColsAsText = c("amplitude", "interval") # also used to factor in order
    , makeBootPlots = FALSE
    , plotEachIterMean = TRUE
){
  bootstrapRes_all <- df %>% 
    bootstrapGroup(nBootstrap = nBootstrap, maxPerCell = maxPerCell, groupingVars = groupingVars, replace = replace, setSeed = setSeed, propColsAsText = propColsAsText)
  
  bootOutput <- analyzeExistingBootstrapRes(
    df
    , bootstrapRes_all
    , nBootstrap = nBootstrap
    , maxPerCell = maxPerCell
    , groupingVars = groupingVars
    , replace = replace
    , setSeed = setSeed
    , dotSize = dotSize
    , textSize = textSize
    , propDifferent = propDifferent
    , CIprop = CIprop
    , legendPosition = legendPosition
    , propColsAsText = propColsAsText
    , makeBootPlots = makeBootPlots
    , plotEachIterMean = plotEachIterMean
  )
  
  return(bootOutput)
}

analyzeExistingBootstrapRes <- function(
    df
    , bootstrapRes_all
    , nBootstrap = 2000
    , maxPerCell = 100
    , groupingVars = exprs(earlyLifeTrt, adultTrt)
    , replace = TRUE
    , setSeed = NULL
    , dotSize = 1
    , textSize = 11
    , propDifferent = 0.15
    , CIprop = 0.05
    , legendPosition = c(0.7, 0.3)
    , propColsAsText = c("amplitude", "interval") # also used to factor in order
    , makeBootPlots = FALSE
    , plotEachIterMean = TRUE
){
  origMeans <- df %>%
    group_by(
      earlyLifeTrt
      , adultTrt
    ) %>%
    summarize_means(propColsAsText)
  
  bootstrapRes <- bootstrapRes_all %>%
    group_by(
      !!! groupingVars, bootIt
    ) %>%
    summarize_means(
      propColsAsText
    )
  
  bootstrapMeans <- bootstrapRes %>%
    pivot_longer(
      cols = propColsAsText
      , names_to = "variable"
      , values_to = "mean"
    ) %>%
    mutate(
      variable = factor(variable, levels = propColsAsText)
    )
  
  
  bootstrapMeanSum <- bootstrapMeans %>%
    group_by(variable, earlyLifeTrt, adultTrt) %>%
    summarize(
      lowerCI = quantile(mean, probs = CIprop / 2, na.rm = TRUE)
      , upperCI = quantile(mean, probs = 1-(CIprop /2), na.rm = TRUE)
      , min = min(mean, na.rm = TRUE)
      , max = max(mean, na.rm = TRUE)
      , mean = mean(mean, na.rm = TRUE)
      , .groups = "drop"
    ) %>%
    relocate(
      mean, .before = lowerCI
    )
  
  
  bootstrapMeanDiffs <- bootstrapMeans %>%
    pivot_wider(
      id_cols = c(bootIt, variable)
      , names_from = c(earlyLifeTrt, adultTrt)
      , names_sep = "_"
      , values_from = mean
    ) %>%
    mutate(
      stdCONvALPS = STD_ALPS - STD_CON
      , lbnCONvALPS = LBN_ALPS - LBN_CON
      , conSTDvLBN = LBN_CON - STD_CON
      , alpsSTDvLBN = LBN_ALPS - STD_ALPS
      # , STD_CONvLBN_ALPS = LBN_ALPS - STD_CON
      # , STD_ALPSvLBN_CON = LBN_CON - STD_ALPS
    )
  
  bootstrapMeanDiffs_long <- bootstrapMeanDiffs %>%
    pivot_longer(
      cols = stdCONvALPS:alpsSTDvLBN
      # cols = stdCONvALPS:STD_ALPSvLBN_CON
      , names_to = "comparison"
      , values_to = "difference"
    )
  
  bootstrapMeanDiffsSum <- bootstrapMeanDiffs_long %>%
    group_by(
      variable
      , comparison
    ) %>%
    summarize(
      meanDiff = mean(difference, na.rm = TRUE)
      , lowerCIDiff = quantile(difference, probs = CIprop / 2, na.rm = TRUE)
      , upperCIDiff = quantile(difference, probs = 1-(CIprop / 2), na.rm = TRUE)
      , minDiff = min(difference, na.rm = TRUE)
      , maxDiff = max(difference, na.rm = TRUE)
      , bothSameDir = ifelse(
        (lowerCIDiff < 0 & upperCIDiff < 0) | (lowerCIDiff > 0 & upperCIDiff > 0)
        , TRUE
        , FALSE
      )
      , p.value = ifelse(
        meanDiff > 0
        , 1 - mean(difference > 0, na.rm = TRUE)
        , 1 - mean(difference < 0, na.rm = TRUE)
      )
      , .groups = "drop"
    ) %>%
    renameBootstrapComparisonGroups()
  
  bootstrapDiffPs <- bootstrapMeanDiffsSum %>%
    formatBootstrapPvaluesCI()
  
  bootOutput <- list(
    "results" = bootstrapRes
    , "originalMeans" = origMeans
    , "meansCI" = bootstrapMeanSum
    , "diffsCI" = bootstrapMeanDiffsSum
    , "P_formatted" = bootstrapDiffPs
    , "distPlots" = list()
    , "plots" = list()
    , "cumulativeFreqPlots" = list()
    , "cumulativeFreqPlots_full" = list()
    , "cumulativeFreqPlots_insets" = list()
    , "boot_ecdf_plots" = list(
      "distPlots" = list()
      , "cumulativeFreqPlots" = list()
      , "cumulativeFreqPlots_full" = list()
      , "cumulativeFreqPlots_insets" = list()
    )
    , "errorPlots" = list()
    , "errors" = list()
    , "extra" = list(
      "longMeans" = bootstrapMeans
      , "meanDiffs" = bootstrapMeanDiffs_long
      , "allBootRes" = bootstrapRes_all
    )
  )
  
  for (propVar in propColsAsText) {
    # errors -----
    errors <- bootstrapMeanSum %>%
      filter(
        variable == propVar
      ) %>%
      combineStress() %>%
      rename(
        y = mean
        , lower = lowerCI
        , upper = upperCI
      )
    
    bootOutput$errors[[propVar]] = errors
    
    # bootstrap plot ----
    thisPropLab <- case_when(
      propVar == "amplitude" ~ "amplitude (pA)"
      , propVar == "interval" ~ "interevent interval (s)"
      , propVar == "riseTime" ~ "rise time (ms)"
      , propVar == "decay9010" ~ "decay time (ms)"
      , propVar == "fwhm" ~ "fwhm (ms)"
      , .default = propVar
    )
    
    bootPlot <- bootstrapRes %>%
      combineStress() %>%
      scatterPlotComboTrt(
        yVar = !! sym(propVar)
        , yLab = thisPropLab
        , dotSize = dotSize
        , fontSize = textSize
        , addMeanSE = FALSE
        , plotJitter = plotEachIterMean
      ) +
      plotError_LMM_comboTrt(
        errors
      )
    
    bootOutput$plots[[propVar]] = bootPlot
    
    # cumulative probability plot ----
    
    thisXMax <- case_when(
      propVar == "amplitude" ~ 125
      , propVar == "interval" ~ 10
      , propVar == "riseTime" ~ 2.2
      , propVar == "decay9010" ~ 80
      , propVar == "fwhm" ~ 20
      , .default = NA
    )
    
    ecdfPlot <- plotCumulativeFreqDist(
      df
      , !! sym(propVar)
      , thisPropLab
      , zoom_x = TRUE
      , xmin = 0
      , xmax = thisXMax
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    bootOutput$cumulativeFreqPlots[[propVar]] = ecdfPlot
    
    ecdfPlot_full <- plotCumulativeFreqDist(
      df
      , !! sym(propVar)
      , thisPropLab
      , textSize = textSize
      , legendPosition = legendPosition
    )
    
    bootOutput$cumulativeFreqPlots_full[[propVar]] = ecdfPlot_full
    
    # Errors on x axis plot --------
    
    errors_x_plot <- errors %>%
      plotBootstrapErrors_xAxis(
        xLab = thisPropLab
        , zoom_x = TRUE
        , xmin = 0
        , xmax = thisXMax
      )
    
    bootOutput$errorPlots[[propVar]] = errors_x_plot
    
    # combined distribution ----------
    ecdf_inset <- ggdraw() +
      draw_plot(ecdfPlot +theme(legend.position = "none")) +
      draw_plot(
        ecdfPlot_full+
          theme(
            legend.position = "none"
            , axis.title.x = element_blank()
            , axis.title.y = element_blank() )
        , x = 0.5
        , y = 0.1
        , width = 0.5
        , height = 0.6
      )
    
    bootOutput$cumulativeFreqPlots_inset[[propVar]] = ecdf_inset
    
    combinedDistPlot <- plot_grid(
      errors_x_plot
      , ecdfPlot + theme(legend.position = "none")
      , nrow = 2
      , rel_heights = c(1, 3)
      , align = "hv"
      , axis = "tlbr"
    )
    
    # can't align axes after making the inset
    # combinedDistPlot <- ggdraw() +
    #   draw_plot(combinedDistPlot) +
    #   draw_plot(
    #     ecdfPlot_full+
    #       theme(
    #         legend.position = "none"
    #         , axis.title.x = element_blank()
    #         , axis.title.y = element_blank() )
    #     , x = 0.5
    #     , y = 0.1
    #     , width = 0.5
    #     , height = 0.4
    #   )
    
    bootOutput$distPlots[[propVar]] = combinedDistPlot
    
    if(makeBootPlots){
      smallerBootstrapRes_all <- bootstrapRes_all %>%
        arrange(
          propVar
        ) %>%
        filter(
          row_number() %% nBootstrap == 0
        )
      # Boot plots -----
      
      boot_ecdfPlot <- plotCumulativeFreqDist(
        smallerBootstrapRes_all
        , !! sym(propVar)
        , thisPropLab
        , zoom_x = TRUE
        , xmin = 0
        , xmax = thisXMax
        , textSize = textSize
        , legendPosition = legendPosition
      )
      
      boot_ecdfPlot_full <- plotCumulativeFreqDist(
        smallerBootstrapRes_all
        , !! sym(propVar)
        , thisPropLab
        , textSize = textSize
        , legendPosition = legendPosition
      )
      
      boot_ecdf_inset <- ggdraw() +
        draw_plot(boot_ecdfPlot +theme(legend.position = "none")) +
        draw_plot(
          boot_ecdfPlot_full+
            theme(
              legend.position = "none"
              , axis.title.x = element_blank()
              , axis.title.y = element_blank() )
          , x = 0.5
          , y = 0.1
          , width = 0.5
          , height = 0.6
        )
      
      boot_combinedDistPlot <- plot_grid(
        errors_x_plot
        , boot_ecdfPlot + theme(legend.position = "none")
        , nrow = 2
        , rel_heights = c(1, 3)
        , align = "hv"
        , axis = "tlbr"
      )
      
      boot_combinedDistPlot <- ggdraw() +
        draw_plot(boot_combinedDistPlot) +
        draw_plot(
          boot_ecdfPlot_full+
            theme(
              legend.position = "none"
              , axis.title.x = element_blank()
              , axis.title.y = element_blank() )
          , x = 0.5
          , y = 0.1
          , width = 0.5
          , height = 0.4
        )
      
      bootOutput$boot_ecdf_plots$cumulativeFreqPlots[[propVar]] = boot_ecdfPlot
      bootOutput$boot_ecdf_plots$cumulativeFreqPlots_full[[propVar]] = boot_ecdfPlot_full
      bootOutput$boot_ecdf_plots$cumulativeFreqPlots_inset[[propVar]] = boot_ecdf_inset
      bootOutput$boot_ecdf_plots$distPlots[[propVar]] = boot_combinedDistPlot
      
    }
  }
  
  return(bootOutput)
}
