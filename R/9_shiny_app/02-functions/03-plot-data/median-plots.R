# Note - doesn't plot by treatment
# use for baseline plot to show the median for each slice
# ROI medians, colored by slice
# slice medians, larger circle
# median of slice medians for the group +/- IQR
# mean of slice medians for group +/- SEM
mmm_plot <- function(
  df
  , res
  , y_title
  , groups
  , colorBy = "median" # or "random" or NULL
  , colorByGroup = FALSE
  , scaleYlog10 = TRUE # priority over sqrt true
  , scaleYsqrt = FALSE
  , sqrtBreaks =  c(0, 0.1, 0.25, 0.5)
) {
  desired_order <- groups
  slice_medians <- df %>%
    filter(
      !is.na( {{ res }} )
    ) %>%
    group_by(group_id, slice_id) %>%
    summarize(slice_median = median( {{ res }} , na.rm = TRUE ), .groups = 'drop')
  
  # get the stats for the plots to avoid calculating in ggplot
  group_stats <- slice_medians %>%
    group_by(group_id) %>%
    summarize(
      med_of_meds = median(slice_median, na.rm = TRUE),
      lower = quantile(slice_median, 0.25, na.rm = TRUE),
      upper = quantile(slice_median, 0.75, na.rm = TRUE),
      mean_of_meds = mean(slice_median, na.rm = TRUE),
      std_err = sd(slice_median, na.rm = TRUE)/sqrt(length(slice_median)),
      num_slices = n_distinct(slice_id, na.rm = TRUE)
    ) %>%
    mutate(
      group_label = paste0(group_id, " (n=", num_slices, ")"),
      std_err_up = mean_of_meds + std_err,
      std_err_dn = mean_of_meds - std_err
    )
  
  #Get labels in user-specified order:
  # Create the full label for each group in desired order
  desired_order_labels <- group_stats %>%
    filter(group_id %in% desired_order) %>%
    arrange(factor(group_id, levels = desired_order)) %>%
    pull(group_label)
  
  #Set factor levels for group_label:
  group_stats$group_label <- factor(group_stats$group_label, levels = desired_order_labels)
  slice_medians_labeled <- slice_medians %>%
    left_join(group_stats %>% select(group_id, group_label), by = "group_id") %>%
    mutate(group_label = factor(group_label, levels = desired_order_labels))
  
  df_labeled <- df %>%
    filter(
      !is.na( {{ res }} )
    ) %>%
    left_join(
      group_stats %>% 
        select(group_id, group_label)
      , by = "group_id"
    ) %>%
    mutate(
      group_label = factor(group_label, levels = desired_order_labels)
    )
  
  
  offset = 0.12
  
  # baseline plot function!
  p <- ggplot(df_labeled, aes(x = group_label, y = {{ res }}, color = slice_id)) +
    geom_jitter( # layer 1: ROI medians, by slice
      show.legend = FALSE,
      position = position_dodge2(width = 0.8),
      size=1,
      alpha = 0.6
    ) +
    geom_point( # layer 2: slice medians
      data = slice_medians_labeled,
      aes(x = group_label, y = slice_median, fill = slice_id ),
      color = "black",
      shape = 21,
      size = 3,
      stroke = 1,
      show.legend = FALSE,
      position = position_dodge2(width = 0.8)
    ) +
    geom_errorbar( # layer 3: IQR for group median
      data = group_stats,
      aes(x = group_label, ymin = lower, ymax = upper),
      color = "red",
      width = 0.25,
      linewidth = 2,
      inherit.aes = FALSE
      , position = position_nudge(-offset)
    ) +
    geom_errorbar( # layer 4: std_err for mean for group mean
      data = group_stats,
      aes(x = group_label, ymin = std_err_dn, ymax = std_err_up),
      color = "black",
      width = 0.25,
      linewidth = 1,
      inherit.aes = FALSE
      , position = position_nudge(offset)
    ) +
    geom_point( # layer 5: group median
      data = group_stats,
      aes(x = group_label, y = med_of_meds),
      color = "red", fill = "yellow", size = 5, shape = 23,
      inherit.aes = FALSE
      , position = position_nudge(-offset)
    ) +
    geom_point( # layer 6: group mean
      data = group_stats,
      aes(x = group_label, y = mean_of_meds),
      color = "black", fill = "grey", size = 5, shape = 23,
      inherit.aes = FALSE
      , position = position_nudge(offset)
    ) +
    theme_classic() +
    labs(
      #    title = "Group, Slice Medians & Group Median-of-Medians",
      x = "Group (number of slices)",
      y = y_title
    )
  
  if(!is.null(colorBy)){
    if(colorBy == "median"){
      if(colorByGroup) {
        colorMap <- df %>%
          addOrderedColors(
            orderVar = {{res}}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , byMax = FALSE
            , revOrder = TRUE
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- df %>%
          addOrderedColors(
            orderVar = {{ res }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    } else {
      if(colorByGroup){
        colorMap <- df %>%
          addRandomColors(
            colorVar = {{ res }}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- df %>%
          addRandomColors(
            colorVar = {{ res }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    }
    
    p <- p +
      scale_color_manual(values = colorMap) +
      scale_fill_manual(values = colorMap)
  }
  
  if(scaleYlog10){
    p <- p +
      scale_y_log10()
  } else {
    if(scaleYsqrt){
      p <- p +
        scale_y_continuous(
          trans = "sqrt"
          , breaks = sqrtBreaks
          , labels = scales::number_format(accuracy = 0.01)
        )
    }
  }
  return(p)
}


scatterPlot_baseline <- function(
    ROIdf
    , sliceDF
    , yVarROI
    , yVarSlice
    , yLab
    , addModelBars = FALSE
    , modelError = NULL
    , modelWidth = 0.9
    , ROI_dotSize = 1
    , ROI_fillAlpha = 0.2
    , ROI_jitterWidth = 0.4
    , slice_dotSize = 3
    , slice_fillAlpha = 1
    , slice_jitterWidth = 0.25
    , textSize = 12
    , zoom_x = FALSE # Zoom to part of x axis
    , xmin = NULL
    , xmax = NULL
    , zoom_y = FALSE # Zoom to part of y axis
    , ymin = NULL
    , ymax = NULL
    , title = NULL
    , hideXAxisLab = TRUE
    , colorBy = "median" # or "random" or NULL
    , colorByGroup = FALSE
    , scaleYlog10 = TRUE # priority over sqrt true
    , scaleYsqrt = FALSE
    , sqrtBreaks =  c(0, 0.1, 0.25, 0.5)
){
  viz <- ROIdf %>%
    filter(
      !is.na( {{yVarROI}} )
    ) %>%
    ggplot(
      aes(
        x = group_id
        , y = {{ yVarROI }}
        , fill = slice_id
        , color = slice_id
      )
    ) +
    jitterGeom(
      alpha = ROI_fillAlpha
      , width = ROI_jitterWidth
      , size = ROI_dotSize
    ) +
    geom_quasirandom(
      data = sliceDF %>%
        filter(
          !is.na( {{ yVarSlice }} )
        )
      , aes(
        x = group_id
        , y = {{ yVarSlice }}
        , fill = slice_id
      )
      , shape = 21
      , size = slice_dotSize
      , color = "black"
      , width = slice_jitterWidth
    ) +
    # expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme_pubr() +
    textTheme(size = textSize) +
    boxTheme() +
    theme(
      legend.position = "none"
    )
  
  if(!is.null(colorBy)){
    if(colorBy == "median"){
      if(colorByGroup) {
        colorMap <- ROIdf %>%
          addOrderedColors(
            orderVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , byMax = FALSE
            , revOrder = TRUE
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- ROIdf %>%
          addOrderedColors(
            orderVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    } else {
      if(colorByGroup){
        colorMap <- ROIdf %>%
          addRandomColors(
            colorVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- ROIdf %>%
          addRandomColors(
            colorVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    }
    
    viz <- viz +
      scale_color_manual(values = colorMap) +
      scale_fill_manual(values = colorMap)
  }
  
  if(scaleYlog10){
    viz <- viz +
      scale_y_log10()
  } else {
    if(scaleYsqrt){
      viz <- viz +
        scale_y_continuous(
          trans = "sqrt"
          , breaks = sqrtBreaks
          , labels = scales::number_format(accuracy = 0.01)
        )
    }
  }
  
  if(hideXAxisLab){
    viz <- viz + theme(
      axis.title.x = element_blank()
    )
  }
  
  if(!is.null(yLab)){
    viz <- viz + labs(y = yLab)
  }
  
  if(addModelBars & !is.null(modelError)){
    viz <- viz + 
      plotError_LMM(
        lmmData = modelError
        , xVar = group_id
        , meanBarWidth = modelWidth
      )
  }
  
  return(viz)
}

scatterPlot_treatment <- function(
    ROIdf
    , sliceDF
    , yVarROI
    , yVarSlice
    , yLab
    , addModelBars = FALSE
    , modelError = NULL
    , modelWidth = 0.9
    , ROI_dotSize = 1
    , ROI_fillAlpha = 0.2
    , ROI_jitterWidth = 0.4
    , slice_dotSize = 3
    , slice_fillAlpha = 1
    , slice_jitterWidth = 0.25
    , textSize = 12
    , zoom_x = FALSE # Zoom to part of x axis
    , xmin = NULL
    , xmax = NULL
    , zoom_y = FALSE # Zoom to part of y axis
    , ymin = NULL
    , ymax = NULL
    , title = NULL
    , hideXAxisLab = TRUE
    , colorBy = "median" # or "random" or NULL
    , colorByGroup = FALSE
    , scaleYlog10 = TRUE # priority over sqrt true
    , scaleYsqrt = FALSE
    , sqrtBreaks =  c(0, 0.1, 0.25, 0.5)
){
  viz <- ROIdf %>%
    filter(
      !is.na( {{yVarROI}} )
    ) %>%
    ggplot(
      aes(
        x = treatment
        , y = {{ yVarROI }}
        , fill = slice_id
        , color = slice_id
      )
    ) +
    jitterGeom(
      alpha = ROI_fillAlpha
      , width = ROI_jitterWidth
      , size = ROI_dotSize
    ) +
    geom_line(
      data = sliceDF %>%
        filter(
          !is.na( {{ yVarSlice }} )
        )
      , aes(
        x = treatment
        , y = {{ yVarSlice }}
        # , color = slice_id
        , group = slice_id
      )
      , color = "grey80"
      , position = position_dodge2(slice_jitterWidth)
    )+
    geom_jitter(
      data = sliceDF %>%
        filter(
          !is.na( {{ yVarSlice }} )
        )
      , aes(
        x = treatment
        , y = {{ yVarSlice }}
        , fill = slice_id
        , group = slice_id
      )
      , shape = 21
      , size = slice_dotSize
      , color = "black"
      , position = position_dodge2(slice_jitterWidth)
    ) +
    # expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme_pubr() +
    textTheme(size = textSize) +
    boxTheme() +
    facet_wrap(
      nrow = 1
      , "group_id"
      , strip.position = "bottom"
    ) +
    theme(
      legend.position = "none"
    ) 
  
  if(!is.null(colorBy)){
    if(colorBy == "median"){
      if(colorByGroup) {
        colorMap <- ROIdf %>%
          filter(
            treatment == "baseline"
          ) %>%
          addOrderedColors(
            orderVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , byMax = FALSE
            , revOrder = TRUE
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- ROIdf %>%
          filter(
            treatment == "baseline"
          ) %>%
          addOrderedColors(
            orderVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    } else {
      if(colorByGroup){
        colorMap <- ROIdf %>%
          filter(
            treatment == "baseline"
          ) %>%
          addRandomColors(
            colorVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = TRUE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
            , group_id
          )
      } else {
        colorMap <- ROIdf %>%
          filter(
            treatment == "baseline"
          ) %>%
          addRandomColors(
            colorVar = {{ yVarROI }}
            , subjectVar = slice_id
            , colorByGroups = FALSE
            , pkg = "rainbow"
            , seedVal = 123
            , dfOrMap = "map"
          )
      }
    }
    
    viz <- viz +
      scale_color_manual(values = colorMap) +
      scale_fill_manual(values = colorMap)
  }
  
  if(scaleYlog10){
    viz <- viz +
      scale_y_log10()
  } else {
    if(scaleYsqrt){
      viz <- viz +
        scale_y_continuous(
          trans = "sqrt"
          , breaks = sqrtBreaks
          , labels = scales::number_format(accuracy = 0.01)
        )
    }
  }
  
  if(hideXAxisLab){
    viz <- viz + theme(
      axis.title.x = element_blank()
    )
  }
  
  if(!is.null(yLab)){
    viz <- viz + labs(y = yLab)
  }
  
  if(addModelBars & !is.null(modelError)){
    viz <- viz + 
      plotError_LMM(
        lmmData = modelError
        , xVar = treatment
        , meanBarWidth = modelWidth
      )
  }
  
  return(viz)
}
