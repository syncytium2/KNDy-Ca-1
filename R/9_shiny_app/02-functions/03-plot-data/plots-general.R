scatterPlot_general <- function(
  df,
  xVar,
  xLab,
  yVar,
  yLab,
  fillVar = NULL,
  fillLimits = NULL,
  fillValues = NULL,
  lineColorVar = NULL,
  lineColorLimits = NULL,
  lineColorValues = NULL,
  textSize = 12,
  zoom_x = FALSE, # Zoom to part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, # Zoom to part of y axis
  ymin = NULL,
  ymax = NULL,
  dotSize = 1.5,
  fillAlpha = 1,
  jitterWidth = 0.42,
  jitterHeight = 0,
  jitterDodge = NULL,
  title = NULL,
  addMean = TRUE,
  addSE = TRUE,
  hideXAxisLab = TRUE
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = {{ fillVar }},
        color = {{ lineColorVar }}
      )
    ) +
    jitterGeom(
      size = dotSize,
      alpha = fillAlpha, # doesn't do anything. Provide the alpha when giving the fillValues
      width = jitterWidth,
      height = jitterHeight
      , dodgeWidth = jitterDodge
    ) +
    labs(title = title)+
    theme_pubr()+
    expand_limits(y=0)+
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)})+
    theme(
      legend.position = "none"
    )+
    textTheme(size = textSize)+
    boxTheme()

  if(addMean){
    if(is.null(jitterDodge)){
      viz <- viz + addMeanHorizontalBar()
    } else {
      viz <- viz + addMeanHorizontalBar(
        barPosition = position_dodge(width = jitterDodge)
      )
    }
  }
  if(addSE){
    if(is.null(jitterDodge)){
      viz <- viz + addMeanSE_vertBar()
    } else {
      viz <- viz + addMeanSE_vertBar(
        barPosition = position_dodge(width = jitterDodge)
      )
    }
  }

  if(!is.null(enquo(lineColorVar))){
    viz <- viz + scale_color_manual(limits = lineColorLimits, values = lineColorValues)
  }
  
  if(!is.null(enquo(fillVar))){
    viz <- viz + scale_fill_manual(limits = fillLimits, values = fillValues)
  }

  if(hideXAxisLab){
    viz <- viz + theme(
      axis.title.x = element_blank()
    )
  }
  
  if(!is.null(xLab)){
    viz <- viz + labs(x = xLab)
  }

  if(!is.null(yLab)){
    viz <- viz + labs(y = yLab)
  }
  
  return(viz)
}

getNiceName <- function(
  varName # as expression or as string, both work
  , labelDF = niceNames #data frame with labels
){
  label <- as.character(varName) # default to name of variable
  if(label %in% colnames(labelDF))
  {
    df <- labelDF %>%
      select(
        {{ varName }}
      )
    label <- df[[1]]
  }
  return(label)
}

addRandomColors <- function(df, colorVar, subjectVar, colorByGroups = FALSE, pkg = "rainbow", seedVal = 123, dfOrMap = "df", ...){
  if(colorByGroups){
    grouped_df <- df %>%
      group_by(
        {{ subjectVar }}
        , ...
      )
  } else{
    grouped_df <- df %>%
      group_by(
        {{ subjectVar }}
      )
  }
  
  grouped_df <- grouped_df %>%
    summarize(
      n = n()
      , .groups = "drop"
    )
  
  if(colorByGroups){
    grouped_df <- grouped_df %>%
      group_by(
        ...
      )
  }
  
  set.seed(seedVal)
  
  ranked_df <- grouped_df %>%
    mutate(
      randNum = runif(n())
      , rank = rank(randNum, ties.method = "first")
    ) %>%
    select(
      -c(n, randNum)
    )
  
  if(pkg == "rainbow"){
    ranked_df <- ranked_df %>%  
      mutate(
        color = rainbow(max(rank))[rank]
      ) %>%
      ungroup()
  }else{
    if(pkg == "viridis"){
      if(!require(viridis))install.packages('viridis')
      ranked_df <- ranked_df %>%  
        mutate(
          # these are different color options
          color = viridis::viridis(max(rank))[rank]
        ) %>%
        ungroup()
    } else { # default rainbow
      ranked_df <- ranked_df %>%  
        mutate(
          color = rainbow(max(rank))[rank]
        ) %>%
        ungroup()
    }
  }
  
  if(dfOrMap == "df"){
    subjectVarStr <- deparse(substitute(subjectVar))
    ellipsisVars <- substitute(alist(...))
    ellipsisStrs <- sapply(ellipsisVars[-1], as.character)
    # print(subjectVarStr)
    # print(ellipsisStrs)
    # add the calculated colors back to the original data
    if(length(ellipsisStrs)>0){
      df <- df %>%
        left_join(
          ranked_df
          , by = c( subjectVarStr, ellipsisStrs)
        )
    } else {
      df <- df %>%
        left_join(
          ranked_df
          , by = c(subjectVarStr)
        )
    }
    return(df)
  } else {
    color_map <- ranked_df %>%
      distinct(
        {{ subjectVar }}
        , color
      ) %>%
      deframe()
    
    return(color_map)
  }
  
  
}

addOrderedColors <- function(
    df
    , orderVar
    , subjectVar
    , colorByGroups = FALSE
    , pkg = "rainbow"
    , byMax = FALSE
    , revOrder = TRUE
    , dfOrMap = "df"
    , ...
) {
  # calculate the  median for each subject within each group
   median_df <- df %>%
    group_by(
      {{ subjectVar }}
      , ...
    ) 
  
  if(byMax){
     median_df <-  median_df %>%
      summarize(
         median_orderVar = max(
          {{ orderVar }}
          , na.rm = TRUE
        )
        , .groups = "drop"
      )
  } else{
     median_df <-  median_df %>%
      summarize( median_orderVar =  median(
        {{ orderVar }}
        , na.rm = TRUE
      )
      , .groups = "drop"
      )
      
  }
  
  if(colorByGroups){
     median_df <-  median_df %>%
      group_by(
        ...
      )
  }
  
  if(revOrder){
    ranked_df <-  median_df %>%
      mutate(
        rank = rank(
          - median_orderVar
          , ties.method = "first"
        )
      )
  } else {
    ranked_df <-  median_df %>%
      mutate(
        rank = rank(
           median_orderVar
          , ties.method = "first"
        )
      )
  }
  
  if(pkg == "rainbow"){
    ranked_df <- ranked_df %>%  
      mutate(
        color = rainbow(max(rank))[rank]
      ) %>%
      ungroup()
  }else{
    if(pkg == "viridis"){
      if(!require(viridis))install.packages('viridis')
      ranked_df <- ranked_df %>%  
        mutate(
          # these are different color options
          color = viridis::viridis(max(rank))[rank]
        ) %>%
        ungroup()
    } else { # default rainbow
      ranked_df <- ranked_df %>%  
        mutate(
          color = rainbow(max(rank))[rank]
        ) %>%
        ungroup()
    }
  }
   
  if(dfOrMap == "df"){
    subjectVarStr <- deparse(substitute(subjectVar))
    ellipsisVars <- substitute(alist(...))
    ellipsisStrs <- sapply(ellipsisVars[-1], as.character)
    # add the calculated colors back to the original data
    if(length(ellipsisStrs)>0){
      df <- df %>%
        left_join(
          ranked_df
          , by = c( subjectVarStr, ellipsisStrs)
        )
    } else {
      df <- df %>%
        left_join(
          ranked_df
          , by = c(subjectVarStr)
        )
    }
    
    return(df)
  } else {
    color_map <- ranked_df %>%
      distinct(
        {{ subjectVar }}
        , color
      ) %>%
      deframe()
    
    return(color_map)
  }
  
}

getRandomSubjects <- function(df, subjectVar, n, seed=123) {
  
  # Sets the seed for the random number generator
  set.seed(seed)
  
  # Step 1: Randomly pick n subjects
  df_subjects <- df %>%
    distinct({{ subjectVar }}) %>% 
    sample_n(n)
  
  IDs <- df_subjects %>%
    pull( {{ subjectVar }} )
  
  print(IDs)
  
  # Step 2: Filter the original dataframe 
  # to include only rows that match the selected subjects
  df_filtered <- df %>% 
    filter({{ subjectVar }} %in% IDs) %>%
    ungroup()
  
  return(df_filtered)
  
  # This is a more streamlined approach to consider:
  # slice_sample() which can handle groups that are smaller than n
}
