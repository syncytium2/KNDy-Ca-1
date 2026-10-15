addSliceIndex <- function(
  df
) {
  df %>%
    # give each slice a unique index
    group_by(
      group_id
    ) %>%
    mutate(
      slice_index = match(slice_id, unique(slice_id))
    ) %>%
    ungroup()
}

groupByROI <- function(
  df
) {
  df %>%
    group_by(
      group_idx
      , treatment
      , slice_id
      , ROI
    )
}

getMediansForROIs <- function(
  df
  , addFreq = TRUE
  , factorGroups = TRUE
  , groupLevels = groups
  , makeComplete = TRUE
) {
  summaryDF <- df %>%
    summarize(
      group_id = unique(group_id)
      , sex = unique(sex)
      , gonads = unique(gonads)
      , treatment_group = unique(treatment_group)
      , slice_id = unique(slice_id)
      , slice_index = unique(slice_index)
      , mouse_id = unique(mouse_id)
      , ROI = unique(ROI)
      , amp_median = median(amplitude)
      , width_median = median(width)
      , interval_median = median(interval)
      , n_events = n()
      , duration = unique(duration)
    )
  if(addFreq){
    summaryDF <- summaryDF %>%
      mutate(
        frequency = n_events / duration / 60
      )
  }
  if(factorGroups & is.vector(groupLevels)){
    summaryDF <- summaryDF %>%
      mutate(
        group_id = factor(group_id, levels = groupLevels)
      )
  }

  if(makeComplete) {
    summaryDF <- summaryDF %>%
      ungroup() %>%
      filter(
        !is.na(treatment) # remove NAs from treatment before making complete
      ) %>%
      complete(
        nesting(
          slice_id
          ,ROI
        )
        , treatment
      ) %>%
      group_by(slice_id, ROI) %>%
      fill(
        group_idx:mouse_id
        , .direction = "downup"
      ) %>%
      ungroup() %>%
      # if no events, frequency is a true zero
      mutate(
        frequency = ifelse(
          is.na(frequency)
          , 0
          , frequency
        )
      )
  }

  return(summaryDF)

}
