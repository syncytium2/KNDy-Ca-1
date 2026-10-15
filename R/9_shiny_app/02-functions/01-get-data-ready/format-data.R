#' make factors
#'
#' @param df a dataframe
#' @param cols the columns that you wish to make factors; if using multiple use c(...)
#'
#' @return a dataframe with the indicated columns made factors
#' @export
#'
#' @examples
#' Demo_dam %>%
#' makeFactors(c(
#'  damID,
#'  dam,
#'  ParaType,
#'  litterNum,
#'  cohort
#' ))
#'
#' makeFactors(Off_ID, c(mouseID))
#' makeFactors(EndPara_off, mouseID)
makeFactors <- function(df, cols){
  df <- df %>%
    mutate(
      across(
        {{ cols }},
        as.factor
      )
    )
  return(df)
}

#' combine sex and gonads variables
#'
#' Create a group_id and group_idx variable to combine
#' sex and gonads variables.
#' This is easier to work with for some plots
#' Factor levels are DI, OVX, MALE, and ORX
#'
#' @param df a dataframe with columns sex and gonads
#'
#' @return a dataframe with the column group_id and group_idx added to make four groups
#' @export
#'
#' @examples
combineGroups <- function(df, groupLevels = groups){
  df <- df %>%
    mutate(
      group_id = case_when(
        sex == "female" & gonads == "intact" ~ "DI"
        , sex == "female" & gonads == "GDX"    ~ "OVX"
        , sex == "male"   & gonads == "intact" ~ "MALE"
        , sex == "male"   & gonads == "GDX"    ~ "ORX"
        , TRUE ~ NA_character_
      )
      , group_id = factor(group_id, levels = groupLevels)
      , group_idx = case_when(
        group_id == "DI" ~ 1
        , group_id == "OVX" ~ 2
        , group_id == "MALE" ~ 3
        , group_id == "ORX" ~ 4
        , TRUE ~ NA
      )
      , group_idx = factor(group_idx, levels = c(1, 2, 3, 4))
    )
  return(df)
}

# td reverse the process, new format lacks sex and gonads columns!
#' combine sex and gonads variables
#'
#' Create a group_id and group_idx variable to combine
#' sex and gonads variables.
#' This is easier to work with for some plots
#' Factor levels are DI, OVX, MALE, and ORX
#'
#' @param df a dataframe with column group_id
#'
#' @return a dataframe with the column sex, gonads and group_idx added to make four groups
#' @export
#'
#' @examples
#'
extractSexGonads <- function(df){
  df <- df %>%
    mutate(
      sex = case_when(
        group_id == "DI" ~ "female",
        group_id == "OVX" ~ "female",
        group_id == "MALE" ~ "male",
        group_id == "ORX" ~ "male"
        , TRUE ~ NA_character_
      ),
      gonads = case_when(
        group_id == "DI" ~ "intact",
        group_id == "OVX" ~ "GDX",
        group_id == "MALE" ~ "intact",
        group_id == "ORX" ~ "GDX"
      )
    )
  return(df)
}



#' Title
#'
#' @param df a dataframe of the events. Should already have the treat_clean column
#' @param treatmentName the treatment name for which you want to extract all events
#' # where this was the second treatment (first after baseline)
#' @param factorOrder Defaults to null, which then makes treatment factors c("baseline", treatmentName, "high K+")
#' # if provide, provide as c(..., ..., ...)
#'
#' @return a list of df_filtered (all events from slices that received that treatment second) and vector of unique sliceIDs for the selected treatment
getDataByTreatment <- function(
    df
    , treatmentName
    , factorOrder = NULL # defaults to c("baseline", treatmentName, "high K+")
) {
  # get all rows where the treat_clean value equals treatmentName
  # and index is 2 (desired treatment occurs as second treatment)
  allRows_secondIndex <- df %>%
    filter(
      treat_clean == treatmentName
      , treatment_index == 2
    )

  # get all of the slice ids from experiments with this treatment
  sliceIDs <- allRows_secondIndex$slice_id %>%
    unique() %>%
    sort()

  # get only the event data for these experiments
  df_filtered <- df %>%
    filter(
      slice_id %in% sliceIDs
    )

  # replace the treatment names with the cleaned versions
  if("treat_clean" %in% names(df)){
    df_filtered <- df_filtered %>%
      mutate(
        treatment = treat_clean
      ) %>%
      select(
        -treat_clean
      )
  }

  if(is.null(factorOrder)){
    # this will make NA any treatments that are not
    # baseline, this treatment, or high K+
    df_filtered <- df_filtered %>%
      mutate(
        treatment = factor(
          treatment
          , levels = c("baseline", treatmentName, "high K+")
        )
      )
  } else {
    df_filtered <- df_filtered %>%
      mutate(
        treatment = factor(
          treatment
          , levels = factorOrde
        )
      )
  }

  # could mutate treatment_group to be treatmentName, but if use
  # df that already has this added, then don't need to redo this here

  return(
    list(
      df_filtered = df_filtered
      , sliceIDs = sliceIDs
    )
  )
}
