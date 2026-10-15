#' Run a mean summary on a dataframe
#'
#' Uses rstatix::get_summary_stats() to return the number, mean, standard error,
#' and standard deviation for selected variables
#'
#' @param df a dataframe to be summarized. Provide a grouped df if desired
#' @param col The column(s) to be summarized. Provide as colName, col1:col5, c(col1, col3), etc
#'
#' @return a dataframe with n, mean, sem, sd for each selected column
#' @export
#'
#' @examples
meanSummary <- function(df, col){ # colName, col1:col5, c(col1, col3) etc 
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "full",
      show = c("n", "mean", "se", "sd")
    ) %>%
    rename(
      "sem" = "se"
    )
  
  return(sumDF)
}

#' Do a quartile summary for selected variables
#' 
#' Uses rstatix::get_summary_stats() five number summary
#'
#' @param df a dataframe to be summarized. Provide as grouped df if desired
#' @param col The column(s) to be summarized. Provide as colName, col1:col5, c(col1, col3), etc
#'
#' @return a dataframe with min, q1, median, q3, max summarized for each variable
#' @export
#'
#' @examples
quartilesSummary <- function(df, col){
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "five_number"
    ) %>%
    relocate(
      max
      , .after = q3
    )
  return(sumDF)
}


# Get maximum -------------------------------------------------------------

#' Get the maximum value from repeated measures
#' 
#' This function first groups the dataframe by the grouping variable, for example a mouse ID,
#' and finds the maximum of the selected column for each group
#' 
#' The result is placed in a new named column, defined by maxColName
#'
#' @param df a dataframe with long-form repeated measures data
#' @param col the column containing the repeated measures values
#' @param maxColName the name of the new column that should contain the maximum value
#' @param groupingVar the variable on which to group and find the maximum for
#'
#' @return a dataframe where the maximum col value for each groupingVar value is placed in the maxColName columns
#' @export
#'
#' @examples
getMaxFromRepMeasures <- function(df, col, maxColName, groupingVar){
  df_max <- df %>%
    group_by( {{ groupingVar }} ) %>%
    summarize(
      {{ maxColName }} := max({{ col }}, na.rm = TRUE),
      .groups = "drop"
    )
  return(df_max)
}

getMaxFromRepMeasures_plusOtherVal <- function(df, col, maxColName, groupingVar, valCol, valAtMaxColName){
  df_max <- df %>%
    group_by( {{ groupingVar }} ) %>%
    summarize(
      {{ maxColName }} := max({{ col }}, na.rm = TRUE),
      {{ valAtMaxColName }} := {{ valCol }}[which.max({{ col }})],
      .groups = "drop"
    )
  return(df_max)
}

getMax <- function(df, col) {
  max <- df[[col]] %>% max(na.rm = TRUE)
  return(max)
}