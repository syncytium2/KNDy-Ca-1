countMiceAndLitters <- function(df, filterByVar, groupingVars){
  count <- df %>%
    filter(
      !is.na({{ filterByVar }})
    ) %>%
    group_by(!!! groupingVars)%>%
    select(
      mouseID,
      damID,
      !!! groupingVars
    ) %>%
    summarise(
      numCells = n(),
      numMice = length(unique(mouseID)),
      numLitters = length(unique(damID)),
      .groups = "drop"
    )
  return(count)
}