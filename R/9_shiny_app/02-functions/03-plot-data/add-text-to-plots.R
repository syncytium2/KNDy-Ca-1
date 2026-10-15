printTtestText <- function(t_test){
  pval <- t_test$p.value
  pText <- case_when(
    pval < 0.001 ~ "<0.001",
    pval > 0.999 ~ ">0.999",
    TRUE ~ as.character(paste0("==", round(pval, 3)))
  )
  text <- paste0( ## Uses plotmath to format
    "list(", 
    "italic(t)", # italic t
    "[", round(t_test$parameter, 2),"]", #subscript df
    "==", round(t_test$statistic, 2), #value of t
    ",italic(p)", #italic p
    pText, # pvalue with appropriate comparison symbol
    ")"
  )
  return(text)
}

# For categorical variables, x=1 is middle of first category, 0 will be far left
addTtestLayer <- function(t_test, xPos = 0, yPos = 0, fontSize = 7){
  annotate(
    "text", 
    x = xPos, y = yPos, 
    label = printTtestText(t_test),
    hjust = 0,
    size = fontSize,
    family = "Arial",
    color = "black",
    parse = TRUE
  )
}