myDisplay <- function(df, docType = doc.type){
  rownames(df) <- gsub("trt","treatment", rownames(df)) # replace 'trt' with 'treatment' in the row names of dataframe
  if(!is.null(docType) && docType == "docx"){
    return(flextable(df))
  } else {
    return(df)
  }
}

subTrtInRowNames <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("early-life trt", sub, row.names(tbl))
  return(tbl)
}

subEarlyLifeTrtInRowNames <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("earlyLifeTrt", sub, row.names(tbl))
  return(tbl)
}

subAdultTrtInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("adultTrt", "adult treatment", row.names(tbl))
  return(tbl)
}

subEarlyLifeTrtInRowNames_quartile <- function(tbl, sub = "early-life treatment") {
  row.names(tbl) <- gsub("earlyLifeTrt1", sub, row.names(tbl))
  return(tbl)
}

subAdultTrtInRowNames_quartile <- function(tbl, sub = "adult treatment") {
  row.names(tbl) <- gsub("adultTrt1", sub, row.names(tbl))
  return(tbl)
}

subSacCycleInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("Sac_cycle", "cycle stage", row.names(tbl))
  return(tbl)
}

subGonadInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("gonads", "gonad status", row.names(tbl))
  return(tbl)
}

subColonInRowNames <- function(tbl) {
  row.names(tbl) <- gsub("\\:", " * ", row.names(tbl))
  return(tbl)
}

subnParColNames <- function(tbl) {
  if("Statistic" %in% colnames(tbl)) {
    colnames(tbl)[colnames(tbl) == 'Statistic'] <- 'F'
  }
  if("p-value" %in% colnames(tbl)) {
    colnames(tbl)[colnames(tbl) == 'p-value'] <- 'p'
  }
  return(tbl)
}

formatPCol <- function(df, pCol = p){
  pColAsText = deparse(substitute(pCol))
  df <- df %>%
    mutate(
      !! pColAsText := ifelse(
        {{ pCol }} < 0.001, "<0.001"
        , ifelse(
          {{ pCol }} > 0.999, ">0.999"
          , format(round({{ pCol }}, 3), nsmall = 3, trim = TRUE)
        )
      )
    )
  return(df)
}

addTableLegend <- function(doc, tableNum, tableLegend){
  fp_bold <- fp_text(font.size = 11, bold = TRUE)
  fp_normal <- fp_text(font.size = 11, bold = FALSE)

  fpar_element <- fpar(ftext(paste0("Table ", tableNum, ". "), prop = fp_bold),
                       ftext(tableLegend, prop = fp_normal))

  doc <- body_add_fpar(doc, fpar_element, style = "Normal")

  return(doc)
}

simplifyLMMOutput <- function(anovaTable){
  tbl <- anovaTable %>%
    subGonadInRowNames() %>%
    subColonInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column("variable") %>%
    mutate(
      `F` = format(round(`F`, 2), nsmall = 2, trim = TRUE)
      , `df` = paste0(round(`num Df`), ", "
                      , format(round(`den Df`, 1)
                               , nsmall = 1
                               , trim = TRUE
                               ))
      , .after = `F`
    ) %>%
    rename(
      p = `Pr(>F)`
    ) %>%
    formatPCol() %>%
    select(
      -c(`num Df`, `den Df`)
    )
  return(tbl)
}

roundDecimals <- function(val, digits){
  format(round(val, digits), nsmall = digits, trim = TRUE)
}

simplifyEMMOutput <- function(emmTbl){
  levelExists <- "level" %in% colnames(emmTbl)

  tbl <- emmTbl %>%
    rename(
      SEM = SE
    ) %>%
    mutate(
      `95% CI` = paste0("[", format(round(lower.CL, 2), nsmall = 2, trim = TRUE), ", ", format(round(upper.CL, 2), nsmall = 2, trim = TRUE), "]")
    )

  if(levelExists){
    tbl <- tbl %>%
      mutate(
        level = as.character(level)
      )
  }

  tbl <- tbl %>%
    select(
      -c(lower.CL, upper.CL)
    )
  return(tbl)
}

simplifyEMMPairsOutput_CI <- function(pairsTbl){
  tbl <- pairsTbl %>%
    summary(
      infer = c(TRUE, TRUE)
      , adjust = "holm"
    ) %>%
    as_tibble()

  tbl <- tbl %>%
    mutate(
      `95% CI` = paste0("[", roundDecimals(lower.CL, 2), ", ", roundDecimals(upper.CL, 2), "]")
      , .before = SE
    ) %>%
    select(
      -c(lower.CL, upper.CL)
    ) %>%
    rename(
      SEM = SE
      , t = t.ratio
      , p = p.value
    ) %>%
    formatPCol()

  return(tbl)
}

addTableInfo <- function(tbl, figLab = thisFigureLab, outcomeLab = thisOutcomeLab, noGroup = TRUE){
  tbl <- tbl %>%
    mutate(
      fig = figLab
      , outcome = outcomeLab
      , .before = contrast
    )

  if(noGroup){
    tbl <- tbl %>%
      mutate(
        `group level` = NA
        , .before = contrast
      )
  }
  return(tbl)
}




simplifyEMMPairsOutput <- function(pairsTbl){
  tbl <- pairsTbl %>%
    as_tibble() %>%
    rename(
      SEM = SE
      , `t ratio` = t.ratio
      , p = p.value
    ) %>%
    formatPCol()
  return(tbl)
}

simplifyJointTestOutput <- function(jt) {
  df <- jt %>%
    as_tibble() %>%
    rename(
      variable = `model term`
    ) %>%
    mutate(
      variable = ifelse(
        variable == "earlyLifeTrt"
        , "early-life treatment"
        , ifelse(
          variable == "adultTrt"
          , "adult treatment"
          , ifelse(
            variable == "earlyLifeTrt:adultTrt"
            , "early-life treatment * adult treatment"
            , NA
          )
        )
      )
      , df = paste0(
        df1
        , ", "
        , df2
      )
      , .after = "variable"
    ) %>%
    rename(
      `p` = p.value
      , `F ratio` = F.ratio
    ) %>%
    relocate(
      `F ratio`
      , .before = "df"
    ) %>%
    formatPCol() %>%
    select(
      -c(df1, df2)
    )
  return(df)
}

simplifyQuartileOutput <- function(df # just one quartile
){
  df %>%
    subEarlyLifeTrtInRowNames_quartile() %>%
    subAdultTrtInRowNames_quartile() %>%
    subColonInRowNames() %>%
    as.data.frame() %>%
    rownames_to_column(var = "fixed effect") %>%
    rename(
      SEM = `Std. Error`
      , p = `Pr(>|t|)`
    ) %>%
    mutate(
      Value = 10^Value
      , SEM = 10^SEM
      , `95% CI` = paste0(
        "[", format(
          round(
            10^`lower bound`
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), ", ", format(
          round(
            10^`upper bound`
            , 2
          )
          , nsmall = 2, trim = TRUE
        ), "]")
      , .after = SEM
    ) %>%
    select(
      -c(`lower bound`, `upper bound`)
    ) %>%
    formatPCol()
}

simplifyAllQuartilesOutput <- function(allDFs){

  resultDF <- NULL

  for(name in names(allDFs)){
    df <- allDFs[[name]]

    simpDF <- simplifyQuartileOutput(df) %>%
      mutate(
        quartile = name
        , .before = `fixed effect`
      )

    resultDF <- bind_rows(resultDF, simpDF)
  }

  return(resultDF)
}

addTableAndCaptionToDoc <- function(tbl, tableCaption, tableNum, doc){
  doc <- doc %>%
    addTableLegend(tableNum, tableCaption) %>%
    body_add_flextable(tbl) %>%
    body_add_par("")
  return(doc)
}

makeManuscriptFlexTable <- function(
    df
    , headerDF = NULL
    , vertLines = c()
    , horzLines = c()
    , fullWidth = TRUE
    , vertMergeCols = c()
    , round1Cols = c()
    , round2Cols = c()
    , round3Cols = c()
){
  if(!is.null(headerDF)){
    tbl <- df %>%
      flextable(
        col_keys = headerDF$col_keys
      ) %>%
      set_header_df(
        mapping = headerDF, key = "col_keys"
      )
  } else {
    tbl <- df %>%
      flextable(

      )
  }

  if(length(vertMergeCols)>0){
    for (col in vertMergeCols) {
      tbl <- tbl %>%
        merge_v(j = col)
    }
  }

  tbl <- tbl %>%
    merge_h(part = "header") %>%
    colformat_num(
      na_str = ""
    )

  if(fullWidth){
    tbl <- tbl %>%
      set_table_properties(
        layout = "autofit"
        , width = 1
      )
  } else {
    tbl <- tbl %>%
      set_table_properties(
        layout = "autofit"
      )
  }

  if(length(round1Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round1Cols, digits = 1)
  }

  if(length(round2Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round2Cols, digits = 2)
  }

  if(length(round3Cols)>0){
    tbl <- tbl %>%
      colformat_double(j = round3Cols, digits = 3)
  }

  tbl <- tbl %>%
    theme_booktabs() %>%
    align(
      align = "center"
      , part = "all"
    )

  if(length(vertLines)>0){
    for (col in vertLines) {
      tbl <- tbl %>%
        vline(j = col)
    }
  }

  if(length(horzLines)>0){
    for (col in horzLines) {
      tbl <- tbl %>%
        hline(i = col)
    }
  }

  tbl <- tbl %>%
    fix_border_issues(

    )

  return(tbl)
}

getLMMFormula <- function(lmm){
  formula <- deparse(lmm$call$formula)
  formula <- subGonadInFormula(formula)
  formula <- subSliceIDInFormula(formula)
  formula <- subMouseIDInFormula(formula)
  return(formula)
}

subGonadInFormula <- function(formula) {
  formula <- gsub("gonads", "gonad status", formula)
  return(formula)
}

subEarlyLifeTrtInFormula <- function(formula){
  formula <- gsub("earlyLifeTrt", "early-life treatment", formula)
  return(formula)
}

subAdultTrtInFormula <- function(formula){
  formula <- gsub("adultTrt", "adult treatment", formula)
  return(formula)
}

subCycleInFormula <- function(formula){
  formula <- gsub("Sac_cycle", "cycle stage", formula)
  return(formula)
}

subSliceIDInFormula <- function(formula){
  formula <- gsub("slice_id", "slice", formula)
  return(formula)
}

subDamIDInFormula <- function(formula){
  formula <- gsub("damID", "dam", formula)
  return(formula)
}

subMouseIDInFormula <- function(formula){
  formula <- gsub("mouse_id", "mouse", formula)
  return(formula)
}


