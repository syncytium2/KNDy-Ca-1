getLMM_model <- function(df, formula){
  mod <- lmer(
    formula
    , df
  )
  return(mod)
}

getLMM_ANOVA <- function(mod, asDF = TRUE){
  ANOVA <- anova(mod)
  if(asDF){
    ANOVA <- ANOVA %>%
      as.data.frame() %>%
      rownames_to_column("effect")
  }
  return(ANOVA)
}

printLMM_summary <- function(mod){
  print(summary(mod))
}

getLMM_fixedEffects <- function(mod, asDF = TRUE){
  coefs <- coef(
    summary(
      mod
    )
  )
  if(asDF){
    coefs <- coefs %>%
      as.data.frame() %>%
      rownames_to_column("effect")
  }
  return(coefs)
}

formatLMM_fixedEffects <- function(
  coefs
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  formatted <- coefs %>%
    mutate(
      `Pr(>|t|)` = case_when(
        `Pr(>|t|)` < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(round(`Pr(>|t|)`, 3))
      )
    ) %>%
    rename(
      p = `Pr(>|t|)`
    ) %>%
    flextable() %>%
    # bold(
    #   i = ~ `p<.05` == "*"
    # ) %>%
    colformat_double(digits = 3) %>%
    fontsize(
      size = fontSize
      , part = "all"
    ) %>%
    autofit(add_w = addWVal, add_h = addHVal)
  return(formatted)
}

formatLMM_ppt <- function(
  resTable
){
  if("Pr(>|t|)" %in% names(resTable)){
    df <- resTable %>%
      mutate(
        `Pr(>|t|)` = case_when(
          `Pr(>|t|)` < 0.001 ~ as.character("<0.001"),
          TRUE ~ as.character(round(`Pr(>|t|)`, 3))
        )
      ) %>%
      rename(
        p = `Pr(>|t|)`
      )
  } else{
    df <- resTable %>%
      mutate(
        `Pr(>F)` = case_when(
          `Pr(>F)` < 0.001 ~ as.character("<0.001"),
          TRUE ~ as.character(round(`Pr(>F)`, 3))
        )
      ) %>%
      rename(
        p = `Pr(>F)`
      )
  }
  return(df)
}

formatMixedLMM_ppt <- function(
  lmm
){
  resTable <- lmm$anova_table %>% as.data.frame() %>% rownames_to_column("effect")
  if("Pr(>|t|)" %in% names(resTable)){
    df <- resTable %>%
      mutate(
        `Pr(>|t|)` = case_when(
          `Pr(>|t|)` < 0.001 ~ as.character("<0.001"),
          TRUE ~ as.character(round(`Pr(>|t|)`, 3))
        )
      ) %>%
      rename(
        p = `Pr(>|t|)`
      )
  } else{
    df <- resTable %>%
      mutate(
        `Pr(>F)` = case_when(
          `Pr(>F)` < 0.001 ~ as.character("<0.001"),
          TRUE ~ as.character(round(`Pr(>F)`, 3))
        )
      ) %>%
      rename(
        p = `Pr(>F)`
      )
  }
  return(df)
}

formatLMM_anova <- function(
  anova
  , fontSize = 11
  , addWVal = 0.1
  , addHVal = 0.1
){
  formatted <- anova %>%
    mutate(
      `Pr(>F)` = case_when(
        `Pr(>F)` < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(round(`Pr(>F)`, 3))
      )
    ) %>%
    rename(
      p = `Pr(>F)`
    ) %>%
    flextable() %>%
    # bold(
    #   i = ~ `p<.05` == "*"
    # ) %>%
    colformat_double(digits = 3) %>%
    fontsize(
      size = fontSize
      , part = "all"
    ) %>%
    autofit(add_w = addWVal, add_h = addHVal)
  return(formatted)
}

runLMM <- function(
    df
    , formula
    , printSummary = FALSE
){
  mod <- getLMM_model(df, formula)
  if(printSummary) printLMM_summary(mod)
  coefs <- getLMM_fixedEffects(mod)
  formatted <- formatLMM_fixedEffects(coefs)
  anova <- getLMM_ANOVA(mod)
  return(list(
    "coefs" = coefs
    , "formated" = formatted
    , "anova" = anova
    , "model" = mod
  ))
}


## NOTE: emmeans used a different DF calculation (Kenward-Rogers) with mixed model than 
## the default for anova from lmerTest (Satterthwaite) - can change either of them, but should probably pick one
## https://stats.stackexchange.com/a/296405

## This GLMM FAQ summarizing LMM df approximation suggests that KR is the more reliable option
## One quote from Stroup (2014) that suggests you may be able to use KR dfs for GLMMs
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable


## Power analysis for GLMM
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#what-are-the-p-values-listed-by-summaryglmerfit-etc.-are-they-reliable
# provides some other resources

# https://sscc.wisc.edu/sscc/pubs/RegDiag-R/normality.html#qqplots

checkLMMassumptions <- function(afexModel){
  modCheck <- check_model(afexModel$full_model, panel = FALSE)
  return(list(
    plot(modCheck)
    , ggplot()+
      geom_qq(aes(sample = residuals(afexModel$full_model))) +
      geom_abline(color = "red")
  ))
}
