# From Michael Roberts - 2021-10-27

# This file contains a function to generate a Linear Mixed Model (LMM) and p
# value statistics of comparisons of each Fixed Effect to the Control.
#
# Input data format: Excel file with three columns
#     Column 1: Response values (e.g. spikes)
#     Column 2: Treatment condition labels (e.g. Control, Drug1, Drug 2)
#     Column 3: Cell IDs (e.g. 1,2,3,4,5)
# Note that lme4 alphabetizes the Treatment conditions and uses the condition
# that appears first in alphabetical order as the control. Therefore, be sure to
# name the control condition so that it sorts properly (e.g. start it with an
# 'a' if necessary).

lmmFunc <- function(df){
  
  # Purpose: To generate a LMM and p value statistics for a data set with one
  # set of fixed effects (e.g. treatment conditions) and one set of random 
  # effects (e.g. neurons).
  #
  # df is a data frame containing one column of measured values (the dependent
  # variable), one column of treatment conditions (the fixed effects), and one
  # column of cell IDs (the random effects).
  
  # Load the lme4 and lmerTest libraries
  suppressMessages(library(lme4, warn.conflicts = F, quietly = T))
  suppressMessages(library(lmerTest, warn.conflicts = F, quietly = T))
  
  
  results <- lmer(Response ~ Treatment + (1|Cell), data = df) 
  print(anova(results))
  summary(results) 
  
}