writeToWorkbook = function(sheetName, df, wb, tableStyle = "TableStyleMedium2"){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = tableStyle)
}

saveDFsToExcel <- function(
  fileBaseName,
  prefix = filePrefix,
  addDate = Sys.Date(),
  saveFolder = dataOutputFolder,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(sheetName, dfList[[sheetName]], wb)
  }
  
  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

saveDFsToCSV <- function(
  prefix = filePrefix,
  addDate = Sys.Date(),
  saveFolder = dataOutputFolder,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  for(sheetName in names(dfList)){
    fileName <- paste0(prefix, sheetName, ".csv")
    write.csv(dfList[[sheetName]], file.path(saveFolder, fileName), row.names=FALSE)
  }
}

saveDFsToExcel_shiny <- function(
  filePath,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(sheetName, dfList[[sheetName]], wb)
  }
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

saveUseInfoDFsToExcel <- function(
    fileBaseName,
    prefix = filePrefix,
    addDate = dateToday,
    saveFolder = dataOutputFolder,
    ...
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(
      sheetName, 
      dfList[[sheetName]], 
      wb,
      tableStyle = "TableStyleLight8"
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="earlyLifeTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "LBN",
      type = "contains",
      style = LBNFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "ALPS",
      type = "contains",
      style = ALPSFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultTrt"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "CON",
      type = "contains",
      style = CONFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="Sac_cycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "diestrus",
      type = "contains",
      style = diestrusFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="Sac_cycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "proestrus",
      type = "contains",
      style = proestrusFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultDi"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="moved"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultProLH"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="adultProEphys"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="litterNum"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "1",
      type = "contains",
      style = litter1Fill
    )
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(dfList[[sheetName]])=="litterNum"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "2",
      type = "contains",
      style = litter2Fill
    )
  }
  
  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)
  
  
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

saveManuscriptInfoDFsToExcel <- function(
    fileBaseName,
    prefix = filePrefix,
    addDate = dateToday,
    saveFolder = dataOutputFolder,
    ...
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(
      sheetName, 
      dfList[[sheetName]], 
      wb,
      tableStyle = "TableStyleLight8"
    )
    
    df <- dfList[[sheetName]]
    
    massCols <- getMatchingCols(df)
    
    # matCols <- getColsWithEnding(df, c("_mass", "_age"))
    
    LHCols <- getMatchingCols(df, "LH_")
    
    bodyMassCols <- getMatchingCols(df, "Body_mass")
    
    tissueMassCols <- getColsWithEnding(df, c("_g"))
    
    highlightMissingCols <- sort(c(
      massCols
      # , matCols
      , getColsByNames(df, c(
        "AGD_adult"
      ))
    ))
    
    highlightMissingIfALPS <- sort(c(
      getColsByNames(df, c(
        "Body_mass_AM"
        , "Body_mass_sac"
        , "Adrenal_mass"
        , "Adrenal_mass_perBody_g"
        , "Adrenal_mass_perBodyAM_g"
        , "ReproTract_mass"
        , "ReproTract_mass_perBody_g"
        , "ReproTract_mass_perBodyAM_g"
        , "cort_hr0"
        , "cort_hr5"
      ))
    ))

    highlightMissingIfFemale <- sort(c( 
      getColsByNames(df, c(
        "VO_age"
        , "VO_mass"
        , "Estrus_age"
        , "Estrus_mass"
        , "numCycles"
        , "cycleLength"
        , "percD"
        , "percE"
        , "percP"
        , "maxLH"
      ))
    ))

    highlightMissingIfProSampling <- sort( 
      getColsByNames(df, c(
        "LH_hr0"
        , "LH_hr5"
        , "LH_hr5.5"
        , "LH_hr6.5"
        , "LH_hr7.5"
        , "LH_hr8.5"
        , "LH_hr9.5"
      ))
    )

    highlightMissingIfMale <- sort( 
      getColsByNames(df, c(
        "PreputialSep_age"
        , "PreputialSep_mass"
      ))
    )
    
    highlightMissingIfMaleALPS <- sort( 
      getColsByNames(df, c(
        "Gonad_mass"
        , "Gonad_mass_perBodyAM_g"
        , "Gonad_mass_perBody_g"
      ))
    )
    
    highlightMissingIfCortAdmin <- sort( 
      getColsByNames(df, c(
        "Body_mass_AM_cortAdmin"
        , "Body_mass_sac_cortAdmin"
        , "Adrenal_mass_cortAdmin"
        , "Adrenal_mass_perBody_g_cortAdmin"
        , "Adrenal_mass_perBodyAM_g_cortAdmin"
        , "ReproTract_mass_cortAdmin"
        , "ReproTract_mass_perBody_g_cortAdmin"
        , "ReproTract_mass_perBodyAM_g_cortAdmin"
        , "Gonad_mass_cortAdmin"
        , "Gonad_mass_perBody_g_cortAdmin"
        , "Gonad_mass_perBodyAM_g_cortAdmin"
        , "cort_hr0_cortAdmin"
        , "cort_hr1"
        , "cort_hr2"
        , "cort_hr3"
        , "cort_hr4"
        , "cort_hr5_cortAdmin"
      ))
    )
    
    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="earlyLifeTrt"),
      rows = 2:(length(df[[1]])+1),
      rule = "LBN",
      type = "contains",
      style = LBNFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="adultTrt"),
      rows = 2:(length(df[[1]])+1),
      rule = "ALPS",
      type = "contains",
      style = ALPSFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="adultTrt"),
      rows = 2:(length(df[[1]])+1),
      rule = "CON",
      type = "contains",
      style = CONFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="usage"),
      rows = 2:(length(df[[1]])+1),
      rule = "diestrus",
      type = "contains",
      style = diestrusFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="usage"),
      rows = 2:(length(df[[1]])+1),
      rule = "proSamp",
      type = "contains",
      style = proSampFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="usage"),
      rows = 2:(length(df[[1]])+1),
      rule = "proEphys",
      type = "contains",
      style = proEphysFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="usage"),
      rows = 2:(length(df[[1]])+1),
      rule = "maleALPS",
      type = "contains",
      style = maleALPSFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = which(colnames(df)=="usage"),
      rows = 2:(length(df[[1]])+1),
      rule = "cortAdmin",
      type = "contains",
      style = cortAdminFill
    )

    conditionalFormatting(
      wb,
      sheetName,
      cols = getColsByNames(df, "hitCycle"),
      rows = 2:(length(dfList[[sheetName]][[1]])+1),
      rule = "== TRUE",
      type = "expression",
      style = trueFill
    )

    for (colNum in highlightMissingCols) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        # rows = 1,
        rows = 2:(length(df[[1]])+1),
        rule ="ISBLANK(INDIRECT(ADDRESS(ROW(), COLUMN())))",
        type = "expression",
        style = falseFill
      )
    }

    sexColIndex <- getColsByNames(df, c("sex"))
    sexColLetter <- int2col(sexColIndex)
    
    for (colNum in highlightMissingIfFemale) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', sexColLetter, '2="F", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
    
    usageColLetter <- int2col(getColsByNames(df, c("usage")))
    
    for (colNum in highlightMissingIfProSampling) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', usageColLetter, '2="proSamp", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
    
    for (colNum in highlightMissingIfMale) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', sexColLetter, '2="M", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
    
    for (colNum in highlightMissingIfMaleALPS) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', usageColLetter, '2="maleALPS", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
    
    for (colNum in highlightMissingIfCortAdmin) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', usageColLetter, '2="cortAdmin", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
    
    for (colNum in highlightMissingIfALPS) {
      conditionalFormatting(
        wb,
        sheetName,
        cols = colNum,
        rows = 2:(length(df[[1]])+1),
        rule = paste0('AND($', usageColLetter, '2!="cortAdmin", $', usageColLetter, '2!="noUse", ISBLANK(', int2col(colNum), '2))'),
        type = "expression",
        style = falseFill
      )
    }
  }

  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)



  saveWorkbook(wb, filePath, overwrite = TRUE)
}

getMatchingCols <- function(df, prefix = "Mass_") {
  cols <- grep(paste0("^", prefix), colnames(df))
  return(cols)
}

getColsWithEnding <- function(df, endings) {
  # Create a regex pattern to match either of the endings
  # paste0 combines the character strings to create the regex pattern
  # endings is expected to be a character vector with the possible endings
  pattern <- paste0("(", paste(endings, collapse="|"), ")$")
  
  # grep() will return the indices of the column names that match the regex pattern
  cols <- grep(pattern, colnames(df))
  
  return(cols)
}

getColsByNames <- function(df, columnNames) {
  # Find which columns from the data frame match the provided list of column names
  cols <- which(colnames(df) %in% columnNames)
  return(cols)
}

greenFill <- function(){
  createStyle(bgFill = "#C6EFCE")
}

yellowFill <- function(){
  createStyle(bgFill = "#ffff00")
}

redFill <- function(){
  createStyle(bgFill = "#FFC7CE")
}

greyFill <- function(){
  createStyle(bgFill = 	"#D3D3D3")
}

blueFill <- function(){
  createStyle(bgFill = "#9edaff")
}

pinkFill <- function(){
  createStyle(bgFill = "#f3b4f6")
}

LBNFill <- createStyle(bgFill = "#33CCCC")

ALPSFill <- createStyle(bgFill = "#000000", fontColour = "#FFFFFF")

CONFill <- createStyle(bgFill = "#D9D9D9")

diestrusFill <- createStyle(bgFill = "#DCE6F1")

proestrusFill <- createStyle(bgFill = "#E4DFEC")

proSampFill <- createStyle(bgFill = "#E4DFEC")
proEphysFill <- createStyle(bgFill = "#F2D8F4")

maleALPSFill <- createStyle(bgFill = "#FDE9D9")
cortAdminFill <- createStyle(bgFill = "#F2DCDB")

trueFill <- createStyle(bgFill = "#C6EFCE", fontColour = "#006100")
falseFill <- createStyle(bgFill = "#FFC7CE")

litter1Fill <- createStyle(bgFill = "#CCECFF")
litter2Fill <- createStyle(bgFill = "#FFCCFF")
