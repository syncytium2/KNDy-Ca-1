getStillImgName = function(
    damID,
    DOB,
    PND,
    ZT,
    min,
    imgFolderPath = damImgsFolder
){
  thisDateTime <- getZT_DateTime(DOB, PND, ZT)
  thisDate <- getZTDate(thisDateTime)
  thisTime <- getZTHour(thisDateTime)
  
  regEx <- paste0(
    # "^.*", # looks for the whole file path when matching, not just file name
    damID,
    "_",
    thisDate,
    "_",
    sprintf("%02d", as.integer(thisTime)),
    ".*",
    sprintf("%02d", as.integer(min)),
    "min",
    "\\.png$")
  
  return(regEx)
  
  # # print(regEx)
  # 
  # fileName <- findMatchingFile(
  #   imgFolderPath,
  #   regEx,
  #   damID,
  #   date = thisDate
  # )
  # 
  # if(!is.na(fileName)){
  #   fileName <- basename(fileName)
  # }
  # 
  # return(fileName)
}

getStillRegEx = function(
    damID,
    DOB,
    PND,
    ZT,
    min
){
  thisDateTime <- getZT_DateTime(DOB, PND, ZT)
  thisDate <- getZTDate(thisDateTime)
  thisTime <- getZTHour(thisDateTime)
  
  regEx <- paste0(
    "^.*", # looks for the whole file path when matching, not just file name
    damID,
    "_",
    thisDate,
    "_",
    sprintf("%02d", as.integer(thisTime)),
    ".*",
    sprintf("%02d", as.integer(min)),
    "min",
    "\\.png$")
  
  return(regEx)
  
  # # print(regEx)
  # 
  # fileName <- findMatchingFile(
  #   imgFolderPath,
  #   regEx,
  #   damID,
  #   date = thisDate
  # )
  # 
  # if(!is.na(fileName)){
  #   fileName <- basename(fileName)
  # }
  # 
  # return(fileName)
}