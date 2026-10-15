
# facets ------------------------------------------------------------------






# Saving ------------------------------------------------------------------



# Add a basename and figure number for file prefix
# This function creates another function that can be used for a particular
# presentation file
makeBaseNameFunc <- function(prefix){
  if(!prefix==""){
    prefix <- paste0(prefix, "_")
  }
  func <- function(figNum){
    baseName <- paste0(prefix, figNum, "_")
    return(baseName)
  }
  return(func)
}

makeSavePPTFunc <- function(presPPT, presFolder, presFileName, addDate){
  fullFileName <- paste0(presFileName)
  if(addDate){
    fullFileName <- paste0(fullFileName, "_", Sys.Date())
  }
  fullFileName <- paste0(fullFileName, ".pptx")
  
  saveFunc <- function(){
    print(presPPT, target = file.path(presFolder, fullFileName))
  }
  return(saveFunc)
}


exportImg_forPurposeFunc <- function(imgType, figNumFunc, plotFolder, compType){
  func <- function(plot, fileBaseName, figNum, units, width, height){
    flexSave(
      baseName = fileBaseName,
      thisFilePrefix = figNumFunc(figNum),
      plot = plot,
      fileType =imgType,
      filePath = plotFolder,
      compType = compType,
      units = units,
      width = width,
      height = height
    )
  }
  return(func)
}

cumFreq_geom <- function(
    zoom_x = FALSE, # Zoom to part of x axis
    xmin = NULL,
    xmax = NULL,
    zoom_y = FALSE, # Zoom to part of y axis
    ymin = NULL,
    ymax = NULL,
    textSize = 11,
    lineSize = 2
) {
  list(
    stat_ecdf(
      size = lineSize
      , geom = "line"
      , pad = FALSE
    ),
    stat_ecdf(
      geom = "point"
      , pad = FALSE
    ),
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}),
    textTheme(size = textSize),
    boxTheme()
  )
}