textTheme <- function(size = 11, boldXText = FALSE, forManuscript = isManuscript){
  theme = theme(
    text = element_text(
      size = size
      # , family = "Arial" # Downloading the fonts is always a thing that trips me up on a new device, but I have some troubleshooting code that we could try and then uncomment this
      , color = "black"
    ),
    axis.text = element_text(
      size = size
      # , family = "Arial"
      , color = "black"
    ),
    strip.text = element_text(face = "bold", size = size),
    axis.title = element_text(face = "bold")
    , axis.title.x = element_text(margin = margin(t = ifelse(forManuscript, -2, 0)))
    , plot.title = element_text(
      hjust = 0.5, size = size
      # , family = "Arial"
      , color = "black"
      , face = "bold"
      , margin = margin(b = -3)
    )
  )
  
  if(boldXText){
    theme <- theme + theme(
      axis.text.x = element_text(face = "bold")
    )
  }
  return(theme)  
}

boxTheme <- function(axisSize = 0.5){
  theme = theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = axisSize),
    axis.ticks = element_line(color = "black"),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.background = element_blank(), # testing, unsure if allowed
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(color = "black", fill = NA, inherit.blank = TRUE),
    legend.background = element_blank(),
    legend.box.background = element_blank()
    , plot.margin = unit(c(0.1,0.1,0,0.1), "cm")
  )
}


# Examples ----------------------------------------------------------------

# AGG - I left these as examples for formatting for different treatment fills/shapes/etc
# because I can never remember how to set this up

earlyLifeFill <- function(
  STD = "STD",
  LBN = "LBN",
  STDColor = ifelse(forManuscript, "#CCCCCC", "white"),
  LBNColor = "cyan3"
  , fillAlpha = 0.7
  , forManuscript = isManuscript
){
  fill <- scale_fill_manual(
    "early life trt", 
    values = alpha(c(STD=STDColor, LBN=LBNColor), fillAlpha)
  )
  return(fill)
}

earlyLifeColor <- function(
  STD = "STD",
  LBN = "LBN",
  STDColor = ifelse(forManuscript, "#CCCCCC", "white"),
  LBNColor = "cyan3"
  , colorAlpha = 1
  , forManuscript = isManuscript
){
  color <- scale_color_manual(
    "early life trt", 
    values = alpha(c(STD = STDColor, LBN = LBNColor), colorAlpha)
  )
  return(color)
}

adultTrtFill <- function(
  CON = "CON",
  ALPS = "ALPS",
  CONColor = "white",
  ALPSColor = "black"
){
  fill <- scale_fill_manual(
    "adult trt", 
    values = c(CON=CONColor, ALPS=ALPSColor)
  )
  return(fill)
}

adultTrtColor <- function(
  CON = "CON",
  ALPS = "ALPS",
  CONColor = "black",
  ALPSColor = "black"
){
  color <- scale_color_manual(
    "adult trt", 
    values = c(CON = CONColor, ALPS = ALPSColor)
  )
  return(color)
}

adultTrtFillShape <- function(
  CON_color = "black",
  ALPS_color = "black",
  CON_fill = "white",
  ALPS_fill = "black"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("CON"=CON_color, 
                 "ALPS"=ALPS_color)),
    scale_fill_manual(
      "treatment", 
      values = c("CON"=CON_fill, 
                 "ALPS"=ALPS_fill)),
    scale_shape_manual(
      "treatment", 
      values = c("CON"=21, 
                 "ALPS"=23))
  )
  return(layers)
}

earlyLifeLineType <- function(
  STD = "STD",
  LBN = "LBN"
){
  lineType <- scale_linetype_manual(
    "early life trt", 
    values = c(STD = "dashed", LBN = "solid")
  )
}

comboTrtFillShape <- function(
  STD_CON_color = ifelse(forManuscript, "#CCCCCC", "white"),
  STD_ALPS_color = "#666666",
  LBN_CON_color = "cyan3",
  LBN_ALPS_color = "#FF0099",
  STD_CON_fill = ifelse(forManuscript, "#CCCCCC", "white"),
  STD_ALPS_fill = "#666666",
  LBN_CON_fill = "cyan3",
  LBN_ALPS_fill = "#FF0099"
  , fillAlpha = 0.7
  , colorAlpha = 1
  , forManuscript = isManuscript
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = alpha(c("STD-CON"=STD_CON_color, 
                 "STD-ALPS"=STD_ALPS_color, 
                 "LBN-CON"=LBN_CON_color, 
                 "LBN-ALPS"=LBN_ALPS_color), colorAlpha)),
    scale_fill_manual(
      "treatment", 
      values = alpha(c("STD-CON"=STD_CON_fill, 
                 "STD-ALPS"=STD_ALPS_fill, 
                 "LBN-CON"=LBN_CON_fill, 
                 "LBN-ALPS"=LBN_ALPS_fill), fillAlpha)),
    scale_shape_manual(
      "treatment", 
      values = c(
        "STD-CON"=ifelse(forManuscript, 16, 21), 
        "STD-ALPS"=ifelse(forManuscript, 16, 21),
        "LBN-CON"=ifelse(forManuscript, 16, 21), 
        "LBN-ALPS"=ifelse(forManuscript, 16, 21)
                 ))
  )
  return(layers)
}

comboTrtLineColor <- function(
  STD_CON_color = "#999999",
  STD_ALPS_color = "#000000",
  LBN_CON_color = "cyan3",
  LBN_ALPS_color = "#FF0099"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("STD-CON"=STD_CON_color, 
                 "STD-ALPS"=STD_ALPS_color, 
                 "LBN-CON"=LBN_CON_color, 
                 "LBN-ALPS"=LBN_ALPS_color))
  )
  return(layers)
}


dosageFillShape <- function(
  color_0 = ifelse(forManuscript, "#CCCCCC", "black"),
  color_2 = "black",
  fill_0 = "white",
  fill_2 = "black"
  , fillAlpha = 1
  , forManuscript = isManuscript
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("0"=color_0, 
                 "2"=color_2),
      labels = c("0" = "0mg/kg",
                 "2" = "2mg/kg")
    ),
    scale_fill_manual(
      "treatment", 
      values = alpha(c("0"=fill_0, 
                 "2"=fill_2), fillAlpha),
      labels = c("0" = "0mg/kg",
                 "2" = "2mg/kg")
    ),
    scale_shape_manual(
      "treatment", 
      values = c("0"=ifelse(forManuscript, 16, 21),
                 "2"=ifelse(forManuscript, 16, 21)),
      labels = c("0" = "0mg/kg",
                 "2" = "2mg/kg")
    )
  )
  return(layers)
}

dosageColor <- function(
  color_0 = "black",
  color_2 = "black"
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = c("0"=color_0, 
                 "2"=color_2))
  )
  return(layers)
}

dosageFill <- function(
  fill_0 = "white",
  fill_2 = "black"
){
  layers <- list(
    scale_fill_manual(
      "treatment", 
      values = c("0"=fill_0, 
                 "2"=fill_2))
  )
  return(layers)
}

allDosageColorList <- c(
  "0"="black", 
  "0.5" = "#1b9e77", # green
  "1" = "#d95f02", # orange
  "3" = "#31688EFF", # purple
  "10" = "#1b9e77", # green
  "20" = "#fdb863", # yellow
  "50" = "#5e3c99" # purple
)

allDosageFillList <- c(
  "0"="white", 
  "0.5" = "#1b9e77", # green
  "1" = "#d95f02", # orange
  "3" = "#31688EFF", # purple
  "10" = "#1b9e77", # green
  "20" = "#fdb863", # yellow
  "50" = "#5e3c99" # purple
)

allDosageShapeList <- c(
  "0"=21, 
  "0.5" = 0,
  "1" = 2,
  "3" = 25,
  "10" = 22,
  "20" = 23,
  "50" = 3
)

allDosageLineList <- c(
  "0"="solid", 
  "0.5" = "solid",
  "1" = "solid",
  "3" = "solid",
  "10" = "solid",
  "20" = "solid",
  "50" = "solid"
)

allDosageFillShape <- function(
    
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = allDosageColorList),
    scale_fill_manual(
      "treatment", 
      values = allDosageFillList),
    scale_shape_manual(
      "treatment", 
      values = allDosageShapeList),
    scale_linetype_manual(
      "treatment",
      values = allDosageLineList)
  )
  return(layers)
}


pilotDosageFillShape <- function(
    doses # c(x,x,x)
){
  layers <- list(
    scale_color_manual(
      "treatment", 
      values = allDosageColorList[names(allDosageColorList) %in% doses]),
    scale_fill_manual(
      "treatment", 
      values = allDosageFillList[names(allDosageFillList) %in% doses]),
    scale_shape_manual(
      "treatment", 
      values = allDosageShapeList[names(allDosageShapeList) %in% doses]),
    scale_linetype_manual(
      "treatment",
      values = allDosageLineList[names(allDosageLineList) %in% doses])
  )
  return(layers)
}
