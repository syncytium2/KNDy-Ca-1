# all ph_label can be read here
# layout_properties(doc, layout = "Title and Content")


addSlide_sectionHead <- function(
    ppt,
    title
){
  ppt <- add_slide(ppt, "Section Header")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
}

addSlide_oneGraph <- function(
  ppt,
  title,
  plot,
  makeEditable = FALSE
){
  if(makeEditable){
    plot <- dml(ggobj=plot)
  }
  
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

addSlide_twoGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
  }
  ppt <- add_slide(ppt, "Two Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  return(ppt)
}

addSlide_twoGraphMoreLeft <- function(
  ppt,
  title,
  plot1,
  plot2,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
  }
  ppt <- add_slide(ppt, "Two content more left")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  return(ppt)
}

addSlide_graphTable <- function(
  ppt,
  title,
  plot,
  table,
  makeEditable = FALSE,
  dontFormat = FALSE,
  textSize = 16
){
  if(makeEditable){
    plot <- dml(ggobj = plot)
  }
  
  if(!dontFormat){
    table <- table %>%
      flextable() %>%
      fontsize(size = textSize, part = "all") %>%
      colformat_double(digits = 3) %>%
      autofit()
  } else { ## have to be careful here - needs to already by a flextable
    # table <- table %>%
    #   autofit()
  }
  
  ppt <- add_slide(ppt, "Two Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = table,
    location = ph_location_label("Content Placeholder 3")
  )
  return(ppt)
}

addSlide_threeGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  plot3,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
    plot3 <- dml(ggobj = plot3)
  }
  ppt <- add_slide(ppt, "Title_threeContent")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  ppt <- ph_with(
    ppt,
    value = plot3,
    location = ph_location_label("Content Placeholder 4")
  )
  return(ppt)
}

addSlide_fourGraph <- function(
  ppt,
  title,
  plot1,
  plot2,
  plot3,
  plot4,
  makeEditable = FALSE
){
  if(makeEditable){
    plot1 <- dml(ggobj = plot1)
    plot2 <- dml(ggobj = plot2)
    plot3 <- dml(ggobj = plot3)
    plot4 <- dml(ggobj = plot4)
  }
  ppt <- add_slide(ppt, "Title_fourContent")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = plot1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = plot2,
    location = ph_location_label("Content Placeholder 3")
  )
  ppt <- ph_with(
    ppt,
    value = plot3,
    location = ph_location_label("Content Placeholder 4")
  )
  ppt <- ph_with(
    ppt,
    value = plot4,
    location = ph_location_label("Content Placeholder 5")
  )
  return(ppt)
}

addSlide_DemoCycles <- function(
  ppt,
  title,
  STDplot,
  LBNplot,
  percDaysPlot,
  makeEditable = FALSE
){
  if(makeEditable){
    STDplot <- dml(ggobj = STDplot)
    LBNplot <- dml(ggobj = LBNplot)
    percDaysPlot <- dml(ggobj = percDaysPlot)
  }
  ppt <- add_slide(ppt, "Title_demoCycles")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  ppt <- ph_with(
    ppt,
    value = STDplot,
    location = ph_location_label("STD")
  )
  ppt <- ph_with(
    ppt,
    value = LBNplot,
    location = ph_location_label("LBN")
  )
  ppt <- ph_with(
    ppt,
    value = percDaysPlot,
    location = ph_location_label("percDays")
  )
  return(ppt)
}

addSlide_oneTable <- function(
  ppt,
  title,
  table,
  textSize = 16,
  dontFormat = FALSE
){
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  
  if(!dontFormat){
    table <- table %>%
      flextable() %>%
      fontsize(size = textSize, part = "all") %>%
      colformat_double(digits = 3) %>%
      autofit()
  } else { ## have to be careful here - needs to already by a flextable
    # table <- table %>%
    #   autofit()
  }
    
  
  ppt <- ph_with(
    ppt,
    value = table,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

addSlide_twoTables <- function(
  ppt,
  title,
  table1,
  table2,
  textSize = 16,
  dontFormat = FALSE
){
  ppt <- add_slide(ppt, "Two Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  
  if(!dontFormat){
    table1 <- table1 %>%
      flextable() %>%
      fontsize(size = textSize, part = "all") %>%
      colformat_double(digits = 3) %>%
      autofit()
    table2 <- table2 %>%
      flextable() %>%
      fontsize(size = textSize, part = "all") %>%
      colformat_double(digits = 3) %>%
      autofit()
  } else { ## have to be careful here - needs to already by a flextable
    # table <- table %>%
    #   autofit()
  }
    
  
  ppt <- ph_with(
    ppt,
    value = table1,
    location = ph_location_label("Content Placeholder 2")
  )
  ppt <- ph_with(
    ppt,
    value = table2,
    location = ph_location_label("Content Placeholder 3")
  )
  return(ppt)
}

addSlide_text <- function(
  ppt,
  title,
  blockList,
  listLevels
){
  ppt <- add_slide(ppt, "Title and Content")
  ppt <- ph_with(
    ppt,
    value = title,
    location = ph_location_label("Title 1")
  )
  
  ppt <- ph_with(
    ppt,
    value = blockList,
    level_list = listLevels,
    location = ph_location_label("Content Placeholder 2")
  )
  return(ppt)
}

makeBullet <- function(theText, fontSize = 22)
  fpar(
    ftext(
      theText,
      fp_text(font.size=fontSize)
    )
  )

addStatsToBottom <- function(
    ppt,
    statsText,
    fontSize = 14
){
  free_loc <- ph_location(
    left = 0.92, top = 7, 
    width = 11.5, height = 0.4)
  
  ppt <- ph_with(ppt, 
                 fpar(statsText, fp_t = fp_text(font.size = fontSize)),
                 location = free_loc )
  return(ppt)
}

addTwoStatsToGraphs <- function(
    ppt,
    statsText1,
    statsText2,
    fontSize = 14
){
  free_loc1 <- ph_location(
    left = 1.75, top = 5.7, 
    width = 4.7, height = .82)
  
  ppt <- ph_with(ppt, 
                 fpar(statsText1, fp_t = fp_text(font.size = fontSize)),
                 location = free_loc1 )
  
  free_loc2 <- ph_location(
    left = 7.5, top = 5.7, 
    width = 4.7, height = .82)
  
  ppt <- ph_with(ppt, 
                 fpar(statsText2, fp_t = fp_text(font.size = fontSize)),
                 location = free_loc2 )
  return(ppt)
}