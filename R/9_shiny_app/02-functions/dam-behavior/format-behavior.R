getZT_DateTime <- function (
  DOB,
  PND = 5, # for start of lights on cycle; so the early AM during lights out will be subsequent day
  ZT = 9, # 0-23; 0 is lights on, either 3a or 4a depending on DST
  PND_byZT0 = TRUE # Just add the ZT time from the time of lights on on the indicated PND
  # so ZT23, for example, will wind up being the next PND than indicated if TRUE
){
  if(PND>=0 && ZT >= 0 && ZT < 24){
    year <- get_year(DOB)
    month <- get_month(DOB)
    day <- get_day(DOB)
    DOB_dateTime <- date_time_build(year, month, day, hour = 3, zone = "America/New_York")
    ZT0_dateTime <- DOB_dateTime %>% 
      add_days(PND, nonexistent = "roll-forward")
    # print(paste("ZT_0", ZT0_dateTime))
    ZT0_dateTimeSys <- as_sys_time(ZT0_dateTime)
    info <- sys_time_info(ZT0_dateTimeSys, "America/New_York")
    isDST <- info$dst
    # print(paste("isDST", isDST))
    
    timeToAdd <- ifelse(isDST, 1, 0) # can't use if statement for row-wise DF calc
    hrToNextDay <- ifelse(isDST, 20, 21)
    
    ZT0_dateTime <- ZT0_dateTime %>%
      add_hours(timeToAdd)
    
    
    if(PND_byZT0){
      reqDate <- add_hours(ZT0_dateTime, ZT)
    } else {
      if(ZT < hrToNextDay){
        reqDate <- add_hours(ZT0_dateTime, ZT)
      } else {
        ZT <- ZT-24
        reqDate <- add_hours(ZT0_dateTime, ZT)
      }
    }
    
    return(reqDate)
  } else {
    print("Enter a valid PND and ZT time")
    return(NA)
  }
}

getZTDate <- function(dateTime){
  # dateOnly <- as_year_month_day(dateTime)
  dateOnly <- date_parse(dateTime)
  dateOnly
  return(dateOnly)
}

getZTHour <- function(dateTime){
  get_hour(dateTime)
}
