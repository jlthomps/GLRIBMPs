#'RMIntense
#'
#'function to compute maximum x-minute rainfall intensities in units of depth/hr
#'
#'@param df unit value precip file
#'@param df.events data frame containing start and end datetimes for storms
#'@param date name of date column in df as POSIX
#'@param rain name of rain column in df with precip unit values
#'@param sdate name of column containing start date in df.events rain file as POSIX
#'@param edate name of column containing end date in df.events rain file as POSIX
#'@param depth name of column in df.event containing rain depth
#'@param xmin string list of requested x-minute max rainfall intensities
#'@return df.events data frame containing storm start and end datetimes and requested intensities
RMIntense <- function(df,date="r.date",rain = "rain",
                      df.events,sdate="StartDate",edate="EndDate",
                      depth="depth",
                      xmin=c(60,180,360)) {
  
  # Compute overall event intensity
  df.events$duration <- (as.numeric(difftime(df.events[,edate],df.events[,sdate],units="hours")))
  df.events$Ievent <- df.events[,depth]/df.events$duration
  
  # Determine x-minute intensities for each of the intensities specified  
  
  for (i in 1:length(xmin)){
    x <- xmin[i]*60
    intensity.var <- paste("I",xmin[i],sep="")
    df.events[,intensity.var] <- NA
    
    #   Isolate individual events and Compute max x-min intensity for each event 
    #   period: compute sum rain and divide by duration. Report x-min intensity 
    #   in units/hr
    
    for (j in 1:nrow(df.events)) {
      subdf <- subset(df,df[,date] >= df.events[j,sdate] & df[,date] <= df.events[j,edate])
      # Initialize intensity vector
      intensity <- numeric(length=nrow(subdf))
      
      for (k in 1:nrow(subdf)){
        enddate <- subdf[k,date]+x
        bdate <- subdf[k,date]
        
        subdf2 <- subset(subdf,subdf[,date] >= bdate & subdf[,date] < enddate)
        intensity[k] <- sum(subdf2[,rain])/(x/60/60)
        
        #      k;bdate;enddate;intensity[k];max(subdf2$rain)
      }
      df.events[j,intensity.var] <- max(intensity,na.rm=TRUE)
    }
  }
  
  return(df.events)
}