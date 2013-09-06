#'RMarf
#'
#'function to compute antecedent rainfall for Rainmaker event files or any file with a list of specified dates. 
#'Input files must have an as.POSIXct formatted date/time column. This format can be achieved by using the RMprep 
#'function. The name of the rainfall column can also be changed as desired using the RMprep function.
#'
#'@param df data frame containing ADAPS precip instantaneous data
#'@param date string name of date column in df
#'@param rain string name of column in df containing instantaneous rain values
#'@param df.events data frame containing storm event start and end datetimes
#'@param sdate string name of start date column in df.events 
#'@param days vector of times in days for summing antecedent rainfall
#'@param varnameout string prefix for resulting antecedent rainfall variable names
#'@return df.events data frame containing requested antecedent rainfall values for each storm
RMarf <- function(df, date="date", rain="rain",
                  df.events, sdate="StartDate", 
                  days=c(0.5,1,2,3,5,10,15,20),
                  varnameout="ARF") {
  
  arfdate <- "arfdate"
  
  #initialize varsum vector
  maxrows <- nrow(df.events)           #determine how many rows are in the dates data frame
  varsum=vector(length=maxrows)
  
  # compute the antecedent rain (ARF) for all identified durations
  for(j in 1:length(days)) {      
    df.events$arfdate <- df.events[,sdate] - days[j]*24*60*60
    
    # Compute ARF for all dates in the sample dates file
    for (i in 1:maxrows){
      subdata <- df[which(df[,date]>= df.events[i,arfdate]
                          & df[,date] < df.events[i,sdate]),]
      
      varsum[i] <- sum(subdata[,rain])
    }
    
    sumname <- paste(varnameout,days[j],sep="")
    
    df.events[,sumname] <- varsum
  }
  df.events <- df.events[,-which(names(df.events)==arfdate)]
  return(df.events)
}