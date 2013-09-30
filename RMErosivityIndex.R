#'RMErosivityIndex
#'
#'function to compute the erosivity index for a storm event in inches squared per minute
#'
#'@param df unit value precip file
#'@param storm_rainmaker data frame containing start and end datetimes for storms
#'@return storm_rainmaker data frame containing storm start and end datetimes and requested intensities
RMErosivityIndex <- function(df,storm_rainmaker) 
  {
  rain_diff <- diff(df$rain,lag=1)
  time_diff <- diff(df$pdate,lag=1,units="mins")
  pdate_diff <- df$pdate[2:nrow(df)]
  intensity_df <- data.frame(rain_diff,time_diff,pdate_diff)
  intensity_df$rain_diff <- ifelse(intensity_df$rain_diff<0,0,intensity_df$rain_diff)
  intensity_df$intensity <- (intensity_df$rain_diff*60)/as.numeric(intensity_df$time_diff)
  intensity_df$energy <- ifelse(intensity_df$intensity>3,intensity_df$rain_diff*1074,ifelse(intensity_df$intensity>0,intensity_df$rain_diff*(916+(331*abs(log10(intensity_df$intensity)))),NA))
  intensity_df <- intensity_df[which(intensity_df$energy>0),]
  norows <- nrow(storm_rainmaker)
  noreps <- nrow(intensity_df)
  intensity_df$stormnum <- -9
  intensity_all <- data.frame(df$pdate)
  intensity_all$stormnum <- -9
  intensity_all$energy <- -9
  intensity_all$pdate_diff <- difftime(df$pdate,Sys.time(),"hours")
  k <- 0
  for (j in 1:norows) {
    intensity_df$diff <- difftime(intensity_df$pdate_diff,storm_rainmaker$StartDate[j],units="hours")
    intensity_sub <- intensity_df[which(abs(as.numeric(intensity_df$diff))<36),]
    noreps <- nrow(intensity_sub)
    for (i in 1:noreps) {
      intensity_sub$stormnum[i] <- ifelse(as.numeric(intensity_sub$pdate_diff[i]-storm_rainmaker$StartDate[j])*as.numeric(storm_rainmaker$EndDate[j]-intensity_sub$pdate_diff[i])>=0,storm_rainmaker$stormnum[j],intensity_sub$stormnum[i])
      intensity_all$stormnum[k+j+i] <- intensity_sub$stormnum[i]
      intensity_all$pdate_diff[k+j+i] <- intensity_sub$pdate_diff[i]
      intensity_all$energy[k+j+i] <- intensity_sub$energy[i]
      intensity_all$df.pdate[k+j+i] <- intensity_sub$pdate[i]
    }
    k <- k+i
  }
  
  intensity_agg <- aggregate(intensity_all$energy,list(intensity_all$stormnum),sum)
  storm_rainmaker <- merge(storm_rainmaker,intensity_agg,by.x="stormnum",by.y="Group.1",all.x=TRUE)
  storm_rainmaker[which(storm_rainmaker$I30<=2.5),]$x <- storm_rainmaker[which(storm_rainmaker$I30<2.5),]$I30*storm_rainmaker[which(storm_rainmaker$I30<2.5),]$x*.01
  storm_rainmaker$x <- ifelse(storm_rainmaker$I30<=0,NA,storm_rainmaker$x)
  storm_rainmaker$x <- ifelse(storm_rainmaker$I30>2.5,storm_rainmaker$x*2.5*.01,storm_rainmaker$x)
  colnames(storm_rainmaker)[colnames(storm_rainmaker)=="x"] <- "EI"
  
  return(storm_rainmaker)
}