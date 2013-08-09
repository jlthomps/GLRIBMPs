##########################################################################################
## Function RMevents

#Compute rainfall event variables based on time series of rain data with only one rain
#gage or one mean radar rain column.
#
# Usage: RMeventsSamples  (df,             # Data frame with rainfall
#                  ieHr=6,          # Interevent period in hours
#                  rainthresh=5.1   # minimum event depth in units of the rain column
#                                   # default is given as 5.1 assuming millimeters (0.2")
#                  rain="rain",     # column of rainfall unit values
#                  time="pdate"     # column with POSIX date
#                  ) 


###########

RMeventsSamples <- function(df,
                            ieHr=6,
                            rain="rain",
                            time="pdate",
                            dfsamples,
                            bdate="bpdate",
                            edate="epdate"){
  
#   df.orig <- read.delim("\\\\igsarmewfsapa/projects/QW Monitoring Team/MMSD/Viruses/Menomonee Viruses/Data compilation/EnDDat precip/MenoFalls_Final.txt")
#   df <- dfRain
#   ieHr <- 6
#   rain <- "rain"
#   time <- "pdate"
#   dfsamples <- dfsamples4
#   bdate <- "Hbpdate"
#   edate <- "Hepdate"
  
  df.orig <- df
  df <- rbind(df[1,],subset(df[-1,],rain>0.0))
  timediff <- difftime(df[2:(nrow(df)),time],df[1:(nrow(df)-1),time],units="secs")
  df$timediff <- c(NA,timediff)
#  dfsamples$Braindate <- dfsamples$bpdate
#  dfsamples$Eraindate <- dfsamples$epdate
  
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  
  for (i in 1:nrow(dfsamples)){

    dtime <- difftime(df[,time],dfsamples[i,bdate],units="secs")
       
    #Look for the beginning and ending dates of the event
    startevent <- which(dtime==max(dtime[dtime<0]))
    startrain <- max(which(df[1:startevent,"timediff"]>=ieSec))
#    beforeevent <- max(which(df[1:startevent,"timediff"]<ieSec))
    endrain <- max(which(df[,time] < dfsamples[i,edate]))
    if(difftime(dfsamples[i,bdate],df[startevent,time],units="secs") > ieSec){
      if(startevent==endrain) {
        BD <- dfsamples[i,bdate]
        ED <- dfsamples[i,edate]
        
      } else {
        ED <- df[endrain,time]
        BD <- df[min(which(df[,time]>dfsamples[i,bdate])),time]
        }
      }else { 
      BD <- df[startrain,time]
      ED <- df[endrain,time]
    }
    if(i>1){  
      Braindate <- c(Braindate,BD)
      Eraindate <- c(Eraindate,ED)
    }else {
      Braindate <- BD
      Eraindate <- ED
    }
    }
  dfsamples$Braindate <- Braindate
  dfsamples$Eraindate <- Eraindate
  #Define data from the original rain file that match the event
  BrainIndex <- numeric()
  ErainIndex <- numeric()  
  for (i in 1:length(Braindate)) {
    dtime <- difftime(df.orig[,time],dfsamples$Braindate[i],units="secs")
    startevent <- which(dtime==max(dtime[dtime<0],na.rm=T))
    BrainIndex[i] <- startevent
    dtime <- difftime(df.orig[,time],dfsamples$Eraindate[i],units="secs")
    endevent <- which(dtime==max(dtime[dtime<=0],na.rm=T))
    ErainIndex[i] <- endevent
  }
  
  dfsamples$Braindate <- df.orig[BrainIndex,time]
  dfsamples$Eraindate <- df.orig[ErainIndex,time]

  #Compute event depth
  depth <- numeric()
#  duration <- numeric()
#  intensity <- numeric()
  for (i in 1:nrow(dfsamples)){
    subdf <- subset(df,df[,time]>=dfsamples$Braindate[i] & df[,time]<=dfsamples$Eraindate[i])
    depth[i] <- sum(subdf[,rain])
#    duration[i] <- as.numeric(difftime(dfsamples$Eraindate[i],dfsamples$Braindate[i],units="hours"))
#    intensity[i] <- depth[i]/duration[i]
  }
  dfsamples$depth <- depth
  return(dfsamples)
}

##########################################################################################