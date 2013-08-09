
# Hydrovol
#
# Computes volumes and max discharge for hydrographs given the discharge time series and 
# the begin and end dates and times of the hydrographs
#

# dfQ, data frame with Q and time
# Q="Q", name of column in dfQ with Q
# time="pdate", name of column in dfQ with POSIXct time
# df.dates, data frame with begin and end dates/times in POSIXct format
# bdate="bpdate", begin date in POSIXct
# edate="epdate", end date in POSIXct
# volume="event.vol", name of resulting volume variable
# Qmax="Qmax", name of Qmax variable
# duration="Eduration", name of resulting duration variable

Hydrovol <- function(dfQ, Q="Q", time="pdate", df.dates, bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration"){
  
  # Compute volumes and max for each hydrograph defined in the df.dates dataframe
event.vol <- numeric()
event.max <- numeric()
  for (i in 1:nrow(df.dates)){
    
    #Determine rows with range of times from last time step before hydrograph to one time step
    #after hydrograph ends and subset that time series
    begin.row <- max(which(dfQ[,time]<df.dates[i,bdate]))
    end.row <- min(which(dfQ[,time]>df.dates[i,edate]))
    subdfQ <- dfQ[begin.row:end.row,]
    
    if(nrow(subdfQ)<3) {
      event.vol[i] <- NA
      event.max[i] <- NA
      next
    }
    
    #Compute volumes for individual hydrographs
    volume <- numeric()
    
    for (j in 2:(nrow(subdfQ)-1)){
      
      if(j==2){
            #Define Q based on position between first two time steps
            diffQ <- subdfQ[j,Q]-subdfQ[(j-1),Q]
            frac <- as.numeric(difftime(subdfQ[(j),time],df.dates[i,bdate],units="secs"))/as.numeric(difftime(subdfQ[(j),time],subdfQ[(j-1),time],units="secs"))
            Q1 <- subdfQ[(j),Q] - diffQ*frac
            volume[j-1] <- mean(c(Q1,subdfQ[j,Q]))*as.numeric(difftime(subdfQ[j,time],df.dates[i,bdate],units="secs"))
            volume[j] <- subdfQ[j,Q]*as.numeric(difftime(subdfQ[(j+1),time],subdfQ[(j),time],units="secs"))/2
      }else{
        if(j==(nrow(subdfQ)-1)){
### COULD SPEED THIS UP BY VECTORIZING THE MIDDLE OF THE HYDROGRAPH COMPUTATION ###          
          #need to add in logic for end of hydrograph
          Q1 <- subdfQ[j,Q] - (subdfQ[j,Q]-subdfQ[(j-1),Q])/4
          diffQ <- subdfQ[j+1,Q]-subdfQ[(j),Q]
          frac <- as.numeric(difftime(subdfQ[(j+1),time],df.dates[i,edate],units="secs"))/as.numeric(difftime(subdfQ[(j+1),time],subdfQ[(j),time],units="secs"))
          Q2 <- subdfQ[(j+1),Q] - diffQ*frac
          volume[j] <- Q1*as.numeric(difftime(subdfQ[(j),time],subdfQ[(j-1),time],units="secs"))/2
          volume[j+1] <- mean(c(Q2,subdfQ[j,Q]))*as.numeric(difftime(df.dates[i,edate],subdfQ[j,time],units="secs"))
        }else{
          #Compute volume for all but the first and last time steps
          volume[j] <- subdfQ[j,Q]*as.numeric(difftime(subdfQ[(j+1),time],subdfQ[(j-1),time],units="secs"))/2
        }
        }
      }
    event.vol[i] <- sum(volume)
    event.max[i] <- max(subdfQ[,Q])
  }

Eduration <- as.numeric(difftime(df.dates[,edate],df.dates[,bdate],units="hours"))
df.dates2 <- cbind(df.dates,data.frame(event.vol=event.vol,Qmax=event.max,duration=Eduration))
return(df.dates2)
}

# 
#     
#     plot(subdfQ[1:5,time],subdfQ[1:5,Q])
#     lines(subdfQ[1:5,time],subdfQ[1:5,Q])
#     points(df.dates[2,1],Q1,pch=3)
#   
#   points(mean(Q1,subdfQ[j,Q])
# 
#   plot(subdfQ[(nrow(subdfQ)-5):nrow(subdfQ),time],subdfQ[(nrow(subdfQ)-5):nrow(subdfQ),Q])
#   lines(subdfQ[(nrow(subdfQ)-5):nrow(subdfQ),time],subdfQ[(nrow(subdfQ)-5):nrow(subdfQ),Q])
#   points(subdfQ[(nrow(subdfQ)-1),time]-difftime(subdfQ[(nrow(subdfQ)-1),time],subdfQ[(nrow(subdfQ)-2),time])/4 ,Q1,pch=4)
#          points(df.dates[2,2],Q2,pch=3)
#          
#   points(mean(Q2,subdfQ[j,Q])
