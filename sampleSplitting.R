#install.packages("googleVis")
#install.packages("dataRetrieval",repos="http://usgs-r.github.com",type="source")

library(dataRetrieval)
library(googleVis)
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/hydrographPDF.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/hydrographInteractive.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/labDataOut.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/getADAPSData.R")
# enter NWIS station id for gaging station
siteNo <- "434034088252401"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2013-11-17'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2013-11-19'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "434034088252401"
# enter the name of the storm(s) (for plot title)
storm_name <- "JF5-28"

# Retrieve data from NWISWeb (if available), or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite)
# example using files getADAPSData(siteNo,StartDt,EndDt,precipSite,stageFile="jf3stage.txt",dischFile="jf3disch.txt","jf3precip.txt","jf3scod.txt")

# Generate interactive googleVis plot
hydrographPlot <- hydrographInteractive(adaps_data_all)
plot(hydrographPlot)

# Generate pdf of hydrograph to save, saved as file, eg 434425090462401hydrograph.pdf 
hydrographPDF(adaps_data_all,storm_name)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
tableOut <- adaps_data_all[,c("agency_cd","site_no","datetime","X01_00065","X02_00060","X05_99909","X04_00045")]
fileName <- paste(siteNo,"data.txt",sep="")
sink(fileName)
cat(siteNo,"\t",strftime(StartDt),"\t",strftime(EndDt),"\n\n")
print(tableOut,row.names=FALSE)
sink()

# enter the maximum possible volume for one sample bottle
maxBottleVol <- 900
# enter the maximum possible volume for one full storm sample
maxSampVol <- 3900
# enter Storm Start date(s)
StormStart <- c(strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
#StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
# enter Storm End date(s) 
StormEnd <- c(strptime("2013-10-05 04:45","%Y-%m-%d %H:%M"))
#StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
# enter Storm Name(s)
StormName <- c("JF5.28")
#StormName <- c("JF6.38","JF6.39")
# enter number for 1st bottle of each storm, if a number other than 1 is desired
#subNum <- c(1,1)

# generate bottle volume table(s) for lab for each storm
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol)
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
print(tableOut)

#if a sample needs to be removed, enter the datetime(s) of the sample(s)
removeDate <- c(strptime("2013-10-05 03:00","%Y-%m-%d %H:%M"))
removeComment <- ""
#removeComment <- c("","Ignore bottle JF6-2, broken in shipment")
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate)
print(tableOut)

#Once you are satisfied with the table output
#enter date(s) when samples were picked up 
bottlePickup <- c("2013-10-07")

# generate text file with storm event sample bottle volume table(s)
fileName <- paste(storm_name,"sampVol",".txt",sep="")
stormNum <- length(StormName)
sink(fileName)
for (i in 1:stormNum) {
  cat(StormName[i],"\t",strftime(StormStart[i]),"\t",strftime(StormEnd[i]),"\n\n")
  print(tableOut[[i]],row.names=FALSE)
  cat("\n\n")
  cat("Lab Sample Volume","\t",sum(tableOut[[i]]$mL),"\t",sum(tableOut[[i]]$perc),"\n\n")
  cat("Max Bottle Volume","\t",maxBottleVol,"\n\n")
  cat("Max Sample Runoff Volume","\t",max(tableOut[[i]]$volume),"\n\n")
  cat("Total Sampled Storm Volume","\t",sum(tableOut[[i]]$volume),"\n\n")
  cat("Bottles ",tableOut[[i]]$subNum[1]," through ",tableOut[[i]]$subNum[length(tableOut[[i]]$subNum)]," picked up ",bottlePickup[i],"\n\n")
  if (length(removeComment[i])>0) {cat(removeComment[i],"\n\n")}
  cat("========================================================================================================","\n\n")
}
sink()
