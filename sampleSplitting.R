#install.packages("googleVis")
#install.packages("dataRetrieval",repos="http://usgs-r.github.com",type="source")

library(dataRetrieval)
library(googleVis)
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/hydrographPDF.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/hydrographInteractive.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/labDataOut.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/getADAPSData.R")
# enter NWIS station id for gaging station
siteNo <- "441624088045601"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2012-07-26'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2012-07-26'
# enter NWIS station id for precipitation gaging station, may or may not be identical to "siteNo"
precipSite <- "441624088045601"
# enter the name of the storm(s) (for plot title)
storm_name <- c("ESW1-26","ESW1-27")
# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/GLRIWATERWAY1TEST.RDB"

# Retrieve data from NWISWeb (if available), or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite,dataFile)
# example using files getADAPSData(siteNo,StartDt,EndDt,precipSite,stageFile="jf3stage.txt",dischFile="jf3disch.txt","jf3precip.txt","jf3scod.txt")

# Generate interactive googleVis plot
hydrographPlot <- hydrographInteractive(adaps_data_all)
plot(hydrographPlot)

# Generate pdf of hydrograph to save, saved as file, eg 434425090462401hydrograph.pdf 
hydrographPDF(adaps_data_all,storm_name)

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
tableOut <- adaps_data_all[,c("agency_cd","site_no","datetime","X01_00065","X02_00060","X05_99909","X04_00045")]
fileName <- paste(siteNo,"data.csv",sep="")
sink(fileName)
cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
write.table(tableOut,file="",sep=",",row.names=FALSE)
sink()

# enter the maximum possible volume for one sample bottle
maxBottleVol <- c(900,800)
# enter the maximum possible volume for one full storm sample
maxSampVol <- c(3900,2000)
# enter Storm Start date(s)
StormStart <- c(strptime("2012-07-26 03:35","%Y-%m-%d %H:%M"),strptime("2012-07-26 14:10","%Y-%m-%d %H:%M"))
#StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
# enter Storm End date(s) 
StormEnd <- c(strptime("2012-07-26 05:45","%Y-%m-%d %H:%M"),strptime("2012-07-26 15:32","%Y-%m-%d %H:%M"))
#StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
# enter Storm Name(s)
StormName <- c("ESW1.26","ESW1.27")
#StormName <- c("JF6.38","JF6.39")
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(1,4)

# generate bottle volume table(s) for lab for each storm
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,subNum=subNum)
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}

#if a sample needs to be removed, enter the datetime(s) of the sample(s)
removeDate <- c(strptime("2012-07-26 04:52","%Y-%m-%d %H:%M"))
removeComment <- c("test")
#removeComment <- c("","Ignore bottle JF6-2, broken in shipment")
tableOut <- labDataOut(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate=removeDate,subNum=subNum)
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}

#Output csv file of all intermediate volumes used for calculations
fileName <- paste(siteNo,"SampleVols.csv",sep="")
sink(fileName)
cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
write.table(tableOut[[length(StormStart)+1]],file="",sep=",",row.names=FALSE)
sink()

#Once you are satisfied with the table output
#enter date(s) when samples were picked up 
bottlePickup <- c("2012-07-26")

# generate text file with storm event sample bottle volume table(s)
fileName <- paste(storm_name[1],"sampVol",".txt",sep="")
sink(fileName)
for (i in 1:length(storm_name)) {
  cat(StormName[i],"\t",strftime(StormStart[i]),"\t",strftime(StormEnd[i]),"\n\n")
  print(tableOut[[i]],row.names=FALSE)
  cat("\n\n")
  cat("Lab Sample Volume","\t",sum(tableOut[[i]]$mL),"mL\t",sum(tableOut[[i]]$perc),"percent\n\n")
  cat("Max Bottle Volume","\t",maxBottleVol[i],"mL\n\n")
  cat("Max Sample Runoff Volume","\t",max(tableOut[[i]]$volume),"cubic feet\n\n")
  cat("Total Sampled Storm Volume","\t",sum(tableOut[[i]]$volume),"cubic feet\n\n")
  cat("Bottles ",tableOut[[i]]$subNum[1]," through ",tableOut[[i]]$subNum[length(tableOut[[i]]$subNum)]," picked up ",bottlePickup,"\n\n")
  if (length(removeComment[i])>0) {cat(removeComment[i],"\n\n")}
  cat("========================================================================================================","\n\n")
}
sink()
