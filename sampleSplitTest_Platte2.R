library(dataRetrieval)
library(googleVis)
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/labDataOut_test.R")
source("M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/getADAPSData_test.R")
# enter NWIS station id for gaging station
siteNo <- "424314090240601"
# enter date to begin pulling data (rounded to the day)
StartDt <- '2008-05-30'
# enter date to stop pulling data (rounded to the day)
EndDt <- '2008-06-15'
# enter the name of the storm(s) (for plot title)
storm_name <- c("S2-066","S2-067","S2-068","S2-069","S2-070","S2-071","S2-072","S2-073","S2-074","S2-075")
# enter path and name of data file if data is not web-available
dataFile <- "M:/NonPoint Evaluation/GLRI Edge-of-field/Splitting Record Conversion to R/PLAT2TEST.RDB"

# Retrieve data from NWISWeb (if available), or use file names to pull data in from files exported by ADAPS
adaps_data_all <- getADAPSData_test(siteNo,StartDt,EndDt,dataFile)
# example using files getADAPSData(siteNo,StartDt,EndDt,precipSite,stageFile="jf3stage.txt",dischFile="jf3disch.txt","jf3precip.txt","jf3scod.txt")

# save merged data for station/storm event, saved as file, eg 434425090462401data.txt 
tableOut <- adaps_data_all[,c("agency_cd","site_no","datetime","X01_00065","X02_00060","X05_99909")]
fileName <- paste(siteNo,"data.csv",sep="")
sink(fileName)
cat("Station:"," ",siteNo,"\t","Start date:"," ",strftime(StartDt),"\t","End date:"," ",strftime(EndDt),"\n\n")
write.table(tableOut,file="",sep=",",row.names=FALSE)
sink()

# enter the maximum possible volume for one sample bottle
maxBottleVol <- c(400,600,600,600,600,600,600,400,600,800)
# enter the maximum possible volume for one full storm sample
maxSampVol <- c(3900,3900,3900,3900,3900,3900,3900,3900,3900,3900)
# enter Storm Start date(s)
StormStart <- c(strptime("2008-05-30 02:49","%Y-%m-%d %H:%M"),strptime("2008-06-05 04:39","%Y-%m-%d %H:%M"),
                strptime("2008-06-06 04:22","%Y-%m-%d %H:%M"),strptime("2008-06-07 22:52","%Y-%m-%d %H:%M"),
                strptime("2008-06-08 08:41","%Y-%m-%d %H:%M"),strptime("2008-06-08 19:03","%Y-%m-%d %H:%M"),
                strptime("2008-06-12 09:03","%Y-%m-%d %H:%M"),strptime("2008-06-12 21:40","%Y-%m-%d %H:%M"),
                strptime("2008-06-14 16:52","%Y-%m-%d %H:%M"),strptime("2008-06-15 04:07","%Y-%m-%d %H:%M"))
#StormStart <- c(strptime("2013-10-03 15:18","%Y-%m-%d %H:%M"),strptime("2013-10-05 2:30","%Y-%m-%d %H:%M"))
# enter Storm End date(s) 
StormEnd <- c(strptime("2008-05-30 08:49","%Y-%m-%d %H:%M"),strptime("2008-06-05 07:21","%Y-%m-%d %H:%M"),
                 strptime("2008-06-06 05:28","%Y-%m-%d %H:%M"),strptime("2008-06-08 01:14","%Y-%m-%d %H:%M"),
                 strptime("2008-06-08 11:39","%Y-%m-%d %H:%M"),strptime("2008-06-08 21:31","%Y-%m-%d %H:%M"),
                 strptime("2008-06-12 10:22","%Y-%m-%d %H:%M"),strptime("2008-06-13 01:36","%Y-%m-%d %H:%M"),
                 strptime("2008-06-14 18:05","%Y-%m-%d %H:%M"),strptime("2008-06-15 09:22","%Y-%m-%d %H:%M"))
#StormEnd <- c(strptime("2013-10-03 21:15","%Y-%m-%d %H:%M"),strptime("2013-10-05 11:30","%Y-%m-%d %H:%M"))
# enter Storm Name(s)
StormName <- c("S2-066","S2-067","S2-068","S2-069","S2-070","S2-071","S2-072","S2-073","S2-074","S2-075")
#StormName <- c("JF6.38","JF6.39")
# enter number for 1st bottle of each storm, if a number other than 1 is desired
subNum <- c(1,1,1,1,16,1,1,5,1,7)

# generate bottle volume table(s) for lab for each storm
tableOut <- labDataOut_test(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,subNum=subNum)
# look at table(s) generated for lab sample instructions for storm event(s). determine if changes are needed
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
bottlePickup <- c("2008-06-15")
removeDate <- c(strptime("2008-05-30 07:44:00","%Y-%m-%d %H:%M")) #"%Y-%m-%d %H:%M"))
removeComment <- c("")
tableOut <- labDataOut_test(adaps_data_all,StormStart,StormEnd,StormName,maxBottleVol,maxSampVol,removeDate=removeDate,subNum=subNum)
for (i in 1:length(StormStart)){
  print(tableOut[[i]])
}
# generate text file with storm event sample bottle volume table(s)
fileName <- paste(storm_name[1],"sampVol",".txt",sep="")
sink(fileName)
for (i in 1:length(storm_name)) {
  cat(StormName[i],"\t",strftime(StormStart[i]),"\t",strftime(StormEnd[i]),"\n\n")
  print(tableOut[[i]],row.names=FALSE)
  cat("\n\n")
  cat("Lab Sample Volume","\t",sum(tableOut[[i]]$mL),"mL\t",sum(tableOut[[i]]$perc),"percent\n\n")
  cat("Max Bottle Volume","\t",maxBottleVol[i],"mL\n\n")
  cat("Max Optimized Bottle Volue","\t",max(tableOut[[i]]$mL),"mL\n\n")
  cat("Max Sample Runoff Volume","\t",max(tableOut[[i]]$volume),"cubic feet\n\n")
  cat("Total Sampled Storm Volume","\t",sum(tableOut[[i]]$volume),"cubic feet\n\n")
  cat("Bottles ",tableOut[[i]]$subNum[1]," through ",tableOut[[i]]$subNum[length(tableOut[[i]]$subNum)]," picked up ",bottlePickup,"\n\n")
  if (length(removeComment[i])>0) {cat(removeComment[i],"\n\n")}
  cat("========================================================================================================","\n\n")
}
sink()

