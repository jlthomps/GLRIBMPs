modelCoefs <- read.delim(file="MMSDmodelCoef.csv",stringsAsFactors=FALSE)
siteNo <- "04087119"
StartDt <- "2012-10-01"
EndDt <- "2014-09-01"
compQW <- 'Cl' # one of c('Cl','Fec','Ec','TSS','TP')

library(dataRetrieval)
library(USGSHydroTools)
dataReq <- modelCoefs[which(modelCoefs$type=='R' & modelCoefs$y==compQW),]
dataDown <- ""
if (!is.na(dataReq$xLogTURB)) {dataDown <- paste(dataDown,"Turb",sep=",")}
if (!is.na(dataReq$xLogCond)) {dataDown <- paste(dataDown,"Cond",sep=",")}
if (!is.na(dataReq$xTemp)) {dataDown <- paste(dataDown,"Temp",sep=",")}
if (sum(grep("Turb",dataDown),0)==1) {
  adaps_turb_in <- retrieveNWISunitData(siteNo,'63680',StartDt,EndDt,format="xml")
  colnames(adaps_turb_in) <- c("agency","siteNo","pdate","tz_cd","turb","rmrk")
}
if (sum(grep("Cond",dataDown),0)==1) {
  adaps_cond_in <- retrieveNWISunitData(siteNo,'00095',StartDt,EndDt,format="xml")
  colnames(adaps_cond_in) <- c("agency","siteNo","pdate","tz_cd","cond","rmrk")
}
if (sum(grep("Temp",dataDown),0)==1) {
  adaps_temp_in <- retrieveNWISunitData(siteNo,'00010',StartDt,EndDt,format="xml")
  colnames(adaps_temp_in) <- c("agency","siteNo","pdate","tz_cd","temp","rmrk")
}
adaps_disch_in <- retrieveNWISunitData(siteNo,'00060',StartDt,EndDt,format="xml")
colnames(adaps_disch_in) <- c("agency","siteNo","pdate","tz_cd","disch","rmrk")

# to calculate daily load
# user will enter type (daily, monthly, annual), site, start and end date, qw pcode
# return calculated value, regression type used for calculation, regression eq? plot? other?
# is data available? check for min and max dates, as well as count. how many can be missing?
# if available, use regular regression to calculate load, output load and type=regular
# if not available, or too much missing, check flow. if available, use flow regression to calculate load, output load and type=flow
# output R-squared?
# can I pull these regressions from somewhere?
# special case, TP flow-only regression is too terrible to use, so can't calculate if other data isn't available
# issue - have to adjust datasets b/c may have more frequent continuous QW data than discharge data, and hydrovol can't calculate
# correctly if this is so. also, need discharge data set to start before and end after continuous QW for LoadInstantaneous
# issue - LoadInstantaneous (and HydroVol) are hella slow with large data sets. Try rewriting with dplyr
# couldn't reproduce regressions, I think b/c differing data sets, not all on nwisweb and maybe some removed?
if ("Turb" %in% dataDown) {
  if nrow()
}
difftime(strptime(EndDt,format="%Y-%m-%d"),strptime(StartDt,format="%Y-%m-%d"))
logcompQW <- dataReq$b + (dataReq$xLogCond * log10(adaps_cond_in$cond))
dfQW <- cbind(adaps_cond_in[,c(1:4)],10^logcompQW)
colnames(dfQW) <- c("agency","siteNo","pdate","tz_cd","logcompQW")
smalldfQW <- dfQW[which(dfQW$pdate<=strptime("2012-10-31","%Y-%m-%d")),]
smalladaps_disch_in <- adaps_disch_in[which(adaps_disch_in$pdate<=strptime("2012-10-31","%Y-%m-%d")),]
smalldfQW <- smalldfQW[which(smalldfQW$pdate %in% smalladaps_disch_in$pdate),]
n <- nrow(smalldfQW)-1
smalldfQW <- smalldfQW[c(2:n),]
instLoad <- LoadInstantaneous(smalldfQW,Conc="logcompQW",sample.time="pdate",Conc2liters=1,df.Q=smalladaps_disch_in,Q="disch",Q.time="pdate",Q2liters=28.3168466)

