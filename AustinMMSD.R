modelCoefs <- read.delim(file="MMSDmodelCoef.csv",stringsAsFactors=FALSE)
siteNo <- "04087088"
StartDt <- "2008-11-01"
EndDt <- "2014-12-31"
compQW <- 'Cl' # one of c('Cl','Fec','Ec','TSS','TP')

library(dataRetrieval)
library(USGSHydroTools)
library(GSqwsr)
dataReg1 <- modelCoefs[which(modelCoefs$type=='R' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
dataReg2 <- modelCoefs[which(modelCoefs$type=='Q' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
dataReg1[is.na(dataReg1)] <- 0
dataReg2[is.na(dataReg2)] <- 0
load("C:/Users/jlthomps/Documents/R/AustinDataUnderwood.RData")


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


if (min(adaps_disch_in$disch,na.rm=TRUE)<=0) {adaps_disch_in[which(adaps_disch_in$disch<=0),]$disch <- NA}
temp <- diff(adaps_disch_in$pdate,lag=1)
temp <- c(1,temp)

adaps_data_reg <- merge(adaps_cond_in,adaps_turb_in[,c(3,6)],by="pdate")
adaps_data_reg <- merge(adaps_data_reg,adaps_temp_in[,c(3,6)],by="pdate")
adaps_data_reg$compQWreg1 <- 10 ^ ((log10(adaps_data_reg$cond) * dataReg1$xLogCond) + (log10(adaps_data_reg$turb) * dataReg1$xLogTURB) + (adaps_data_reg$temp * dataReg1$xTemp) + dataReg1$b)
adaps_data_reg <- merge(adaps_data_reg,adaps_disch_in[,c(3,6)],by="pdate",all=TRUE)
adaps_data_reg$decYear <- getDecYear(adaps_data_reg$pdate)
adaps_data_reg$sinDY <- sin(adaps_data_reg$decYear*2*pi)
adaps_data_reg$cosDY <- cos(adaps_data_reg$decYear*2*pi)
adaps_data_reg$compQWreg2 <- exp(1) ^ ((log10(adaps_data_reg$disch) * dataReg2$xLogQ) + (adaps_data_reg$disch * dataReg2$xQ) + (adaps_data_reg$sinDY * dataReg2$sinDY) + (adaps_data_reg$cosDY * dataReg2$cosDY) + dataReg2$b)
adaps_data_reg$compQWreg <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWreg1,adaps_data_reg$compQWreg2)



adaps_data_regFit <- adaps_data_reg[!is.na(adaps_data_reg$disch),c(1,10)]
adaps_data_regFit$pdate2 <- as.numeric(adaps_data_regFit$pdate)
fit <- loess(disch ~ pdate2, adaps_data_regFit)
adaps_data_reg$dischint <- predict(fit,adaps_data_reg$pdate)

adaps_data_regInt <- adaps_data_reg[is.na(adaps_data_reg$disch),]


adaps_data_reg2 <- adaps_data_reg[is.na(adaps_data_reg$compQWreg1),]
temp <- diff(adaps_data_reg$pdate,lag=1)
temp <- c(1,temp)
adaps_data_reg$difftime <- temp
adaps_data_reg

smalldfQW <- adaps_data_reg[which(adaps_data_reg$pdate<=strptime("2012-10-31","%Y-%m-%d")),]
smalladaps_disch_in <- adaps_disch_in[which(adaps_disch_in$pdate<=strptime("2012-10-31","%Y-%m-%d")),]
smalldfQW <- smalldfQW[which(smalldfQW$pdate %in% smalladaps_disch_in$pdate),]
n <- nrow(smalldfQW)-1
smalldfQW <- smalldfQW[c(2:n),]
instLoad <- LoadInstantaneous(smalldfQW,Conc="logcompQWreg1",sample.time="pdate",Conc2liters=1,df.Q=smalladaps_disch_in,Q="disch",Q.time="pdate",Q2liters=28.3168466)


cl_check <- merge(smalldfQW[,c(1,5,13)],cl_data[,c(15,23)],by.x="pdate",by.y="startDateTime")
