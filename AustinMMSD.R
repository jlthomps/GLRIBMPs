#Load model coefficients from file
#set site and parameter to choose correct model coefficients
modelCoefs <- read.delim(file="C:/Users/jlthomps/Desktop/git/GLRIBMPs/MMSDmodelCoef.csv",stringsAsFactors=FALSE)
siteNo <- "04087088"
StartDt <- "2008-11-01"
EndDt <- "2014-12-31"
compQW <- 'Cl' # one of c('Cl','Fec','Ec','TSS','TP')

library(dataRetrieval)
library(USGSHydroTools)
dataReg1 <- modelCoefs[which(modelCoefs$type=='R' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
dataReg2 <- modelCoefs[which(modelCoefs$type=='Q' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
dataReg1[is.na(dataReg1)] <- 0
dataReg2[is.na(dataReg2)] <- 0

#load previously saved continuous and merged data for appropriate site
load("C:/Users/jlthomps/Desktop/git/GLRIBMPs/AustinDataUnderwood.RData")


# to calculate daily load
# user will enter type (daily, monthly, annual), site, start and end date, qw pcode
# return calculated value, regression type used for calculation, regression eq? plot? other?
# is data available? check for min and max dates, as well as count. how many can be missing?
# if available, use regular regression to calculate load, output load and type=regular
# if not available, or too much missing, check flow. if available, use flow regression to calculate load, output load and type=flow
# output R-squared?


#replace any negative discharge values with 'NA'
if (min(adaps_disch_in$disch,na.rm=TRUE)<=0) {adaps_disch_in[which(adaps_disch_in$disch<=0),]$disch <- NA}
#merge continuous data and calculate compQW based on original regression model
adaps_data_reg <- merge(adaps_cond_in,adaps_turb_in[,c(3,6)],by="pdate",all=TRUE)
adaps_data_reg <- merge(adaps_data_reg,adaps_temp_in[,c(3,6)],by="pdate",all=TRUE)
adaps_data_reg$compQWreg1 <- 10 ^ ((log10(adaps_data_reg$cond) * dataReg1$xLogCond) + (log10(adaps_data_reg$turb) * dataReg1$xLogTURB) + (adaps_data_reg$temp * dataReg1$xTemp) + dataReg1$b)
#merge continuous data frame with discharge data 
adaps_data_reg <- merge(adaps_data_reg,adaps_disch_in[,c(3,6)],by="pdate",all=TRUE)
#interpolate discharge data where missing
library(GSqwsr)
adaps_data_reg$decYear <- getDecYear(adaps_data_reg$pdate)

mindecYear <- 2010
maxdecYear <- 2015
subNum <- (maxdecYear-mindecYear)*4
adaps_data_reg$dischint <- NA
for (i in 1:subNum) {
  startDY <- mindecYear + (i-1)*0.25
  endDY <- startDY + 0.25
  adaps_data_reg_sub <- adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]
  adaps_data_Int <- adaps_data_reg_sub[!is.na(adaps_data_reg_sub$disch),c(1,10)]
  adaps_data_Int$pdate2 <- as.numeric(adaps_data_Int$pdate)
  fit <- loess(disch ~ pdate2, adaps_data_Int)
  adaps_data_reg_sub$dischint <- predict(fit,adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]$pdate)
  adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]$dischint <- adaps_data_reg_sub$dischint
  #adaps_data_reg <- merge(adaps_data_reg,adaps_data_reg_sub[,c(1,12)],by="pdate",all=TRUE)
}
if (min(adaps_data_reg$dischint,na.rm=TRUE)<=0) {adaps_data_reg[which(adaps_data_reg$dischint<=0),]$dischint <- NA}
adaps_data_reg$dischComb <- ifelse(!is.na(adaps_data_reg$disch),adaps_data_reg$disch,adaps_data_reg$dischint)

#calculate compQW based on Q-only regression
adaps_data_reg$sinDY <- sin(adaps_data_reg$decYear*2*pi)
adaps_data_reg$cosDY <- cos(adaps_data_reg$decYear*2*pi)
adaps_data_reg$compQWreg2 <- exp(1) ^ ((log10(adaps_data_reg$dischComb) * dataReg2$xLogQ) + (adaps_data_reg$dischComb * dataReg2$xQ) + (adaps_data_reg$sinDY * dataReg2$sinDY) + (adaps_data_reg$cosDY * dataReg2$cosDY) + dataReg2$b)
#create composite compQW based on original model when continuous data is available, then Q-only model
adaps_data_reg$compQWreg <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWreg1,adaps_data_reg$compQWreg2)

#check to see if any discharge gaps are greater than 1 day
adaps_data_dischInt <- adaps_data_reg[!is.na(adaps_data_reg$disch),]
temp <- diff(adaps_data_dischInt$pdate,lag=1)
adaps_data_dischInt$diff <- c(1,diff(adaps_data_dischInt$pdate,lag=1))
adaps_data_dischInt_sub <- adaps_data_dischInt[which(adaps_data_dischInt$diff>(60*60*24)),]

#calculate instantaneous load flux
adaps_data_reg$dischCombL <- adaps_data_reg$dischComb*28.3168466
adaps_data_reg$loadFlux <- adaps_data_reg$dischCombL * adaps_data_reg$compQWreg

#roll up to daily and check for gaps
adaps_data_regLoad <- adaps_data_reg[!is.na(adaps_data_reg$loadFlux),c(1,19)]
temp <- diff(adaps_data_regLoad$pdate,lag=1)
adaps_data_regLoad$date <- as.Date(adaps_data_regLoad$pdate)
dailyFlux <- aggregate(loadFlux ~ date,data=adaps_data_regLoad,length)

smalldfQW <- adaps_data_reg[which(adaps_data_reg$pdate<=strptime("2011-10-31","%Y-%m-%d")),]
smalldfQW <- smalldfQW[!is.na(smalldfQW$compQWreg),]
smalldfQW <- smalldfQW[!is.na(smalldfQW$dischComb),]
#smalladaps_disch_in <- adaps_disch_in[which(adaps_disch_in$pdate<=strptime("2012-10-31","%Y-%m-%d")),]
#smalldfQW <- smalldfQW[which(smalldfQW$pdate %in% smalladaps_disch_in$pdate),]
n <- nrow(smalldfQW)-1
smalldfQW <- smalldfQW[c(2:n),]
instLoad <- LoadInstantaneous(smalldfQW,Conc="compQWreg",sample.time="pdate",Conc2liters=1,df.Q=smalldfQW,Q="dischComb",Q.time="pdate",Q2liters=28.3168466)


cl_check <- merge(smalldfQW[,c(1,5,13)],cl_data[,c(15,23)],by.x="pdate",by.y="startDateTime")
