siteNos <- c("04087119","04087050","04087142","04087030","04087120","04087088")
startDts <- c("2008-11-01","2008-11-01","2008-11-01","2008-11-01","2008-11-01","2010-02-01")
endDts <- c("2014-12-31","2014-12-31","2014-12-31","2014-12-31","2014-12-31","2014-12-31")
loadFiles <- c("AustinDataHoney.RData","AustinDataLittleMenomonee.RData","AustinDataMenomonee16.RData","AustinDataMenomoneeFalls.RData","AustinDataWawa.RData","AustinDataUnderwood.RData")
compQWs <- c("Cl","Fec","Ec","TSS","TP")
modelCoefs <- read.delim(file="/Users/jlthomps/Desktop/git/GLRIBMPs/MMSDmodelCoef.csv",stringsAsFactors=FALSE)
dailyResults=list()

for (k in 1:6) {
  siteNo <- siteNos[k]
  StartDt <- startDts[k]
  EndDt <- endDts[k]
  loadFile <- loadFiles[k]
  #load previously saved continuous and merged data for appropriate site
  load(paste("/Users/jlthomps/Desktop/git/GLRIBMPs/",loadFile,sep=""))
  for (j in 1:5) {
    compQW <- compQWs[j] # one of c('Cl','Fec','Ec','TSS','TP')
    
    #library(dataRetrieval)
    #library(USGSHydroTools)
    dataReg1 <- modelCoefs[which(modelCoefs$type=='R' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
    dataReg2 <- modelCoefs[which(modelCoefs$type=='Q' & modelCoefs$y==compQW & modelCoefs$staid==as.numeric(siteNo)),]
    dataReg1[is.na(dataReg1)] <- 0
    dataReg2[is.na(dataReg2)] <- 0
    
    #replace any negative discharge values with 'NA'
    if (min(adaps_disch_in$disch,na.rm=TRUE)<=0) {adaps_disch_in[which(adaps_disch_in$disch<=0),]$disch <- NA}
    #merge continuous data and calculate compQW based on original regression model
    adaps_data_reg <- merge(adaps_cond_in,adaps_turb_in[,c(3,6)],by="pdate",all=TRUE)
    adaps_data_reg <- merge(adaps_data_reg,adaps_temp_in[,c(3,6)],by="pdate",all=TRUE)
    adaps_data_reg$compQWlog1 <- ((log10(adaps_data_reg$cond) * dataReg1$xLogCond) + (log10(adaps_data_reg$turb) * dataReg1$xLogTURB) + (adaps_data_reg$temp * dataReg1$xTemp) + dataReg1$b)
    adaps_data_reg$compQWreg1 <- (10 ^ ((log10(adaps_data_reg$cond) * dataReg1$xLogCond) + (log10(adaps_data_reg$turb) * dataReg1$xLogTURB) + (adaps_data_reg$temp * dataReg1$xTemp) + dataReg1$b)) * dataReg1$BiasCorr
    #merge continuous data frame with discharge data 
    adaps_data_reg <- merge(adaps_data_reg,adaps_disch_in[,c(3,6)],by="pdate",all=TRUE)
    #interpolate discharge data where missing
    #library(GSqwsr)
    dateTime <- as.POSIXlt(adaps_data_reg$pdate)
    year <- dateTime$year + 1900
    jan1 <- as.POSIXlt(paste(year,"-01-01",sep=""),format="%Y-%m-%d")
    jan1NextYear <- as.POSIXlt(paste(year+1,"-01-01",sep=""),format="%Y-%m-%d")
    decimal <- as.numeric(difftime(adaps_data_reg$pdate, jan1, units = "secs"))
    decimal <- decimal/as.numeric(difftime(jan1NextYear, jan1, units = "secs"))
    adaps_data_reg$decYear <- year + decimal
    #adaps_data_reg$decYear <- getDecYear(adaps_data_reg$pdate)
    
    mindecYear <- 2010
    maxdecYear <- 2015
    subNum <- (maxdecYear-mindecYear)*4
    adaps_data_reg$dischint <- NA
    for (i in 1:subNum) {
      startDY <- mindecYear + (i-1)*0.25
      endDY <- startDY + 0.25
      adaps_data_reg_sub <- adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]
      adaps_data_Int <- adaps_data_reg_sub[!is.na(adaps_data_reg_sub$disch),c(1,11)]
      adaps_data_Int$pdate2 <- as.numeric(adaps_data_Int$pdate)
      fit <- loess(disch ~ pdate2, adaps_data_Int)
      adaps_data_reg_sub$dischint <- predict(fit,adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]$pdate)
      adaps_data_reg[which(adaps_data_reg$decYear>=startDY & adaps_data_reg$decYear<=endDY),]$dischint <- adaps_data_reg_sub$dischint
    }
    if (min(adaps_data_reg$dischint,na.rm=TRUE)<=0) {adaps_data_reg[which(adaps_data_reg$dischint<=0),]$dischint <- NA}
    adaps_data_reg$dischComb <- ifelse(!is.na(adaps_data_reg$disch),adaps_data_reg$disch,adaps_data_reg$dischint)
    date.sequence <- as.data.frame(seq(strptime(StartDt,format="%Y-%m-%d"),strptime(EndDt,format="%Y-%m-%d"),3600))
    colnames(date.sequence) <- "pdate"
    adaps_data_reg <- merge(adaps_data_reg,date.sequence,by="pdate",all=TRUE)
    library(dataRetrieval)
    daily_disch <- readNWISdv(siteNo,"00060",StartDt,EndDt,statCd="00003")
    adaps_data_reg$day <- as.Date(adaps_data_reg$pdate)
    adaps_data_reg <- merge(adaps_data_reg,daily_disch[,c(3,5)],by.x="day",by.y="Date",all.x=TRUE)
    adaps_data_reg$dischComb <- ifelse(!is.na(adaps_data_reg$dischComb),adaps_data_reg$dischComb,adaps_data_reg$X_00060_00003)
    adaps_data_reg <- adaps_data_reg[order(adaps_data_reg$pdate),]
    adaps_data_reg <- adaps_data_reg[which(!is.na(adaps_data_reg$dischComb)),]
    
    dateTime <- as.POSIXlt(adaps_data_reg$pdate)
    year <- dateTime$year + 1900
    jan1 <- as.POSIXlt(paste(year,"-01-01",sep=""),format="%Y-%m-%d")
    jan1NextYear <- as.POSIXlt(paste(year+1,"-01-01",sep=""),format="%Y-%m-%d")
    decimal <- as.numeric(difftime(adaps_data_reg$pdate, jan1, units = "secs"))
    decimal <- decimal/as.numeric(difftime(jan1NextYear, jan1, units = "secs"))
    adaps_data_reg$decYear <- year + decimal
    
    
    #calculate compQW based on Q-only regression
    adaps_data_reg$sinDY <- sin(adaps_data_reg$decYear*2*pi)
    adaps_data_reg$cosDY <- cos(adaps_data_reg$decYear*2*pi)
    adaps_data_reg$compQWlog2 <- ((log10(adaps_data_reg$dischComb) * dataReg2$xLogQ) + (adaps_data_reg$dischComb * dataReg2$xQ) + (adaps_data_reg$sinDY * dataReg2$sinDY) + (adaps_data_reg$cosDY * dataReg2$cosDY) + dataReg2$b)
    adaps_data_reg$compQWreg2 <- exp(1) ^ ((log10(adaps_data_reg$dischComb) * dataReg2$xLogQ) + (adaps_data_reg$dischComb * dataReg2$xQ) + (adaps_data_reg$sinDY * dataReg2$sinDY) + (adaps_data_reg$cosDY * dataReg2$cosDY) + dataReg2$b)
    adaps_data_reg$compQWreg1Error <- dataReg1$SE*dataReg1$t95
    adaps_data_reg$compQWreg2Error <- dataReg2$SE * dataReg2$t95
    adaps_data_reg$compQWHigh1 <- 10 ^ (adaps_data_reg$compQWlog1 + adaps_data_reg$compQWreg1Error)
    adaps_data_reg$compQWLow1 <- 10 ^ (adaps_data_reg$compQWlog1 - adaps_data_reg$compQWreg1Error)
    adaps_data_reg$compQWHigh2 <- exp(1) ^ (adaps_data_reg$compQWlog2 + adaps_data_reg$compQWreg2Error)
    adaps_data_reg$compQWLow2 <- exp(1) ^ (adaps_data_reg$compQWlog2 - adaps_data_reg$compQWreg2Error)
    #create composite compQW based on original model when continuous data is available, then Q-only model
    adaps_data_reg$compQWreg <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWreg1,adaps_data_reg$compQWreg2)
    adaps_data_reg$compQWhigh <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWHigh1,adaps_data_reg$compQWHigh2)
    adaps_data_reg$compQWlow <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWLow1,adaps_data_reg$compQWLow2)
    
    #calculate instantaneous load flux
    adaps_data_reg$dischCombL <- adaps_data_reg$dischComb*28.3168466
    adaps_data_reg$loadFlux <- adaps_data_reg$dischCombL * (adaps_data_reg$compQWreg/1000000)
    adaps_data_reg$loadFluxhigh <- adaps_data_reg$dischCombL * (adaps_data_reg$compQWhigh/1000000)
    adaps_data_reg$loadFluxlow <- adaps_data_reg$dischCombL * (adaps_data_reg$compQWlow/1000000)
    
    #roll up to daily
    adaps_data_regLoad <- adaps_data_reg[!is.na(adaps_data_reg$loadFlux),c(2,11,15,20,27:33)]
    adaps_data_regLoad <- adaps_data_regLoad[order(adaps_data_regLoad$pdate),]
    temp <- diff(adaps_data_regLoad$pdate,lag=1)
    temp2 <- diff(adaps_data_regLoad$pdate,lead=1)
    #adaps_data_regLoad$date <- as.Date(adaps_data_regLoad$pdate)
    #dateXlt <- as.POSIXlt(adaps_data_reg$pdate)
    #adaps_data_regLoad$date <- paste(dateXlt$year+1900,dateXlt$mon+1,dateXlt$mday,sep=".")
    adaps_data_regLoad$date <- strftime(adaps_data_regLoad$pdate,format="%Y-%m-%d")
    temp <- c(0,temp*60)
    temp2 <- c(temp2*60,0)
    adaps_data_regLoad$bpdate <- adaps_data_regLoad$pdate-temp/2
    adaps_data_regLoad$epdate <- adaps_data_regLoad$pdate+temp2/2
    adaps_data_regLoad$duration <- (temp + temp2)/2
    adaps_data_regLoad$load <- adaps_data_regLoad$loadFlux * (adaps_data_regLoad$duration)
    adaps_data_regLoad$compQWreg1 <- ifelse(!is.na(adaps_data_regLoad$compQWreg1),1,0)
    adaps_data_regLoad$compQWreg2 <- ifelse(!is.na(adaps_data_regLoad$compQWreg2) & adaps_data_regLoad$compQWreg1==0,1,0)
    adaps_data_regLoad$loadHigh <- adaps_data_regLoad$loadFluxhigh * (adaps_data_regLoad$duration)
    adaps_data_regLoad$loadLow <- adaps_data_regLoad$loadFluxlow * (adaps_data_regLoad$duration)
    
    #library(GSHydroTools)
    #hydroAll <- Hydrovol(dfQ=adaps_data_regLoad,Q="dischComb",time="pdate",df.dates=adaps_data_regLoad,bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
    #hydroTest <- adaps_data_regLoad[which(adaps_data_regLoad$pdate<=strptime("11/30/2008",format="%m/%d/%Y")),]
    #hydroDates <- hydroTest[1:(nrow(hydroTest)-1),]
    #hydroTest <- Hydrovol(dfQ=hydroTest,Q="dischComb",time="pdate",df.dates=hydroDates,bdate="bpdate",edate="epdate",volume="event.vol",Qmax="Qmax",duration="Eduration")
    
    dailyLoad <- aggregate(load ~ date,data=adaps_data_regLoad,sum)
    dailyCount <- aggregate(load ~ date,data=adaps_data_regLoad,length)
    dailyRegCount <- aggregate(compQWreg1 ~ date,data=adaps_data_regLoad,sum)
    dailyRegQCount <- aggregate(compQWreg2 ~ date,data=adaps_data_regLoad,sum)
    dailyLoadHigh <- aggregate(loadHigh ~ date,data=adaps_data_regLoad,sum)
    dailyLoadLow <- aggregate(loadLow ~ date,data=adaps_data_regLoad,sum)
    dailyLoadCounts <- merge(dailyLoad,dailyCount,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyRegCount,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyRegQCount,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyLoadHigh,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyLoadLow,by="date")
    colnames(dailyLoadCounts) <- c("date","loadKg","loadCount","regCount","QregCount","loadHighkg","loadLowkg")
    sites <- rep(siteNo,nrow(dailyLoadCounts))
    compQws <- rep(compQW,nrow(dailyLoadCounts))
    dailyLoadCounts$site <- sites
    dailyLoadCounts$compQW <- compQws
    
    dailyLoadCounts$month_val <- substr(dailyLoadCounts$date,6,7)
    dailyLoadCounts$year_val <- substr(dailyLoadCounts$date,1,4)
    dailyLoadCounts$monYear <- paste(dailyLoadCounts$month_val,dailyLoadCounts$year_val,sep=".")
    dailyLoadCounts$wy_val <- ifelse(as.numeric(dailyLoadCounts$month_val)>=10,as.character(as.numeric(dailyLoadCounts$year_val)+1),dailyLoadCounts$year_val)
    dailyResults[[k*j]] <- dailyLoadCounts
    monthlyLoads <- aggregate(loadKg ~ monYear,data=dailyLoadCounts,sum)
    monthlyCount <- aggregate(loadKg ~ monYear,data=dailyLoadCounts,length)
    monthlyLoadHigh <- aggregate(loadHighkg ~ monYear,data=dailyLoadCounts,sum)
    monthlyLoadLow <- aggregate(loadLowkg ~ monYear,data=dailyLoadCounts,sum)
    monthlyLoadCounts <- merge(monthlyLoads,monthlyCount,by="monYear")
    monthlyLoadCounts <- merge(monthlyLoadCounts,monthlyLoadHigh,by="monYear")
    monthlyLoadCounts <- merge(monthlyLoadCounts,monthlyLoadLow,by="monYear")
    colnames(monthlyLoadCounts) <- c("monYear","loadKg","daysCount","loadHighkg","loadLowkg")
    monthlyLoadCounts <- monthlyLoadCounts[order(monthlyLoadCounts$monYear),]
    
    annualLoads <- aggregate(loadKg ~ wy_val,data=dailyLoadCounts,sum)
    annualCount <- aggregate(loadKg ~ wy_val,data=dailyLoadCounts,length)
    annualLoadHigh <- aggregate(loadHighkg ~ wy_val,data=dailyLoadCounts,sum)
    annualLoadLow <- aggregate(loadLowkg ~ wy_val,data=dailyLoadCounts,sum)
    annualLoadCounts <- merge(annualLoads,annualCount,by="wy_val")
    annualLoadCounts <- merge(annualLoadCounts,annualLoadHigh,by="wy_val")
    annualLoadCounts <- merge(annualLoadCounts,annualLoadLow,by="wy_val")
    colnames(annualLoadCounts) <- c("waterYear","loadKg","daysCount","lowHighkg","loadLowkg")
    annualLoadCounts <- annualLoadCounts[order(annualLoadCounts$waterYear),]
    
    pathToSave <- "/Users/jlthomps/Documents/R/MMSD/MMSDoutput"
    fileSave <- paste(pathToSave,"/",siteNo,compQW,"daily.txt",sep="")
    write.table(dailyLoadCounts,file=fileSave)
    fileSave <- paste(pathToSave,"/",siteNo,compQW,"monthly.txt",sep="")
    write.table(monthlyLoadCounts,file=fileSave)
    fileSave <- paste(pathToSave,"/",siteNo,compQW,"annual.txt",sep="")
    write.table(annualLoadCounts,file=fileSave)
    
    dailyLoadCounts$cumLoad <- (cumsum(dailyLoadCounts$loadKg))/1000000
    #dailyLoadCounts$cumHigh <- dailyLoadCounts$cumLoad + (dailyLoadCounts$loadHighkg - dailyLoadCounts$loadKg)/1000000
    dailyLoadCounts$cumHigh <- (cumsum(dailyLoadCounts$loadHighkg))/1000000
    #dailyLoadCounts$cumLow <- dailyLoadCounts$cumLoad + (dailyLoadCounts$loadLowkg - dailyLoadCounts$loadKg)/1000000
    dailyLoadCounts$cumLow <- (cumsum(dailyLoadCounts$loadLowkg))/1000000
    dailyLoadCounts$plotDate <- strptime(dailyLoadCounts$date,format="%Y-%m-%d")
    mainTxt <- paste("Cumulative load (kilotons) of ",compQW," at station ",siteNo," with 95% CIs",sep="")
    pdf(paste(pathToSave,"/",siteNo,compQW,"cumLoadPlot.pdf",sep=""),width=10,height=8)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumLoad,xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,max(dailyLoadCounts$cumHigh)))
    par(new=T)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumHigh,xlab="",type="l",col="blue",lty="dashed",ylab="",ylim=c(0,max(dailyLoadCounts$cumHigh)))
    par(new=T)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumLow,xlab="",type="l",col="blue",lty="dashed",ylab="",ylim=c(0,max(dailyLoadCounts$cumHigh)))
    dev.off()
    
    mainTxt <- paste("Load (kilograms) of ",compQW," at station ",siteNo," with 95% CIs",sep="")
    pdf(paste(pathToSave,"/",siteNo,compQW,"loadPlot.pdf",sep=""),width=10,height=8)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$loadKg,xlab="Date",log="y",type="l",lwd=3,ylab="Load (kg)",main=mainTxt,ylim=c(1,max(dailyLoadCounts$loadHighkg)))
    par(new=T)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$loadHighkg,xlab="",log="y",type="l",col="blue",lty="dashed",ylab="",ylim=c(1,max(dailyLoadCounts$loadHighkg)))
    par(new=T)
    plot(dailyLoadCounts$plotDate,dailyLoadCounts$loadLowkg,xlab="",log="y",type="l",col="blue",lty="dashed",ylab="",ylim=c(1,max(dailyLoadCounts$loadHighkg)))
    dev.off()
    
    #dailyLoadCounts$pdate <- as.POSIXct(dailyLoadCounts$date,tz="America/Chicago",format="%Y-%m-%d")
    dailyLoadCounts$plotPOS <- as.POSIXct(dailyLoadCounts$plotDate)
    loadQPlot <- merge(adaps_data_reg,dailyLoadCounts,by.x="pdate",by.y="plotPOS",all=TRUE)
    mainTxt <- paste("Daily load (",compQW,") and instantaneous discharge vs time at station ",siteNo,sep="")
    pdf(paste(pathToSave,"/",siteNo,compQW,"loadQPlot.pdf",sep=""),width=10,height=8)
    plot(loadQPlot$pdate,loadQPlot$loadKg,xlab="Date",type="p",ylab="Load (kgs)",main=mainTxt,col="red")
    par(new=T)
    plot(loadQPlot$pdate,loadQPlot$dischComb,axes=F,xlab="",ylab="",type="l",col="blue")
    axis(side=4)
    mtext("Discharge (cfs)",side=4,line=2,col="blue")
    legend("topright",c("Load (kgs)","Discharge (cfs)"),lty=c(NA,1),lwd=c(2.5,2.5),pch=c(1,NA),col=c("red","blue"))
    dev.off()
    
    cat(paste(mainTxt,"\n"))
    
    
#     mainTxt <- paste("Load (",compQW,") and discharge vs time at station ",siteNo,sep="")
#     pdf(paste(siteNo,compQW,"loadInstQPlot.pdf",sep=""),width=10,height=8)
#     plot(adaps_data_regLoad$pdate,adaps_data_regLoad$load,xlab="Date",type="p",ylab="Load (kgs)",main=mainTxt,col="red")
#     par(new=T)
#     plot(adaps_data_regLoad$pdate,adaps_data_regLoad$dischComb,axes=F,xlab="",ylab="",type="l",col="blue")
#     axis(side=4)
#     mtext("Discharge (cfs)",side=4,line=2,col="blue")
#     legend("topright",c("Load (kgs)","Discharge (cfs)"),lty=c(NA,1),lwd=c(2.5,2.5),pch=c(1,NA),col=c("red","blue"))
#     dev.off()
  }
}

allDailyResults <- do.call(cbind,dailyResults)
allDailyResults$cumLoad <- (cumsum(allDailyResults$loadKg))/1000000
allDailyResults$cumHigh <- (cumsum(allDailyResults$loadHighkg))/1000000
allDailyResults$cumLow <- (cumsum(allDailyResults$loadLowkg))/1000000
allDailyResults$plotDate <- strptime(allDailyResults$date,format="%Y-%m-%d")
if (allDailyResults$siteNo=="04087119") {allDailyResults$plotColor <- "blue"}
else if (allDailyResults$siteNo=="04087050") {allDailyResults$plotColor <- "red"}
else if (allDailyResults$siteNo=="04087142") {allDailyResults$plotColor <- "green"}
else if (allDailyResults$siteNo=="04087030") {allDailyResults$plotColor <- "pink"}
else if (allDailyResults$siteNo=="04087120") {allDailyResults$plotColor <- "yellow"}
else {allDailyResults$plotColor <- "purple"}

dailyLoadEcHoney <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087119"),]
dailyLoadEcLittleMen <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087050"),]
dailyLoadEcMen16 <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087142"),]
dailyLoadEcMenFalls <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087030"),]
dailyLoadEcMenWawa <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087120"),]
dailyLoadEcUnder <- allDailyResults[which(allDailyResults$compQW=="Ec" && allDailyResults$siteNo=="04087088"),]

dailyLoadFecHoney <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087119"),]
dailyLoadFecLittleMen <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087050"),]
dailyLoadFecMen16 <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087142"),]
dailyLoadFecMenFalls <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087030"),]
dailyLoadFecMenWawa <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087120"),]
dailyLoadFecUnder <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087088"),]

dailyLoadFecHoney <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087119"),]
dailyLoadFecLittleMen <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087050"),]
dailyLoadFecMen16 <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087142"),]
dailyLoadFecMenFalls <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087030"),]
dailyLoadFecMenWawa <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087120"),]
dailyLoadFecUnder <- allDailyResults[which(allDailyResults$compQW=="Fec" && allDailyResults$siteNo=="04087088"),]
dailyLoadTP <- allDailyResults[which(allDailyResults$compQW=="TP"),]
dailyLoadTSS <- allDailyResults[which(allDailyResults$compQW=="TSS"),]

mainTxt <- "Cumulative load (kilotons) of E Coli"
pdf(paste(pathToSave,"/","EcCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrom=c(3,3))
dailyLoad

mainTxt <- paste("Cumulative load (kilotons) of ",compQW," at station ",siteNo," with 95% CIs",sep="")
pdf(paste(pathToSave,"/",siteNo,compQW,"cumLoadPlot.pdf",sep=""),width=10,height=8)
plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumLoad,xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,max(dailyLoadCounts$cumHigh)))
par(new=T)
plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumHigh,xlab="",type="l",col="blue",lty="dashed",ylab="",ylim=c(0,max(dailyLoadCounts$cumHigh)))
par(new=T)
plot(dailyLoadCounts$plotDate,dailyLoadCounts$cumLow,xlab="",type="l",col="blue",lty="dashed",ylab="",ylim=c(0,max(dailyLoadCounts$cumHigh)))
dev.off()
  