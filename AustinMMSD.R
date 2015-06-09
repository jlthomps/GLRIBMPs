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

allDailyResults <- do.call(rbind,dailyResults)
allDailyResults$plotDate <- strptime(allDailyResults$date,format="%Y-%m-%d")

dailyLoadEc <- allDailyResults[which(allDailyResults$compQW=="Ec"),]
dailyLoadEc$cumLoad <- (cumsum(dailyLoadEc$loadKg))/1000000
dailyLoadEc09 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2009"),]
dailyLoadEc09$cumLoad <- (cumsum(dailyLoadEc09$loadKg))/1000000
dailyLoadEc10 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2010"),]
dailyLoadEc10$cumLoad <- (cumsum(dailyLoadEc10$loadKg))/1000000
dailyLoadEc11 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2011"),]
dailyLoadEc11$cumLoad <- (cumsum(dailyLoadEc11$loadKg))/1000000
dailyLoadEc12 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2012"),]
dailyLoadEc12$cumLoad <- (cumsum(dailyLoadEc12$loadKg))/1000000
dailyLoadEc13 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2013"),]
dailyLoadEc13$cumLoad <- (cumsum(dailyLoadEc13$loadKg))/1000000
dailyLoadEc14 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2014"),]
dailyLoadEc14$cumLoad <- (cumsum(dailyLoadEc14$loadKg))/1000000
dailyLoadEc15 <- dailyLoadEc[which(dailyLoadEc$wy_val=="2015"),]
dailyLoadEc15$cumLoad <- (cumsum(dailyLoadEc15$loadKg))/1000000

dailyLoadFec <- allDailyResults[which(allDailyResults$compQW=="Fec"),]
dailyLoadFec$cumLoad <- (cumsum(dailyLoadFec$loadKg))/1000000
dailyLoadFec09 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2009"),]
dailyLoadFec09$cumLoad <- (cumsum(dailyLoadFec09$loadKg))/1000000
dailyLoadFec10 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2010"),]
dailyLoadFec10$cumLoad <- (cumsum(dailyLoadFec10$loadKg))/1000000
dailyLoadFec11 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2011"),]
dailyLoadFec11$cumLoad <- (cumsum(dailyLoadFec11$loadKg))/1000000
dailyLoadFec12 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2012"),]
dailyLoadFec12$cumLoad <- (cumsum(dailyLoadFec12$loadKg))/1000000
dailyLoadFec13 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2013"),]
dailyLoadFec13$cumLoad <- (cumsum(dailyLoadFec13$loadKg))/1000000
dailyLoadFec14 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2014"),]
dailyLoadFec14$cumLoad <- (cumsum(dailyLoadFec14$loadKg))/1000000
dailyLoadFec15 <- dailyLoadFec[which(dailyLoadFec$wy_val=="2015"),]
dailyLoadFec15$cumLoad <- (cumsum(dailyLoadFec15$loadKg))/1000000

dailyLoadCl <- allDailyResults[which(allDailyResults$compQW=="Cl"),]
dailyLoadCl$cumLoad <- (cumsum(dailyLoadCl$loadKg))/1000000
dailyLoadCl09 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2009"),]
dailyLoadCl09$cumLoad <- (cumsum(dailyLoadCl09$loadKg))/1000000
dailyLoadCl10 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2010"),]
dailyLoadCl10$cumLoad <- (cumsum(dailyLoadCl10$loadKg))/1000000
dailyLoadCl11 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2011"),]
dailyLoadCl11$cumLoad <- (cumsum(dailyLoadCl11$loadKg))/1000000
dailyLoadCl12 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2012"),]
dailyLoadCl12$cumLoad <- (cumsum(dailyLoadCl12$loadKg))/1000000
dailyLoadCl13 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2013"),]
dailyLoadCl13$cumLoad <- (cumsum(dailyLoadCl13$loadKg))/1000000
dailyLoadCl14 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2014"),]
dailyLoadCl14$cumLoad <- (cumsum(dailyLoadCl14$loadKg))/1000000
dailyLoadCl15 <- dailyLoadCl[which(dailyLoadCl$wy_val=="2015"),]
dailyLoadCl15$cumLoad <- (cumsum(dailyLoadCl15$loadKg))/1000000

dailyLoadTP <- allDailyResults[which(allDailyResults$compQW=="TP"),]
dailyLoadTP$cumLoad <- (cumsum(dailyLoadTP$loadKg))/1000
dailyLoadTP09 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2009"),]
dailyLoadTP09$cumLoad <- (cumsum(dailyLoadTP09$loadKg))/1000
dailyLoadTP10 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2010"),]
dailyLoadTP10$cumLoad <- (cumsum(dailyLoadTP10$loadKg))/1000
dailyLoadTP11 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2011"),]
dailyLoadTP11$cumLoad <- (cumsum(dailyLoadTP11$loadKg))/1000
dailyLoadTP12 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2012"),]
dailyLoadTP12$cumLoad <- (cumsum(dailyLoadTP12$loadKg))/1000
dailyLoadTP13 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2013"),]
dailyLoadTP13$cumLoad <- (cumsum(dailyLoadTP13$loadKg))/1000
dailyLoadTP14 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2014"),]
dailyLoadTP14$cumLoad <- (cumsum(dailyLoadTP14$loadKg))/1000
dailyLoadTP15 <- dailyLoadTP[which(dailyLoadTP$wy_val=="2015"),]
dailyLoadTP15$cumLoad <- (cumsum(dailyLoadTP15$loadKg))/1000

dailyLoadTSS <- allDailyResults[which(allDailyResults$compQW=="TSS"),]
dailyLoadTSS$cumLoad <- (cumsum(dailyLoadTSS$loadKg))/1000000
dailyLoadTSS09 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2009"),]
dailyLoadTSS09$cumLoad <- (cumsum(dailyLoadTSS09$loadKg))/1000000
dailyLoadTSS10 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2010"),]
dailyLoadTSS10$cumLoad <- (cumsum(dailyLoadTSS10$loadKg))/1000000
dailyLoadTSS11 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2011"),]
dailyLoadTSS11$cumLoad <- (cumsum(dailyLoadTSS11$loadKg))/1000000
dailyLoadTSS12 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2012"),]
dailyLoadTSS12$cumLoad <- (cumsum(dailyLoadTSS12$loadKg))/1000000
dailyLoadTSS13 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2013"),]
dailyLoadTSS13$cumLoad <- (cumsum(dailyLoadTSS13$loadKg))/1000000
dailyLoadTSS14 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2014"),]
dailyLoadTSS14$cumLoad <- (cumsum(dailyLoadTSS14$loadKg))/1000000
dailyLoadTSS15 <- dailyLoadTSS[which(dailyLoadTSS$wy_val=="2015"),]
dailyLoadTSS15$cumLoad <- (cumsum(dailyLoadTSS15$loadKg))/1000000

allDailyResults[which(allDailyResults$site=="04087119"),]$plotColor <- "blue"
allDailyResults[which(allDailyResults$site=="04087050"),]$plotColor <- "red"
allDailyResults[which(allDailyResults$site=="04087142"),]$plotColor <- "green"
allDailyResults[which(allDailyResults$site=="04087030"),]$plotColor <- "pink"
allDailyResults[which(allDailyResults$site=="04087120"),]$plotColor <- "brown"
allDailyResults[which(allDailyResults$site=="04087088"),]$plotColor <- "purple"

xMinLim <- as.POSIXct(min(allDailyResults$plotDate))
xMaxLim <- as.POSIXct(max(allDailyResults$plotDate))
xMin09 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2009"),]$plotDate))
xMax09 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2009"),]$plotDate))
xMin10 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2010"),]$plotDate))
xMax10 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2010"),]$plotDate))
xMin11 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2011"),]$plotDate))
xMax11 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2011"),]$plotDate))
xMin12 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2012"),]$plotDate))
xMax12 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2012"),]$plotDate))
xMin13 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2013"),]$plotDate))
xMax13 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2013"),]$plotDate))
xMin14 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2014"),]$plotDate))
xMax14 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2014"),]$plotDate))
xMin15 <- as.POSIXct(min(allDailyResults[which(allDailyResults$wy_val=="2015"),]$plotDate))
xMax15 <- as.POSIXct(max(allDailyResults[which(allDailyResults$wy_val=="2015"),]$plotDate))

mainTxt <- "Cumulative load (kilotons) of Chloride"
pdf(paste(pathToSave,"/","AllClCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrow=c(3,3))
plot(dailyLoadCl[which(dailyLoadCl$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="blue",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadCl[which(dailyLoadCl$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="red",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadCl[which(dailyLoadCl$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="green",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadCl[which(dailyLoadCl$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="pink",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadCl[which(dailyLoadCl$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="orange",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadCl[which(dailyLoadCl$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl[which(dailyLoadCl$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,600),col="purple",xlim=c(xMinLim,xMaxLim))
mainTxt <- "Water Year 2009"
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadCl09[which(dailyLoadCl09$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl09[which(dailyLoadCl09$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin09,xMax09))
mainTxt <- "Water Year 2010"
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadCl10[which(dailyLoadCl10$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl10[which(dailyLoadCl10$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin10,xMax10))
mainTxt <- "Water Year 2011"
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadCl11[which(dailyLoadCl11$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl11[which(dailyLoadCl11$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin11,xMax11))
mainTxt <- "Water Year 2012"
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadCl12[which(dailyLoadCl12$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl12[which(dailyLoadCl12$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin12,xMax12))
mainTxt <- "Water Year 2013"
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadCl13[which(dailyLoadCl13$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl13[which(dailyLoadCl13$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin13,xMax13))
mainTxt <- "Water Year 2014"
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadCl14[which(dailyLoadCl14$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl14[which(dailyLoadCl14$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin14,xMax14))
mainTxt <- "Water Year 2015"
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087119"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="blue",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087050"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="red",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087142"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="green",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087030"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="pink",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087120"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="orange",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadCl15[which(dailyLoadCl15$site=="04087088"),]$plotDate,((cumsum(dailyLoadCl15[which(dailyLoadCl15$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,150),col="purple",xlim=c(xMin15,xMax15))
barplot(0,0,axes=FALSE)
legend("center",lwd=c(3,3,3,3,3,3),col=c("blue","red","green","pink","orange","purple"),legend=c("Honey Creek","Little Menomonee","Menomonee and 16th Street","Menomonee Falls","Menomonee @ Wawautosa","Underwood Creek"))
dev.off()

mainTxt <- "Cumulative load (kilotons) of E Coli"
pdf(paste(pathToSave,"/","AllEcCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrow=c(3,3))
plot(dailyLoadEc[which(dailyLoadEc$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="blue",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadEc[which(dailyLoadEc$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="red",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadEc[which(dailyLoadEc$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="green",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadEc[which(dailyLoadEc$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="pink",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadEc[which(dailyLoadEc$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="orange",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadEc[which(dailyLoadEc$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc[which(dailyLoadEc$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,1000000),col="purple",xlim=c(xMinLim,xMaxLim))
mainTxt <- "Water Year 2009"
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadEc09[which(dailyLoadEc09$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc09[which(dailyLoadEc09$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin09,xMax09))
mainTxt <- "Water Year 2010"
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadEc10[which(dailyLoadEc10$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc10[which(dailyLoadEc10$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin10,xMax10))
mainTxt <- "Water Year 2011"
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadEc11[which(dailyLoadEc11$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc11[which(dailyLoadEc11$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin11,xMax11))
mainTxt <- "Water Year 2012"
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadEc12[which(dailyLoadEc12$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc12[which(dailyLoadEc12$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin12,xMax12))
mainTxt <- "Water Year 2013"
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadEc13[which(dailyLoadEc13$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc13[which(dailyLoadEc13$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin13,xMax13))
mainTxt <- "Water Year 2014"
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadEc14[which(dailyLoadEc14$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc14[which(dailyLoadEc14$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin14,xMax14))
mainTxt <- "Water Year 2015"
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087119"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="blue",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087050"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="red",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087142"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="green",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087030"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="pink",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087120"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="orange",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadEc15[which(dailyLoadEc15$site=="04087088"),]$plotDate,((cumsum(dailyLoadEc15[which(dailyLoadEc15$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,10000),col="purple",xlim=c(xMin15,xMax15))
barplot(0,0,axes=FALSE)
legend("center",lwd=c(3,3,3,3,3,3),col=c("blue","red","green","pink","orange","purple"),legend=c("Honey Creek","Little Menomonee","Menomonee and 16th Street","Menomonee Falls","Menomonee @ Wawautosa","Underwood Creek"))
dev.off()

mainTxt <- "Cumulative load (kilotons) of Fecal Coliform"
pdf(paste(pathToSave,"/","AllFecCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrow=c(3,3))
plot(dailyLoadFec[which(dailyLoadFec$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="blue",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadFec[which(dailyLoadFec$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="red",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadFec[which(dailyLoadFec$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="green",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadFec[which(dailyLoadFec$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="pink",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadFec[which(dailyLoadFec$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="orange",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadFec[which(dailyLoadFec$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec[which(dailyLoadFec$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,6500),col="purple",xlim=c(xMinLim,xMaxLim))
mainTxt <- "Water Year 2009"
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadFec09[which(dailyLoadFec09$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec09[which(dailyLoadFec09$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin09,xMax09))
mainTxt <- "Water Year 2010"
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadFec10[which(dailyLoadFec10$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec10[which(dailyLoadFec10$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin10,xMax10))
mainTxt <- "Water Year 2011"
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadFec11[which(dailyLoadFec11$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec11[which(dailyLoadFec11$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin11,xMax11))
mainTxt <- "Water Year 2012"
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadFec12[which(dailyLoadFec12$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec12[which(dailyLoadFec12$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin12,xMax12))
mainTxt <- "Water Year 2013"
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadFec13[which(dailyLoadFec13$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec13[which(dailyLoadFec13$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin13,xMax13))
mainTxt <- "Water Year 2014"
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadFec14[which(dailyLoadFec14$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec14[which(dailyLoadFec14$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin14,xMax14))
mainTxt <- "Water Year 2015"
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087119"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="blue",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087050"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="red",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087142"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="green",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087030"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="pink",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087120"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="orange",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadFec15[which(dailyLoadFec15$site=="04087088"),]$plotDate,((cumsum(dailyLoadFec15[which(dailyLoadFec15$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,4000),col="purple",xlim=c(xMin15,xMax15))
barplot(0,0,axes=FALSE)
legend("center",lwd=c(3,3,3,3,3,3),col=c("blue","red","green","pink","orange","purple"),legend=c("Honey Creek","Little Menomonee","Menomonee and 16th Street","Menomonee Falls","Menomonee @ Wawautosa","Underwood Creek"))
dev.off()

mainTxt <- "Cumulative load (tons) of Total Phosphorus"
pdf(paste(pathToSave,"/","AllTPCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrow=c(3,3))
plot(dailyLoadTP[which(dailyLoadTP$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="blue",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTP[which(dailyLoadTP$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="red",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTP[which(dailyLoadTP$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="green",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTP[which(dailyLoadTP$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="pink",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTP[which(dailyLoadTP$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="orange",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTP[which(dailyLoadTP$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP[which(dailyLoadTP$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,100),col="purple",xlim=c(xMinLim,xMaxLim))
mainTxt <- "Water Year 2009"
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTP09[which(dailyLoadTP09$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP09[which(dailyLoadTP09$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin09,xMax09))
mainTxt <- "Water Year 2010"
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTP10[which(dailyLoadTP10$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP10[which(dailyLoadTP10$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin10,xMax10))
mainTxt <- "Water Year 2011"
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTP11[which(dailyLoadTP11$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP11[which(dailyLoadTP11$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin11,xMax11))
mainTxt <- "Water Year 2012"
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTP12[which(dailyLoadTP12$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP12[which(dailyLoadTP12$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin12,xMax12))
mainTxt <- "Water Year 2013"
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTP13[which(dailyLoadTP13$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP13[which(dailyLoadTP13$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin13,xMax13))
mainTxt <- "Water Year 2014"
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTP14[which(dailyLoadTP14$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP14[which(dailyLoadTP14$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin14,xMax14))
mainTxt <- "Water Year 2015"
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087119"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="blue",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087050"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="red",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087142"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="green",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087030"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="pink",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087120"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="orange",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTP15[which(dailyLoadTP15$site=="04087088"),]$plotDate,((cumsum(dailyLoadTP15[which(dailyLoadTP15$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,40),col="purple",xlim=c(xMin15,xMax15))
barplot(0,0,axes=FALSE)
legend("center",lwd=c(3,3,3,3,3,3),col=c("blue","red","green","pink","orange","purple"),legend=c("Honey Creek","Little Menomonee","Menomonee and 16th Street","Menomonee Falls","Menomonee @ Wawautosa","Underwood Creek"))
dev.off()

mainTxt <- "Cumulative load (kilotons) of Total Suspended Solids"
pdf(paste(pathToSave,"/","AllTSSCumLoadPlot.pdf",sep=""),width=10,height=8)
par(mfrow=c(3,3))
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="blue",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="red",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="green",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="pink",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="orange",xlim=c(xMinLim,xMaxLim))
par(new=T)
plot(dailyLoadTSS[which(dailyLoadTSS$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS[which(dailyLoadTSS$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,30),col="purple",xlim=c(xMinLim,xMaxLim))
mainTxt <- "Water Year 2009"
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin09,xMax09))
par(new=T)
plot(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS09[which(dailyLoadTSS09$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin09,xMax09))
mainTxt <- "Water Year 2010"
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin10,xMax10))
par(new=T)
plot(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS10[which(dailyLoadTSS10$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin10,xMax10))
mainTxt <- "Water Year 2011"
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin11,xMax11))
par(new=T)
plot(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS11[which(dailyLoadTSS11$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin11,xMax11))
mainTxt <- "Water Year 2012"
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin12,xMax12))
par(new=T)
plot(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS12[which(dailyLoadTSS12$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin12,xMax12))
mainTxt <- "Water Year 2013"
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin13,xMax13))
par(new=T)
plot(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS13[which(dailyLoadTSS13$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin13,xMax13))
mainTxt <- "Water Year 2014"
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin14,xMax14))
par(new=T)
plot(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS14[which(dailyLoadTSS14$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin14,xMax14))
mainTxt <- "Water Year 2015"
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087119"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087119"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="blue",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087050"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087050"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="red",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087142"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087142"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="green",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087030"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087030"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="pink",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087120"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087120"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="orange",xlim=c(xMin15,xMax15))
par(new=T)
plot(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087088"),]$plotDate,((cumsum(dailyLoadTSS15[which(dailyLoadTSS15$site=="04087088"),]$loadKg))/1000000),xlab="Date",type="l",lwd=3,ylab="Cumulative load (kilotons)",main=mainTxt,ylim=c(0,15),col="purple",xlim=c(xMin15,xMax15))
barplot(0,0,axes=FALSE)
legend("center",lwd=c(3,3,3,3,3,3),col=c("blue","red","green","pink","orange","purple"),legend=c("Honey Creek","Little Menomonee","Menomonee and 16th Street","Menomonee Falls","Menomonee @ Wawautosa","Underwood Creek"))
dev.off()

  