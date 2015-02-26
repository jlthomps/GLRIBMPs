siteNos <- c("04087119","04087050","04087142","04087030","04087129","04087088")
startDts <- c("2008-11-01","2008-11-01","2008-11-01","2008-11-01","2008-11-01","2010-02-01")
endDts <- c("2014-12-31","2014-12-31","2014-12-31","2014-12-31","2014-12-31","2014-12-31")
loadFiles <- c("AustinDataHoney.RData","AustinDataLittleMenomonee.RData","AustinDataMenomonee16.RData","AustinDataMenomoneeFalls.RData","AustinDataWawa.RData","AustinDataUnderwood.RData")
compQWs <- c("Cl","Fec","Ec","TSS","TP")
modelCoefs <- read.delim(file="/Users/jlthomps/GLRIBMPs/MMSDmodelCoef.csv",stringsAsFactors=FALSE)

for (k in 1:6) {
  siteNo <- siteNos[k]
  StartDt <- startDts[k]
  EndDt <- endDts[k]
  loadFile <- loadFiles[k]
  #load previously saved continuous and merged data for appropriate site
  load(paste("/Users/jlthomps/GLRIBMPs/",loadFile,sep=""))
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
    }
    if (min(adaps_data_reg$dischint,na.rm=TRUE)<=0) {adaps_data_reg[which(adaps_data_reg$dischint<=0),]$dischint <- NA}
    adaps_data_reg$dischComb <- ifelse(!is.na(adaps_data_reg$disch),adaps_data_reg$disch,adaps_data_reg$dischint)
    
    #calculate compQW based on Q-only regression
    adaps_data_reg$sinDY <- sin(adaps_data_reg$decYear*2*pi)
    adaps_data_reg$cosDY <- cos(adaps_data_reg$decYear*2*pi)
    adaps_data_reg$compQWreg2 <- exp(1) ^ ((log10(adaps_data_reg$dischComb) * dataReg2$xLogQ) + (adaps_data_reg$dischComb * dataReg2$xQ) + (adaps_data_reg$sinDY * dataReg2$sinDY) + (adaps_data_reg$cosDY * dataReg2$cosDY) + dataReg2$b)
    #create composite compQW based on original model when continuous data is available, then Q-only model
    adaps_data_reg$compQWreg <- ifelse(!is.na(adaps_data_reg$compQWreg1),adaps_data_reg$compQWreg1,adaps_data_reg$compQWreg2)
    
    #calculate instantaneous load flux
    adaps_data_reg$dischCombL <- adaps_data_reg$dischComb*28.3168466
    adaps_data_reg$loadFlux <- adaps_data_reg$dischCombL * (adaps_data_reg$compQWreg/1000000)
    
    #roll up to daily
    adaps_data_regLoad <- adaps_data_reg[!is.na(adaps_data_reg$loadFlux),c(1,9,16:19)]
    temp <- diff(adaps_data_regLoad$pdate,lag=1)
    temp2 <- diff(adaps_data_regLoad$pdate,lead=1)
    adaps_data_regLoad$date <- as.Date(adaps_data_regLoad$pdate)
    temp <- c(0,temp)
    temp2 <- c(temp2,0)
    adaps_data_regLoad$diff <- temp
    adaps_data_regLoad$diff2 <- temp2
    adaps_data_regLoad$duration <- (temp + temp2)/2
    adaps_data_regLoad$load <- adaps_data_regLoad$loadFlux * (adaps_data_regLoad$duration*60)
    adaps_data_regLoad$compQWreg1 <- ifelse(!is.na(adaps_data_regLoad$compQWreg1),1,0)
    adaps_data_regLoad$compQWreg2 <- ifelse(!is.na(adaps_data_regLoad$compQWreg2) & adaps_data_regLoad$compQWreg1==0,1,0)
    dailyLoad <- aggregate(load ~ date,data=adaps_data_regLoad,sum)
    dailyCount <- aggregate(load ~ date,data=adaps_data_regLoad,length)
    dailyRegCount <- aggregate(compQWreg1 ~ date,data=adaps_data_regLoad,sum)
    dailyRegQCount <- aggregate(compQWreg2 ~ date,data=adaps_data_regLoad,sum)
    dailyLoadCounts <- merge(dailyLoad,dailyCount,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyRegCount,by="date")
    dailyLoadCounts <- merge(dailyLoadCounts,dailyRegQCount,by="date")
    colnames(dailyLoadCounts) <- c("date","loadKg","loadCount","regCount","QregCount")
    sites <- rep(siteNo,nrow(dailyLoadCounts))
    compQws <- rep(compQW,nrow(dailyLoadCounts))
    dailyLoadCounts$site <- sites
    dailyLoadCounts$compQW <- compQws
    
    dailyLoadCounts$month_val <- substr(dailyLoadCounts$date,6,7)
    dailyLoadCounts$year_val <- substr(dailyLoadCounts$date,1,4)
    dailyLoadCounts$wy_val <- ifelse(as.numeric(dailyLoadCounts$month_val)>=10,as.character(as.numeric(dailyLoadCounts$year_val)+1),dailyLoadCounts$year_val)
    annualLoads <- aggregate(loadKg ~ wy_val,data=dailyLoadCounts,sum)
    
    fileSave <- paste(siteNo,compQW,"daily.txt",sep="")
    write.table(dailyLoadCounts,file=fileSave)
    fileSave <- paste(siteNo,compQW,"annual.txt",sep="")
    write.table(annualLoads,file=fileSave)
    
    dailyLoadCounts$cumLoad <- (cumsum(dailyLoadCounts$loadKg))/1000000
    mainTxt <- paste("Cumulative load (kilotons) of ",compQW," at station ",siteNo,sep="")
    pdf(paste(siteNo,compQW,"cumLoadPlot.pdf",sep=""),width=10,height=8)
    plot(dailyLoadCounts$date,dailyLoadCounts$cumLoad,xlab="Date",type="l",ylab="Cumulative load (kilotons)",main=mainTxt)
    dev.off()
    
    #dailyLoadCounts$pdate <- as.POSIXct(dailyLoadCounts$date,tz="America/Chicago",format="%Y-%m-%d")
    loadQPlot <- merge(adaps_data_reg,dailyLoadCounts,by.x="pdate",by.y="date",all=TRUE)
    mainTxt <- paste("Load (",compQW,") and discharge vs time at station ",siteNo,sep="")
    pdf(paste(siteNo,compQW,"loadQPlot.pdf",sep=""),width=10,height=8)
    plot(loadQPlot$pdate,loadQPlot$loadKg,xlab="Date",type="p",ylab="Load (kgs)",main=mainTxt,col="red")
    par(new=T)
    plot(loadQPlot$pdate,loadQPlot$dischComb,axes=F,xlab="",ylab="",type="l",col="blue")
    axis(side=4)
    mtext("Discharge (cfs)",side=4,line=2,col="blue")
    legend("topright",c("Load (kgs)","Discharge (cfs)"),lty=c(NA,1),lwd=c(2.5,2.5),pch=c(1,NA),col=c("red","blue"))
    dev.off()
  }
}
  