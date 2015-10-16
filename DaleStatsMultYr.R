library(dataRetrieval)
library(zoo)
library(EflowStats)
setwd("C:/Users/jlthomps/Desktop/git/GLRIBMPs")
sites <- c("04060993","04062011","04026450","04027000")
BarbStats=list()
for (i in 1:length(sites)) {
  site <- sites[i]
  siteInfo <- readNWISsite(site)
  startdate <- "1900-10-01"
  enddate <- "2015-09-30"
  property <- "00060"
  obs_url <- constructNWISURL(site,property,startdate,enddate,"dv")
  flowData <- importWaterML1(obs_url,asDateTime=TRUE,tz="America/Chicago")
  if (nrow(flowData)>0) {
    colnames(flowData) <- c("agency_cd","site_no","dateTime","tz_cd","flow","remark")
    Q90 <- quantile(flowData$flow,.1,names=FALSE,na.rm=TRUE)
    Q10 <- quantile(flowData$flow,.9,names=FALSE,na.rm=TRUE)
    Q50 <- quantile(flowData$flow,.5,names=FALSE,na.rm=TRUE)
    Qmax <- max(flowData$flow,na.rm=TRUE)
    Qmin <- min(flowData$flow,na.rm=TRUE)
    minDate <- min(flowData$dateTime)
    maxDate <- max(flowData$dateTime)
    minDate <- as.character(format(minDate,"%Y-%m-%d"))
    maxDate <- as.character(format(maxDate,"%Y-%m-%d"))
    cat(Q90,i,site,sep=" ")
  } else {
    cat("no data available for site",site,startdate,enddate,sep=" ")
    Q90 <- paste("no data available for site",site,startdate,enddate,sep=" ")
    Q10 <- NA
    Q50 <- NA
    Qmax <- NA
    Qmin <- NA
    minDate <- NA
    maxDate <- NA
  }
  urlpeak <- "http://nwis.waterdata.usgs.gov/nwis/peak?site_no"
  url <- paste(urlpeak,"=",site,"&agency_cd=USGS&format=rdb&begin_date=",startdate,"&end_date=",enddate,sep="")
  doc<-read.table(url,sep="\t",stringsAsFactors=FALSE)
  if (doc[1,1]=="No sites/data found using the selection criteria specified ") {
    QinstMax <- doc[1,1]
  } else {
    peakValues<-data.frame(as.Date(doc$V3[3:nrow(doc)],format="%Y-%m-%d"),as.numeric(doc$V5[3:nrow(doc)]))
    colnames(peakValues)<-c('date','discharge')
    QinstMax <- max(peakValues$discharge,na.rm=TRUE)
  }
  startdate <- "1901-04-01"
  enddate <- "2015-03-31"
  obs_url <- constructNWISURL(site,property,startdate,enddate,"dv")
  flowData2 <- importWaterML1(obs_url,asDateTime=TRUE,tz="America/Chicago")
  if (nrow(flowData2)>0 & max(flowData2$X_00060_00003,na.rm=TRUE)!=-Inf) {
    colnames(flowData2) <- c("agency_cd","site_no","dateTime","tz_cd","flow","remark")
    flowData2$month_val <- as.numeric(format(flowData2$dateTime,"%m"))
    flowData2$year_val <- as.numeric(format(flowData2$dateTime,"%Y"))
    flowData2$wy_val <- ifelse(as.numeric(flowData2$month_val)>=4,as.character(as.numeric(flowData2$year_val)+1),flowData2$year_val)
    day7mean <- rollapply(flowData2$flow, 7, mean, align = "right", fill = NA)
    day7rollingavg <- data.frame(flowData2, day7mean)
    rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != "NA")
    min7daybyyear <- aggregate(rollingavgs7day$day7mean, list(rollingavgs7day$wy_val), min, na.rm=TRUE)
    colnames(min7daybyyear) <- c("year","min7day")
    min7daybyyear$site <- site
    min7daybyyear$station_nm <- siteInfo$station_nm
    if (i==1) {
      allmin7daybyyear <- min7daybyyear
    } else {
      allmin7daybyyear <- rbind(allmin7daybyyear,min7daybyyear)
    }
    sort_7day <- sort(min7daybyyear$min7day)
    rank_50 <- floor(findrank(length(sort_7day), 0.5))
    if (rank_50 > 0) {
      l7Q2 <- round(sort_7day[rank_50], digits=2)
    } else { l7Q2 <- NaN }
    rank_90 <- floor(findrank(length(sort_7day), 0.9))
    if (rank_90 > 0) {
      l7Q10 <- round(sort_7day[rank_90], digits = 2)
    } else { l7Q10 <- NaN }
    min7daybyyear$rank <- rank(min7daybyyear$min7day)
    min7daybyyear$probability <- min7daybyyear$rank/max(min7daybyyear$rank)
    min7daybyyear <- min7daybyyear[which(min7daybyyear$min7day>0),]
    if (nrow(min7daybyyear)>0) {
      mean7daybyyear <- mean(min7daybyyear$min7day)
      stdev7daybyyear <- sd(min7daybyyear$min7day)
      skewtemp <- min7daybyyear[,1:2]
      colnames(skewtemp) <- c("year","discharge")
      skew7daybyyear <- skew(skewtemp)
      lmfit <- lm(log10(min7daybyyear$min7day) ~ (log10(min7daybyyear$probability)))
      Q72lm <- round(10^(unname(lmfit[[1]][1]) + unname(lmfit[[1]][2])*log10(0.5)),digits=2)
      Q710lm <- round(10^(unname(lmfit[[1]][1]) + unname(lmfit[[1]][2])*log10(0.1)),digits=2)
    } else {
      cat("no non-NA 7-day minimums for site",sites[i],startdate2,enddate2,sep=" ")
      Q72lm <- paste("no non-NA 7-day minimums available for site",site,startdate,enddate,sep=" ")
      Q710lm <- paste("no non-NA 7-day minimums available for site",site,startdate,enddate,sep=" ")
    }
  } else {
    l7Q2 <- NA
    l7Q10 <- NA
    Q72lm <- NA
    Q710lm <- NA
  }
  barbStats <- as.data.frame(cbind(site,siteInfo$station_nm,Q90,Q10,Q50,Qmax,Qmin,minDate,maxDate,QinstMax,l7Q2,l7Q10,Q72lm,Q710lm),stringsAsFactors=FALSE)
  BarbStats[[i]] <- barbStats
}

allBarbStats <- do.call(rbind,BarbStats)
write.table(allBarbStats,file="allDaleStatsMultiYr.txt",sep="\t",row.names=FALSE)
write.table(allmin7daybyyear,file="allDaleYearlyMins.txt",sep="\t",row.names=FALSE)

