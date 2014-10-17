library(dataRetrieval)
library(USGSwsStats)
library(EflowStats)
library(NWCCompare)
library(zoo)
setwd("C:/Users/jlthomps/Desktop/git/GLRIBMPs")
sites <- read.table("Gaging stations Warren.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,colClasses="character")
sites$station <- ifelse(nchar(sites$station)<9,paste('0',sites$station,sep=""),sites$station)
Q90 <- rep(0,nrow(sites))
Q72 <- rep(0,nrow(sites))
Q710 <- rep(0,nrow(sites))
Q72lm <- rep(0,nrow(sites))
Q710lm <- rep(0,nrow(sites))
numYear <- rep(0,nrow(sites))
minDate <- rep(Sys.Date(),nrow(sites))
maxDate <- rep(Sys.Date(),nrow(sites))
for (i in 1:nrow(sites)) {
  site <- sites[i,1]
  startdate <- "1969-10-01"
  enddate <- "2008-09-30"
  property <- "00060"
  obs_url <- constructNWISURL(site,property,startdate,enddate,"dv")
  flowData <- getXMLWML1.1Data(obs_url)
  if (nrow(flowData)>0) {
    colnames(flowData) <- c("datetime","flow")
    Q90[i] <- quantile(flowData$flow,.1,names=FALSE)
    cat(Q90[i],i,site,sites[i,1],sep=" ")
    maxDate[i] <- max(flowData$datetime)
  }
  else {
    cat("no data available for site",site,startdate,enddate,sep=" ")
    Q90[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
  }
  startdate2 <- "1969-04-01"
  enddate2 <- "2008-03-31"
  obs_url2 <- constructNWISURL(site,property,startdate2,enddate2,"dv")
  flowData2 <- getXMLWML1.1Data(obs_url2)
  if (nrow(flowData2)>0) {
    flowData2$month_val <- substr(flowData2$date,6,7)
    flowData2$year_val <- substr(flowData2$date,1,4)
    flowData2$day_val <- substr(flowData2$date,9,10)
    flowData2$jul_val <- strptime(flowData2$date,"%Y-%m-%d")$yday+1
    flowData2$wy_val <- ifelse(as.numeric(flowData2$month_val)>=4,as.character(as.numeric(flowData2$year_val)+1),flowData2$year_val)
    day7mean <- rollmean(flowData2$discharge, 7, align = "right", 
                         na.pad = TRUE)
    day7rollingavg <- data.frame(flowData2, day7mean)
    rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                                "NA")
    min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                               list(rollingavgs7day$wy_val), min, na.rm=TRUE)
    colnames(min7daybyyear) <- c("year","min7day")
    min7daybyyear$rank <- rank(min7daybyyear$min7day)
    min7daybyyear$probability <- min7daybyyear$rank/max(min7daybyyear$rank)
    #plot(log10(min7daybyyear$probability),log10(min7daybyyear$min7day))
    lmfit <- lm(log10(min7daybyyear$min7day) ~ (log10(min7daybyyear$probability)))
    #abline(lmfit)
    Q72lm[i] <- 10^(log10(unname(lmfit[[1]][1])) + unname(lmfit[[1]][2])*log10(0.5))
    Q710lm[i] <- 10^(unname(lmfit[[1]][1]) + unname(lmfit[[1]][2])*log10(0.1))
    Q72[i] <- NWCportalL7Q2(flowData2)
    Q710[i] <- NWCportalL7Q10(flowData2)
    cat(Q72[i],i,site,sites[i,1],sep=" ")
    cat(Q710[i],i,site,sites[i,1],sep=" ")
    minDate[i] <- min(flowData2$date)
    numYear[i] <- nrow(min7daybyyear)
  }
  else {
    cat("no data available for site",site,startdate,enddate,sep=" ")
    Q72[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
    Q710[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
    Q72lm[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
    Q710lm[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
  }
}
stationsQ90Q72Q710 <- data.frame(sites$station,Q90,Q72,Q710,Q72lm,Q710lm,stringsAsFactors=FALSE)
write.table(stationsQ90Q72Q710,file="stationsQ90Q72Q710.txt",sep="\t",row.names=FALSE)

