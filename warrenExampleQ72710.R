library(dataRetrieval)
library(USGSwsStats)
library(EflowStats)
library(NWCCompare)
library(zoo)
setwd("/Users/jlthomps/GLRIBMPs")
sites <- c("055451345","05435950","05435943","05426500","05408500","05403000","05402500","05400500","05400000","05399000","05398500","05397000","05396000","05394000","05392400","05392000","05370500","040085119","05367000","05364500","05358000","05355500","04084445","04083545","04083500","04083000","04075500","04068000","04028000")
files <- paste(sites,"data.rdb",sep="")
#sites <- read.table("Gaging stations Warren.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,colClasses="character")
#sites$station <- ifelse(nchar(sites$station)<9,paste('0',sites$station,sep=""),sites$station)
Q90 <- rep(0,length(sites))
Q72 <- rep(0,length(sites))
Q710 <- rep(0,length(sites))
Q72lm <- rep(0,length(sites))
Q710lm <- rep(0,length(sites))
numYear <- rep(0,length(sites))
minDate <- rep(Sys.Date(),length(sites))
maxDate <- rep(Sys.Date(),length(sites))
min7dayperSite <- as.data.frame(cbind(0,0,0))
colnames(min7dayperSite) <- c("siteNo","year","min7day")
for (i in 1:length(sites)) {
  #site <- sites[i,1]
  file <- files[i]
  startdate <- "1969-10-01"
  enddate <- "2008-09-30"
  property <- "00060"
  dataPath <- "/Users/jlthomps/GLRIBMPs/"
  flowData <- read.table(paste(dataPath, file, sep = ""), sep = "\t", stringsAsFactors = FALSE, header = TRUE)
  flowData <- flowData[-1, c(1,3)]
  flowData$DATE <- strptime(flowData$DATE, format = "%Y%m%d")
  flowData$VALUE <- as.numeric(flowData$VALUE)
  flowData1 <- flowData[which(flowData$DATE<=strptime(enddate,format="%Y-%m-%d")),]
  #obs_url <- constructNWISURL(site,property,startdate,enddate,"dv")
  #flowData <- getXMLWML1.1Data(obs_url)
  if (nrow(flowData1)>0) {
    colnames(flowData1) <- c("datetime","flow")
    Q90[i] <- quantile(flowData1$flow,.1,names=FALSE,na.rm=TRUE)
    cat(Q90[i],i,sites[i],sep=" ")
    maxDate[i] <- max(flowData1$datetime)
  } else {
    cat("no data available for site",sites[i],startdate,enddate,sep=" ")
    Q90[i] <- paste("no data available for site",sites[i],startdate,enddate,sep=" ")
  }
  startdate2 <- "1970-04-01"
  enddate2 <- "2009-03-31"
  #obs_url2 <- constructNWISURL(site,property,startdate2,enddate2,"dv")
  flowData2 <- flowData[which(flowData$DATE>=strptime(startdate2,format="%Y-%m-%d")),]
  if (nrow(flowData2)>0 & max(flowData2$VALUE,na.rm=TRUE)!=-Inf) {
    flowData2$month_val <- substr(flowData2$DATE,6,7)
    flowData2$year_val <- substr(flowData2$DATE,1,4)
    flowData2$day_val <- substr(flowData2$DATE,9,10)
    flowData2$jul_val <- strptime(flowData2$DATE,"%Y-%m-%d")$yday+1
    flowData2$wy_val <- ifelse(as.numeric(flowData2$month_val)>=4,as.character(as.numeric(flowData2$year_val)+1),flowData2$year_val)
    day7mean <- rollapply(flowData2$VALUE, 7, mean, align = "right", fill = NA)
    day7rollingavg <- data.frame(flowData2, day7mean)
    rollingavgs7day <- subset(day7rollingavg, day7rollingavg$day7mean != 
                                "NA")
    min7daybyyear <- aggregate(rollingavgs7day$day7mean, 
                               list(rollingavgs7day$wy_val), min, na.rm=TRUE)
    colnames(min7daybyyear) <- c("year","min7day")
    siteNo <- rep(site,nrow(min7daybyyear))
    min7dayperSite <- rbind(min7dayperSite,cbind(siteNo,min7daybyyear))
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
    #plot(log10(min7daybyyear$probability),log10(min7daybyyear$min7day))
    min7daybyyear <- min7daybyyear[which(min7daybyyear$min7day>0),]
    if (nrow(min7daybyyear)>0) {
    mean7daybyyear <- mean(min7daybyyear$min7day)
    stdev7daybyyear <- sd(min7daybyyear$min7day)
    skewtemp <- min7daybyyear[,1:2]
    colnames(skewtemp) <- c("year","discharge")
    skew7daybyyear <- skew(skewtemp)
    lmfit <- lm(log10(min7daybyyear$min7day) ~ (log10(min7daybyyear$probability)))
    Q72lm[i] <- round(10^(unname(lmfit[[1]][1]) + unname(lmfit[[1]][2])*log10(0.5)),digits=2)
    Q710lm[i] <- round(10^(unname(lmfit[[1]][1]) + unname(lmfit[[1]][2])*log10(0.1)),digits=2)
    } else {
      cat("no non-NA 7-day minimums for site",sites[i],startdate2,enddate2,sep=" ")
      Q72lm[i] <- paste("no non-NA 7-day minimums available for site",sites[i],startdate,enddate,sep=" ")
      Q710lm[i] <- paste("no non-NA 7-day minimums available for site",sites[i],startdate,enddate,sep=" ")
    }
    Q72[i] <- l7Q2
    Q710[i] <- l7Q10
    cat(Q72[i],i,sites[i],sep=" ")
    cat(Q710[i],i,sites[i],sep=" ")
    minDate[i] <- min(flowData2$DATE)
    numYear[i] <- nrow(min7daybyyear)
  } else {
    cat("no data available for site",sites[i],startdate,enddate,sep=" ")
    Q72[i] <- paste("no data available for site",sites[i],startdate,enddate,sep=" ")
    Q710[i] <- paste("no data available for site",sites[i],startdate,enddate,sep=" ")
    Q72lm[i] <- paste("no data available for site",sites[i],startdate,enddate,sep=" ")
    Q710lm[i] <- paste("no data available for site",sites[i],startdate,enddate,sep=" ")
  }
}
min7dayperSite <- min7dayperSite[which(min7dayperSite$siteNo>0),]
write.table(min7dayperSite,file="annual7dayminimums.txt",sep="\t",row.names=FALSE)
stationsQ90Q72Q710 <- data.frame(sites,Q90,Q72,Q710,Q72lm,Q710lm,minDate,maxDate,numYear,stringsAsFactors=FALSE)
write.table(stationsQ90Q72Q710,file="stationsQ90Q72Q710.txt",sep="\t",row.names=FALSE)

