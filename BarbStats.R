library(dataRetrieval)
setwd("C:/Users/jlthomps/Desktop/git/GLRIBMPs")
sites <- c("040869416","04087118","04086600","040869415","04087000","04087030","04087070","04087088","04087119","04087120","04087159","04087204","04087214","04087220")
water_years <- c("2004","2007","2010","2013")
BarbStats=list()
for (i in 1:length(sites)) {
  site <- sites[i]
  for (j in 1:length(water_years)) {
    waterYear <- water_years[j]
    startdate <- paste(as.character(as.numeric(waterYear)-1),"10","01",sep="-")
    enddate <- paste(waterYear,"09","30",sep="-")
    property <- "00060"
    obs_url <- constructNWISURL(site,property,startdate,enddate,"dv")
    flowData <- importWaterML1(obs_url,asDateTime=TRUE,tz="America/Chicago")
    if (nrow(flowData)>0) {
      colnames(flowData) <- c("agency_cd","site_no","dateTime","tz_cd","remark","flow")
      Q90 <- quantile(flowData$flow,.1,names=FALSE,na.rm=TRUE)
      Q10 <- quantile(flowData$flow,.9,names=FALSE,na.rm=TRUE)
      Q50 <- quantile(flowData$flow,.5,names=FALSE,na.rm=TRUE)
      Qmax <- max(flowData$flow,na.rm=TRUE)
      cat(Q90,i,site,sep=" ")
    } else {
      cat("no data available for site",site,startdate,enddate,sep=" ")
      Q90 <- paste("no data available for site",site,startdate,enddate,sep=" ")
      Q10 <- NA
      Q50 <- NA
      Qmax <- NA
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
    barbStats <- as.data.frame(cbind(site,Q90,Q10,Q50,Qmax,waterYear,QinstMax),stringsAsFactors=FALSE)
    BarbStats[[j+((i-1)*4)]] <- barbStats
  }
}

allBarbStats <- do.call(rbind,BarbStats)
write.table(allBarbStats,file="allBarbStats.txt",sep="\t",row.names=FALSE)

