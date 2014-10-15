library(dataRetrieval)
library(USGSwsStats)
library(EflowStats)
library(USGSwsGraphs)
sites <- read.table("Gaging station Q90.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,colClasses="character")
sites$station <- ifelse(nchar(sites$station)<9,paste('0',sites$station,sep=""),sites$station)
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
  }
  else {
    cat("no data available for site",site,startdate,enddate,sep=" ")
    Q90[i] <- paste("no data available for site",site,startdate,enddate,sep=" ")
  }
}
stationsQ90 <- data.frame(sites$station,Q90,stringsAsFactors=FALSE)
write.table(stationsQ90,file="stationsQ90.txt",sep="\t",row.names=FALSE)

