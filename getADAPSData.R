#' Function to return adaps_data_all df from NWISWeb or previously retrieved RDB files
#' 
#' This function accepts an NWIS gage site id, an NWIS precip site id, a StartDate, an EndDate and file names as needed
#' 
#' @param siteNo NWIS gaging station id
#' @param precipSite NWIS precipitation station id
#' @param StartDt a date to start data pulls
#' @param EndDt a date to end data pulls
#' @param stageFile string containg name of rdb file containing stage data
#' @param dischFile string containing name of rdb file containing discharge data
#' @param precipFile string containing name of rdb file containing precip data
#' @param scodFile string containing name of rdb file containing sample code data
#' @return adaps_data_all data frame containing merged ADAPS data for the requested site and date range
#' @export
#' @examples
#' siteNo <- "434425090462401"
#' StartDt <- '2013-10-03'
#' EndDt <- '2013-10-05'
#' precipSite <- "434425090462401"
#' getADAPSData(siteNo,StartDt,EndDt,precipSite)
getADAPSData <- function(siteNo,StartDt,EndDt,precipSite,dataFile="") {
if (nchar(dataFile)<=3) {
POR <- getDataAvailability(siteNo,interactive=FALSE)
POR <- POR[which(POR$service=="uv"&POR$parameter_cd %in% c("00060","00065","99909")),]
PORprecip <- getDataAvailability(precipSite,interactive=FALSE)
PORprecip <- PORprecip[which(PORprecip$service=="uv"&PORprecip$parameter_cd=="00045"),]
if ((length(unique(POR$parameter_cd))+length(unique(POR$parameter_cd)))>=4) {
  if (max(POR$startDate)<=StartDt&min(POR$endDate)>=EndDt) {
    adaps_stage_in <- retrieveUnitNWISData(siteNo,'00065',StartDt,EndDt,format="tsv",interactive=FALSE)    
    adaps_discharge_in <- retrieveUnitNWISData(siteNo,'00060',StartDt,EndDt,format="tsv",interactive=FALSE)
    if (siteNo!=precipSite) {
      adaps_precip_in <- retrieveUnitNWISData(precipSite,'00045',StartDt,EndDt,format="tsv",interactive=FALSE)
    } else {
      adaps_precip_in <- retrieveUnitNWISData(siteNo,'00045',StartDt,EndDt,format="tsv",interactive=FALSE)
    }
    scode_url <- constructNWISURL(siteNo,'99909',StartDt,EndDt,"uv",format="tsv",interactive=FALSE)
    scode_url <- paste(scode_url,"&access=3",sep="")
    adaps_scode_in <- getRDB1Data(scode_url,asDateTime=TRUE)
    adaps_data<-merge(adaps_stage_in[c(1,2,3,5)],adaps_discharge_in[c(3,5)],by="datetime",all=T)
    adaps_data<-merge(adaps_precip_in[c(3,5)],adaps_data,by="datetime",all=T)
    adaps_data_all <- merge(adaps_data,adaps_scode_in[c(3,5)],by="datetime",all=T)
    colnames(adaps_data_all) <- c("datetime","X04_00045","agency_cd","site_no","X01_00065","X02_00060","X05_99909")
  } else {cat(paste("ADAPS data not available on via NWISWeb for selected site, date range and parameter codes","\n",sep=" "))}
}} else {
  adaps_data_in <- read.delim(dataFile,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),strip.white=TRUE,fill=TRUE,comment.char="#")
  adaps_data_in <- adaps_data_in[-1, ]
  adaps_data_in$datetime <- as.POSIXct(strptime(paste(adaps_data_in$YEAR,sprintf("%02s",adaps_data_in$MONTH),sprintf("%02s",adaps_data_in$DAY),sprintf("%02d",as.numeric(adaps_data_in$MINUTE)%/%60),sprintf("%02d",as.numeric(adaps_data_in$MINUTE)%%60),sep=""),"%Y%m%d%H%M"))
  adaps_data_in$pcode <- substr(adaps_data_in$NAME,mean(nchar(adaps_data_in$NAME))-4,mean(nchar(adaps_data_in$NAME)))
  adaps_scode <- adaps_data_in[which(adaps_data_in$pcode=="99909"),c("datetime","VALUE")]
  colnames(adaps_scode) <- c("datetime","p99909")
  adaps_stage <- adaps_data_in[which(adaps_data_in$pcode=="00065"),c("datetime","VALUE")]
  colnames(adaps_stage) <- c("datetime","p00065")
  adaps_precip <- adaps_data_in[which(adaps_data_in$pcode=="00045"),c("datetime","VALUE")]
  colnames(adaps_precip) <- c("datetime","p00045")
  adaps_disch <- adaps_data_in[which(adaps_data_in$pcode=="00060"),c("datetime","VALUE")]
  colnames(adaps_disch) <- c("datetime","p00060")
  
  adaps_data <- merge(adaps_stage,adaps_disch,by="datetime",all=T)
  adaps_data <- merge(adaps_precip,adaps_data,by="datetime",all=T)
  adaps_data <- merge(adaps_scode,adaps_data,by="datetime",all=T)
  adaps_data$p00065 <- as.numeric(adaps_data$p00065)
  adaps_data$p00060 <- as.numeric(adaps_data$p00060)
  adaps_data$p00045 <- as.numeric(adaps_data$p00045)
  adaps_data_all <- data.frame(adaps_data,rep("USGS",nrow(adaps_data)),rep(siteNo,nrow(adaps_data)),stringsAsFactors=FALSE)
  colnames(adaps_data_all) <- c("datetime","X05_99909","X04_00045","X02_00060","X01_00065","agency_cd","site_no")
}
for (i in 1:nrow(adaps_data_all)) {
  adaps_data_all$cum_00045[i] <- sum(adaps_data_all$X04_00045[1:i],na.rm=TRUE)
}
return(adaps_data_all)
}