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
getADAPSData <- function(siteNo,StartDt,EndDt,precipSite,stageFile="",dischFile="",precipFile="",scodFile="") {
if (length(stageFile<=1)) {
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
  } else {cat(paste("ADAPS data not available on via NWISWeb for selected site and date range","\n",sep=" "))}
}} else {
  adaps_stage_in <- read.delim(stageFile,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),fill=TRUE,comment.char="#")
  adaps_stage_in <- adaps_stage_in[-1, ]
  adaps_stage_in$DATETIME <- as.POSIXct(strptime(adaps_stage_in$DATETIME,"%Y%m%d%H%M%S"))
  adaps_stage_in <- adaps_stage_in[,c("DATETIME","VALUE")]
  colnames(adaps_stage_in) <- c("datetime","X01_00065")
  
  adaps_disch_in <- read.delim(dischFile,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),fill=TRUE,comment.char="#")
  adaps_disch_in <- adaps_disch_in[-1, ]
  adaps_disch_in$DATETIME <- as.POSIXct(strptime(adaps_disch_in$DATETIME,"%Y%m%d%H%M%S"))
  adaps_disch_in <- adaps_disch_in[,c("DATETIME","VALUE")]
  colnames(adaps_disch_in) <- c("datetime","X02_00060")
  
  adaps_precip_in <- read.delim(precipFile,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),fill=TRUE,comment.char="#")
  adaps_precip_in <- adaps_precip_in[-1, ]
  adaps_precip_in$DATETIME <- as.POSIXct(strptime(adaps_precip_in$DATETIME,"%Y%m%d%H%M%S"))
  adaps_precip_in <- adaps_precip_in[,c("DATETIME","VALUE")]
  colnames(adaps_precip_in) <- c("datetime","X04_00045")
  
  adaps_scode_in <- read.delim(scodFile,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),fill=TRUE,comment.char="#")
  adaps_scode_in <- adaps_scod_in[-1, ]
  adaps_scode_in$DATETIME <- as.POSIXct(strptime(adaps_scod_in$DATETIME,"%Y%m%d%H%M%S"))
  adaps_scode_in <- adaps_scod_in[,c("DATETIME","VALUE")]
  colnames(adaps_scode_in) <- c("datetime","X05_99909")
  
  adaps_data <- merge(adaps_stage_in,adaps_disch_in,by="datetime",all=T)
  adaps_data <- merge(adaps_precip_in,adaps_data,by="datetime",all=T)
  adaps_data$X01_00065 <- as.numeric(adaps_data$X01_00065)
  adaps_data$X02_00060 <- as.numeric(adaps_data$X02_00060)
  adaps_data$X04_00045 <- as.numeric(adaps_data$X04_00045)
  adaps_data <- data.frame(adaps_data,rep("USGS",nrow(adaps_data)),rep(siteNo,nrow(adaps_data)),stringsAsFactors=FALSE)
  adaps_data_all <- merge(adaps_data,adaps_scode_in,by="datetime",all=T)
  colnames(adaps_data_all) <- c("datetime","X01_00045","X02_00060","X01_00065","agency_cd","site_no")
}
for (i in 1:nrow(adaps_data_all)) {
  adaps_data_all$cum_00045[i] <- sum(adaps_data_all$X04_00045[1:i],na.rm=TRUE)
}
return(adaps_data_all)
}