#'whatQW
#'
#'Get QW parameters for a USGS site
#'
#'@param site character USGS site ID (usually 8 digits)
#'@param minCount number filter out parameters with less than this number
#'@param endDate text
#'@param startDate text
#'@param getFullNames logical to make a slow call to get the full names of parameters
#'@param ignoreGroups character filter out certain parameter groups (this can only be used if getFullNames was TRUE)
#'@return QWcodes dataframe
#'@keywords qw data retrieval
#'@export
#'@examples
#'site <- "04027000"
#'qw1 <- whatQW(site, getFullNames=FALSE)
whatQW <- function(site, minCount=50, endDate="2013-01-01", startDate = "",
                   getFullNames=TRUE, ignoreGroups=c("Physical", "Information")){
  
  urlQWcodes <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",site,sep = "")
  
  QWcodes <- read.delim(  
    urlQWcodes, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  QWcodes <- QWcodes[-1,]
  
  QWcodes <- with(QWcodes, data.frame(parameter_cd=parm_cd, statCd=stat_cd, 
                  startDate=begin_date,endDate=end_date, count=count_nu,
                  service=data_type_cd,stringsAsFactors = FALSE))
  
  QWcodes <- QWcodes[!is.na(QWcodes$parameter_cd),]
  QWcodes <- QWcodes["" != QWcodes$parameter_cd,]
  
  QWcodes$startDate <- as.Date(QWcodes$startDate)
  QWcodes$endDate <- as.Date(QWcodes$endDate)
  
  QWcodes$count <- as.numeric(QWcodes$count)
  
  QWcodes <- QWcodes["qw" == QWcodes$service,]
  QWcodes$statCd <- NULL
  QWcodes <- QWcodes[QWcodes$count > minCount,]
  
  if (nchar(endDate) > 0) QWcodes <- QWcodes[QWcodes$endDate > as.Date(endDate),]
  if (nchar(startDate) > 0) QWcodes <- QWcodes[QWcodes$startDate < as.Date(startDate),]
  
  if (getFullNames){
    pCodeINFO <- getMultipleParameterNames(QWcodes$parameter_cd, interactive = TRUE)
    QWcodes <- merge(QWcodes,pCodeINFO,all=TRUE)    
    for(i in ignoreGroups) QWcodes <- QWcodes[QWcodes$parameter_group_nm != i,]
  }
  return(QWcodes)
}