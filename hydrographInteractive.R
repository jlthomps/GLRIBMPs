#' Function to return hydrographInteractive html to create interactive hydrograph plot through googleVis
#' 
#' This function accepts a merged data frame of ADAPS data for a site/storm event
#' 
#' @param adaps_data_all data frame containing merged ADAPS data for the requested storm event
#' @return hydrographInteractive html object containing code to create googleVis plot
#' @export
#' @examples
#' siteNo <- "434425090462401"
#' StartDt <- '2013-10-03'
#' EndDt <- '2013-10-05'
#' precipSite <- "434425090462401"
#' adaps_data_all <- getADAPSData(siteNo,StartDt,EndDt,precipSite)
#' hydrographInteractive(adaps_data_all)
hydrographInteractive <- function(adaps_data_all) {
  adaps_data_samples <- adaps_data_all[which(adaps_data_all$X05_99909>0),c("datetime","X02_00060")]
  adaps_data_plot <- adaps_data_all[,c("datetime","cum_00045","X01_00065","X02_00060")]
  rain_data <- data.frame(adaps_data_all[,c("datetime","cum_00045")],rep("Rain",nrow(adaps_data_all)),rep(NA,nrow(adaps_data_all)),stringsAsFactors=FALSE)
  names(rain_data) <- c("datetime","value","name","label")
  stage_data <- data.frame(adaps_data_all[,c("datetime","X01_00065")],rep("Stage",nrow(adaps_data_all)),rep(NA,nrow(adaps_data_all)),stringsAsFactors=FALSE)
  names(stage_data) <- c("datetime","value","name","label")
  disch_data <- data.frame(adaps_data_all[,c("datetime","X02_00060")],rep("Discharge",nrow(adaps_data_all)),rep(NA,nrow(adaps_data_all)),stringsAsFactors=FALSE)
  names(disch_data) <- c("datetime","value","name","label")
  sample_data <- data.frame(adaps_data_samples[,c("datetime","X02_00060")],rep("Sample Stage",nrow(adaps_data_samples)),adaps_data_samples[,c("X02_00060")],stringsAsFactors=FALSE)
  names(sample_data) <- c("datetime","value","name","label")
  plot_data <- rbind(rain_data,stage_data,disch_data,sample_data)
  hydrographInteractive <- gvisAnnotatedTimeLine(plot_data,datevar="datetime",numvar="value",idvar="name",annotationvar="label",options=list(colors="['blue','green','red','white']",displayAnnotations=TRUE,legendPosition="newRow",scaleColumns="[0,2]",scaleType='allfixed',width=1500, height=700,thickness="[2,2,2,.5]"))
  
  return(hydrographInteractive)
}