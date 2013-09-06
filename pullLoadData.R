#'pullLoadData
#'
#'function to load in pre-calculated QW load data for use in regressions
#'
#'@param loadFile string name of csv file containing calculated QW loads and storm start and end dates
#'@return storm_vol_load data frame containing storm start and end dates, calculated load values and various attributes
#' loadFile <- "EastRiverVolumesLoads.csv"
#' pullLoadData(loadFile)
pullLoadData <- function(loadFile) {
storm_vol_load <- read.csv(loadFile,header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%Y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%Y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","num_split","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad","stormName")
return(storm_vol_load)
}