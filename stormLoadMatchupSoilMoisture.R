#'stormLoadMatchupSoilMoisture
#'
#'function to match up data output from rainmaker functions as well as calculated storm loads
#'
#'@param storm_rainmaker data frame containing output from RMIntense and RMarf
#'@param storm_vol_load data frame of prepared calculated load data for storms
#'@param keepAll vector of column names for desired variables
#'@return data_sub data frame of merged storm load data and rainmaker data
stormLoadMatchupSoilMoisture <- function(storm_rainmaker,storm_vol_load,adaps_soilmoisture,keepAll) {

storm_vol_load_sub <- storm_vol_load[which(storm_vol_load$frozen=='N' & storm_vol_load$estimated=='N'),]
storm_vol_load_Start <- aggregate(storm_vol_load_sub$Start, list(storm_vol_load_sub$num_split), FUN = min)
colnames(storm_vol_load_Start) <- c("num","Start")
storm_vol_load_End <- aggregate(storm_vol_load_sub$End, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_End) <- c("num","End")
storm_vol_load_peak <- aggregate(storm_vol_load_sub$peakDisch, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_peak) <- c("num","peakDisch")
storm_vol_load_load <- aggregate(storm_vol_load_sub$TPLoad, list(storm_vol_load_sub$num_split), FUN = sum)
colnames(storm_vol_load_load) <- c("num","TPLoad")
storm_vol_load_merge <- merge(storm_vol_load_load, storm_vol_load_Start, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_End, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_peak, by = "num")

storms_list <- storm_vol_load_merge[,c(3,4)]
storms_list$Start <- storms_list$Start - (120*60)
storms_list$num <- c(1:nrow(storms_list))
norows <- nrow(storm_vol_load_merge)
noreps <- nrow(storm_rainmaker)
storm_rainmaker$stormnum <- -9
for (i in 1:noreps) {
  for (j in 1:norows) {
    storm_rainmaker$stormnum[i] <- ifelse(as.numeric(storm_rainmaker$StartDate.x[i]-storms_list$Start[j])*as.numeric(storms_list$End[j]-storm_rainmaker$EndDate.x[i])>=0,storms_list$num[j],storm_rainmaker$stormnum[i])
  }
}

storm_rainmaker_agg_startdt <- aggregate(storm_rainmaker$StartDate.x,list(storm_rainmaker$stormnum), min)
storm_rainmaker_agg_enddt <- aggregate(storm_rainmaker$EndDate.x,list(storm_rainmaker$stormnum),max)
storm_rainmaker_agg_sum <- aggregate(storm_rainmaker[,4:5],list(storm_rainmaker$stormnum),sum)
storm_rainmaker_agg <- aggregate(storm_rainmaker[,c(6:11,15:18)],list(storm_rainmaker$stormnum),max)
data_merge <- merge(storm_rainmaker_agg,storm_rainmaker_agg_startdt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_enddt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_sum,by.x="Group.1",by.y="Group.1")
storm_vol_load_merge$num <- c(1:nrow(storm_vol_load_merge))
data_merge <- merge(storm_vol_load_merge,data_merge,by.x="num",by.y="Group.1",all.x=TRUE)
data_merge$day_match <- data_merge$x.x-60
data_merge$start_match <- data_merge$Start-60
soil_rain_match <- findInterval(data_merge$day_match,sort(adaps_soilmoisture$pdate))
soil_storm_match <- findInterval(data_merge$start_match,sort(adaps_soilmoisture$pdate))
stormnum <- 1:length(soil_rain_match)
soil_rain_match_df <- data.frame(soil_rain_match,stormnum)
soil_rain_match_df_sub <- soil_rain_match_df[which(soil_rain_match>0),]
soil_rain_match_df_sub$value <- adaps_soilmoisture$VALUE[soil_rain_match_df_sub$soil_rain_match]
soil_storm_match_df <- data.frame(soil_storm_match,stormnum)
soil_storm_match_df_sub <- soil_storm_match_df[which(soil_storm_match>0),]
soil_storm_match_df_sub$value <- adaps_soilmoisture$VALUE[soil_storm_match_df_sub$soil_storm_match]
colnames(soil_storm_match_df_sub) <- c("soil_storm_match","stormnum","soil_storm_value")
colnames(soil_rain_match_df_sub) <- c("soil_rain_match","stormnum","soil_rain_value")
data_merge <- merge(data_merge,soil_storm_match_df_sub,by.x="num",by.y="stormnum",all.x=TRUE)
data_merge <- merge(data_merge,soil_rain_match_df_sub,by.x="num",by.y="stormnum",all.x=TRUE)
data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",as.POSIXlt(data_merge$End)$yday+1,sep="")
data_merge$sinDY <- sin(as.numeric(data_merge$decYear)*2*pi)
data_merge$cosDY <- cos(as.numeric(data_merge$decYear)*2*pi)
data_sub <- data_merge[,keepAll]
data_sub$remark <- ""

return(data_sub)
}