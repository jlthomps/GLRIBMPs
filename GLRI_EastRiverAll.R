setwd('/Users/jlthomps-pr/git/GLRIBMPs/')
#read in storm event startdate, enddate, estimated, type, frozen, number, peakDisch, runoff amount and loads from file
storm_vol_load <- read.csv("EastRiverVolumesLoads.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%Y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%Y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","num_split","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")

library(Rainmaker)
library(dataRetrieval)
#read in file of precip exported from adaps (data is not all available via NWISWeb)
adaps_precip_in <- read.csv("eastRiverPrecip.rdb",header=T,stringsAsFactors=FALSE,sep="\t",comment.char="#")
library(stringr)
adaps_precip_in$time <- str_pad(adaps_precip_in$TIME,6,side="left",pad="0")
adaps_precip_in$pdate <- as.POSIXct(paste(adaps_precip_in$DATE,adaps_precip_in$time),format="%Y%m%d %H%M%S")
df <- adaps_precip_in[,c(4,10)]
colnames(df) <- c("rain","pdate")
df$rain <- as.numeric(df$rain)

# run Rainmaker on imported precip data
rainmaker_out <- as.data.frame(RMevents(df,ieHr=.5,rainthresh=0,rain="rain",time="pdate")[1])
colnames(rainmaker_out) <- c("stormnum","StartDate","EndDate","rain")
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,10,15,30,60))
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=c(1,3,5,7),varnameout="ARF")
source("/Users/jlthomps-pr/git/GLRIBMPs/RMErosivityIndex.R")
#erosivity_index <- RMErosivityIndex(df,storm_rainmaker)
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")

# keep all - estimated, frozen, etc
storm_vol_load_sub <- storm_vol_load
# keep all events split out, goes to 118 because of two combined
storm_vol_load_Start <- aggregate(storm_vol_load_sub$Start, list(storm_vol_load_sub$num_split), FUN = min)
colnames(storm_vol_load_Start) <- c("num","Start")
storm_vol_load_End <- aggregate(storm_vol_load_sub$End, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_End) <- c("num","End")
storm_vol_load_peak <- aggregate(storm_vol_load_sub$peakDisch, list(storm_vol_load_sub$num_split), FUN = max)
colnames(storm_vol_load_peak) <- c("num","peakDisch")
storm_vol_load_load <- aggregate(storm_vol_load_sub$TPLoad, list(storm_vol_load_sub$num_split), FUN = sum)
colnames(storm_vol_load_load) <- c("num","TPLoad")
storm_vol_info <- unique(storm_vol_load_sub[,c(3,5,7)])
colnames(storm_vol_info) <- c("estimated","frozen","num")
storm_vol_load_merge <- merge(storm_vol_load_load, storm_vol_load_Start, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_End, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_peak, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_info, by="num")

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
#storm_rainmaker_agg <- aggregate(storm_rainmaker,list(storm_rainmaker$num), c(min(x$startdate),max(x$enddate),max(x$theisen),max(x$p5max.inches.per.hour),max(x$p10max.inches.per.hour),max(x$p15max.inches.per.hour),max(x$p30max.inches.per.hour),max(x$p60max.inches.per.hour),max(x$ei)))
#storm_rainmaker_agg <- ddply(storm_rainmaker[,2:3], c("storm_rainmaker$num"), summarize, min_start=min(df$enddate),max_theisen=max(df$theisen))
# aggregate data to the storm level, using min start, max end, sum of rain and duration and max of intensities and ARFs. down to 14
storm_rainmaker_agg_startdt <- aggregate(storm_rainmaker$StartDate.x,list(storm_rainmaker$stormnum), min)
storm_rainmaker_agg_enddt <- aggregate(storm_rainmaker$EndDate.x,list(storm_rainmaker$stormnum),max)
storm_rainmaker_agg_sum <- aggregate(storm_rainmaker[,4:5],list(storm_rainmaker$stormnum),sum)
storm_rainmaker_agg <- aggregate(storm_rainmaker[,c(6:11,15:18)],list(storm_rainmaker$stormnum),max)
data_merge <- merge(storm_rainmaker_agg,storm_rainmaker_agg_startdt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_enddt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_sum,by.x="Group.1",by.y="Group.1")
storm_vol_load_merge$num <- c(1:nrow(storm_vol_load_merge))
data_merge <- merge(data_merge,storm_vol_load_merge,by.x="Group.1",by.y="num")
data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",as.POSIXlt(data_merge$End)$yday+1,sep="")
data_sub <- data_merge[,c(2:11,14:16,19:22)]
colnames(data_sub) <- c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","TPLoad","peakDisch","estimated","frozen","decYear")
data_sub$p5max.inches.per.hour <- ifelse(data_sub$p5max.inches.per.hour==-9,0,data_sub$p5max.inches.per.hour)
data_sub$p10max.inches.per.hour <- ifelse(data_sub$p10max.inches.per.hour==-9,0,data_sub$p10max.inches.per.hour)
data_sub$p15max.inches.per.hour <- ifelse(data_sub$p15max.inches.per.hour==-9,0,data_sub$p15max.inches.per.hour)
data_sub$p30max.inches.per.hour <- ifelse(data_sub$p30max.inches.per.hour==-9,0,data_sub$p30max.inches.per.hour)
data_sub$p60max.inches.per.hour <- ifelse(data_sub$p60max.inches.per.hour==-9,0,data_sub$p60max.inches.per.hour)
data_sub$intensity <- ifelse(data_sub$intensity==-9,0,data_sub$intensity)
data_sub$TPLoad <- ifelse(data_sub$TPLoad==-9,0,data_sub$TPLoad)
data_sub$remark <- ""

# save data_sub with merged data ready for regression
save(data_sub,file="dataSubEastRiverAll.RData")

aov_data <- aov(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch,data_sub)
reg_lm <- lm(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch,data=data_sub)

siteName <- "EastRiverAll"
investigateResponse <- "TPLoading"
transformResponse <- "lognormal"
pathToSave <- paste("/Users/jlthomps-pr/Documents/R/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/",investigateResponse,"_regressionA.pdf",sep=""))
par(mfrow=c(4,1))
plot(adaps_precip_in$pdate,adaps_precip_in$VALUE,type="l",col="blue",ylab="precip")
plot(storm_vol_load$Start,storm_vol_load$peakDisch,col="red")
plot(storm_vol_load$End,storm_vol_load$peakDisch,col="red")
dev.off()

library(GSqwsr)
library(dataRetrieval)
siteName <- "EastRiverAll"
siteNo <- '441624088045601'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- siteINFO$station_nm
investigateResponse <- "TPLoading"
transformResponse <- "lognormal"
pathToSave <- paste("/Users/jlthomps-pr/Documents/R/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/",investigateResponse,"_regression.pdf",sep=""))
plot(data_sub$ARF7,log(data_sub$TPLoad),xlab="ARF7",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~ARF7,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$rain_amount,log(data_sub$TPLoad),xlab="rain",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~rain_amount,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$intensity,log(data_sub$TPLoad),xlab="intensity",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~intensity,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$peakDisch,log(data_sub$TPLoad),xlab="peakDisch",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~peakDisch,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$p30max.inches.per.hour,log(data_sub$TPLoad),xlab="p30max",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~p30max.inches.per.hour,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$duration,log(data_sub$TPLoad),xlab="duration",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~duration,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
dev.off()

pathToSave <- paste("/Users/jlthomps-pr/Documents/R/GLRI/",siteName[1],sep="")
data_sub_cens <- importQW(data_sub,c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")

##########################################################
# Preliminary Assessment Plots:
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(data_sub_cens,investigateResponse)
predictVariableScatterPlots(data_sub_cens,investigateResponse)
dev.off()
##########################################################

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
predictVariables <- predictVariables[which(predictVariables != "estimated")]
predictVariables <- predictVariables[which(predictVariables != "frozen")]
kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                                     "BIC", transformResponse)
steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod

#Save plotSteps to file:
source("/Users/jlthomps-pr/git/GLRIBMPs/plotStepsGLRI.R")
source("/Users/jlthomps-pr/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGLRI(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

vif(modelReturn)

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
source("/Users/jlthomps-pr/git/GLRIBMPs/summaryPrintoutGLRI.R")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################


#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  
