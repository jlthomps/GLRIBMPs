setwd('/Users/jlthomps/Desktop/git/GLRIBMPs/')
storm_vol_load <- read.csv("EastRiverVolumesLoads.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%Y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%Y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","num_split","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")
#source("M:/NonPoint Evaluation/GLRI Edge-of-field/R/RRainmaker.R")
library(Rainmaker)
library(dataRetrieval)
site_no <- "441624088045601"
StartDt <- strftime(min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
#EndDt <- strftime(max(storm_vol_load[which(storm_vol_load$frozen=='N'),]$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')
#storm_rainmaker <- retrieveNWISData(site_no,'00060',StartDt,EndDt,StatCd='00003',format="tsv")
adaps_precip_in <- read.csv("eastRiverPrecip.rdb",header=T,stringsAsFactors=FALSE,sep="\t",comment.char="#")
library(stringr)
adaps_precip_in$time <- str_pad(adaps_precip_in$TIME,6,side="left",pad="0")
adaps_precip_in$pdate <- as.POSIXct(paste(adaps_precip_in$DATE,adaps_precip_in$time),format="%Y%m%d %H%M%S")
df <- adaps_precip_in[,c(4,10)]
colnames(df) <- c("rain","pdate")
df$rain <- as.numeric(df$rain)
rainmaker_out <- as.data.frame(RMevents(df,ieHr=2,rainthresh=0,rain="rain",time="pdate")[1])
colnames(rainmaker_out) <- c("stormnum","StartDate","EndDate","rain")
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,10,15,30,60))
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=c(1,3,5,7),varnameout="ARF")
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")
adaps_soilmoisture <- read.csv("eastRiverSoilMoisture.rdb",header=T,stringsAsFactors=FALSE,sep="\t",comment.ch="#")
adaps_soilmoisture$time <- str_pad(adaps_soilmoisture$TIME,6,side="left",pad="0")
adaps_soilmoisture$pdate <- as.POSIXct(paste(adaps_soilmoisture$DATE,adaps_soilmoisture$time),format="%Y%m%d %H%M%S")
#storm_rainmaker <- read.csv("EastRiverRainmaker.csv",header=T,stringsAsFactors=FALSE)
#storm_rainmaker$startdate <- strptime(storm_rainmaker$startdate,format="%m/%d/%Y %H:%M")
#storm_rainmaker$enddate <- strptime(storm_rainmaker$enddate,format="%m/%d/%Y %H:%M")
storm_vol_load_sub <- storm_vol_load[which(storm_vol_load$frozen=='N' & storm_vol_load$estimated=='N'),]
storm_vol_load_Start <- aggregate(storm_vol_load_sub$Start, list(storm_vol_load_sub$num), FUN = min)
colnames(storm_vol_load_Start) <- c("num","Start")
storm_vol_load_End <- aggregate(storm_vol_load_sub$End, list(storm_vol_load_sub$num), FUN = max)
colnames(storm_vol_load_End) <- c("num","End")
storm_vol_load_peak <- aggregate(storm_vol_load_sub$peakDisch, list(storm_vol_load_sub$num), FUN = max)
colnames(storm_vol_load_peak) <- c("num","peakDisch")
storm_vol_load_load <- aggregate(storm_vol_load_sub$TPLoad, list(storm_vol_load_sub$num), FUN = sum)
colnames(storm_vol_load_load) <- c("num","TPLoad")
storm_vol_load_merge <- merge(storm_vol_load_load, storm_vol_load_Start, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_End, by = "num")
storm_vol_load_merge <- merge(storm_vol_load_merge, storm_vol_load_peak, by = "num")
# removing two partial storms (the other pieces of them are estimates)
#storm_vol_load_sub <- storm_vol_load_sub[which(storm_vol_load_sub$num!=9 & storm_vol_load_sub$num!=57),]
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
data_merge$day_match <- data_merge$x.x-60
data_merge$start_match <- data_merge$Start-60
data_merge <- data_merge[which(data_merge$x.x>=min(adaps_soilmoisture$pdate,na.rm=TRUE)),]
data_merge$soil_rain <- adaps_soilmoisture[findInterval(data_merge$day_match,sort(adaps_soilmoisture$pdate)),]$VALUE
data_merge$soil_storm <- adaps_soilmoisture[findInterval(data_merge$start_match,sort(adaps_soilmoisture$pdate)),]$VALUE
#data_sub <- data_merge[,c(2:11,14:15,21:22,29,32,35,36)]
data_sub <- data_merge[,c(2:11,14:16,19:20,23:24)]
#data_sub <- data_merge[,c(2,16,19:20)]

#data_sub <- data_merge[,c(3:8,24,27)]
#colnames(data_sub) <- c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","peakDisch","stormRunoff","TPLoad","decYear","soil_rain","soil_storm")
colnames(data_sub) <- c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","TPLoad","peakDisch","decYear","soil_rain","soil_storm")
#colnames(data_sub) <- c("intensity","TPLoad","peakDisch","decYear")
#data_sub$stormRunoff <- ifelse(data_sub$stormRunoff==-9,0,data_sub$stormRunoff)
data_sub$p5max.inches.per.hour <- ifelse(data_sub$p5max.inches.per.hour==-9,0,data_sub$p5max.inches.per.hour)
data_sub$p10max.inches.per.hour <- ifelse(data_sub$p10max.inches.per.hour==-9,0,data_sub$p10max.inches.per.hour)
data_sub$p15max.inches.per.hour <- ifelse(data_sub$p15max.inches.per.hour==-9,0,data_sub$p15max.inches.per.hour)
data_sub$p30max.inches.per.hour <- ifelse(data_sub$p30max.inches.per.hour==-9,0,data_sub$p30max.inches.per.hour)
data_sub$p60max.inches.per.hour <- ifelse(data_sub$p60max.inches.per.hour==-9,0,data_sub$p60max.inches.per.hour)
data_sub$intensity <- ifelse(data_sub$intensity==-9,0,data_sub$intensity)
data_sub$soil_rain <- as.numeric(data_sub$soil_rain)
data_sub$soil_storm <- as.numeric(data_sub$soil_storm)
data_sub$TPLoad <- ifelse(data_sub$TPLoad==-9,0,data_sub$TPLoad)
data_sub$remark <- ""

aov_data <- aov(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch*soil_rain*soil_storm,data_sub)
reg_lm <- lm(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch*soil_rain*soil_storm,data=data_sub)

siteName <- "EastRiverSoilMoisture"
investigateResponse <- "TPLoading"
transformResponse <- "lognormal"
pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/",investigateResponse,"_regressionA.pdf",sep=""))
par(mfrow=c(4,1))
#plot(adaps_soilmoisture$pdate,adaps_soilmoisture$VALUE,type="l",ylab="soil moisture")
#par(new=TRUE)
plot(adaps_precip_in$pdate,adaps_precip_in$VALUE,type="l",col="blue",ylab="precip")
#par(new=TRUE)
plot(storm_vol_load$Start,storm_vol_load$peakDisch,col="red")
#par(new=TRUE)
plot(storm_vol_load$End,storm_vol_load$peakDisch,col="red")
dev.off()

library(GSqwsr)
library(dataRetrieval)
siteName <- "EastRiverSoilMoisture"
siteNo <- '441624088045601'
siteINFO <-  getNWISSiteInfo(siteNo)
siteINFO$station.nm <- siteINFO$station_nm
investigateResponse <- "TPLoading"
transformResponse <- "lognormal"
pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/",investigateResponse,"_regression.pdf",sep=""))
plot(data_sub$ARF1,log(data_sub$TPLoad),xlab="ARF1",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~ARF1,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$ARF3,log(data_sub$TPLoad),xlab="ARF3",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~ARF3,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$ARF5,log(data_sub$TPLoad),xlab="ARF5",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~ARF5,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
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
plot(data_sub$p5max.inches.per.hour,log(data_sub$TPLoad),xlab="p5max",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~p5max.inches.per.hour,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$p10max.inches.per.hour,log(data_sub$TPLoad),xlab="p10max",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~p10max.inches.per.hour,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$p15max.inches.per.hour,log(data_sub$TPLoad),xlab="p15max",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~p15max.inches.per.hour,data=data_sub)
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
plot(data_sub$p60max.inches.per.hour,log(data_sub$TPLoad),xlab="p60max",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~p60max.inches.per.hour,data=data_sub)
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
plot(data_sub$soil_rain,log(data_sub$TPLoad),xlab="soil_rain",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~soil_rain,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
plot(data_sub$soil_storm,log(data_sub$TPLoad),xlab="soil_storm",ylab="Total P load")
lm_mod <- lm(log(TPLoad)~soil_storm,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
dev.off()

pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
data_sub_cens <- importQW(data_sub,c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","peakDisch","decYear","soil_rain","soil_storm"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I15","I30","I60","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
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
#predictVariables <- predictVariables[which(predictVariables != "stormRunoff")]
kitchenSink <- createFullFormula(data_sub_cens,investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                                     "BIC", transformResponse)
steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod

#Save plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,data_sub_cens,transformResponse)
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

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
#pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,"/",investigateResponse,sep="")
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
#pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,sep="")
##########################################################

##########################################################
# Import model parameters from csv file:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
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
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################


#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  
