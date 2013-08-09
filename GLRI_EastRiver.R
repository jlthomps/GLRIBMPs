setwd('/Users/jlthomps/Documents/R/')
#storm_vol_load <- read.csv("Yard_storm_vol_load.csv",header=T,stringsAsFactors=FALSE)
#storm_vol_load <- read.csv("McCoy16_storm_vol_load.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load <- read.csv("EastRiverVolumesLoads.csv",header=T,stringsAsFactors=FALSE)
#storm_rainmaker <- read.csv("Yard_rainmaker.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")
#source("M:/NonPoint Evaluation/GLRI Edge-of-field/R/RRainmaker.R")
#library(dataRetrieval)
site_no <- "441624088045601"
StartDt <- strftime(min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
#EndDt <- strftime(max(storm_vol_load[which(storm_vol_load$frozen=='N'),]$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')
#storm_rainmaker <- retrieveNWISData(site_no,'00060',StartDt,EndDt,StatCd='00003',format="tsv")
#df <- storm_rainmaker[,3:4]
#colnames(df) <- c("pdate","rain")
#df$pdate <- as.POSIXct(strftime(df$pdate))
#rainmaker_out <- RMevents(df,ieHr=.25,rainthresh=0.1,rain="rain",time="pdate")
storm_rainmaker <- read.csv("EastRiverRainmaker.csv",header=T,stringsAsFactors=FALSE)
storm_rainmaker$startdate <- strptime(storm_rainmaker$startdate,format="%m/%d/%Y %H:%M")
storm_rainmaker$enddate <- strptime(storm_rainmaker$enddate,format="%m/%d/%Y %H:%M")
storm_vol_load_sub <- storm_vol_load[which(storm_vol_load$frozen=='N' & storm_vol_load$estimated=='N'),]
# removing two partial storms (the other pieces of them are estimates) and one that looks like an outlier and had an incorrect split at the lab
storm_vol_load_sub <- storm_vol_load_sub[which(storm_vol_load_sub$num!=9 & storm_vol_load_sub$num!=57 & storm_vol_load_sub$num!=14),]
storms_list <- storm_vol_load_sub[,1:2]
storms_list$Start <- storms_list$Start - (120*60)
storms_list$num <- c(1:nrow(storms_list))
norows <- nrow(storm_vol_load_sub)
noreps <- nrow(storm_rainmaker)
storm_rainmaker$num <- -9
for (i in 1:noreps) {
  for (j in 1:norows) {
    storm_rainmaker$num[i] <- ifelse(as.numeric(storm_rainmaker$startdate[i]-storms_list$Start[j])*as.numeric(storms_list$End[j]-storm_rainmaker$enddate[i])>=0,storms_list$num[j],storm_rainmaker$num[i])
  }
}
#storm_rainmaker_agg <- aggregate(storm_rainmaker,list(storm_rainmaker$num), c(min(x$startdate),max(x$enddate),max(x$theisen),max(x$p5max.inches.per.hour),max(x$p10max.inches.per.hour),max(x$p15max.inches.per.hour),max(x$p30max.inches.per.hour),max(x$p60max.inches.per.hour),max(x$ei)))
#storm_rainmaker_agg <- ddply(storm_rainmaker[,2:3], c("storm_rainmaker$num"), summarize, min_start=min(df$enddate),max_theisen=max(df$theisen))
storm_rainmaker_agg_startdt <- aggregate(storm_rainmaker[,1],list(storm_rainmaker$num), min)
storm_rainmaker_agg_enddt <- aggregate(storm_rainmaker[,2],list(storm_rainmaker$num),max)
storm_rainmaker_agg <- aggregate(storm_rainmaker[,3:9],list(storm_rainmaker$num),max)
data_merge <- merge(storm_rainmaker_agg,storm_rainmaker_agg_startdt,by.x="Group.1",by.y="Group.1")
data_merge <- merge(data_merge,storm_rainmaker_agg_enddt,by.x="Group.1",by.y="Group.1")
storm_vol_load_sub$num <- c(1:nrow(storm_vol_load_sub))
data_merge <- merge(data_merge,storm_vol_load_sub,by.x="Group.1",by.y="num")
data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",data_merge$End$yday+1,sep="")
data_sub <- data_merge[,c(3:8,17,24,27)]
colnames(data_sub) <- c("p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ei","stormRunoff","TPLoad","decYear")
data_sub$stormRunoff <- ifelse(data_sub$stormRunoff==-9,0,data_sub$stormRunoff)
data_sub$p5max.inches.per.hour <- ifelse(data_sub$p5max.inches.per.hour==-9,0,data_sub$p5max.inches.per.hour)
data_sub$p10max.inches.per.hour <- ifelse(data_sub$p10max.inches.per.hour==-9,0,data_sub$p10max.inches.per.hour)
data_sub$p15max.inches.per.hour <- ifelse(data_sub$p15max.inches.per.hour==-9,0,data_sub$p15max.inches.per.hour)
data_sub$p30max.inches.per.hour <- ifelse(data_sub$p30max.inches.per.hour==-9,0,data_sub$p30max.inches.per.hour)
data_sub$p60max.inches.per.hour <- ifelse(data_sub$p60max.inches.per.hour==-9,0,data_sub$p60max.inches.per.hour)
data_sub$ei <- ifelse(data_sub$ei==-9,0,data_sub$ei)
data_sub$TPLoad <- ifelse(data_sub$TPLoad==-9,0,data_sub$TPLoad)
data_sub$remark <- ""

storm_vol_load_sub$decYear <- paste(strftime(storm_vol_load_sub$End,"%Y"),".",storm_vol_load_sub$End$yday+1,sep="")
data_all_storms <- storm_vol_load_sub[,c(8,15,18)]
data_all_storms <- data_all_storms[data_all_storms$stormRunoff!=-9,]
data_all_storms$remark <- ""

# qw_url<-"http://www.waterqualitydata.us/Result/search?siteid=USGS-05382257&pCode=00530&startDateLo=10-01-1990&startDateHi=07-30-2013&command.avoid=STORET&mimeType=tab&zip=no"
# McCoyYard_qw<-read.delim(qw_url,header=TRUE,quote="\"",dec=".",sep="\t",colClasses=c("character"),fill=TRUE)
# McCoyYard_qw$dateTime<-strptime(paste(McCoyYard_qw$ActivityStartDate, 
#                                     McCoyYard_qw$ActivityStartTime.Time, sep = " "), "%Y-%m-%d %H:%M:%S")
# startDate<-strftime(min(McCoyYard_qw$dateTime),'%Y-%m-%d')
# endDate<-strftime(max(McCoyYard_qw$dateTime),'%Y-%m-%d')
# McCoyYard_dailydisch<-retrieveNWISData('05382257','00060',startDate,endDate,StatCd="00003")
# McCoyYard_dailytemp<-retrieveNWISData('05382257','00010',startDate,endDate,StatCd="00003")
# McCoyYard_dailyprecip<-retrieveNWISData('05382257','00045',startDate,endDate,StatCd="00006")
# siteName <- "McCoyYard_raw"
# siteName$station.nm <- "McCoy Yard"
# investigateResponse <- "p00530"
# pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
# pdf(paste(pathToSave,"/",investigateResponse,"_raw.pdf",sep=""))
# par(mfrow=c(4,1))
# plot(McCoyYard_dailydisch$dateTime,McCoyYard_dailydisch$value,type="l",ylab="Discharge 00060")
# plot(McCoyYard_qw$dateTime,McCoyYard_qw$ResultMeasureValue,col="red")
# plot(McCoyYard_dailytemp$dateTime,McCoyYard_dailytemp$value,type="l",ylab="Temp 00010")
# plot(McCoyYard_dailyprecip$dateTime,McCoyYard_dailyprecip$value,type="l",ylab="Precip 00045")
# dev.off()

library(GLRIRegression)
library(dataRetrieval)
siteName <- "EastRiver"
siteName$station.nm <- "East River 441624088045601"
investigateResponse <- "TPLoading"
transformResponse <- "normal"
pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
pdf(paste(pathToSave,"/",investigateResponse,"_regression.pdf",sep=""))
plot(data_sub$stormRunoff,data_sub$TPLoad,xlab="runoff vol",ylab="Total P load")
lm_mod <- lm(TPLoad~stormRunoff,data=data_sub)
abline(lm_mod,col="red")
lm_coef <- coef(lm_mod)
mtext(bquote(y==.(lm_coef[2])*x+.(lm_coef[1])),adj=1,padj=0)
mtext(bquote(R2==.(summary(lm_mod)$adj.r.squared)),adj=0,padj=3)
dev.off()

pathToSave <- paste("C:/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
data_sub_cens <- importQW(data_sub,c("p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ei","stormRunoff","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
#data_sub_cens <- importQW(data_all_storms,c("volume.cf","decYear"),"p00530_tons","remark","",0.005,"User","tons","Unk","","00530","p00530")
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
analyzeSteps(steps, investigateResponse,siteName, xCorner = 0.01)
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
resultPlots(data_sub_cens,modelReturn,siteName)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(data_sub_cens,modelReturn,siteName)
dev.off()
#####################################################

#####################################################
# Plot predictions using unit values:
library(USGSwsBase)
# StartDt <- min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5)
# UV <- readNWIS(site_no,dtype="uv",begin.date=StartDt,param='00060')
UV <- retrieveUnitNWISData(site_no,'00060',StartDt,'2013-07-30',"tsv")
#pdf(paste(pathToSave,"/",investigateResponse,"_prediction_2.pdf",sep=""))
#predictionPlot(UV,data_sub_cens,modelReturn,transformResponse,siteName)
#dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintout(modelReturn, siteName, saveOutput=TRUE,fileName)
#####################################################


#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  
