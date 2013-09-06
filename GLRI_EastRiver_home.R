setwd('/Users/jlthomps/Documents/R/')
storm_vol_load <- read.csv("EastRiverVolumesLoads.csv",header=T,stringsAsFactors=FALSE)
storm_vol_load$Start <- strptime(storm_vol_load$Start,format="%m/%d/%Y %H:%M")
storm_vol_load$Stop <- strptime(storm_vol_load$Stop,format="%m/%d/%Y %H:%M")
colnames(storm_vol_load) <- c("Start","End","estimated","type","frozen","num","num_split","peakDisch","stormRunoff","SSLoad","ChlorideLoad","NitrateLoad","AmmoniumLoad","TKNLoad","DissPLoad","TPLoad","TNLoad","OrgNLoad")
source("/Users/jlthomps/GLRIBMPs/RRainmaker.R")
library(dataRetrieval)
site_no <- "441624088045601"
StartDt <- strftime(min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
EndDt <- strftime(max(storm_vol_load[which(storm_vol_load$frozen=='N'),]$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')
adaps_precip_in <- retrieveUnitNWISData(site_no,'00045',StartDt,EndDt,format="tsv")
library(stringr)
colnames(adaps_precip_in) <- c("agency_cd","site_no","pdate","tz_cd","rain","remark")
df <- adaps_precip_in[,c(5,3)]
colnames(df) <- c("rain","pdate")

rainmaker_out <- as.data.frame(RMevents(df,ieHr=2,rainthresh=0,rain="rain",time="pdate")[1])
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=c(5,10,15,30,60))
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=c(1,3,5,7),varnameout="ARF")
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")

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
data_merge <- merge(data_merge,storm_vol_load_merge,by.x="Group.1",by.y="num")
data_merge$decYear <- paste(strftime(data_merge$End,"%Y"),".",as.POSIXlt(data_merge$End)$yday+1,sep="")

data_sub <- data_merge[,c(2:11,14:16,19:20)]
colnames(data_sub) <- c("intensity","I5","I10","I15","I30","I60","ARF1","ARF3","ARF5","ARF7","rain","duration","TPLoad","peakDisch","decYear")
data_sub$TPLoad <- log10(data_sub$TPLoad)

reg_lm <- lm(TPLoad~intensity+ARF7+peakDisch+rain+ARF5+duration+I60+I5+log(intensity)+log(I30)+log(duration)+log(peakDisch),data=data_sub)

library(GLRIRegression)
library(dataRetrieval)
siteName$site <- "EastRiver"
siteName$station.nm <- "East River 441624088045601"
investigateResponse <- "TPLoad"
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")

##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(data_sub,investigateResponse)
predictVariableScatterPlots(data_sub,investigateResponse)
dev.off()
##########################################################



#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub)[-which(names(data_sub) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
predictVariables <- predictVariables[which(predictVariables != "stormRunoff")]
kitchenSink <- createFullFormula(data_sub,c("TPLoad","remark","I30","ARF3","ARF5","I15"))

k <- log(length(data_sub[,investigateResponse]))
distribution <- transformResponse
formulaToUse <- paste(investigateResponse," ~ 1",sep="")

DT.mod <- do.call("stepAIC", args=list(object=call("lm",
                                                   formulaToUse,
                                                   data=data_sub),
                                       scope=list(lower= ~ 1, upper=formula(paste("~ ",kitchenSink,sep=""))), k=k, keep = extractAIC))
#Plot residuals
plot(data_sub$TPLoad,DT.mod$residuals,xlab="log(TP Load)",ylab="residuals")

#returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
 #                                    "BIC", transformResponse)
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
# Print summary in console:
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintout(modelReturn, siteName, saveOutput=TRUE,fileName)
#####################################################


#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  
