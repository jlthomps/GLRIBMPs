###########################################################################
# Do once:
#library(devtools)
#install.packages(c("USGSwsData","USGSwsBase","USGSwsGraphs","USGSwsQW","USGSwsStats","dataRetrieval","USGSwsQWSR"),repos="http://usgs-r.github.com")
###########################################################################

# pull in pre-calculated QW load values
library(dataRetrieval)
source("/Users/jlthomps/Desktop/git/GLRIBMPs/pullLoadData.R")
setwd('/Users/jlthomps/Documents/R/')
loadFile <- "EastRiverVolumesLoads.csv"
storm_vol_load <- pullLoadData(loadFile)

# use load info to pull in precipitation data from ADAPS and calculate storm info using rainmaker
library(stringr)
siteNo <- "441624088045601"
StartDt <- strftime(min(storm_vol_load[which(storm_vol_load$frozen=='N'),]$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
EndDt <- strftime(max(storm_vol_load[which(storm_vol_load$frozen=='N'),]$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')
adaps_precip_in <- retrieveUnitNWISData(siteNo,'00045',StartDt,EndDt,format="tsv")
df <- adaps_precip_in[,c(5,3)]
colnames(df) <- c("rain","pdate")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/RMarf.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/RMEvents.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/RMIntense.R")
# choose desired dry interval between storms
stormInt <- 2
# choose desired rain amount threshold, in units of precip values
rainAmt <- 0
rainmaker_out <- as.data.frame(RMevents(df,ieHr=stormInt,rainthresh=rainAmt,rain="rain",time="pdate")[1])
# choose desired intensity values (ie 5 minute, 10 minute, 60 minute)
intens <- c(5,10,15,30,60)
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=intens)
# choose antecedent rain intervals (ie 1 day, 5 days)
arfDays <- c(1,3,5,7)
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=arfDays,varnameout="ARF")
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")
# pull in previously saved ADAPS soil moisture unit data b/c soil moisture from NWISWeb starts at 20130513
#adaps_soilm_in <- retrieveUnitNWISData(siteNo,'74207',StartDt,EndDt,format="tsv")
adaps_soilmoisture <- read.csv("glri_soilmoisture.txt",header=T,stringsAsFactors=FALSE,sep="\t")
adaps_soilmoisture$time <- str_pad(adaps_soilmoisture$TIME,6,side="left",pad="0")
adaps_soilmoisture$pdate <- as.POSIXct(paste(adaps_soilmoisture$DATE,adaps_soilmoisture$time),format="%Y%m%d %H%M%S")
# merge data for storm loads and rainmaker output
source("/Users/jlthomps/Desktop/git/GLRIBMPs/stormLoadMatchup.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/stormLoadMatchupSplit.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/stormLoadMatchupSoilMoisture.R")
#choose columns to keep for analysis
keepVars <- names(storm_rainmaker)[-which(names(storm_rainmaker) %in% c("stormnum","StartDate.x","EndDate.x","StartDate.y","EndDate.y","rain.y"))]
#keepAll <- c("decYear","TPLoad","peakDisch","sinDY","cosDY",keepVars)
#data_sub <- stormLoadMatchup(storm_rainmaker,storm_vol_load,keepAll)
keepAll <- c("decYear","TPLoad","peakDisch","sinDY","cosDY","soil_rain_value","soil_storm_value","num",keepVars)
data_sub <- stormLoadMatchupSoilMoisture(storm_rainmaker,storm_vol_load,adaps_soilmoisture,keepAll)

DTMaxCols <- na.omit(data_sub)
DTMaxRows <- data_sub[,colSums(is.na(data_sub)) <= nrow(data_sub)*0.5] 
DTMaxRows <- na.omit(DTMaxRows)

#List columns removed to maximize rows:
names(data_sub)[!(names(DTMaxCols) %in% names(data_sub))]
names(data_sub)[!(names(DTMaxRows) %in% names(data_sub))]
setdiff(data_sub$num,DTMaxRows$num)
setdiff(data_sub$num,DTMaxCols$num)
#Choose which to use: DTMaxCols or DTMaxRows:
DT <- data_sub[,c("TPLoad",names(DTMaxCols))]
data_sub <- na.omit(DT)

DT <- data_sub[,c("TPLoad",names(DTMaxRows))]
data_sub <- na.omit(DT)

# set necessary site information and inputs to step-wise regression
library(USGSwsQWSR)
keepCens <- keepAll[-which(keepAll %in% "TPLoad")]
data_sub_cens <- importQW(data_sub,keepCens,"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "EastRiver"
siteINFO <-  getSiteFileData(siteNo, interactive=FALSE)
# name of value column in data_sub_cens object
investigateResponse <- "TPLoading"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/GLRI/",siteName,sep="")

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
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
colnames(steps) <- c("step","BIC","Deviance","Resid.Dev","Resid.Df","Correlation","Slope","RMSE","PRESS","scope","response")


#Save plotSteps to file:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRI.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGLRI(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
##########################################################

##########################################################
# Import model parameters from csv file if desired:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#Example of how to remove auto-generated outliers:
outliers <- findOutliers(modelReturn,data_sub_cens,transformResponse)
if(length(outliers) >0) data_sub_cens <- data_sub_cens[-outliers,]
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(data_sub_cens,investigateResponse,newFormula,
                                  transformResponse=transformResponse)
steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
plotStepsGLRI(steps,data_sub_cens,transformResponse)
##########################################################

##########################################################
# Or, don't do the stepwise regression, just get the model coefficients using csv file:
modelReturn <- censReg(paste(investigateResponse," ~ ", newFormula, sep=""), dist=transformResponse, data=data_sub_cens)
#####################################################

#####################################################
#Save NEW plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps_2.pdf",sep=""))
plotStepsGLRI(steps,data_sub_cens,transformResponse)
dev.off()
#####################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, steps, siteINFO, saveOutput=TRUE,fileName)
#####################################################


#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  

# Probably want to save data at this point:
fileToSave <- paste(pathToSave, "regressionData.csv",sep="/")
write.table(data_sub, fileToSave, row.names=FALSE, sep=",")