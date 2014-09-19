###########################################################################
# Do once:
#library(devtools)
#install.packages(c("USGSwsData","USGSwsBase","USGSwsGraphs","USGSwsQW","USGSwsStats","dataRetrieval","USGSwsQWSR"),repos="http://usgs-r.github.com")
###########################################################################

# pull in pre-calculated QW load values
library(dataRetrieval)
source("/Users/jlthomps/Desktop/git/GLRIBMPs/pullLoadData.R")
setwd('C:/Users/jlthomps/Documents/R/')
loadFile <- "WBF1VolumesLoads.csv"
storm_vol_load <- pullLoadData(loadFile)

# use load info to pull in precipitation data from ADAPS and calculate storm info using rainmaker
library(stringr)
siteNo <- "434034088252401"
StartDt <- strftime(min(storm_vol_load$Start,na.rm=TRUE) - (60*60*24*5),'%Y-%m-%d')
EndDt <- strftime(max(storm_vol_load$End,na.rm=TRUE) + (60*60*24*5),'%Y-%m-%d')

storm_vol_load$decYear <- paste(strftime(storm_vol_load$End,"%Y"),".",as.POSIXlt(storm_vol_load$End)$yday+1,sep="")
storm_vol_load$sinDY <- sin(as.numeric(storm_vol_load$decYear)*2*pi)
storm_vol_load$cosDY <- cos(as.numeric(storm_vol_load$decYear)*2*pi)
storm_vol_load$month_val <- substr(strftime(storm_vol_load$Start,"%Y-%m-%d"),6,7)
storm_vol_load$year_val <- substr(strftime(storm_vol_load$Start,"%Y-%m-%d"),1,4)
storm_vol_load$day_val <- substr(strftime(storm_vol_load$Start,"%Y-%m-%d"),9,10)
storm_vol_load$jul_val <- storm_vol_load$Start$yday+1
storm_vol_load$wy_val <- ifelse(as.numeric(storm_vol_load$month_val)>=10,as.character(as.numeric(storm_vol_load$year_val)+1),storm_vol_load$year_val) 

#choose columns to keep for analysis
keepAll <- c("decYear","Tpload","sinDY","cosDY","stormRunoff","Precip","P5max","P10max","P15max","P30max","P60max","EI","wy_val")
data_sub <- storm_vol_load[,keepAll]
data_sub$remark <- ""

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
data_sub2013 <- data_sub[data_sub$wy_val=="2013",]
keepCens <- keepAll[-which(keepAll %in% c("Tpload","wy_val"))]
data_sub_cens <- importQW(data_sub2013,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12013"
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

##################################################################################
#Run w/o using StormRunoff
# set necessary site information and inputs to step-wise regression
library(GSqwsr)
keepCens <- keepAll[-which(keepAll %in% c("Tpload","stormRunoff","wy_val"))]
data_sub_cens <- importQW(data_sub2013,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12013NoRunoff"
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


##################################################################################
#Run w/o using StormRunoff or Precip
# set necessary site information and inputs to step-wise regression
library(GSqwsr)
keepCens <- keepAll[-which(keepAll %in% c("Tpload","stormRunoff","Precip","wy_val"))]
data_sub_cens <- importQW(data_sub2013,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12013NoRunoffNoPrecip"
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

######################################## WY2014
# set necessary site information and inputs to step-wise regression
library(GSqwsr)
data_sub2014 <- data_sub[data_sub$wy_val=="2014",]
keepCens <- keepAll[-which(keepAll %in% c("Tpload","wy_val"))]
data_sub_cens <- importQW(data_sub2014,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12014"
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

##################################################################################
#Run w/o using StormRunoff
# set necessary site information and inputs to step-wise regression
library(GSqwsr)
keepCens <- keepAll[-which(keepAll %in% c("Tpload","stormRunoff","wy_val"))]
data_sub_cens <- importQW(data_sub2014,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12014NoRunoff"
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


##################################################################################
#Run w/o using StormRunoff or Precip
# set necessary site information and inputs to step-wise regression
library(GSqwsr)
keepCens <- keepAll[-which(keepAll %in% c("Tpload","stormRunoff","Precip","wy_val"))]
data_sub_cens <- importQW(data_sub2014,keepCens,"Tpload","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "WBF12014NoRunoffNoPrecip"
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

