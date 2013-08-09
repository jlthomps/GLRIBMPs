###########################################################################
# Do once:
library(devtools)
install_github("USGSwsData", "USGS-R")
install_github("USGSwsBase", "USGS-R")
install.packages("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/Lorenz_R_script/Austin/USGSwsGraphs_0.7.zip", repos=NULL,type="source")
install.packages("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/Lorenz_R_script/Austin/USGSwsQW_0.4.zip", repos=NULL,type="source")
install.packages("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/Lorenz_R_script/Austin/USGSwsStats_0.5.zip", repos=NULL,type="source")

install.packages("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/GLRIRegression_1.0.1.tar.gz", repos=NULL,type="source")
###########################################################################


################################################################################
#Start here if data have already been pulled...I moved data retrieval to the bottom of this page:

library(GLRIRegression)
library(dataRetrieval)
##########################
# Required Inputs:
siteName <- "Jones"
investigateResponse <- "Phosphorus_WW.P"
transformResponse <- "lognormal" # Options are: "normal" or "lognormal"
# investigateResponse <- "Ammonia.N"  #Ammonia and ammonium
# investigateResponse <- "Nitrite.N"
# investigateResponse <- "Nitrate.N"
# investigateResponse <- "NO2PlusNO3.N"  #Nitrate plus Nitrite
# investigateResponse <- "NitrogenTotal_WW.sum"
# investigateResponse <- "OrthoPhosphate.P"
# investigateResponse <- "Phosphorus_WW.P"
# investigateResponse <- "Chloride"
# investigateResponse <- "SuspSed"
##########################


siteTable <- siteTable
pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,sep="")

load(paste(pathToSave,"QW.RData",sep="/"))
load(paste(pathToSave,"UV.RData",sep="/"))
load(paste(pathToSave,"QWcodes.RData",sep="/"))
load(paste(pathToSave,"siteINFO.RData",sep="/"))
load(paste(pathToSave,"DTComplete.RData",sep="/"))
DT <- DTComplete[c(investigateResponse,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
datetime <- DT$datetime
################################################################################




################################################################################
# Play with maximizing rows/columns
subDT <- DTComplete[c(investigateResponse,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
subDT <- subDT[,names(subDT) != investigateResponse]

#Rows removed to maximize columns (remove rows with any missing values):
DTMaxCols <- na.omit(subDT)

#Columns removed to maximize rows (Remove columns with more than 10% NAs):
DTMaxRows <- subDT[,colSums(is.na(subDT)) <= nrow(subDT)*0.5]  
DTMaxRows <- na.omit(DTMaxRows)

#List columns removed to maximize rows:
names(DTMaxCols)[!(names(DTMaxCols) %in% names(DTMaxRows))]


#Choose which to use: DTMaxCols or DTMaxRows:
DT <- DTComplete[,c(investigateResponse,names(DTMaxCols))]
DT <- na.omit(DT)

DT <- DTComplete[,c(investigateResponse,names(DTMaxRows))]
DT <- na.omit(DT)

################################################################################


#################################################################################
library(googleVis)
source("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/plotInteractive.R")
names(UV)
plotInteractive(UV, QW, investigateResponse,"YSI.6136.UP_Turb")
# plotInteractive(UV, QW, investigateResponse,"TURBIDITY.NTU_Turb")
# plotInteractive(UV, QW, investigateResponse,"Turb")
# plotInteractive(UV, QW, investigateResponse,"Wtemp")
################################################################################


################################################################################
# If you want to take out a UV set of data, for example YSI.6136_Turb:
UVnew <- UV[,-which(names(UV) %in% c("YSI.6026_Turb","YSI.6026_Turb_cd"))]
# UVnew <- UV[,-which(names(UV) %in% c("YSI.6136_Turb","YSI.6136_Turb_cd"))]
UVnew <- UVnew[complete.cases(UVnew),]
# mergeReturn <- mergeDatasets(QW, UVnew, QWcodes, max.diff="30 mins")
mergeReturn <- mergeDatasets(QW, UVnew, QWcodes, max.diff="2 hours")
DTComplete <- mergeReturn$DTComplete
QWcodes <- mergeReturn$QWcodes
DT <- DTComplete[c(investigateResponse,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
DT <- na.omit(DT)
datetime <- DT$datetime
################################################################################

##########################################################



##########################################################
# Preliminary Assessment Plots:
# pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(DT,investigateResponse)
predictVariableScatterPlots(DT,investigateResponse)
dev.off()
##########################################################



#################################################################################################
#Kitchen sink:
predictVariables <- names(DT)[-which(names(DT) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]

kitchenSink <- createFullFormula(DT,investigateResponse)

returnPrelim <- prelimModelDev(DT,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod

#Save plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,DT,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

vif(modelReturn)

#################################################################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

##########################################################
# Generate a csv file to customize model parameters (can do without running kitchen sink):
pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,"/",investigateResponse,sep="")
choices <- generateParamChoices(predictVariables,modelReturn,pathToSave,save=TRUE)
pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,sep="")
##########################################################

##########################################################
# Import model parameters from csv file:
pathToParam <- paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
##########################################################

##########################################################
#Example of how to remove auto-generated outliers:
outliers <- findOutliers(modelReturn,DT,transformResponse)
if(length(outliers) >0) DT <- DT[-outliers,]
##########################################################

##########################################################
#If you want to re-do stepwise regression:
returnPrelimNew <- prelimModelDev(DT,investigateResponse,newFormula,
                                  transformResponse=transformResponse)
steps <- returnPrelimNew$steps
modelResult <- returnPrelimNew$modelStuff
modelReturn <- returnPrelimNew$DT.mod
plotSteps(steps,DT,transformResponse)
##########################################################


##########################################################
# Or, don't do the stepwise regression, just get the model coefficients using csv file:
modelReturn <- censReg(paste(investigateResponse," ~ ", newFormula, sep=""), dist=transformResponse, data=DT)
#####################################################

#####################################################
#Save NEW plotSteps to file:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_plotSteps_2.pdf",sep=""))
plotSteps(steps,DT,transformResponse)
dev.off()
#####################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_summaryPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(DT,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_summaryResidPlot_2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(DT,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Plot predictions using unit values:
pdf(paste(pathToSave,"/",investigateResponse,"/",investigateResponse,"_prediction_2.pdf",sep=""))
predictionPlot(UV,DT,modelReturn,transformResponse,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSave,"/",investigateResponse,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

#####################################################
# save model results:
# saveModelResults(pathToSave, DT, UV, modelReturn, siteINFO)  #Not sure this is the best structure
#####################################################

#Want to save a dataframe (aka, save an output)?
fileToSave <- paste(pathToSave, "modelResult.csv",sep="/")
write.table(modelResult, fileToSave, row.names=FALSE, sep=",")  

#Want to export all the UV data to csv file?
fileToSave <- paste(pathToSave, "UV.csv",sep="/")
write.table(UV, fileToSave, row.names=FALSE, sep=",") 

############################################################################
############################################################################
############################################################################
# Create models without using WQ variables
# (Flow, log(flow), sinDY, and cosDY only):

siteTable <- siteTable
siteName <- "Grand"
investigateResponse <- "Phosphorus_WW.P"
transformResponse <- "lognormal" # Options are: "normal" or "lognormal"

pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels2/",siteName,sep="")

load(paste(pathToSave,"QW.RData",sep="/"))
load(paste(pathToSave,"UV.RData",sep="/"))
load(paste(pathToSave,"QWcodes.RData",sep="/"))
load(paste(pathToSave,"siteINFO.RData",sep="/"))
load(paste(pathToSave,"DTComplete.RData",sep="/"))
#######################################################

# Create new pathToSave:
pathToSaveNew <- paste(pathToSave,"/",investigateResponse,"/withoutWQ/",sep="")
dir.create(file.path(pathToSaveNew), showWarnings = FALSE)

#####################################################
# create model:
DT <- DTComplete[c(investigateResponse,"Flow", "decYear","sinDY","cosDY","datetime")]
# Or, if flow is not available:
# DT <- DTComplete[c(investigateResponse, "decYear","sinDY","cosDY","datetime")]

DT <- na.omit(DT)
datetime <- DT$datetime
predictVariables <- names(DT)[-which(names(DT) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]

kitchenSink <- createFullFormula(DT,investigateResponse)

returnPrelim <- prelimModelDev(DT,investigateResponse,kitchenSink,
                               "BIC", #Other option is "AIC"
                               transformResponse)

steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod
#####################################################

##########################################################
#Save plotSteps to file:
pdf(paste(pathToSaveNew, investigateResponse,"_plotSteps.pdf",sep=""))
plotSteps(steps,DT,transformResponse)
dev.off()

pdf(paste(pathToSaveNew, investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeSteps(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()
#####################################################

##########################################################
#Save steps to file:
fileToSave <- paste(pathToSaveNew, investigateResponse,"_steps.csv",sep="")
write.table(steps, fileToSave, row.names=FALSE, sep=",") 
##########################################################

#####################################################
# Plot summary plots:
pdf(paste(pathToSaveNew, investigateResponse,"_summaryPlot2.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultPlots(DT,modelReturn,siteINFO)
dev.off()

pdf(paste(pathToSaveNew,investigateResponse,"_summaryResidPlot.pdf",sep=""), paper="a4r") #a4r makes it landscape...if you want that
resultResidPlots(DT,modelReturn,siteINFO)
dev.off()
#####################################################

#####################################################
# Plot predictions using unit values:
pdf(paste(pathToSaveNew,investigateResponse,"_prediction.pdf",sep=""))
predictionPlot(UV,DT,modelReturn,transformResponse,siteINFO)
dev.off()
#####################################################

#####################################################
# Print summary in console:
fileName <- paste(pathToSaveNew,investigateResponse,"Summary.txt", sep="")
summaryPrintout(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################






#######################################################
# Data Retrieval:
# Only do this for a new site:
##########################
# Required Inputs:
site <- "04119400"
siteName <- "Grand"
##########################

siteINFO <-  getSiteFileData(site, interactive=FALSE)
UVcodes <- whatUV(site)

# The reported end date in NWIS must be later than the endDate in the function below or it filters it out 
#(so we aren't retrieving data that ended before the real-time sensors went on line):
QWcodes <- whatQW(site, minCount=20, endDate="2012-06-01",ignoreGroups="Information")

# To output to csv a table of parameters for a given site
#fileToSave <- paste(pathToSave, "manitowocQW.csv",sep="/")
#write.table(QWcodes, fileToSave, row.names=FALSE, sep=",")

AustinList <- c("00940","00608","00613","00631","62855","00671","00665","80154","00618")
# AustinList <- c("00940","00530","00665","50468","31616")  #MMSD Realtime sites
QWcodes <- QWcodes[which(QWcodes$parameter_cd %in% AustinList),]

UVPList <- c("00010", "00060", "00095", "00400", "63680", "00300")
# UVPList <- c("00010", "00060", "00095", "63680", "00300")   #MMSD Realtime sites
UVP <- unique(UVcodes$pCode[which(UVcodes$pCode %in% UVPList)])

BeginDate <- max(UVcodes$StartDate[which(UVcodes$pCode %in% UVP)])

QW <- importNWISqw(site, params=AustinList, begin.date=BeginDate)
QW$datetime <- as.POSIXct(paste(QW$sample_dt," ",QW$sample_tm, ":00",sep=""),tz="UTC")


UV <- getMultipleUV(site, BeginDate, UVP)
UV <- getMultipleUV(site, "2011-04-09", UVPList)

mergeReturn <- mergeDatasets(QW, UV, QWcodes)
DTComplete <- mergeReturn$DTComplete
QWcodes <- mergeReturn$QWcodes

pathToSave <- paste("M:/QW Monitoring Team/GLRI Nutrients/GLRI nutrients/Regressions/DataForModels/",siteName,sep="")

# Probably want to save data at this point:
save(QW,file=paste(pathToSave,"QW.RData",sep="/"))
save(UV,file=paste(pathToSave,"UV.RData",sep="/"))
save(QWcodes,file=paste(pathToSave,"QWcodes.RData",sep="/"))
save(DTComplete,file=paste(pathToSave,"DTComplete.RData",sep="/"))
save(siteINFO,file=paste(pathToSave,"siteINFO.RData",sep="/"))
################################################################################
################################################################################
################################################################################
