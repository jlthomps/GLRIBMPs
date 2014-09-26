###########################################################################
# Do once:
#library(devtools)
#install.packages(c("USGSwsData","USGSwsBase","USGSwsGraphs","USGSwsQW","USGSwsStats","dataRetrieval","USGSwsQWSR"),repos="http://usgs-r.github.com")
###########################################################################

library(dataRetrieval)
siteNo <- "04087119"
StartDt <- "2008-10-01"
EndDt <- "2014-09-01"
adaps_disch_in <- retrieveNWISunitData(siteNo,'00060',StartDt,EndDt,format="xml")
colnames(adaps_disch_in) <- c("agency","siteNo","pdate","tz_cd","disch","rmrk")
adaps_cond_in <- retrieveNWISunitData(siteNo,'00095',StartDt,EndDt,format="xml")
colnames(adaps_cond_in) <- c("agency","siteNo","pdate","tz_cd","cond","rmrk")
adaps_turb_in <- retrieveNWISunitData(siteNo,'63680',StartDt,EndDt,format="xml")
colnames(adaps_turb_in) <- c("agency","siteNo","pdate","tz_cd","turb","rmrk")
adaps_temp_in <- retrieveNWISunitData(siteNo,'00010',StartDt,EndDt,format="xml")
colnames(adaps_temp_in) <- c("agency","siteNo","pdate","tz_cd","temp","rmrk")
adaps_do_in <- retrieveNWISunitData(siteNo,'00300',StartDt,EndDt,format="xml")
colnames(adaps_do_in) <- c("agency","siteNo","pdate","tz_cd","do","rmrk")

#00530 TSS
tss_data <- retrieveNWISqwData(siteNo,'00530',StartDt,EndDt,expanded=TRUE)

#00665 Phosphorus
tp_data <- retrieveNWISqwData(siteNo,'00665',StartDt,EndDt,expanded=TRUE)

#00940 Chloride
cl_data <- retrieveNWISqwData(siteNo,'00940',StartDt,EndDt,expanded=TRUE)

#31616 fecal coliform
fecal_data <- retrieveNWISqwData(siteNo,'31616',StartDt,EndDt,expanded=TRUE)

#50468 E coli
ecoli_data <- retrieveNWISqwData(siteNo,'50468',StartDt,EndDt,expanded=TRUE)


#choose columns to keep for analysis
keepVars <- names(storm_rainmaker)[-which(names(storm_rainmaker) %in% c("stormnum","StartDate.x","EndDate.x","StartDate.y","EndDate.y","rain.y"))]
keepAll <- c("decYear","TPLoad","peakDisch","sinDY","cosDY",keepVars)
data_sub <- stormLoadMatchup(storm_rainmaker,storm_vol_load,keepAll)

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
#Example of how to remove auto-generated outliers:
outliers <- findOutliers(modelReturn,data_sub_cens,transformResponse)
if(length(outliers) >0) data_sub_cens <- data_sub_cens[-outliers,]
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

##########################################################
# Import model parameters from csv file if desired:
pathToParam <- paste(pathToSave,"/",investigateResponse,"ModelParams.csv",sep="")
choicesNew <- read.csv(pathToParam)
newFormula <-createFormulaFromDF(choicesNew)
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