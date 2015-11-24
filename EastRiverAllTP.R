setwd('/Users/jlthomps/Desktop/git/GLRIBMPs/')
load("dataSubEastRiverAll.RData")

aov_data <- aov(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch,data_sub)
reg_lm <- lm(TPLoad~intensity*p5max.inches.per.hour*p10max.inches.per.hour*p15max.inches.per.hour*p30max.inches.per.hour*p60max.inches.per.hour*ARF1*ARF3*ARF5*ARF7*rain_amount*duration*peakDisch,data=data_sub)

# for pre-BMP installation at this site, limit to data before November of 2014
data_sub <- data_sub[which(data_sub$decYear<2014.3),]

library(GSqwsr)
library(dataRetrieval)
siteName <- "EastRiverAll"
siteNo <- '441624088045601'
siteINFO <-  readNWISsite(siteNo)
siteINFO$station.nm <- siteINFO$station_nm
investigateResponse <- "TPLoading"
transformResponse <- "lognormal"


pathToSave <- paste("/Users/jlthomps/Documents/R/GLRI/",siteName[1],sep="")
data_sub_cens <- importQW(data_sub,c("intensity","p5max.inches.per.hour","p10max.inches.per.hour","p15max.inches.per.hour","p30max.inches.per.hour","p60max.inches.per.hour","ARF1","ARF3","ARF5","ARF7","rain_amount","duration","peakDisch","frozen","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
data_sub_cens2 <- data_sub_cens[,c(1:13,15:16)]
##########################################################
# Preliminary Assessment Plots:
pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
plotQQTransforms(data_sub_cens2,investigateResponse)
predictVariableScatterPlots(data_sub_cens2,investigateResponse)
dev.off()
##########################################################

#################################################################################################
#Kitchen sink:
predictVariables <- names(data_sub_cens)[-which(names(data_sub_cens) %in% investigateResponse)]
predictVariables <- predictVariables[which(predictVariables != "datetime")]
predictVariables <- predictVariables[which(predictVariables != "decYear")]
predictVariables <- predictVariables[which(predictVariables != "frozen")]
kitchenSink <- createFullFormula(data_sub_cens2,investigateResponse)

returnPrelim <- prelimModelDev(data_sub_cens,investigateResponse,kitchenSink,
                               "BIC", transformResponse)
steps <- returnPrelim$steps
modelResult <- returnPrelim$modelStuff
modelReturn <- returnPrelim$DT.mod

#Save plotSteps to file:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsGLRIAll.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsGLRIAll(steps,data_sub_cens,transformResponse)
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

