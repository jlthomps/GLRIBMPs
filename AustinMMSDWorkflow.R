###########################################################################
# Do once:
#library(devtools)
#install.packages(c("USGSwsData","USGSwsBase","USGSwsGraphs","USGSwsQW","USGSwsStats","dataRetrieval","USGSwsQWSR"),repos="http://usgs-r.github.com")
###########################################################################

library(dataRetrieval)
siteNo <- "04087119"
StartDt <- "2008-10-01"
EndDt <- "2014-09-01"
adaps_disch_in <- getNWISunitData(siteNo,'00060',StartDt,EndDt,format="xml")
colnames(adaps_disch_in) <- c("agency","siteNo","pdate","tz_cd","disch","rmrk")
adaps_cond_in <- getNWISunitData(siteNo,'00095',StartDt,EndDt,format="xml")
colnames(adaps_cond_in) <- c("agency","siteNo","pdate","tz_cd","cond","rmrk")
adaps_turb_in <- getNWISunitData(siteNo,'63680',StartDt,EndDt,format="xml")
colnames(adaps_turb_in) <- c("agency","siteNo","pdate","tz_cd","turb","rmrk")
adaps_temp_in <- getNWISunitData(siteNo,'00010',StartDt,EndDt,format="xml")
colnames(adaps_temp_in) <- c("agency","siteNo","pdate","tz_cd","temp","rmrk")
adaps_do_in <- getNWISunitData(siteNo,'00300',StartDt,EndDt,format="xml")
colnames(adaps_do_in) <- c("agency","siteNo","pdate","tz_cd","do","rmrk")

#00530 TSS
tss_data <- getNWISqwData(siteNo,'00530',StartDt,EndDt,expanded=TRUE)

#00665 Phosphorus
tp_data <- getNWISqwData(siteNo,'00665',StartDt,EndDt,expanded=TRUE)

#00940 Chloride
cl_data <- getNWISqwData(siteNo,'00940',StartDt,EndDt,expanded=TRUE)

#31616 fecal coliform
fecal_data <- getNWISqwData(siteNo,'31616',StartDt,EndDt,expanded=TRUE)

#50468 E coli
ecoli_data <- getNWISqwData(siteNo,'50468',StartDt,EndDt,expanded=TRUE)

library(lubridate)
adaps_cond_in$pdate2 <- force_tz(adaps_cond_in$pdate,tzone="UTC")
adaps_disch_in$pdate2 <- force_tz(adaps_disch_in$pdate,tzone="UTC")
adaps_do_in$pdate2 <- force_tz(adaps_do_in$pdate,tzone="UTC")
adaps_temp_in$pdate2 <- force_tz(adaps_temp_in$pdate,tzone="UTC")
adaps_turb_in$pdate2 <- force_tz(adaps_turb_in$pdate,tzone="UTC")

# Merge response variable samples 
dataMerge <- merge(cl_data[,c(1:4,8)],fecal_data[,c(1,3:4,8)],by="dateTime",all=TRUE)
dataMerge <- merge(dataMerge,tss_data[,c(1,3,4,8)],by="dateTime",all=TRUE)
dataMerge <- merge(dataMerge,tp_data[,c(1,4,5,9)],by="dateTime",all=TRUE)
dataMerge <- merge(dataMerge,ecoli_data[,c(1,3,4,8)],by="dateTime",all=TRUE)
colnames(dataMerge) <- c("dateTime","site","remark00940Cl","result00940Cl","rptlev00940Cl","remark31616Fec","result31616Fec","rptlev31616Fec","remark00530TSS","result00530TSS","rptlev00530TSS","remark00665TP","result00665TP","rptlev00665TP","remark50468Ec","result50468Ec","rptlev50468Ec")

# Add nearest instantaneous measurements to response variable samples

n <- nrow(dataMerge)
for (i in 1:n) {
  adaps_disch_sub <- adaps_disch_in[adaps_disch_in$pdate2<=dataMerge$dateTime[i]+(15*60) & adaps_disch_in$pdate2>=dataMerge$dateTime[i]-(15*60),]
  adaps_cond_sub <- adaps_cond_in[adaps_cond_in$pdate2<=dataMerge$dateTime[i]+(15*60) & adaps_cond_in$pdate2>=dataMerge$dateTime[i]-(15*60),]
  adaps_do_sub <- adaps_do_in[adaps_do_in$pdate2<=dataMerge$dateTime[i]+(15*60) & adaps_do_in$pdate2>=dataMerge$dateTime[i]-(15*60),]
  adaps_temp_sub <- adaps_temp_in[adaps_temp_in$pdate2<=dataMerge$dateTime[i]+(15*60) & adaps_temp_in$pdate2>=dataMerge$dateTime[i]-(15*60),]
  adaps_turb_sub <- adaps_turb_in[adaps_turb_in$pdate2<=dataMerge$dateTime[i]+(15*60) & adaps_turb_in$pdate2>=dataMerge$dateTime[i]-(15*60),]  
  if (nrow(adaps_disch_sub)>0) {
    dataMerge$q[i] <- adaps_disch_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_disch_sub$pdate2)))),c("disch")]
  } else {
    dataMerge$q[i] <- NA
  }
  if (nrow(adaps_do_sub)>0) {
    dataMerge$do[i] <- adaps_do_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_do_sub$pdate2)))),c("do")]
  } else {
    dataMerge$do[i] <- NA
  }
  if (nrow(adaps_cond_sub)>0) {
    dataMerge$cond[i] <- adaps_cond_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_cond_sub$pdate2)))),c("cond")]
  } else {
    dataMerge$cond[i] <- NA
  }
  if (nrow(adaps_temp_sub)>0) {
    dataMerge$temp[i] <- adaps_temp_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_temp_sub$pdate2)))),c("temp")]
  } else {
    dataMerge$temp[i] <- NA
  }
  if (nrow(adaps_turb_sub)>0) {
    dataMerge$turb[i] <- adaps_turb_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_turb_sub$pdate2)))),c("turb")]
  } else {
    dataMerge$turb[i] <- NA
  }
}

dataMerge$decYear <- getDecYear(dataMerge$dateTime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)

##############################################################################
# Regression for Computed Chloride

data_sub <- dataMerge[!is.na(dataMerge$result00940Cl),]
#data_sub <- data_sub[which(data_sub$dateTime<strptime("2011-10-01",format="%Y-%m-%d")),]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev31616Fec","result31616Fec","remark31616Fec","remark00530TSS","result00530TSS","rptlev00530TSS","result00665TP","remark00665TP","rptlev00665TP","rptlev50468Ec","remark50468Ec","result50468Ec","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark00940Cl","result00940Cl","rptlev00940Cl","q","decYear","sinDY","cosDY")]
data_sub$result00940Cl <- log(data_sub$result00940Cl)
data_sub$rptlev00940Cl <- log(data_sub$rptlev00940Cl)

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result00940Cl","remark00940Cl","","rptlev00940Cl","User","tons","Unk","","00940","CompCl")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Honey Creek"
siteINFO <-  getNWISSiteInfo(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompCl"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/MMSD/",siteName,sep="")

# ##########################################################
# # Preliminary Assessment Plots:
# # pdf(paste(pathToSave,"/InitialQQGraphs",investigateResponse,".pdf",sep=""))
# pdf(paste(pathToSave,"/",investigateResponse,"_InitialQQGraphs.pdf",sep=""))
# plotQQTransforms(data_sub_cens,investigateResponse)
# predictVariableScatterPlots(data_sub_cens,investigateResponse)
# dev.off()
# ##########################################################

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

##############################################################################
# Regression for Computed Fec

data_sub <- dataMerge[!is.na(dataMerge$result00530TSS),]
#data_sub <- data_sub[which(data_sub$remark50468Ec!='>'),]
#data_sub <- data_sub[which(data_sub$dateTime<strptime("2011-10-01",format="%Y-%m-%d")),]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev00665TP","result00665TP","remark00665TP","remark50468Ec","result50468Ec","rptlev50468Ec","result00940Cl","remark00940Cl","rptlev00940Cl","rptlev31616Fec","remark31616Fec","result31616Fec","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark00530TSS","result00530TSS","rptlev00530TSS","q","decYear","sinDY","cosDY")]
data_sub$result00530TSS <- log(data_sub$result00530TSS)
data_sub$rptlev00530TSS <- log(data_sub$rptlev00530TSS)

# set necessary site information and inputs to step-wise regression
library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result00530TSS","remark00530TSS","","rptlev00530TSS","User","tons","Unk","","00530","CompTSS")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Honey Creek"
siteINFO <-  getNWISSiteInfo(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompTSS"
# choose 'normal' or 'lognormal' distribution for data
transformResponse <- "lognormal"

pathToSave <- paste("/Users/jlthomps/Documents/R/MMSD/",siteName,sep="")

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


