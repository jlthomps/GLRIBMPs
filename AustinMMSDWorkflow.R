###########################################################################
# Do once:
#library(devtools)
#install.packages(c("USGSwsData","USGSwsBase","USGSwsGraphs","USGSwsQW","USGSwsStats","dataRetrieval","GSqwsr"),repos="http://usgs-r.github.com")
###########################################################################

library(dataRetrieval)
siteNo <- "04087088"
StartDt <- "2008-11-01"
EndDt <- "2009-12-31"
adaps_disch_in <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
StartDt <- "2010-01-01"
EndDt <- "2010-12-31"
adaps_disch_in2 <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in2 <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in2 <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in2 <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in2 <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
StartDt <- "2011-01-01"
EndDt <- "2011-12-31"
adaps_disch_in3 <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in3 <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in3 <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in3 <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in3 <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
StartDt <- "2012-01-01"
EndDt <- "2012-12-31"
adaps_disch_in4 <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in4 <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in4 <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in4 <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in4 <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
StartDt <- "2013-01-01"
EndDt <- "2013-12-31"
adaps_disch_in5 <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in5 <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in5 <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in5 <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in5 <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
StartDt <- "2014-01-01"
EndDt <- "2014-12-31"
adaps_disch_in6 <- readNWISuv(siteNo,'00060',StartDt,EndDt,tz="America/Chicago")
adaps_cond_in6 <- readNWISuv(siteNo,'00095',StartDt,EndDt,tz="America/Chicago")
adaps_turb_in6 <- readNWISuv(siteNo,'63680',StartDt,EndDt,tz="America/Chicago")
adaps_temp_in6 <- readNWISuv(siteNo,'00010',StartDt,EndDt,tz="America/Chicago")
adaps_do_in6 <- readNWISuv(siteNo,'00300',StartDt,EndDt,tz="America/Chicago")
adaps_disch_in <- rbind(adaps_disch_in,adaps_disch_in2,adaps_disch_in3,adaps_disch_in4,adaps_disch_in5,adaps_disch_in6)
colnames(adaps_disch_in) <- c("agency","siteNo","pdate","tz_cd","rmrk","disch")
adaps_cond_in <- rbind(adaps_cond_in,adaps_cond_in2,adaps_cond_in3,adaps_cond_in4,adaps_cond_in5,adaps_cond_in6)
colnames(adaps_cond_in) <- c("agency","siteNo","pdate","tz_cd","rmrk","cond")
adaps_turb_in <- rbind(adaps_turb_in,adaps_turb_in2,adaps_turb_in3,adaps_turb_in4,adaps_turb_in5,adaps_turb_in6)
colnames(adaps_turb_in) <- c("agency","siteNo","pdate","tz_cd","rmrk","turb")
adaps_temp_in <- rbind(adaps_temp_in,adaps_temp_in2,adaps_temp_in3,adaps_temp_in4,adaps_temp_in5,adaps_temp_in6)
colnames(adaps_temp_in) <- c("agency","siteNo","pdate","tz_cd","rmrk","temp")
adaps_do_in <- rbind(adaps_do_in,adaps_do_in2,adaps_do_in3,adaps_do_in4,adaps_do_in5,adaps_do_in6)
colnames(adaps_do_in) <- c("agency","siteNo","pdate","tz_cd","rmrk","do")

StartDt <- "2008-11-01"
EndDt <- "2014-12-31"

#00530 TSS
tss_data <- readNWISqw(siteNo,'00530',StartDt,EndDt,tz="America/Chicago")

#00665 Phosphorus
tp_data <- readNWISqw(siteNo,'00665',StartDt,EndDt,tz="America/Chicago")

#00940 Chloride
cl_data <- readNWISqw(siteNo,'00940',StartDt,EndDt,tz="America/Chicago")

#31616 fecal coliform
fecal_data <- readNWISqw(siteNo,'31616',StartDt,EndDt,tz="America/Chicago")

#50468 E coli
ecoli_data <- readNWISqw(siteNo,'50468',StartDt,EndDt,tz="America/Chicago")

#library(lubridate)
#adaps_cond_in$pdate2 <- force_tz(adaps_cond_in$pdate,tzone="UTC")
#adaps_disch_in$pdate2 <- force_tz(adaps_disch_in$pdate,tzone="UTC")
#adaps_do_in$pdate2 <- force_tz(adaps_do_in$pdate,tzone="UTC")
#adaps_temp_in$pdate2 <- force_tz(adaps_temp_in$pdate,tzone="UTC")
#adaps_turb_in$pdate2 <- force_tz(adaps_turb_in$pdate,tzone="UTC")

# Merge response variable samples 
dataMerge <- merge(cl_data[,c(2,14:15,19,23)],fecal_data[,c(14:15,19,23)],by="startDateTime",all=TRUE)
dataMerge <- merge(dataMerge,tss_data[,c(14:15,19,23)],by="startDateTime",all=TRUE)
dataMerge <- merge(dataMerge,tp_data[,c(14:15,19,23)],by="startDateTime",all=TRUE)
dataMerge <- merge(dataMerge,ecoli_data[,c(14:15,19,23)],by="startDateTime",all=TRUE)
colnames(dataMerge) <- c("dateTime","site","remark00940Cl","result00940Cl","rptlev00940Cl","remark31616Fec","result31616Fec","rptlev31616Fec","remark00530TSS","result00530TSS","rptlev00530TSS","remark00665TP","result00665TP","rptlev00665TP","remark50468Ec","result50468Ec","rptlev50468Ec")

# Add nearest instantaneous measurements to response variable samples

n <- nrow(dataMerge)
for (i in 1:n) {
  adaps_disch_sub <- adaps_disch_in[adaps_disch_in$pdate<=dataMerge$dateTime[i]+(15*60) & adaps_disch_in$pdate>=dataMerge$dateTime[i]-(15*60),]
  adaps_cond_sub <- adaps_cond_in[adaps_cond_in$pdate<=dataMerge$dateTime[i]+(15*60) & adaps_cond_in$pdate>=dataMerge$dateTime[i]-(15*60),]
  adaps_do_sub <- adaps_do_in[adaps_do_in$pdate<=dataMerge$dateTime[i]+(15*60) & adaps_do_in$pdate>=dataMerge$dateTime[i]-(15*60),]
  adaps_temp_sub <- adaps_temp_in[adaps_temp_in$pdate<=dataMerge$dateTime[i]+(15*60) & adaps_temp_in$pdate>=dataMerge$dateTime[i]-(15*60),]
  adaps_turb_sub <- adaps_turb_in[adaps_turb_in$pdate<=dataMerge$dateTime[i]+(15*60) & adaps_turb_in$pdate>=dataMerge$dateTime[i]-(15*60),]  
  if (nrow(adaps_disch_sub)>0) {
    dataMerge$q[i] <- adaps_disch_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_disch_sub$pdate)))),c("disch")]
  } else {
    dataMerge$q[i] <- NA
  }
  if (nrow(adaps_do_sub)>0) {
    dataMerge$do[i] <- adaps_do_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_do_sub$pdate)))),c("do")]
  } else {
    dataMerge$do[i] <- NA
  }
  if (nrow(adaps_cond_sub)>0) {
    dataMerge$cond[i] <- adaps_cond_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_cond_sub$pdate)))),c("cond")]
  } else {
    dataMerge$cond[i] <- NA
  }
  if (nrow(adaps_temp_sub)>0) {
    dataMerge$temp[i] <- adaps_temp_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_temp_sub$pdate)))),c("temp")]
  } else {
    dataMerge$temp[i] <- NA
  }
  if (nrow(adaps_turb_sub)>0) {
    dataMerge$turb[i] <- adaps_turb_sub[sapply(dataMerge$dateTime[i],function(x) which.min(abs(difftime(x,adaps_turb_sub$pdate)))),c("turb")]
  } else {
    dataMerge$turb[i] <- NA
  }
}

library(GSqwsr)
dataMerge$decYear <- getDecYear(dataMerge$dateTime)
dataMerge$sinDY <- sin(dataMerge$decYear*2*pi)
dataMerge$cosDY <- cos(dataMerge$decYear*2*pi)
dataMerge <- dataMerge[dataMerge$q>0,]

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
#data_sub$result00940Cl <- log(data_sub$result00940Cl)
#data_sub$rptlev00940Cl <- log(data_sub$rptlev00940Cl)

# set necessary site information and inputs to step-wise regression
#library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result00940Cl","remark00940Cl","","rptlev00940Cl","User","tons","Unk","","00940","CompCl")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Underwood"
siteINFO <-  readNWISsite(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompCl"
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsMMSD.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsMMSD(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##############################################################################
# Regression for Computed Fecal Coliform

data_sub <- dataMerge[!is.na(dataMerge$result31616Fec),]
data_sub <- data_sub[data_sub$remark31616Fec!=">",]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev00940Cl","result00940Cl","remark00940Cl","remark00530TSS","result00530TSS","rptlev00530TSS","result00665TP","remark00665TP","rptlev00665TP","rptlev50468Ec","remark50468Ec","result50468Ec","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark31616Fec","result31616Fec","rptlev31616Fec","q","decYear","sinDY","cosDY")]
#data_sub$result31616Fec <- log(data_sub$result31616Fec)
#data_sub$rptlev31616Fec <- log(data_sub$rptlev31616Fec)

# set necessary site information and inputs to step-wise regression
#library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result31616Fec","remark31616Fec","","rptlev31616Fec","User","tons","Unk","","31616","CompFec")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Underwood"
siteINFO <-  readNWISsite(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompFec"
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsMMSD.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsMMSD(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##############################################################################
# Regression for Computed TSS

data_sub <- dataMerge[!is.na(dataMerge$result00530TSS),]
#data_sub <- data_sub[which(data_sub$dateTime<strptime("2011-10-01",format="%Y-%m-%d")),]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev00940Cl","result00940Cl","remark00940Cl","remark31616Fec","result31616Fec","rptlev31616Fec","result00665TP","remark00665TP","rptlev00665TP","rptlev50468Ec","remark50468Ec","result50468Ec","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark00530TSS","result00530TSS","rptlev00530TSS","q","decYear","sinDY","cosDY")]
#data_sub$result00530TSS <- log(data_sub$result00530TSS)
#data_sub$rptlev00530TSS <- log(data_sub$rptlev00530TSS)

# set necessary site information and inputs to step-wise regression
#library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result00530TSS","remark00530TSS","","rptlev00530TSS","User","tons","Unk","","00530","CompTSS")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Underwood"
siteINFO <-  readNWISsite(siteNo)
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsMMSD.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsMMSD(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##############################################################################
# Regression for Computed Total Phosphorus

data_sub <- dataMerge[!is.na(dataMerge$result00665TP),]
#data_sub <- data_sub[which(data_sub$dateTime<strptime("2011-10-01",format="%Y-%m-%d")),]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev31616Fec","result31616Fec","remark31616Fec","remark00530TSS","result00530TSS","rptlev00530TSS","result00530TSS","remark00530TSS","rptlev00530TSS","rptlev50468Ec","remark50468Ec","result50468Ec","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark00665TP","result00665TP","rptlev00665TP","q","decYear","sinDY","cosDY")]
#data_sub$result00665TP <- log(data_sub$result00665TP)
#data_sub$rptlev00665TP <- log(data_sub$rptlev00665TP)

# set necessary site information and inputs to step-wise regression
#library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result00665TP","remark00665TP","","rptlev00665TP","User","tons","Unk","","00665","CompTP")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Underwood"
siteINFO <-  readNWISsite(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompTP"
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsMMSD.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsMMSD(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################

##############################################################################
# Regression for Computed E Coli

data_sub <- dataMerge[!is.na(dataMerge$result50468Ec),]
data_sub <- data_sub[data_sub$remark50468Ec!=">",]
data_sub <- data_sub[data_sub$result50468Ec>0,]
#data_sub <- data_sub[which(data_sub$dateTime<strptime("2011-10-01",format="%Y-%m-%d")),]
#choose columns to keep for analysis
keepVars <- names(dataMerge)[-which(names(dataMerge) %in% c("site","rptlev31616Fec","result31616Fec","remark31616Fec","remark00530TSS","result00530TSS","rptlev00530TSS","result00665TP","remark00665TP","rptlev00665TP","rptlev00665TP","remark00665TP","result00665TP","do","cond","temp","turb"))]
keepAll <- keepVars
data_sub <- data_sub[which(names(data_sub) %in% keepAll)]
data_sub <- data_sub[!is.na(data_sub$q),]
data_sub <- data_sub[,c("dateTime","remark50468Ec","result50468Ec","rptlev50468Ec","q","decYear","sinDY","cosDY")]
#data_sub$result50468Ec <- log(data_sub$result50468Ec)
#data_sub$rptlev50468Ec <- log(data_sub$rptlev50468Ec)

# set necessary site information and inputs to step-wise regression
#library(GSqwsr)
data_sub_cens <- importQW(data_sub,c("q","decYear","sinDY","cosDY"),"result50468Ec","remark50468Ec","","rptlev50468Ec","User","tons","Unk","","50468","CompEc")
#data_sub_cens <- importQW(data_sub,c("intensity","I5","I10","I60","ARF1","rain","duration","peakDisch","decYear"),"TPLoad","remark","",0.005,"User","tons","Unk","","00665","TPLoading")
siteName <- "Underwood"
siteINFO <-  readNWISsite(siteNo)
# name of value column in data_sub_cens object
investigateResponse <- "CompEc"
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
source("/Users/jlthomps/Desktop/git/GLRIBMPs/plotStepsMMSD.R")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/analyzeStepsGLRI.R")
pdf(paste(pathToSave,"/",investigateResponse,"_plotSteps.pdf",sep=""))
plotStepsMMSD(steps,data_sub_cens,transformResponse)
dev.off()

pdf(paste(pathToSave,"/",investigateResponse,"_analyzeSteps.pdf",sep=""))
analyzeStepsGLRI(steps, investigateResponse,siteINFO, xCorner = 0.01)
dev.off()

#################################################################################################

#####################################################
# Print summary in console:
source("/Users/jlthomps/Desktop/git/GLRIBMPs/summaryPrintoutGLRI.R")
fileName <- paste(pathToSave,"/", investigateResponse,"Summary_2.txt", sep="")
summaryPrintoutGLRI(modelReturn, siteINFO, saveOutput=TRUE,fileName)
#####################################################


