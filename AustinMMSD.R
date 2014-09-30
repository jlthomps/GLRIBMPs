modelCoefs <- read.delim(file="MMSDmodelCoef.csv",stringsAsFactors=FALSE)
library(dataRetrieval)
siteNo <- "04087119"
StartDt <- "2008-10-01"
EndDt <- "2014-09-01"
compQW <- 'Cl' # one of c('Cl','Fec','Ec','TSS','TP')
dataReq <- modelCoefs[which(modelCoefs$type=='R' & modelCoefs$y==compQW),]
dataReq <- colnames(dataReq[which()])
