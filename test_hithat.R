library(XML)
library(chron)
library(zoo)
library(doBy)
library(RCurl)
library(lmomco)
library(dataRetrieval)

sites<-"02178400"
a<-sites
load_data<-"C:/Users/jlthomps/My Documents/R/dv_test.txt"
qfiletempf<-read.table(load_data,sep="\t",stringsAsFactors=FALSE)
colnames(qfiletempf)<-c("date","discharge")
qfiletempf$date<-as.Date(qfiletempf$date)
obs_data<-get_obsdata(qfiletempf)
qfiletempf<-obs_data
x<-obs_data
qfiletemp<-obs_data

