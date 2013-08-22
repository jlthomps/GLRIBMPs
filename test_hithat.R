library(XML)
library(chron)
library(zoo)
library(doBy)
library(RCurl)
library(lmomco)
library(dataRetrieval)
library(HITHATStats)

sites<-"02178400"
#sites<-"00000000"
a<-sites
load_data<-"C:/Users/jlthomps/My Documents/R/dv_test.txt"
#load_data<-"C:/Users/jlthomps/My Documents/R/dv_fake.txt"
qfiletempf<-read.table(load_data,sep="\t",stringsAsFactors=FALSE)
colnames(qfiletempf)<-c("date","discharge")
qfiletempf$date<-as.Date(qfiletempf$date)
obs_data<-get_obsdata(qfiletempf)
qfiletempf<-obs_data
x<-obs_data
qfiletemp<-obs_data

#load_peak<-"C:/Users/jlthomps/My Documents/R/peak_fake.txt"
load_peak<-"C:/Users/jlthomps/My Documents/R/peak_test.txt"
qfilepeak<-read.table(load_peak,sep="\t",stringsAsFactors=FALSE,header=F)
colnames(qfilepeak)<-c("date","discharge")
qfilepeak$date<-as.Date(qfilepeak$date)
peak_data<-get_obsdata(qfilepeak)
peak_data$logval <- log10(peak_data$discharge)
qfilepeak <- peak_data