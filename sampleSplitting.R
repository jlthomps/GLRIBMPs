library(dataRetrieval)
siteNo <- "441624088045601"
StartDt <- '2013-07-08'
EndDt <- '2013-07-09'
adaps_stage_in <- retrieveUnitNWISData(siteNo,'00065',StartDt,EndDt,format="tsv")
adaps_discharge_in <- retrieveUnitNWISData(siteNo,'00060',StartDt,EndDt,format="tsv")
adaps_precip_in <- retrieveUnitNWISData(siteNo,'00045',StartDt,EndDt,format="tsv")
#adaps_scode_in <- retrieveUnitNWISData(siteNo,'99909',StartDt,EndDt,format="tsv")
adaps_data<-merge(adaps_stage_in[c(3,5)],adaps_discharge_in[c(3,5)],by="datetime")
adaps_data<-merge(adaps_precip_in[c(3,5)],adaps_data,by="datetime")

par(mar=c(5,4,4,4))
plot(adaps_data$datetime,adaps_data$X02_00060,xlab="Datetime",ylab="Q, Stage",col="blue",type="l")
lines(adaps_data$datetime,adaps_data$X01_00065,xlab="",ylab="",col="red",type="l")
par(new=T)
plot(adaps_data$datetime,adaps_data$X04_00045,axes=F,xlab="",ylab="",col="green",type="l")
axis(side=4)
mtext("Rain",side=4)
legend("topright",c("Q","Stage","Rain"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("blue","red","green"))

