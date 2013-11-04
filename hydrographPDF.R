#' Function to generate and save pdf version of the hydrograph plot 
#' 
#' This function accepts a merged data frame of ADAPS data for a site/storm event
#' 
#' @param adaps_data_all data frame containing merged ADAPS data for the requested storm event
#' @param storm_name string containing the storm name title for the plot
#' @export
hydrographPDF <- function(adaps_data_all,storm_name) {
  adaps_data_samples <- adaps_data_all[which(adaps_data_all$X05_99909>0),c("datetime","X02_00060")]
  adaps_data_plot <- adaps_data_all[,c("datetime","cum_00045","X01_00065","X02_00060")]
  plot_00045 <- adaps_data_plot[which(!is.na(adaps_data_plot$cum_00045)),]
  plot_00060 <- adaps_data_plot[which(!is.na(adaps_data_plot$X02_00060)),]
  plot_00065 <- adaps_data_plot[which(!is.na(adaps_data_plot$X01_00065)),]
  plot_samp <- adaps_data_samples[which(!is.na(adaps_data_samples$X02_00060)),]
  pdf(paste(siteNo,storm_name,"hydrograph.pdf",sep=""),width=10,height=8)
  par(mar=c(8,4,5,4),xpd=T)
  plot(plot_00065$datetime,plot_00065$X01_00065,xaxt="n",xlab="",ylab="Rain, Stage",col="red",type="l",main=paste(siteNo," storm number ",storm_name,sep=""))
  lines(plot_00045$datetime,plot_00045$cum_00045,xlab="",ylab="",col="blue",type="l")
  #plot(plot_00045$datetime,plot_00045$cum_00045,xaxt="n",xlab="",ylab="Rain",col="blue",type="l",lwd=.5,main=paste(siteNo," storm number ",storm_name,sep=""))
  #lines(plot_00065$datetime,plot_00065$X01_00065,xlab="",ylab="",col="red",type="l")
  a<-seq(min(adaps_data_plot$datetime),max(adaps_data_plot$datetime),14400)
  axis.POSIXct(1,at=a,format="%m/%d %H:%M",las=2)
  mtext("Datetime",side=1,line=6)
  par(new=T)
  plot(plot_00060$datetime,plot_00060$X02_00060,axes=F,xlab="",ylab="",col="green",type="l")
  points(plot_samp$datetime,plot_samp$X02_00060,xlab="",ylab="",col="purple",pch="o")
  axis(side=4)
  mtext("Discharge",side=4,line=2)
  legend("topright",c("ADAPS Corrected Q","ADAPS Corrected Stage","Storm Rain","Samples"),lty=c(1,1,1,NA),lwd=c(2.5,2.5,2.5),pch=c(NA,NA,NA,1),col=c("green","red","blue","purple"))
  dev.off()
}