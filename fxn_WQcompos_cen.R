
# Package Hydrovol
# function WQ_compos
# function to composite samples weighted by the associated volume
# the result is a volume-weighted concentration and summation of volumes
#
# df.samples<-dfsamples3
# sampleID<-"USGS_ID",
# parms<-parms,
# volume<-"event.vol"
# cenparms<-cenparms
# cenvals<-cenvals
# Sdate<-"GMTbpdate"
# Edate<-"GMTbedate"


WQcompos_cen <- function(df.samples,sampleID,parms,volume="Evolume",cenparms="",cenvals="",Sdate,Edate){
  
  # 
  ID <- unique(df.samples[,sampleID])
  
  rows <- numeric()
  for (i in 1:length(ID)) rows[i] <- match(x=ID[i], table=df.samples[,sampleID])[1]
  IDdf <- df.samples[rows,]
  numrows <- length(ID)
  
  for (i in 1:numrows){
    subdf <- subset(df.samples,df.samples[,sampleID]==ID[i])
    IDdf[i,volume] <- sum(subdf[,volume])
#    IDdf <- cbind(subIDdf,volume)
    for (j in 1:length(parms)) IDdf[i,parms[j]] <- sum(subdf[,parms[j]]*subdf[,volume])/IDdf[i,volume]
    
    for (j in 1:length(cenparms)) {
      numcen <- sum(subdf[,cenparms[j]]==cenvals[j])
      IDdf[i,cenparms[j]] <- ifelse(numcen>0, paste("BLD",numcen,":",nrow(subdf),sep=""),NA)
    }
    IDdf[i,Sdate] <- subdf[1,Sdate]
    IDdf[i,Edate] <- subdf[dim(subdf)[1],Edate]
  }
  return(IDdf)
}
# 
# df.samples <- events.MF.hydro2
# sampleID <- "USGS_ID"
# parms <- c("Human_virus","Human")
# volume <- "volume"
