
# Package Hydrovol
# function WQ_compos
# function to composite samples weighted by the associated volume
# the result is a volume-weighted concentration and summation of volumes
#

# df.samples, dataframe with sample results and volumes
# sampleID, IDs for compositing samples (multiple samples will have the same ID)
# parms, Parameters to composite
# volume="Evolume", 

WQcompos <- function(df.samples,sampleID,parms,volume="Evolume"){
  
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
  }
  return(IDdf)
}
# 
# df.samples <- events.MF.hydro2
# sampleID <- "USGS_ID"
# parms <- c("Human_virus","Human")
# volume <- "volume"
