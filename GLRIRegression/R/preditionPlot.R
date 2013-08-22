#'predictionPlot
#'
#'Plots summary plots
#'
#'@param localUV dataframe of unit values
#'@param localDT dataframe in wide format
#'@param finalModel censReg model results
#'@param transformResponse string can be "lognormal" or "normal", perhaps try to generalize this more in future
#'@param siteINFO dataframe including station name (station.nm) and siteID (site.no) (easiestly retrieved from dataRetrieval package)
#'@return plot
#'@keywords scatterplot
#'@export
#'@examples
#'\dontrun{}
predictionPlot <- function(localUV,localDT,finalModel,transformResponse="lognormal",siteINFO){
  
  parOriginal <- par(no.readonly = TRUE)
  
  responseVariable <- rownames(attributes(finalModel$terms)$factors)[1]
  predictedReturn <- runPred(localUV,localDT,finalModel,transformResponse,dfReady=FALSE)

  logPlot <- ""
  predVal <- predictedReturn$ESTIM
  
  if ("lognormal" == transformResponse){
    logPlot <- "y"
    predVal <- predictedReturn$BACKEST
  }
  
  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")
  
  newUVList <- createFullDT(formulaToUse, localUV)
  colNames <- c("",newUVList$colNames)
  newUV <- newUVList$DT
  newUV <- newUV[,which(names(newUV) %in% c("datetime",colNames))]
  newUV <- na.omit(newUV)
  
  if (ncol(newUV) > 2){
    testFinite <- apply(newUV[,-1], 1, function(x) all(is.finite(x)))
  } else {
    testFinite <- is.finite(newUV[,-1])
  }
  newUV <- newUV[testFinite,]
  
  plot(newUV$datetime,predVal,
       xlab="Date", ylab=responseVariable,col="blue",type="l",log=logPlot)
  points(localDT$datetime, localDT[[responseVariable]]@.Data[,2],col="red",pch=20)
  
  if (sum(finalModel$CENSFLAG) > 0){
    cenValsY <- localDT[[responseVariable]][modelReturn$CENSFLAG]
    cenValsX <- localDT$datetime[finalModel$CENSFLAG]
    
    segments(x0=cenValsX, y0=cenValsY, x1=cenValsX, y1=0.0001,col="red")
  }
  
  prettyName <- simpleCap(siteINFO$station.nm)
  prettyName <- gsub("Wi", "WI",prettyName) #Consider other states.
  
  title(paste(responseVariable, " at ", prettyName, " (", siteINFO$site.no, ")", sep=""))
  
  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")
  formulaToUse <- paste(responseVariable, " ~ ", formulaToUse)
  mtext(formulaToUse, side=3, line=0.5,cex=0.7)
  
  par(parOriginal)
}