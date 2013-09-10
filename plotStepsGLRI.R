#'plotStepsGLRI
#'
#'Run stepwise regression and generate simplifed model output.
#'
#'@param steps dataframe
#'@param localDT dataframe of potential input variables to model
#'@param transformResponse ring can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return retVal list of modelStuff, steps, DT.mod
#'@keywords plot
#'@export
#'@examples
#' DTComplete <- DTComplete
#' UV <- UV
#' QWcodes <- QWcodes
#' response <- QWcodes$colName[1]
#' DT <- DTComplete[c(response,getPredictVariables(names(UV)), "decYear","sinDY","cosDY","datetime")]
#' DT <- na.omit(DT)
#' kitchenSink <- createFullFormula(DT,response)
#' returnPrelim <- prelimModelDev(DT,response,kitchenSink)
#' steps <- returnPrelim$steps
#' plotSteps(steps,DT)
plotStepsGLRI <- function(steps,localDT,transformResponse="lognormal"){
 
  parOriginal <- par(no.readonly = TRUE)
  
  responseVariable <-steps$response[1]
  nSteps <- nrow(steps)

  logPlot <- ""
  distribution <- transformResponse
  yMin <- min(localDT[,responseVariable]@.Data[,2], na.rm=TRUE) #?
  xMin <- yMin
  lmFormula <- "obs ~ pred"
  
  if ("lognormal" == transformResponse){
    logPlot <- "xy"
    lmFormula <- "log10(obs) ~ log10(pred)"
  }
  
  for (i in 2:nSteps){
    formulaToUse <- substring(steps$scope[i],3, nchar(steps$scope[i]))
    formulaToUse <- paste(responseVariable, formulaToUse,sep=" ~ ")
    cat(formulaToUse, "\n\n")
   
    modelReturn <- do.call("censReg", list(formulaToUse, data=localDT, dist=distribution))
        
    outlier <- findOutliers(modelReturn,localDT, transformResponse)

    responseValue <- localDT[,responseVariable]@.Data[,2]
    
    df <- data.frame(obs=responseValue, pred=modelReturn$YPRED)
    
    dfOutliers <- data.frame(obsOut=responseValue[outlier],predOut=modelReturn$YPRED[outlier])

    lineFit <- do.call("lm", list(lmFormula, data=df))
    par(tcl=0.3)
    
    
    plot(df$pred,df$obs,ylab="Observed",xlab="Predicted",
         main=paste(responseVariable, i, sep=":"),
         ylim=c(yMin,max(c(df$obs,df$pred))),xlim=c(xMin,max(c(df$obs,df$pred))),log=logPlot)
    
    points(dfOutliers$predOut,dfOutliers$obsOut, col="red",pch=16)
    
#     plot(df$obs,df$pred,ylab="Predicted",xlab="Observed",
#          main=paste(responseVariable, i, sep=":"),
#          ylim=c(yMin,max(df$obs)),xlim=c(xMin,max(df$obs)),log=logPlot)
#     
#     points(dfOutliers$obsOut, dfOutliers$predOut, col="red",pch=16)
    
    if (sum(modelReturn$CENSFLAG) > 0){
      cenValsX <- modelReturn$YPRED[modelReturn$CENSFLAG]
      cenValsY <- responseValue[modelReturn$CENSFLAG]
          
      segments(x0=cenValsX, y0=cenValsY, x1=cenValsX, y1=yMin*.001)
    }
    
    abline(lineFit, col="red")
    abline(0,1,col="blue")
    mtext(formulaToUse, side=3, line=-1,cex=0.7)
    corStep <- cor(df$obs, df$pred)
    goodness <- paste("slope: ",formatC(lineFit$coefficients[2],digits=4), 
                      ", cor: ", formatC(corStep,digits=4), 
                      ", R2: ", formatC(modelReturn$RSQ/100, digits=4), 
                      ", StdErrPercMean: ", formatC(rmse(modelReturn)/mean(modelReturn$YPRED),digits=4), sep="")
    mtext(goodness,side=3,line=0.5,cex=0.7)
    
   
  }
  
  par(parOriginal)
  
}