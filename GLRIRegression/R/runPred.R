#'runPred
#'
#'Run the model
#'
#'@param localUV dataframe of unit values
#'@param localDT dataframe in wide format
#'@param finalModel censReg model results
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@param dfReady logical if the localDT dataframe already includes time/log columns
#'@return predictedReturn list
#'@keywords AMLE fit
#'@export
#'@examples
#'\dontrun{}
runPred <- function(localUV,localDT,finalModel,transformResponse="lognormal",dfReady=TRUE){
  
  evaluat <- runFit(localDT,finalModel,transformResponse)
  orderOfVariables <- colnames(evaluat$XLCAL)

  terms <- attributes(finalModel$terms)$term.labels
  formulaToUse <- paste(terms,collapse=" + ")

  if (!dfReady){
    if ("sinDY" %in% terms | "cosDY" %in% terms | "decYear" %in% terms){
      decYear <- getDecYear(localUV$datetime)
      localUV$decYear <- decYear
      localUV$sinDY <- sin(decYear * 2 * pi)
      localUV$cosDY <- cos(decYear * 2 * pi)
    }
    
    newUVList <- createFullDT(formulaToUse, localUV)
    colNames <- c("",newUVList$colNames)
    newUV <- newUVList$DT
    
    newUV <- newUV[,which(names(newUV) %in% colNames)]
    newUV <- na.omit(newUV)

    testFinite <- apply(newUV, 1, function(x) all(is.finite(x)))
    newUV <- newUV[testFinite,]
        
  } else {
    newUV <- localUV
  }

  if (length(orderOfVariables[-1]) > 1){
    newUV <- newUV[,orderOfVariables[-1]]
  } 

  PredictionData <- as.matrix(cbind(1, newUV))
  
  rownames(PredictionData) <- NULL
  colnames(PredictionData)[1] <- ""
  
  predictedReturn <- censReg_AMLE.pred(evaluat, PredictionData)
  return(predictedReturn)
}