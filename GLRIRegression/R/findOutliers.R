#'findOutliers
#'
#'Find index of outliers using external studentized residuals.
#'
#'@param localDT DTframe that includes all response and predictor variables
#'@param modelReturn list returned from censReg
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return outlier vector of index numbers
#'@keywords studentized residuals
#'@export
#'@examples
#'\dontrun{}
findOutliers <- function(modelReturn, localDT, transformResponse="lognormal"){
  modelCoef <- modelReturn$PARAML
  names(modelCoef) <- c(dimnames(modelReturn$XLCAL)[[2]],"logSigma")
  modelCoefList <- list()
  modelCoefList[[steps$response[1]]] <- modelCoef
  
  StRes.all.extReturn <- externalStudentRes(localDT, modelCoefList,transformResponse)
  outlier <- which(StRes.all.extReturn$StRes.all.ext > 3 | StRes.all.extReturn$StRes.all.ext < -3)
  return(outlier)
  
}