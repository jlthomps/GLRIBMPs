#'createFullFormula
#'
#'Creates text for the 'kitchen sink' formula.
#'
#'@param localDT dataframe of potential input variables to model
#'@param responseVariable string column header of single response variable to model
#'@return kitchenSink text string of formula that includes all variables and log variables
#'@keywords studentized residuals
#'@export
#'@examples
#'\dontrun{}
createFullFormula <- function(localDT,responseVariable){

  predictVariables <- names(localDT)[-which(names(localDT) %in% responseVariable)]
  predictVariables <- predictVariables[which(predictVariables != "datetime")]
  predictVariables <- predictVariables[which(predictVariables != "decYear")]
  
  logVariables <- predictVariables[("pH" != predictVariables) & ("decYear" != predictVariables)]
  # Don't want negative logs:
  logVariables <- names(which(sapply(localDT[,which(names(localDT) %in% logVariables)], function(x) min(as.numeric(x),na.rm=TRUE) ) > 0))
  predictString <- paste(predictVariables,collapse=" + ")
  
  if (length(logVariables) == 0){
    kitchenSink <- predictString
  } else {
    logString <- as.character(sapply(paste("log(",logVariables,")",sep=""),function(x)x))
    
    
    logString <- paste(logString,collapse=" + ")
    
    kitchenSink <- paste(predictString,logString,sep=" + ")
  }
  return(kitchenSink)
}