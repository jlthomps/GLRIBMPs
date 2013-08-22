#'createModelEquation
#'
#'Create the model equation including coefficients.
#'
#'@param modelReturn censReg model results
#'@return combo string
#'@keywords equation
#'@export
#'@examples
#'\dontrun{}
createModelEquation <- function(modelReturn){
  responseVariable <- rownames(attributes(modelReturn$terms)$factors)[1]
  
  termNames <- names(coef(modelReturn))
  termNames[1] <- ""
  coefficients <- round(as.numeric(coef(modelReturn)),digits=4)
  distribution <- modelReturn$dist
  
  if ("lognormal" == distribution){    
    responseVariable <- paste("ln(",responseVariable,")",sep="")
  }
  
  combo <- paste( round(coef(modelReturn),digits=4), names(coef(modelReturn)))
  
#   combo <- paste(coefficients,termNames,sep="")
  
  combo <- paste(combo, collapse=" + ")
  
  combo <- paste(responseVariable, " = ", combo, sep="")
  return(combo)
  
}