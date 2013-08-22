#'generateParamChoices
#'
#'Generate a file to be used to narrow down allowed parameters in model creation.
#'
#'@param predictVariables string vector of prediction variables
#'@param modelReturn censReg object returned from censReg
#'@param pathToSave string
#'@param save logical
#'@keywords model parameter list
#'@export
#'@examples
#'\dontrun{}
generateParamChoices <- function(predictVariables, modelReturn, pathToSave="", save=FALSE){
  
  predictVariables <- predictVariables[which(predictVariables != "datetime")]
  predictVariables <- predictVariables[which(predictVariables != "decYear")]
  
  fullVariables <- predictVariables
    
  excludeTheseColumns <- c("sinDY", "cosDY", "pH","decYear")
  calibrationData <- modelReturn$call[["data"]][predictVariables]
  
  for (i in predictVariables){
    if(sum(calibrationData[i] <= 0) > 0) {
      excludeTheseColumns <- append(excludeTheseColumns,i)
    }
  }
    
  logVariables <- paste("log(", fullVariables[!(fullVariables %in% excludeTheseColumns)], ")", sep="")
  
  fullVariables <- c(fullVariables,logVariables)
  numVars <- length(fullVariables)
  
  df <- data.frame(matrix(rep(0,(numVars*numVars)+numVars),nrow=numVars,ncol=numVars+1))
  row.names(df) <- fullVariables
  colnames(df) <- c("Scalar",fullVariables)
  
  df <- cbind(variableNames=fullVariables, df)
  
  initialModel <- attributes(modelReturn$terms)$term.labels
  
#   initialModel <- as.character(modelResultNames[2:(length(modelResultNames)-1)])
  
  df$Scalar[which(rownames(df) %in% initialModel)] <- 1
  responseVariable <- rownames(attributes(modelReturn$terms)$factors)[1]
  
  if (save){
    fileToSave <- paste(pathToSave,"/",responseVariable,"ModelParams.csv",sep="")
    write.table(df, fileToSave, row.names=FALSE, sep=",")    
  }
  
  return(df)
}