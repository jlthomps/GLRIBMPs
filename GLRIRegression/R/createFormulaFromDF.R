#'createFormulaFromDF
#'
#'Creates text for the 'kitchen sink' formula.
#'
#'@param df dataframe
#'@return modelFormula text string of formula that includes variables and log variables in df
#'@keywords formula creation
#'@export
#'@examples
#'\dontrun{}
createFormulaFromDF <- function(df){
  
  scalarVariables <- NULL
  scalarVariables <- as.character(df$variableNames[which(1 == df$Scalar)])
  scalarVariables <- paste(scalarVariables,collapse=" + ")
  
  interactionSet <- df[,-2]
  interactionVariables <- ""
  
  modelFormula <- scalarVariables
  
  for (i in colnames(interactionSet)){
    
    var1 <- as.character(interactionSet$variableNames[which(1 == interactionSet[[i]])])
    
    if(substring(i, 1, 4) == "log."){
      i <- paste("log(", substring(i, 5, (nchar(i)-1)), ")",sep="")
    }
    
    if(length(var1) > 0){
      vars <- paste(i,var1,sep=":")
      vars <- paste(vars,collapse=" + ")
      interactionVariables <- paste(interactionVariables,vars,sep=" + ")
    }
    
  }
  
  if (nchar(interactionVariables) > 0){
    interactionVariables <- substring(interactionVariables,4,nchar(interactionVariables))
    modelFormula <- interactionVariables
  }
  
  if (nchar(interactionVariables) > 0 & nchar(scalarVariables) > 0){
    modelFormula <- paste(scalarVariables, interactionVariables, sep= " + ")
  }
  
  if(any(grepl("sinDY",modelFormula) | grepl("cosDY",modelFormula))){
    if(!any(grepl("sinDY",modelFormula) & grepl("cosDY",modelFormula))){
      warning("Attempting to create model with only sinDY or cosDY, not both")
      
    }    
  }
  
  return(modelFormula)
}