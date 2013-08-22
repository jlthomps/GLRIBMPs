#'createFullDT
#'
#'Get log and interaction columns
#'
#'@param formulaToUse formula
#'@param localDT dataframe of potential input variables to model
#'@return fullDTList list of 
#'@keywords transforms
#'@export
#'@examples
#'\dontrun{}
createFullDT <- function(formulaToUse, localDT){
  
  splt <- unlist(strsplit(formulaToUse, " \\+ "))
  
  interactionIndex <- grep("\\:",splt )
  interactionVariables <- c()
  
  if(length(interactionIndex) != 0){
    interactionNames <- splt[interactionIndex]
    
    #Set up new interaction columns:
    for (i in interactionIndex){
      vars <- strsplit(splt[i],"\\:")[[1]]
      
      logVars <- grep("log\\(",vars )
      
      if(length(logVars)>0){
        if (length(logVars)>1){
          rawVars <- substr(vars,5,nchar(vars)-1)          
          colName <- paste("log",rawVars[1],"Andlog",rawVars[2],sep="")
          localDT[[colName]]<- log(localDT[[rawVars[1]]])*log(localDT[[rawVars[2]]])
          
        } else {
          logSingleName <- vars[logVars]
          logSingleName <- substr(logSingleName,5,nchar(logSingleName)-1)
          nonLogName <- vars[-logVars]
          colName <- paste(nonLogName,"Andlog",logSingleName,sep="")
          localDT[[colName]] <- localDT[[nonLogName]]*log(localDT[[logSingleName]])          
        }
        interactionVariables <- c(interactionVariables,colName)
      } else {
        colName <- paste(vars,collapse="And")
        localDT[[colName]] <- localDT[[vars[1]]]*localDT[[vars[2]]]
        
        interactionVariables <- c(interactionVariables,colName)
      }

      splt[i] <- colName
    }    
    
    
    spltReduced <- splt[-interactionIndex]
  } else {
    interactionVariables <- NA
    spltReduced <- splt
  }

  logIndex <- grep("log\\(",spltReduced )
  
  if (length(logIndex)>0){
    logVariables <- substr(spltReduced[logIndex],5,nchar(spltReduced[logIndex])-1)
    colName <- paste("log",logVariables,sep="")
    
    localDT[colName] <- log(localDT[logVariables])
    
    splt[logIndex] <- colName
  }
  
  modelFormula <- paste(splt,collapse=" + ")
  
  fullDTList <- list(DT=localDT, modelFormula=modelFormula, colNames=splt)
  
  return(fullDTList)

}