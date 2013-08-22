#'summaryPrintout
#'
#'Summarize the model results
#'
#'@param modelReturn censReg model results
#'@param siteINFO dataframe including station name (station.nm) and siteID (site.no) (easiestly retrieved from dataRetrieval package)
#'@param saveOutput logical 
#'@param fileName string full path and file name to save to
#'@keywords summary text
#'@export
#'@examples
#'\dontrun{}
summaryPrintout <- function(modelReturn, siteINFO, saveOutput=FALSE,fileName){
  
  if(saveOutput) sink(fileName)
  
  responseVariable <- rownames(attributes(modelReturn$terms)$factors)[1]
  
  termNames <- names(coef(modelReturn))
  coefficients <- as.numeric(coef(modelReturn))
  
  StCoef <- with(modelReturn, PARAML/STDDEV)
  modelStuff <- with(modelReturn, data.frame(Term=c(names(coef(modelReturn)),"logSigma"),
                                        Coefficient=round(PARAML,digits=3), 
                                        StdError=round(STDDEV,digits=3), 
                                        pValue=round(PVAL,digits=3),
                                        StCoef=round(StCoef,digits=3)
  ))
  
  termNames <- paste(termNames, collapse=" + ")
  printFormula <- paste(responseVariable, " ~ ", termNames, sep="")
  numCensored <- sum(modelReturn$CENSFLAG)
  
  prettyName <- simpleCap(siteINFO$station.nm)
  prettyName <- gsub("Wi", "WI",prettyName) #Consider other states.
  cat(responseVariable, " at ", prettyName, " (", siteINFO$site.no, ")\n")
  cat("Number of observations: ", modelReturn$NOBSC, "\n")
  cat("Distribution: ", modelReturn$dist, "\n")
  cat("Method: ", modelReturn$method, "\n")
  cat("Degrees of freedom: ", modelReturn$DF, "\n")
  cat("RMSE: ", rmse(modelReturn), "\n")
  cat("Number of censored values: ", numCensored,"\n")
  cat(printFormula, "\n")
  print(modelStuff)
  cat("\n")
  cat("Correlation matrix of coefficients:","\n")
  CORdf <- cov2cor(modelReturn$COV)
  CVdf <- as.data.frame(round(CORdf,digits=4))
  colnames(CVdf) <- c(names(coef(modelReturn)),"logSigma")
  rownames(CVdf) <- c(names(coef(modelReturn)),"logSigma")
  print(CVdf)
  cat("\nTo clarify:\n")
  cat("RSQ: ", modelReturn$RSQ, "\n")
  cat("LLR: ", modelReturn$LLR, "\n")
  cat("SCORR: ", modelReturn$SCORR, "\n")
  cat("LLRAML: ", modelReturn$LLRAML, "\n")
  cat("PLEVAML: ", modelReturn$PLEVAML, "\n")
  cat("IERR: ", modelReturn$IERR, "\n")  
  
  if (saveOutput) sink()
}