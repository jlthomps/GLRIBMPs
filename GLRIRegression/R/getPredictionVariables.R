#' getPredictVariables
#' 
#' Returns a character vector of prediction variables.
#'
#'@param DTnames column names of DT dataframe
#'@return predictVariables string predict variables based on column headers
#'@keywords predict
#'@export
#'@examples
#'\dontrun{}
getPredictVariables <- function(DTnames){
  splitNames <- sapply(strsplit(DTnames, "_"),function(x)x[length(x)])
  splitNamesAvoid <- sapply(strsplit(DTnames, "_"),function(x)x[1])
  
  commentIndex <- which("cd" == splitNames & !(splitNamesAvoid %in% c("agency", "site", "tz")))
  
  predictVariables <- DTnames[commentIndex-1]
  return(predictVariables)
}