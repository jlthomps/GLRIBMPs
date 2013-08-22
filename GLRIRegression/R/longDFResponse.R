#'longDFResponse
#'
#'Reshapes a wide dataframe to long.
#'
#'@param x dataframe in wide format
#'@param predictVariables character vector
#'@param responseVariable character
#'@return Long dataframe with value, variable, and response columns
#'@keywords reshape long
#'@export
#'@examples
#'\dontrun{}
longDFResponse <- function(x,predictVariables,responseVariable){
  DTsub <- x[,predictVariables]
  DTsub$responseHigh <- x[,responseVariable]@.Data[,2]
  DTsub$responseLow <- x[,responseVariable]@.Data[,1]
  
  x <- DTsub
  initialize <- rep(NA,((ncol(x)-2)*nrow(x)))
  Long <- data.frame(value=initialize,variable=initialize,response=initialize)
  for(i in 1:(ncol(x)-2)){
    Long[(1:nrow(x))+(i-1)*(nrow(x)),'value'] <- x[,i]
    Long[(1:nrow(x))+(i-1)*(nrow(x)),'variable'] <- rep(names(x)[i],nrow(x))
    Long[(1:nrow(x))+(i-1)*(nrow(x)),'response'] <- x[,(ncol(x)-1)]
  }
  Long[,'variable']<-factor(Long[,'variable'])
  
  Long <- Long[!is.na(Long$value),]
  Long <- Long[Long$value>0,]
  return(Long)
}