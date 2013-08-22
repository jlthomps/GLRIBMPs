#'saveModelResults
#'
#'Saves critical objects for model analysis
#'
#'@param pathToSave full path for saving results
#'@param DT dataframe of potential input variables to model
#'@param UV dataframe of unit values
#'@param modelReturn results from censReg
#'@param siteINFO dataframe that includes station.nm and site.no
#'@keywords save model results
#'@export
#'@examples
#'\dontrun{}
saveModelResults <- function(pathToSave, DT, UV, modelReturn, siteINFO){
  
  responseVariable <- rownames(attributes(modelReturn$terms)$factors)[1]
  
  save(DT,file=paste(pathToSave,"/DT", responseVariable,".RData",sep=""))
  save(modelReturn,file=paste(pathToSave,"/Model", responseVariable,".RData",sep=""))
  save(siteINFO,file=paste(pathToSave,"siteINFO.RData",sep="/"))
  
  save(UV,file=paste(pathToSave,"UV.RData",sep="/"))
  
}
