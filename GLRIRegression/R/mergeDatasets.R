#'mergeDatasets
#'
#'Creates a list of merged datasets, along with updating the QWcodes dataframe.
#'
#'@param QW dataframe water quality dataset
#'@param UV dataframe unit value dataset
#'@param QWcodes dataframe
#'@param max.diff string default is "2 hours"
#'@return retList list including the DT list and updated QWcodes
#'@keywords merge datasets
#'@export
#'@examples
#'\dontrun{}
mergeDatasets <- function(QW, UV, QWcodes,max.diff="2 hours"){

  namesToCheck <- getPredictVariables(names(UV))

  ColName <- ""
  index <- as.numeric(which(sapply(QW, function(x) class(x)) == "qw"))
  
  for (i in 1:nrow(QWcodes)){
    for(j in names(QW)[index]){ 
      #     analyte.name <- unique(QW[,j]@analyte.name[!is.na(QW[,j]@analyte.name)])
      pcode <- unique(QW[,j]@unique.code[!is.na(QW[,j]@unique.code)])
      if (pcode == QWcodes$parameter_cd[i]){
        ColName <- j
        break
      }
    }
    QWcodes$colName[i] <- ColName
  }
    
  for (j in namesToCheck){
    subUV <- UV[c("datetime", j)]
    subUV <- na.omit(subUV)
    DT <- mergeNearest(QW["datetime"], dates.left="datetime", all.left=TRUE,
                              right=subUV, dates.right="datetime", max.diff=max.diff)
    DT$datetime.right <- NULL
    if (namesToCheck[1] == j){
      DTmerged <- DT
    } else {
      DTmerged <- merge(DTmerged,DT,by=("datetime.left"))
    }
  }
  
  colnames(DTmerged) <- c("datetime",colnames(DTmerged)[-1])
  
  DT_single <- merge(DTmerged,QW,by=("datetime"))
  
  DT_single$decYear <- getDecYear(DT_single$datetime)
  DT_single$sinDY <- sin(DT_single$decYear*2*pi)
  DT_single$cosDY <- cos(DT_single$decYear*2*pi)
  
  retList <- list(DTComplete=DT_single, QWcodes=QWcodes)
  return(retList)

}