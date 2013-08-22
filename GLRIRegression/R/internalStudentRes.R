#'internalStudentRes
#'
#'Compute internally Studentized residuals
#'
#'@param DT DTframe that includes all response and predictor variables
#'@param modelCoef named list of model coefficients
#'@param logResponse logical
#'@return StRes.all
#'@keywords studentized residuals
#'@export
#'@examples
#'\dontrun{}
internalStudentRes <- function(DT, modelCoef,logResponse=FALSE){
  StRes.all <- nrow(DT)
  h <- numeric()
  StRes <- numeric()
  responseVariables <- names(modelCoef)
  
  if (is.list(modelCoef)) {
    indexVector <- responseVariables
  } else {
    indexVector <- c(1)
  }
  
  for (i in indexVector){

    m2Coef <- modelCoef[[i]]
    modelvars <- names(m2Coef)[-c(1,length(m2Coef))]
    
    rawDTNames <- sapply(strsplit(modelvars,"log"),function(x)x[1])  # Obviously could do more transforms
    
    logDTNames <- sapply(strsplit(modelvars,"log"),function(x)x[2])
    logDTNames <- substring(logDTNames,2,nchar(logDTNames)-1)
    
    logData <- as.data.frame(sapply(DT[,logDTNames[!is.na(logDTNames)]], function(x) log(x)))
    colnames(logData) <- paste("log",logDTNames[!is.na(logDTNames)],sep="")
    
    DT <- cbind(DT, logData)
    
    rawDTNames["" == rawDTNames] <- colnames(logData)
    
    if(length(m2Coef)>1){
 
      dat200 <- as.data.frame(DT[,rawDTNames]) #Generate DT frame with only variables from the current model
      names(dat200) <- rawDTNames
      
      #Compute residuals
      responseVariable <- i
#       y<-log(as.numeric(DT[,responseVariable])+1) #generalize this for censored DT--optional transformation (probably just log)
      responseValue <- DT[,responseVariable]@.Data[,2]       
      y <- responseValue
      
      if (logResponse){
        y<-log(responseValue) #add 1?
      }
      
      
      X <- as.matrix(cbind(rep(1,nrow(dat200)),dat200))
      Beta <- m2Coef[-length(m2Coef)]
      predictions <- X%*%Beta
      residuals <- predictions - y
      
      #Compute H matrix of leverage values
      H <- X%*%(ginv((t(X)%*%X)))%*%t(X)
      for (j in 1:nrow(X)) h[j] <- H[j,j] #leverage values on the diagonal of the matrix are used to compute studentized residuals
      
      s <- sqrt(sum(residuals^2)/(nrow(X)-length(Beta)))
      for (j in 1:nrow(X)) StRes[j] <- residuals[j]/(s/(sqrt(1-h[j]))) #Compute studentized residuals
      StRes.all <- cbind(StRes.all,StRes) #Add studentized residuals to DT frame for current model
    }
  }
  StRes.all <- StRes.all[,-1]
  if(is.matrix(StRes.all)) colnames(StRes.all) <- responseVariables
  return(StRes.all)
}