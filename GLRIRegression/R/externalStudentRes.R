#'externalStudentRes
#'
#'Compute internally Studentized residuals
#'
#'@param localDT DTframe that includes all response and predictor variables
#'@param modelCoefList list of model coefficients
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return StRes.all
#'@keywords studentized residuals
#'@export
#'@examples
#'\dontrun{}
externalStudentRes <- function(localDT, modelCoefList,transformResponse="lognormal"){
  #Initialize needed vectors
  StRes.all.ext <- nrow(localDT)
  LOOresiduals.ext <- nrow(localDT)
  LOOresiduals <- numeric()
  PRESS <- numeric() #vector for PRESS statistic
  PR2 <- numeric() #vector for R^2 (probably not meaningful for these regressions)
  PaR2 <- numeric() #vector for adjusted R^2 (probably not meaningful for these regressions)
  h <- numeric()
  StRes <- numeric()
  responseVariables <- names(modelCoefList)
  
  for (i in c(1:length(responseVariables))){
    responseVariable <- names(modelCoefList)[i]
    m2Coef <- modelCoefList[[responseVariable]]
    modelvars <- names(m2Coef)[-c(1,length(m2Coef))]
    
    rightSideFormulaToUse <- paste(modelvars,collapse=" + ")
    returnList <- createFullDT(rightSideFormulaToUse, localDT)
    
    DT00 <- returnList$DT
    formulaToUse <- returnList$modelFormula
    formulaToUse <- formula(paste(responseVariable," ~ ", formulaToUse,sep=""))
    
    colNames <- returnList$colNames
#     DT00 <- DT00[,c(responseVariable,colNames)]
    
    LOOresiduals <- numeric() #initialize cross validation error vector from leave-one-out routine
    
    if(length(m2Coef)>1){
      for (k in 1:nrow(DT00)){
        # Compute model for leave-one-out models
        subdf <- DT00[-k,]
        distribution <- transformResponse        
        
        m2 <- do.call("censReg", list(formulaToUse, data=subdf, dist=distribution))

        #Compute residuals for *all* observations using leave-one-out models
        
        y <- DT00[,responseVariable]@.Data[,2]
        
#         if ("lognormal" == transformResponse){
#           y<-log(subRespAll)
#         }
        
        DT00pred <- as.data.frame(DT00[,colNames])
        names(DT00pred) <- colNames
        
        X <- as.matrix(cbind(rep(1,nrow(DT00pred)),DT00pred))
        
#         DT00pred <- as.data.frame(DT00[,c(colNames,"datetime")])
#         names(DT00pred) <- c(colNames,"datetime")

#         predReturn <- runPred(DT00pred,DT00[,c(responseVariable, colNames)],
#                               m2,transformResponse=transformResponse)
        predReturn <- runPred(DT00pred,DT00,
                              m2,transformResponse=transformResponse, dfReady=TRUE)
        
        predictions <- predReturn$ESTIM
        
        if ("lognormal" == transformResponse){
          predictions <- predReturn$BACKEST
        }
  
        Beta <- coef(m2)        
#         predictions <- X%*%Beta
        residuals <- predictions - y
        
        
        LOOresiduals[k] <- residuals[k]

        H <- X%*%(ginv((t(X)%*%X)))%*%t(X) #compute leverage
        h[k] <- H[k,k] #extract diagonal of leverage matrix
        
        s <- sqrt(sum(residuals^2)/(nrow(X)-length(Beta)))
        StRes[k] <- residuals[k]/(s/(sqrt(1-h[k]))) #compute final studentized residuals
      }
#       as.data.frame(StRes)
      StRes.all.ext <- cbind(StRes.all.ext,StRes) # add column of studentized residuals for each individual model
      LOOresiduals.ext <- cbind(LOOresiduals.ext,LOOresiduals) #keep track of leave-one-out cross validation errors
      
      PRESS[i] <- sum((LOOresiduals)^2) #Compute PRESS statistic
#       PR2[i] <-rSqr(observed=y,resid=LOOresiduals,p=length(Beta))[1] #Compute R^2 (probably not meaningful for these regressions)
#       PaR2[i] <-rSqr(observed=y,resid=LOOresiduals,p=length(Beta))[2] #Compute adjusted R^2 (probably not meaningful for these regressions)
    }
  }
  
  StRes.all.ext <- StRes.all.ext[,-1]
#   LOOresiduals.ext <- as.DT.frame(LOOresiduals.ext[,-1])
#   if(is.matrix(LOOresiduals.ext)) names(LOOresiduals.ext) <- responseVariables
#   
#   StdErrLOOresid <- apply(X=LOOresiduals.ext,2,stderr)
#   SEoverMean <- StdErrLOOresid/apply(DT[,names(LOOresiduals.ext)],2,mean)
  
  if(is.matrix(StRes.all.ext)) colnames(StRes.all.ext) <- responseVariables
  
  return(list(StRes.all.ext=StRes.all.ext,PRESS=PRESS))
}