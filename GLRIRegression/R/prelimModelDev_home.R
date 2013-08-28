#'prelimModelDev
#'
#'Run stepwise regression and generate simplifed model output.
#'
#'@param localDT dataframe of potential input variables to model
#'@param responseVariable string column header of single response variable to model
#'@param upperBoundFormula string of upper bound for model generation
#'@param k string either "AIC", "BIC", or value of the multiple of the number of degrees of freedom used for the penalty.
#'@param transformResponse string can be "normal" or "lognormal", perhaps try to generalize this more in future
#'@return retVal list of modelStuff, steps, localDT.mod
#'@keywords studentized residuals
#'@export
#'@examples
#'\dontrun{}
prelimModelDev <- function(localDT,responseVariable,
                           upperBoundFormula,
                           k="AIC",
                           transformResponse="lognormal"){
  
  if ("AIC" == k){
    k <- 2
  } else if ("BIC" == k){
    k <- log(length(localDT[,responseVariable]))
  }
  
  distribution <- transformResponse
  formulaToUse <- paste(responseVariable," ~ 1",sep="")
 
  DT.mod <- do.call("stepAIC", args=list(object=call("lm",
                                formulaToUse,
                                data=localDT),
                           scope=list(lower= ~ 1, upper=formula(paste("~ ",upperBoundFormula,sep=""))), k=k, keep = extractAIC))
  
  pathToModel <- DT.mod$anova
  
  steps <- with(pathToModel, data.frame(step=Step, AIC=AIC,
             Deviance=Deviance, Resid.Dev=get('Resid. Dev'), Resid.Df=get('Resid. Df')))
  
  if(any(grepl("sinDY",as.character(steps$step)) | grepl("cosDY",as.character(steps$step)))){
    
    if (!(any(grepl("sinDY",as.character(steps$step)) & any(grepl("cosDY",as.character(steps$step)))))){
  
      if(any(grepl("sinDY",as.character(steps$step)))){
        firstIndex <- which(grepl("sinDY",as.character(steps$step)))[1]
        lowerScope <- paste(as.character(steps$step[2:firstIndex]),collapse=" ")
        lowerScope <- paste("~ ", substring(lowerScope, 2, nchar(lowerScope)), " + cosDY",sep="")
      } else {
        firstIndex <- which(grepl("cosDY",as.character(steps$step)))[1]
        lowerScope <- paste(as.character(steps$step[2:firstIndex]),collapse=" ")
        lowerScope <- paste("~ ", substring(lowerScope, 2, nchar(lowerScope)), " + sinDY",sep="")
      }
      
      formulaToUseNew <- paste(responseVariable,lowerScope)
      
      DT.mod <- do.call("stepAIC", args=list(object=call("censReg",
                               formulaToUseNew,
                               data=localDT,
                               dist=distribution),
                   scope=list(lower= formula(lowerScope), upper=formula(paste("~ ",upperBoundFormula,sep=""))), k=k))
  
      pathToModel2 <- DT.mod$anova
      
      steps2 <- with(pathToModel2, data.frame(step=Step, AIC=AIC,
                  Deviance=Deviance, Resid.Dev=get('Resid. Dev'), Resid.Df=get('Resid. Df')))
      steps2$step <- as.character(steps2$step)
      
      if(any(grepl("sinDY",as.character(steps$step)))){
        steps2$step[1] <- "+ cosDY"
      } else {
        steps2$step[1] <- "+ sinDY"
      }
      
            
      steps <- rbind(steps[1:firstIndex,],steps2)
    }
  }
  
  steps$Correlation <- rep(NA,nrow(steps))
  steps$Slope <- rep(NA,nrow(steps))
  
  steps$Res.St.Error <- rep(NA,nrow(steps))
  steps$PRESS <- rep(NA,nrow(steps))
  responseValue <- localDT[,responseVariable]@.Data[,2]
  steps$scope <- as.character(steps$step)
  steps$response <- rep(responseVariable,nrow(steps))
  
  
  lmFormula <- "pred ~ obs"
  if ("lognormal" == transformResponse){
    lmFormula <- "log10(pred) ~ log10(obs)"
  }
  
  if(nrow(steps) > 1){
    cat("Analyzing steps (total=",nrow(steps),"): ")
    for(j in 2:nrow(steps)){
      cat(j,"\n")
      steps$scope[j] <- paste(steps$scope[j-1],steps$scope[j],sep=" ")
      
      formulaToUse <- substring(steps$scope[j],3, nchar(steps$scope[j]))
      formulaToUse <- paste(responseVariable, formulaToUse,sep=" ~ ")
      modelReturnStep <- do.call("lm", list(formulaToUse, data=localDT))
      
      modelCoef <- modelReturnStep$PARAML
      names(modelCoef) <- c(dimnames(modelReturnStep$XLCAL)[[2]],"logSigma")
      modelCoefList <- list()
      modelCoefList[[steps$response[1]]] <- modelCoef
      
      StRes.all.extReturn <- externalStudentRes(localDT, modelCoefList,transformResponse)
      steps$PRESS[j] <- StRes.all.extReturn$PRESS
            
      df <- data.frame(obs=responseValue, pred=modelReturnStep$YPRED)
      lineFit <- do.call("lm", list(lmFormula, data=df))
      corStep <- cor(df$obs, df$pred)
      steps$Slope[j] <- lineFit$coefficients[2]
      steps$Correlation[j] <- corStep
      steps$Res.St.Error[j] <- rmse(modelReturnStep)
      
      
    }
  }
  
  StCoef <- with(DT.mod, PARAML/STDDEV)
  modelStuff <- with(DT.mod, data.frame(names=c(dimnames(DT.mod$XLCAL)[[2]],"logSigma"),
                                        coefficients=PARAML, 
                                        StdError=STDDEV, 
                                        pValue=PVAL,
                                        StCoef=StCoef
  ))

  retVal <- list(modelStuff=modelStuff, steps=steps, DT.mod=DT.mod)
  return(retVal)
}
  
  