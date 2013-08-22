#'plotQQTransforms
#'
#'Plots Q-Q of predict variables.
#'
#'@param localDT dataframe in wide format
#'@param responseVariable character
#'@param printLinear logical
#'@param printLog logical
#'@param printSquare logical
#'@param printSQRT logical
#'@param printEXP logical
#'@return plot
#'@keywords qq
#'@export
#'@examples
#'\dontrun{}
plotQQTransforms <- function(localDT, responseVariable,printLinear=TRUE,printLog=TRUE,printSquare=FALSE,
                             printSQRT=FALSE,printEXP=FALSE){
  
  
  explanvar <- names(localDT)[-which(names(localDT) %in% responseVariable)]
  explanvar <- explanvar[which(explanvar != "datetime")]
  explanvar <- explanvar[which(explanvar != "decYear")]
  explanvar <- explanvar[which(explanvar != "sinDY")]
  explanvar <- explanvar[which(explanvar != "cosDY")]
  
  shapiro <- function(s) as.numeric(shapiro.test(s)[2])

  sh <- sapply(localDT[,explanvar],shapiro)
  explanvar.p <- paste(explanvar,": p=",round(sh,3),sep="")
  names(explanvar.p) <- explanvar
  
  sh <- sapply(log10(0.01+localDT[,explanvar]),shapiro)
  explanvar.log.p <- paste(explanvar,": p=",round(sh,3),sep="")
  names(explanvar.log.p) <- explanvar
  
  dfsh <- (localDT[,explanvar])*(localDT[,explanvar])
  sh <- sapply(dfsh,shapiro)
  explanvar.sq.p <- paste(explanvar,": p=",round(sh,3),sep="")
  names(explanvar.sq.p) <- explanvar
  
  dfsh <- sqrt(localDT[,explanvar])
  sh <- sapply(dfsh,shapiro)
  explanvar.sqrt.p <- paste(explanvar,": p=",round(sh,3),sep="")
  names(explanvar.sqrt.p) <- explanvar
  
  localDTLong <- longDFResponse(localDT,explanvar,responseVariable)
  if(printLog){
    p <- qqmath(~(0.0001+localDTLong$value)|localDTLong$variable,
                main="Log",ylab="Predict Variables Units",
                scales = list(y = list(log=10,relation = 'free')),
                strip=strip.custom(par.strip.text=list(cex=0.75)))
    dimnames(p)[[1]] <- explanvar.log.p[dimnames(p)[[1]]]
    print(p)
  }

  if(printLinear){
    p <- qqmath(~(localDTLong$value)|localDTLong$variable,
                main="Linear",ylab="Predict Variables Units",
                scales = list(y = list(relation = 'free')),
                strip=strip.custom(par.strip.text=list(cex=0.75)))
    dimnames(p)[[1]] <- explanvar.p[dimnames(p)[[1]]]
    print(p)
  }
  
  if(printSquare){
    p <- qqmath(~(localDTLong$value^2)|localDTLong$variable,
                main="Square",ylab="Predict Variables Units",
                scales = list(y = list(relation = 'free')),
                strip=strip.custom(par.strip.text=list(cex=0.75)))
    dimnames(p)[[1]] <- explanvar.sq.p[dimnames(p)[[1]]]
    print(p)
  }
  
  if(printSQRT){
    p <- qqmath(~sqrt(localDTLong$value)|localDTLong$variable,
                main="SQRT",ylab="Predict Variables Units",
                scales = list(y = list(relation = 'free')),
                strip=strip.custom(par.strip.text=list(cex=0.75)))
    dimnames(p)[[1]] <- explanvar.sqrt.p[dimnames(p)[[1]]]
    print(p)
  }
  
  if(printEXP){
    noFlow <- localDTLong["Flow" != localDTLong$variable,]
    noFlow$variable <- factor(as.character(noFlow$variable))
    p <- qqmath(~exp(noFlow$value)|noFlow$variable,
                main="EXP",ylab="Predict Variables Units",
                scales = list(y = list(relation = 'free')),
                strip=strip.custom(par.strip.text=list(cex=0.75)))
    print(p)
  }
}