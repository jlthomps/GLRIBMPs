#'resultResidPlots
#'
#'Plots summary with residuals plots
#'
#'@param localDT dataframe in wide format
#'@param finalModel censReg model results
#'@param siteINFO dataframe including station name (station.nm) and siteID (site.no) (easiestly retrieved from dataRetrieval package)
#'@return plot
#'@keywords scatterplot
#'@export
#'@examples
#'\dontrun{}
resultResidPlots <- function(localDT,finalModel,siteINFO){
  
  parOriginal <- par(no.readonly = TRUE)
  
  responseVariable <- rownames(attributes(finalModel$terms)$factors)[1]
  terms <- attributes(finalModel$terms)$term.labels
  distribution <- finalModel$dist
  
  logPlot <- ""
  yMin <- min(localDT[,responseVariable]@.Data[,2], na.rm=TRUE)
  xMin <- yMin
  residLog <- ""
  formulaForBestFit <- "pred ~ obs"
  logIndividual <- ""
  
  
  if ("lognormal" == distribution) {
    logPlot <- "xy"
    formulaForBestFit <- "log10(pred) ~ log10(obs)"
    residLog <- "x"
#     logIndividual <- "y"
  }
  
  addLabel <- function(labelString, x=0.05, y=0.93) {
    x1 <- grconvertX(x, from="npc", to="user")
    y1 <- grconvertY(y, from="npc", to="user")  
    text(x=x1, y=y1, labelString, cex=1.3, font=2)
  }
  
  numTerms <- length(terms)
  # There'll be 4 graphs:
  # 1. obs vs. pred
  # 2. pred vs. resid
  # 3. time vs resid
  # 4. quantile vs resid quantile
  # Plus...the number of terms
  
  minGrid <- 4 + numTerms
  
  for (i in 1:10){ #so, would break if more than 96 terms....I think we should be safe
    iSqr <- i*i
    if (iSqr > minGrid) break
  }
  
  grid <- i
  A <- 1:minGrid
  if (length(A) < (grid*grid)){
    numZeros <-  (grid*grid) - length(A)
    for(j in 1:numZeros){
      A <- c(A,0)
    }
  }
  layoutVector <- rep(rep(1,grid),time=length(A)/grid)
  layoutVector[layoutVector==1] <- A
  layout(matrix(layoutVector, grid, grid, byrow = TRUE))
  #   layout.show(minGrid)
  
  op <- par(
    mar=c(4,4,0,0),   # margins of individual subplots
    oma=c(2,0.1,2,0.1),    # margins of overall plot
    #     mfrow=c(3,3),      # number of rows and columns of subplots
    tcl=0.3,           # make ticks 'innies' rather than 'outies'
    mgp=c(2,0.4,0),  # margin line for drawing the 1) axis title, 2) axis label, 3) axis line
    las=1)             # force axis labels to be always horizontal
  
  responseValue <- localDT[,responseVariable]@.Data[,2]
  df <- data.frame(obs=responseValue, pred=finalModel$YPRED)
  resid <- residuals(finalModel)
  lineFit <- do.call("lm", list(formulaForBestFit, data=df))
  
  #1: 
  plot(df$pred,df$obs,ylab=paste("Measured ", responseVariable,sep=""),
       xlab=paste("Predicted ", responseVariable,sep=""),
       ylim=c(yMin,max(df$obs)),xlim=c(xMin,max(df$obs)),log=logPlot)
  abline(lineFit, col="red")
  addLabel("A")
  
  #2:
  plot(df$pred, resid, ylab="Residuals",xlab=paste("Predicted ", responseVariable,sep=""),
       xlim=c(xMin,max(df$obs)),log=residLog)
  addLabel("B")
  
  #3:
  xScale <- pretty(localDT$decYear)
  plot(localDT$decYear, resid,ylab="Residuals",xlab="Time", xaxt="n")
  axis(1, at=xScale, labels=xScale)
  addLabel("C")
  
  #4: 
  qqnorm(resid, main="", ylab="Residuals quantiles", xlab="Theoretical quantiles")
  qqline(resid, col="red")
  axis(side=3, labels=FALSE)
  axis(side=4, labels=FALSE)    
  addLabel("D") 
  
  #Now loop:
  originalTerms <- terms
  y <- localDT[,responseVariable]@.Data[,2]
  
  #   formulaToUse <- as.character(formula(finalModel))[3]
  formulaToUse <- paste(terms,collapse=" + ")
  
  
  fullDTList <- createFullDT(formulaToUse, localDT)
  localDT <- fullDTList$DT
  terms <- fullDTList$colNames
  
  alphabet <- toupper(letters)
  for (i in 1:numTerms){
    
    rawNames <- strsplit(terms[i],"log")[[1]][1]
    isInteraction <- length(grep(":", originalTerms[i])) > 0
    
    if ("" == rawNames & !isInteraction ){
      colName <- strsplit(terms[i],"log")[[1]][2]
      logPlot <- paste("x",logIndividual,sep="")
    } else {  # this will plot all interactions on a linear scale...not sure if that's the best, but not sure exactly what would make the most sense...and not sure if it's worth wasting time over
      logPlot <- logIndividual
      colName <- terms[i]
    }
    x <- localDT[,colName]
    plot(x,resid,
         ylab="Residuals",
         xlab=originalTerms[i],
         log=logPlot)
    addLabel(alphabet[4+i])
    
  }
  
  prettyName <- simpleCap(siteINFO$station.nm)
  prettyName <- gsub("Wi", "WI",prettyName) #Consider other states.
  topLine <- paste(responseVariable, " at ", prettyName, " (", siteINFO$site.no, ")", sep="")
  modelText <- createModelEquation(finalModel)
  
  title(main=topLine, sub=modelText,outer=TRUE, line=1)
  
  
  
  par(parOriginal)
}