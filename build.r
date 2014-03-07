# Build the package:
library(zoo)
library(chron)
library(doBy)
library(XML)
library(RCurl)
library(hydroGOF)
library(lmomco)
library(devtools)

setwd("C:/Users/jlthomps/Desktop/git")
load_all("EflowStats/",reset = TRUE)
setwd("C:/Users/jlthomps/Desktop/git/EflowStats")
document()
check()  
run_examples()
# test()   Assumes testthat type tests in GLRI/inst/tests
setwd("C:/Users/jlthomps/Desktop/git/")
build("EflowStats")
install("EflowStats")

library(EflowStats)

setwd("C:/Users/jlthomps/Desktop/git")
load_all("NWCCompare/",reset = TRUE)
setwd("C:/Users/jlthomps/Desktop/git/NWCCompare")
document()
check()  
run_examples()
# test()   Assumes testthat type tests in GLRI/inst/tests
setwd("C:/Users/jlthomps/Desktop/git/")
build("NWCCompare")
install("NWCCompare")

#install.packages("NWCCompare",repos="http://usgs-r.github.com",type="source")
#in DOS terminal, go to C:\Users\jlthomps\Desktop\git
#Rcmd INSTALL --build NWCCompare