#'simpleCap
#'
#'Return string with initial capital letters
#'
#'@param x string
#'@return retVal corrected string
#'@keywords string formatting
#'@export
#'@examples
#'exString <- "BAD RIVER"
#'exString <- simpleCap(exString)
simpleCap <- function(x) {
  x <- tolower(x)
  s <- strsplit(x, " ")[[1]]
  retVal <- paste(toupper(substring(s, 1,1)), substring(s, 2),
                  sep="", collapse=" ")
  return(retVal)
}