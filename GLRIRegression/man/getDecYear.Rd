\name{getDecYear}
\alias{getDecYear}
\title{getDecYear}
\usage{
  getDecYear(dateTime)
}
\arguments{
  \item{dateTime}{POSIXct}
}
\value{
  decYear numeric
}
\description{
  Returns decimal year from POSIXct
}
\examples{
date1 <- as.POSIXct("2012-01-02 12:38:00")
decYear <- getDecYear(date1)
date2 <- as.POSIXct("2012-07-02 12:38:00")
decYear2 <- getDecYear(date2)
}
\keyword{conversion}
\keyword{dateTime}

