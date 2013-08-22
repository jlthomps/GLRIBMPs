\name{whatQW}
\alias{whatQW}
\title{whatQW}
\usage{
  whatQW(site, minCount = 50, endDate = "2013-01-01",
    startDate = "", getFullNames = TRUE,
    ignoreGroups = c("Physical", "Information"))
}
\arguments{
  \item{site}{character USGS site ID (usually 8 digits)}

  \item{minCount}{number filter out parameters with less
  than this number}

  \item{endDate}{text}

  \item{startDate}{text}

  \item{getFullNames}{logical to make a slow call to get
  the full names of parameters}

  \item{ignoreGroups}{character filter out certain
  parameter groups (this can only be used if getFullNames
  was TRUE)}
}
\value{
  QWcodes dataframe
}
\description{
  Get QW parameters for a USGS site
}
\examples{
site <- "04027000"
qw1 <- whatQW(site, getFullNames=FALSE)
}
\keyword{data}
\keyword{qw}
\keyword{retrieval}

