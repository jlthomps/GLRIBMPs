\name{analyzeSteps}
\alias{analyzeSteps}
\title{analyzeSteps}
\usage{
  analyzeSteps(steps, responseVariable, siteINFO,
    xCorner = 0.65, yCorner = 0.3)
}
\arguments{
  \item{steps}{dataframe}

  \item{responseVariable}{string column header of single
  response variable to model}

  \item{xCorner}{number represents the x-upper-left corner
  of legend in \% plot area}

  \item{yCorner}{number represents the y-upper-left corner
  of legend in \% plot area}

  \item{siteINFO}{dataframe including station name
  (station.nm) and siteID (site.no) (easiestly retrieved
  from dataRetrieval package)}
}
\value{
  plot
}
\description{
  Plot showing improvment in model choices
}
\examples{
\dontrun{}
}
\keyword{scatterplot}

