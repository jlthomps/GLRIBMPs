\name{findOutliers}
\alias{findOutliers}
\title{findOutliers}
\usage{
  findOutliers(modelReturn, localDT,
    transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{DTframe that includes all response and
  predictor variables}

  \item{modelReturn}{list returned from censReg}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  outlier vector of index numbers
}
\description{
  Find index of outliers using external studentized
  residuals.
}
\examples{
\dontrun{}
}
\keyword{residuals}
\keyword{studentized}

