\name{externalStudentRes}
\alias{externalStudentRes}
\title{externalStudentRes}
\usage{
  externalStudentRes(localDT, modelCoefList,
    transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{DTframe that includes all response and
  predictor variables}

  \item{modelCoefList}{list of model coefficients}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  StRes.all
}
\description{
  Compute internally Studentized residuals
}
\examples{
\dontrun{}
}
\keyword{residuals}
\keyword{studentized}

