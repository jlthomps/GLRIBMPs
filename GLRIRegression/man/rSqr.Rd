\name{rSqr}
\alias{rSqr}
\title{R Squared}
\usage{
  rSqr(observed, resid, p)
}
\arguments{
  \item{observed}{number vector of observed data}

  \item{resid}{number vector of residual data}

  \item{p}{not sure}
}
\value{
  df list with rSquare and adjusted rSquare
}
\description{
  Calculates R^2 and adjusted R^2 values
}
\examples{
observed <- runif(10, 5.0, 7.5)
resid <- runif(10, 0.1, 0.2)
p<-1
rList <- rSqr(observed, resid, p)
}
\keyword{rSquared}

