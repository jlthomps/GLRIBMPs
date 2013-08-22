\name{runFit}
\alias{runFit}
\title{runFit}
\usage{
  runFit(localDT, finalModel,
    transformResponse = "lognormal")
}
\arguments{
  \item{finalModel}{results from censReg}

  \item{localDT}{dataframe of potential input variables to
  model}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  evaluat list
}
\description{
  Run the fitting routine
}
\examples{
\dontrun{}
}
\keyword{AMLE}
\keyword{fit}

