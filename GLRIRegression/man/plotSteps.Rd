\name{plotSteps}
\alias{plotSteps}
\title{plotSteps}
\usage{
  plotSteps(steps, localDT,
    transformResponse = "lognormal")
}
\arguments{
  \item{steps}{dataframe}

  \item{localDT}{dataframe of potential input variables to
  model}

  \item{transformResponse}{ring can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  retVal list of modelStuff, steps, DT.mod
}
\description{
  Run stepwise regression and generate simplifed model
  output.
}
\examples{
\dontrun{}
}
\keyword{plot}

