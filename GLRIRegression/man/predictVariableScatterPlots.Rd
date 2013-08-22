\name{predictVariableScatterPlots}
\alias{predictVariableScatterPlots}
\title{predictVariableScatterPlots}
\usage{
  predictVariableScatterPlots(localDT, responseVariable,
    transformResponse = "lognormal")
}
\arguments{
  \item{localDT}{dataframe in wide format}

  \item{responseVariable}{character}

  \item{transformResponse}{string can be "normal" or
  "lognormal", perhaps try to generalize this more in
  future}
}
\value{
  plot
}
\description{
  Plots scatterplot of predict variables.
}
\examples{
\dontrun{}
}
\keyword{scatterplot}

