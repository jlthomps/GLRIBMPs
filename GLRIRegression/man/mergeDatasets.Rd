\name{mergeDatasets}
\alias{mergeDatasets}
\title{mergeDatasets}
\usage{
  mergeDatasets(QW, UV, QWcodes, max.diff = "2 hours")
}
\arguments{
  \item{QW}{dataframe water quality dataset}

  \item{UV}{dataframe unit value dataset}

  \item{QWcodes}{dataframe}

  \item{max.diff}{string default is "2 hours"}
}
\value{
  retList list including the DT list and updated QWcodes
}
\description{
  Creates a list of merged datasets, along with updating
  the QWcodes dataframe.
}
\examples{
\dontrun{}
}
\keyword{datasets}
\keyword{merge}

