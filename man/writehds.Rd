% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writehds.r
\name{writehds}
\alias{writehds}
\title{Write MT3DMS .hds File}
\usage{
writehds(hds, ofl)
}
\arguments{
\item{hds}{is the hds data frame}

\item{ofl}{is the output hds file name}
}
\description{
This function writes a hds dataframe created in R (i.e., from readhds) to a binary hds file
composed of the following elements:
}
\details{
writehds(hds, ofl)
}
\examples{
writehds(hds = h, ofl = "T04R1.hds")
}
