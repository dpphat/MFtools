% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readobs.r
\name{readobs}
\alias{readobs}
\title{Read the .obs MT3DMS output file}
\usage{
readobs(INFL = NA, NOBS)
}
\arguments{
\item{INFL}{This is the full path name of the .obs file.}

\item{NOBS}{This is the number of observation wells in the .obs file. This value tells readobs how to parse the .obs table.}
}
\description{
This function reads the .obs file and generates a tibble data frame of concentrations over time and step at each observation
well location. Column headings represent the Layer, Row, Column indices of the observation wells.
}
