% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatValues.R
\name{formatValues}
\alias{formatValues}
\title{Define how values are to be displayed}
\usage{
formatValues(.df, .digit, .maxex)
}
\arguments{
\item{.df}{Parameter estimates output from \code{\link[=define_param_table]{define_param_table()}} with
modifications ready for formatting.}

\item{.digit}{Number of significant digits. Default is three digits}

\item{.maxex}{Maximum number of significant digits before moving to scientific
notation. Default is \code{NULL}}
}
\description{
Define what is in estimate column and what is in square brackets.
}
\keyword{internal}
