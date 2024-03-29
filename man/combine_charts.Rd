% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chart.R
\name{combine_charts}
\alias{combine_charts}
\title{Combine charts}
\usage{
combine_charts(chartlist, ncol = NULL, nrow = NULL, labels = "AUTO", ...)
}
\arguments{
\item{chartlist}{List of charts to combine.}

\item{ncol}{(optional) number of columns in the plot grid.}

\item{nrow}{(optional) number of rows in the plot grid.}

\item{labels}{(optional) labels to use for each individual plot. \code{"AUTO"}}

\item{...}{further arguments passed to \code{\link[=ggarrange]{ggarrange()}}.
(default value) auto-generates uppercase labels, and \code{"auto"} does the same
for lowercase labels.}
}
\value{
An object of class \code{ggarrange} containing a list of \code{ggplot}s.
}
\description{
Assemble multiple charts on the same page. Wrapper around \code{\link[=ggarrange]{ggarrange()}} with
different defaults.
}
\examples{
# TODO...
}
\seealso{
\code{\link[=chart]{chart()}}, \code{\link[=ggarrange]{ggarrange()}}
}
\concept{Combine plots on the same page}
\keyword{hplot}
