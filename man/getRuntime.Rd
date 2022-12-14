% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evaluateResults.R
\name{getRuntime}
\alias{getRuntime}
\title{Helper function to compute runtime statistics}
\usage{
getRuntime(x, analyte)
}
\arguments{
\item{x}{(data.frame) with one column specifying the Runtime}

\item{analyte}{(character) specifying current analyzed marker}
}
\value{
(data.frame) containing runtime statistics (min, mean, median, max)
}
\description{
Helper function to compute runtime statistics
}
\author{
Tatjana Ammer \email{tatjana.ammer@roche.com}
}
