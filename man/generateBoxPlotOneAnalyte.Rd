% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.R
\name{generateBoxPlotOneAnalyte}
\alias{generateBoxPlotOneAnalyte}
\title{Wrapper function to generate one boxplot for a specified analyte}
\usage{
generateBoxPlotOneAnalyte(
  errorListAll,
  colList,
  nameList,
  catList,
  catLabels,
  a,
  errorParam,
  outline = TRUE,
  withMean = TRUE,
  withCats = TRUE,
  withDirect = TRUE,
  titlePart = NULL,
  outputDir,
  filenamePart = NULL,
  ylim1 = c(0, 100),
  ylim2 = c(100, 1000),
  ...
)
}
\arguments{
\item{errorListAll}{(list) containing the overall benchmark results per algorithm}

\item{colList}{(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)}

\item{nameList}{(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used}

\item{catList}{(character) vector specifying the categories for which the boxes should be drawn}

\item{catLabels}{(character) vector specifying the labels to the associated categories used for the x-axis}

\item{a}{(character) specifying the analyte for which the boxplot should be generated}

\item{errorParam}{(charcter) specifying for which error measure the plot should be generated}

\item{outline}{(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)}

\item{withMean}{(logical) indicating whether the mean should be plotted as well (default: TRUE)}

\item{withCats}{(logical) set to TRUE if categories (e.g. pathological fraction) should be plotted (default: FALSE)}

\item{withDirect}{(logical) indicating whether the box of the direct method should be elongated to facilitate comparison (default:TRUE)}

\item{titlePart}{(character) specifying the latter part of the title}

\item{outputDir}{(character) specifying a output directory}

\item{filenamePart}{(character) specifying a filename for the plot}

\item{ylim1}{(numeric) vector specifying the limits in y-direction for the first granular scale}

\item{ylim2}{(numeric) vector specifying the limits in y-direction for the second less detailed scale}

\item{...}{additional arguments passed forward to other functions}
}
\value{
No return value. Instead, a plot is generated.
}
\description{
Wrapper function to generate one boxplot for a specified analyte
}
\author{
Tatjana Ammer \email{tatjana.ammer@roche.com}
}
