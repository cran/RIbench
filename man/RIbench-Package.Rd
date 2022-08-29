\docType{package} 
\name{RIbench-package} 
\alias{RIbench} 
\alias{RIbench-package}
\title{RIbench: Benchmark Suite for the Standardized Evaluation of Indirect Methods for Reference Interval Estimation} 
\description{RIbench enables the automated evaluation and comparison of any existing and novel indirect method in a systematic way. 
	Indirect methods take routine measurements of diagnostic tests, containing pathological and non-pathological samples 
	as input and use sophisticated statistical methods to derive a model describing the distribution of the non-pathological 
	samples, which can then be used to derive reference intervals. The benchmark suite contains 5,760 simulated data sets 
	with varying difficulty.
	To include any indirect method, a custom wrapper function needs to be provided. 
	The package offers functions for generating the test sets \code{\link{generateBiomarkerTestSets}}, 
	executing the indirect method \code{\link{evaluateBiomarkerTestSets}} and evaluating the results \code{\link{evaluateAlgorithmResults}}. 
}
\details{   
 \tabular{ll}{     
 Package: \tab RIbench \cr
 Type: \tab Package \cr     
 Version: \tab 1.0.0 \cr     
 Date: \tab 2022-08-27 \cr
 License: \tab GPL (>=3) \cr     
 LazyLoad: \tab yes \cr   
 } 
} 
\author{
	Tatjana Ammer \email{tatjana.ammer@roche.com}, 
    Christopher M Rank \email{christopher.rank@roche.com},
    Andre Schuetzenmeister \email{andre.schuetzenmeister@roche.com}
} 
\references{
	Ammer, T., Schuetzenmeister, A., Prokosch, HU., Zierk, J., Rank, C.M., Rauh, M. RIbench: A Proposed Benchmark for the Standardized Evaluation of Indirect Methods for Reference Interval Estimation. Clin Chem (2022) [Accepted, July 12].
} 
\keyword{package}
