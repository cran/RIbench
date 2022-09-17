## ----global_options, echo=FALSE, eval=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=4, fig.align='center', fig.path ='./figures/', 
		echo=TRUE, eval=TRUE, warning=FALSE, message=TRUE)
			  
# increasing the width of the stdout-stream
options(width=200)

## ----processOverview, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
#  # load RIbench package
#  library(RIbench)
#  # set directory from where a ./Data folder will be generated
#  workingDir <- tempdir()
#  
#  # generate all test sets
#  generateBiomarkerTestSets(workingDir = workingDir)
#  
#  # evaluate all test sets using existing or new indirect method ('myOwnAlgo')
#  #    with pre-specified R-function ('estimateModel') (provided by the user)
#  evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel', libs = c('myOwnAlgo'))
#  
#  # evaluate all results, create plots and compute the benchmark score
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "myOwnAlgo")
#  

## ----load_testsetDef, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# load RIbench package and load testset definition
library(RIbench)
testsets <- loadTestsetDefinition()
str(testsets)


## ----generate_testsets, echo=TRUE, eval  =FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
#  # set directory from where a ./Data folder will be generated
#  workingDir <- tempdir()
#  print(workingDir)
#  # generate all test sets
#  generateBiomarkerTestSets(workingDir = workingDir)
#  

## ----include_algo_bc, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
#  # load R-package with the indirect method to be investigated
#  library(myOwnAlgo)
#  
#  estimateModel <- function(Data = NULL, ... ){
#  	
#  # PLACEHOLDER: insert your own function for the estimation of a (shifted) Box-Cox transformed normal distribution here
#  	
#  	
#  # Initialize an RWDRI object with the estimated model parameters (lambda, mu, sigma, shift) and return it
#  	obj 		<- list()
#  	obj$Lambda 	<- lambda 	# power parameter lambda, only if Box-Cox transformation is integrated into your method.
#  							# For normal distribution set to 1 and subtract 1 from the estimated mean.
#  	obj$Mu 		<- mu		# mean
#  	obj$Sigma 	<- sigma	# standard deviation
#  	obj$Shift	<- shift	# shift, only if 2-parameter Box-Cox transformation is integrated into your method,
#  							# otherwise set to 0.
#  	obj$Method 	<- "myOwnAlgo"
#  	class(obj) 	<- "RWDRI"
#  	
#  	return(obj)
#  }
#  

## ----include_algo_RIs, echo=TRUE, eval =FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------
#  # load R-package with the indirect method to be investigated
#  library(myOwnAlgo)
#  
#  estimateRIs <- function(Data = NULL, percentiles  = c(0.025,0.975), ... ){
#  	
#  # PLACEHOLDER: insert your own function to estimate reference intervals here
#  	
#  # Initialize a data frame with the specified percentiles and fill the PointEst column with the corresponding
#  # estimates obtained by your own method (RIResultMyOwnAlgo)
#  	
#  	RIResult          <- data.frame(Percentile = percentiles, PointEst = NA)
#  	RIResult$PointEst <- RIResultMyOwnAlgo
#  	
#  	return(RIResult)
#  }

## ----run_ind_method, echo=TRUE, eval =FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
#  # The evaluation of all test sets can take several hours or longer depending on the computation time of the algorithm
#  
#  evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), sourceFiles = list("MyAlgoWrapper.R"),
#  		requireDecimals = FALSE, requirePercentiles = FALSE,
#  		subset ='all', timeLimit = 14400)

## ----run_ind_method_refineR_opt1, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------
#  # Exemplary evaluation for only 'Calcium' test sets.
#  progress <- evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), subset = "Ca")

## ----run_ind_method_refineR_opt2, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------
#  # Exemplary evaluation for only a subset testsets that follow a skewed distribution.
#  progress <- evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), subset = "skewed")

## ----run_ind_method_refineR_opt3, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------
#  # Exemplary evaluation for a subset of 3 testsets per biomarker.
#  progress <- evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), subset = 3)

## ----run_ind_method_refineR_opt4, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------
#  
#  testsets <- loadTestsetDefinition()
#  # Exemplary evaluation for a customized subset with all test sets that have a pathological fraction <= 30%.
#  progress <- evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), subset = testsets[testsets$fractionPathol <= 0.3,] )

## ----run_ind_method_param, echo=TRUE, eval =FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Define wrapper function with the additional 'model' argument and save to script e.g. called 'Test_RIEst_2pBoxCox.R'
#  
#  estimateModelDec <- function(Data = NULL, model = NULL, ... ){
#  	
#  # PLACEHOLDER: insert your own function for estimation of a (shifted) Box-Cox transformed normal distribution here
#  	
#  	
#  # Initialize an RWDRI object with the estimated model parameters (lambda, mu, sigma, shift) and return it
#  	obj 		<- list()
#  	obj$Lambda 	<- lambda 	# power parameter lambda, only if Box-Cox transformation is integrated into your method.
#  	# For normal distribution set to 1 and subtract 1 from the estimated mean.
#  	obj$Mu 		<- mu		# mean
#  	obj$Sigma 	<- sigma	# standard deviation
#  	obj$Shift	<- shift	# shift, only if 2-parameter Box-Cox transformation is integrated into your method,
#  	# otherwise set to 0.
#  	obj$Method 	<- "myOwnAlgo"
#  	class(obj) 	<- "RWDRI"
#  	
#  	return(obj)
#  }
#  
#  progress <- evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModel',
#  		libs = c('myOwnAlgo'), sourceFiles = list("Test_RIEst_2pBoxCox"), params = list("model='2pBoxCox'"))

## ----run_ind_method_reqDec, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Define wrapper function and save to script e.g. called 'Test_RIEst_dec.R'
#  
#  estimateModelDec <- function(Data = NULL, decimals = NULL, ... ){
#  	
#  # PLACEHOLDER: insert your own function for estimation of a (shifted) Box-Cox transformed normal distribution here
#  	
#  	
#  # Initialize an RWDRI object with the estimated model parameters (lambda, mu, sigma, shift) and return it
#  	obj 		<- list()
#  	obj$Lambda 	<- lambda 	# power parameter lambda, only if Box-Cox transformation is integrated into your method.
#  	# For normal distribution set to 1 and subtract 1 from the estimated mean.
#  	obj$Mu 		<- mu		# mean
#  	obj$Sigma 	<- sigma	# standard deviation
#  	obj$Shift	<- shift	# shift, only if 2-parameter Box-Cox transformation is integrated into your method,
#  	# otherwise set to 0.
#  	obj$Method 	<- "myOwnAlgo"
#  	class(obj) 	<- "RWDRI"
#  	
#  	return(obj)
#  }
#  
#  
#  evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'myOwnAlgo', algoFunction = 'estimateModelDec',
#  		libs = c('myOwnAlgo'), sourceFiles = "Test_RIEst_dec.R",
#  		requireDecimals = TRUE)

## ----run_ind_method_reqPerc, echo=TRUE, eval =FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
#  # save e.g. the following function into a script called "Test_RIEst.R"
#  
#  # load R-package with the indirect method to be investigated
#  library(myOwnAlgo)
#  
#  # function requires an argument called percentiles to use that for the direct estimation of the specified percentiles
#  estimateRIs <- function(Data = NULL, percentiles = c(0.025,0.975), ... ){
#  	
#  	# estimate reference intervals with custom defined function
#  	RIResultMyOwnAlgo <- findPerc(Data)
#  	
#  	# save estimations into required format
#  	RIResult          <- data.frame(Percentile = percentiles, PointEst = RIResultMyOwnAlgo)
#  	
#  	return(RIResult)
#  }
#  
#  
#  # set requirePercentile to TRUE and specify the file that contains the R code for the wrapper function
#  #    for the direct estimation of reference intervals / percentiles
#  evaluateBiomarkerTestSets(workingDir = workingDir, algoName = "myOwnAlgo", algoFunction = "estimateRIs",
#  		libs = "myOwnAlgo", sourceFiles = "Test_RIEst.R",
#  		requirePercentiles = TRUE)
#  

## ----eval_results_moreAlgos, echo=TRUE, eval =FALSE---------------------------------------------------------------------------------------------------------------------------------------------------
#  # Using default parameters, this re-produces the figures shown in Ammer et al., 2022.
#  # As the results for the different algorithms do not exists, this evaluation does not work and just shows an exemplary call of the function.
#  evaluateAlgorithmResults(workingDir = workingDir, algoNames = c("Hoffmann", "TML", "kosmic", "TMC", "refineR"))
#  

## ----eval_results, echo=TRUE, eval =FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # define color for refineR
#  col_refineR <- rgb(20, 130, 250, maxColorValue = 255)
#  # evaluate results for only one algorithm
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "refineR", cols = col_refineR)
#  

## ----eval_results_def_Ca, echo=TRUE, eval =FALSE------------------------------------------------------------------------------------------------------------------------------------------------------
#  # define color for TML
#  col_TML 	<- rgb(160,94,181,maxColorValue =255)
#  
#  # evaluate results for only one biomarker and set color
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "TML", subset = 'Ca', cols = col_TML)
#  

## ----eval_results_def_skewed, echo=TRUE, eval =FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
#  # define color for TMC
#  col_TMC 	<- rgb(127, 255, 212, maxColorValue = 255)
#  
#  # evaluate results for only one distribution type
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "TMC", subset = 'skewed', cols = col_TMC)
#  

## ----eval_results_customized, echo=TRUE, eval =FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
#  # define color for kosmic
#  col_kosmic 	<- rgb(237,139,0,maxColorValue =255)
#  # evaluate results for only a subset of testsets with defined characteristics (i.e. pathological fraction <= 30%)
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "refineR",
#  		subset = testsets[testsets$fractionPathol <= 0.3,], cols = col_kosmic)
#  

## ----refineR_Example, echo=TRUE, eval =FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------
#  # load RIbench package
#  library(RIbench)
#  # set directory from where a ./Data folder will be generated
#  workingDir <- tempdir()
#  
#  # generate all test sets
#  generateBiomarkerTestSets(workingDir = workingDir, subset = 3)
#  
#  # evaluate all test sets using existing or new indirect method ('myOwnAlgo') with pre-specified R-function ('estimateModel')
#  evaluateBiomarkerTestSets(workingDir = workingDir, algoName = 'refineR', algoFunction = 'findRI', libs = c('refineR'), subset = 3)
#  
#  # evaluate all results, create plots and compute the benchmark score
#  benchmarkScore <- evaluateAlgorithmResults(workingDir = workingDir, algoNames = "refineR", subset = 3)
#  

