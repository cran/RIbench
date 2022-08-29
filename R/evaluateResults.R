
#'Convenience Function to generate all result plots and calculate the benchmark score 
#' 
#' @param workingDir		(character) specifying the working directroy: Plots will be stored in workingDir/evalFolder and results will be used from workingDir/Results/algoName/biomarker; 
#' @param algoNames			(character) vector specifying all algorithms that should be part of the evaluation
#' @param subset			(character, numeric, or data.frame) to specify for which subset the algorithm should be evaluated. 
#' 							character options:	'all' (default) for all test sets,
#' 												a distribution type: 'normal', 'skewed', 'heavilySkewed', 'shifted';
#' 												a biomarker: 'Hb', 'Ca', 'FT4', 'AST', 'LACT', 'GGT', 'TSH', 'IgE', 'CRP', 'LDH'; 
#' 												'Runtime' for runtime analysis subset; 							
#' 							numeric option: number of test sets per biomarker, e.g. 10;
#' 							data.frame: customized subset of table with test set specifications   
#' @param cutoffZ			(integer) specifying if and if so which cutoff for the absolute z-score deviation should be used to 
#' 								classify results as implausible and exclude them from the overall benchmark score (default: 5)
#' @param evalFolder		(character) specifying the name of the ouptut directory, Plots will be stored in workingDir/evalFolder, default: 'Evaluation'
#' @param withDirect		(logical) indicating whether the direct method should be simulated for comparison (default:TRUE)
#' @param withMean			(logical) indicating whether the mean should be plotted as well (default: TRUE) 
#' @param outline			(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)
#' @param errorParam 		(character) specifying for which error parameter the data frame should be generated, choose between absolute z-score deviation ("zzDevAbs_Ov"), 
#' 								absolute percentage error ("AbsPercError_Ov"), and absolute error ("AbsError_Ov")
#' @param cols				(character) vector specifying the colors used for the different algorithms 
#' @param ...				additional arguments to be passed to the method, e.g. 
#' 								"truncNormal" (logical) vector specifying if a normal distribution truncated at zero shall be assumed, can be either TRUE/FALSE or a vector with TRUE/FALSE for each algorithm;
#' 								"colDirect" (character) specifying the color used for the direct method, default: "grey" 
#' 								"ylab" (character) specifying the label for the y-axis

#' @return (data frame) containing the computed benchmark results
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' # Ensure that 'generateBiomarkerTestSets()' and 'evaluateBiomarkerTestSets() is called 
#' # with the same workingDir and for all mentioned algorithms before calling this function.
#' 
#' # first example, evaluation for several algorithms 
#' benchmarkScore <- evaluateAlgorithmResults(workingDir=tempdir(), 
#' 			algoNames=c("Hoffmann", "TML", "kosmic", "TMC", "refineR"))
#' # The function will create several plots saved in workingDir/Evaluation.
#' 
#' # second example, evaluation for only one algorithm and a defined subset
#' benchmarkScore <- evaluateAlgorithmResults(workingDir = tempdir(), 
#' 			algoNames = "refineR", subset = 'Ca')
#' 
#' # third example, saving the results in a different folder, and setting a different cutoff
#' # for the absolute z-score deviation
#' benchmarkScore <- evaluateAlgorithmResults(workingDir = tempdir(), algoNames = "refineR", 
#' 		subset = 'Ca', cutoffZ = 4, evalFolder = "Eval_Test")
#' }
#'  
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

evaluateAlgorithmResults <- function(workingDir = "", algoNames = NULL, subset = "all", evalFolder = "Evaluation", withDirect = TRUE, 
		withMean = TRUE, outline = TRUE, errorParam = c("zzDevAbs_Ov", "AbsPercError_Ov", "AbsError_Ov"), cutoffZ = 5, cols = NULL, ...){

	args = list(...)
	# check input parameters 
	stopifnot(is.character(workingDir))
	stopifnot(is.character(algoNames))
	stopifnot(is.character(subset) | is.numeric(subset) | is.data.frame(subset))
	stopifnot(is.character(evalFolder))
	stopifnot(is.logical(withDirect))
	stopifnot(is.logical(withMean))
	stopifnot(is.logical(outline))
	stopifnot(is.character(errorParam))
	stopifnot(is.numeric(cutoffZ))
	stopifnot(is.null(cols) | is.character(cols))
	
	stopifnot(names(args) %in% c("colDirect","truncNormal"))	# stop if args contains something else than colDirect or truncNormal
	
	errorParam <- match.arg(errorParam[1], choices =c("zzDevAbs_Ov", "AbsPercError_Ov", "AbsError_Ov"))
	
	
	colDirect <- "grey"
	if(!is.null(args$colDirect))
		colDirect = args$colDirect

	# set ylab
	if(!is.null(args$ylab))
		ylab = args$ylab
	else if(errorParam =="zzDevAbs_Ov")
		ylab ="Absolute Z-Score Deviation"
	else if(errorParam =="AbsPercError_Ov")
		ylab ="Absolute Percentage Error"
	else if(errorParam =="AbsError_Ov")
		ylab = "Absolute Error"
	
	

	truncNormal <- FALSE
	if(!is.null(args$truncNormal)){
		stopifnot(is.logical(args$truncNormal))
		stopifnot(length(args$truncNormal) == 1 | length(args$truncNormal) == length(algoNames))
		truncNormal <- args$truncNormal
	}
		
	# set up evaluation directory
	if(!dir.exists(file.path(workingDir,evalFolder))){
		dir.create(path =file.path(workingDir, evalFolder),recursive =TRUE)
	}
	outputDir <- file.path(workingDir, evalFolder)
	
	# check specified subset and load testcase definition for it
	if(is.character(subset)){
		tableTCs <- loadTestsetDefinition()
		
		if(subset == "all"){
			# all
			analytes <- unique(as.character(tableTCs$Analyte))
			
		}else if(subset == "normal" | subset == "skewed" | subset == "heavilySkewed" | subset == "shifted"){
			# distribution type
			warning("The overall score is only comparable to the scores featured in the original publication if all test sets are evaluated.", call. = FALSE)
			tableTCs <- tableTCs[tableTCs$Distribution == subset,]			
			analytes <- unique(as.character(tableTCs$Analyte))
						
		}else if(subset %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH")){
			warning("The overall score is only comparable to the scores featured in the original publication if all test sets are evaluated.", call. = FALSE)
			tableTCs <- tableTCs[tableTCs$Analyte == subset,]
			analytes <- subset
			
		} else if(subset == "Runtime"){
			warning("The overall score is only comparable to the scores featured in the original publication if all test sets are evaluated.", call. = FALSE)
			tableTCs <- tableTCs[tableTCs$RuntimeSet == 1,]
			analytes <- unique(as.character(tableTCs$Analyte))
			
		} else {
			
 			stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
							a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), 'Runtime' for the subset used to evaluate the runtime,
							a number to randomly assign N test sets per biomarker, 
							or the filtered table with the test sets that wished to be evaluated.")
		}
	} else if(is.numeric(subset)){
		# check input parameter
		stopifnot(subset >= 1 & subset <= 576)
		
		# define subset with N
		warning("The overall score is only comparable to the scores featured in the original publication if all test sets are evaluated.", call. = FALSE)
		tableTCs <- loadTestsetDefinition()
		tableTCs <- defineSubset(tableTCs,N = subset)
		analytes <- unique(as.character(tableTCs$Analyte))
		
	} else if(is.data.frame(subset)){
		## use specified table
		warning("The overall score is only comparable to the scores featured in the original publication if all test sets are evaluated.", call. = FALSE)
		analytes <- unique(as.character(subset$Analyte))
		tableTCs <- subset
	}else {
		stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
						a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), 'Runtime' for the subset used to evaluate the runtime,
						a number to randomly assign N test sets per biomarker, 
						or the filtered table with the test sets that wished to be evaluated.")
	}	
	
	# initialize colList if not set
	if (is.null(cols)){
		cols = topo.colors(n = length(algoNames))
	}
		
		
	errorList    <- NULL
	errorListRIs <- NULL
	
	message("Read result files and compute performance measures ...")
	# read result files and compute RIs / use pre-calculated RIs and calculate performance measures 
	for(a in 1:length(algoNames)){
		algo = algoNames[a]
		resIn <- readResultFilesAll(analytes = analytes, algo = algo, baseDir = workingDir, tableTCs = tableTCs)
		
		# compute RIs or use precalculated RIs
		if(inherits(x = resIn[[1]][[1]], what = "RWDRI")){
			if(length(truncNormal) == length(algoNames) & length(truncNormal) > 1)
				allRI <- computeRIsAll(analytes = analytes, algo = algo, resIn = resIn, tableTCs = tableTCs, truncNormal = truncNormal[a])
			else 
				allRI <- computeRIsAll(analytes = analytes, algo = algo, resIn = resIn, tableTCs = tableTCs, truncNormal = truncNormal)
		}else {
			allRI <- getRIsAllwithoutModel(analytes = analytes, algo = algo, resIn = resIn, tableTCs = tableTCs)
		}
		# compute performance measures
		errorAlgo 			<- computePerfMeasAll(analytes = analytes, algo =algo, risIn = allRI, tableTCs =tableTCs, cutoffZ = cutoffZ)
		errorList[[a]] 		<- errorAlgo
		errorListRIs[[a]] 	<- errorAlgo$resRIs
	}
	
	names(errorList) 	<- algoNames
	names(errorListRIs) <- algoNames
	
	
	# run direct method if specified 
	if(withDirect){
		message("Simulate direct method ... ")
		errorDirect <- runDirectMethod(tableTCs = tableTCs, N = 120, cutoffZ = cutoffZ)
		
		errorList    <- c(list(errorDirect), errorList)
		errorListRIs <- c(list(errorDirect$resRIs), errorListRIs)
		names(errorList)[1] 	<- paste0("direct_120")
		names(errorListRIs)[1] 	<- paste0("direct_120")
		
		cols <- c(colDirect, cols)
		
	}
	
	message("Generate plots ... ")
	# generate plots manuscript 


	if(is.data.frame(subset) || (subset == "all" | subset == "normal" | subset == "skewed" | subset == "heavilySkewed" | subset == "shifted" | subset == "Runtime" | is.numeric(subset) )) {
		message("Distributon types")
		catListDist <- paste0("'", unique(tableTCs$Distribution), "'")
		catListDist <- paste0("Distribution == ",catListDist)
		
		catLabelsDist <- unique(tableTCs$Distribution)
		
		# get results for distribution types
		error_subsetDist <- list()
		for (a in 1:length(names(errorListRIs))){
			error_subsetDist[[a]] <- mergeAnalytes(tableTCs = tableTCs, errorList = errorListRIs[[a]], 	catList = catListDist, catLabels = catLabelsDist, distTypes = TRUE)
		}
		
		names(error_subsetDist) <- names(errorListRIs)
		
		# generate boxplots per distribution type - overall performance
		generateBoxplotsDistTypes(errorListAll = error_subsetDist, 
				colList = cols,
				nameList = names(error_subsetDist),
				catList = catListDist, 
				catLabels = catLabelsDist, 
				errorName = errorParam, 
				outline = outline, 
				withMean = withMean, 
				withDirect = withDirect, 
				withCats = TRUE, 
				ylab = ylab,
				outputDir = outputDir, 
				filenamePart = "overallPerformance_DistTypes"
		)
		
		
		message("Distribution types and split by pathological fraction")
		# generate boxplots per distribution type split by pathological fraction
	
		error_subsetDistP 		<- list()
		error_subsetDistP[[1]] 	<- error_subsetDist[[1]]
		
		for (a in 2:length(names(errorListRIs))){
				error_subsetDistP[[a]] <- getSubset(subsetDef = catLabelsDist, distType =TRUE, tableTCs = tableTCs, 
						errorList = error_subsetDist[[a]], category ="fractionPathol", restrict = NULL)	
			
		}
		names(error_subsetDistP) <- names(errorListRIs)
						
		patholSubset <- unique(tableTCs$fractionPathol)
		
		# if the subset is restricted to only some pathological fractions, remove the remaining ones
		if(is.data.frame(subset) & length(patholSubset) != 8){
			nCats <- length(patholSubset)
			
			if(0 %in% patholSubset)
				patholSubset[patholSubset == 0] <- as.character("0.0")
		
			catLabelsP <- paste0("p=",as.character(patholSubset))
				
			for (a in 2:length(names(error_subsetDistP))){
				for(d in 1:length(names(error_subsetDistP[[a]]))){
					toRemove <-	names(error_subsetDistP[[a]][[d]])[!(names(error_subsetDistP[[a]][[d]]) %in% catLabelsP)]
					error_subsetDistP[[a]][[d]] <- error_subsetDistP[[a]][[d]][-which(names(error_subsetDistP[[a]][[d]]) %in% toRemove)]			
				}
			}
			
		}else {
			nCats 	 	<- length(error_subsetDistP[[2]][[1]])
			catLabelsP 	<- names(error_subsetDistP[[2]][[1]])
		}

		
		nameListP <- unlist(lapply(names(errorListRIs)[-1], function(x) {paste(x, catLabelsP)}))
		colListP  <- unlist(lapply(cols[-1], function(x) {rep(x, nCats)}))
				
		generateBoxplotsMultipleCats(analytes = catLabelsDist, 
				errorListAll = error_subsetDistP,
				colList      = c(colDirect, colListP),
				nameList     = c("direct 120", nameListP),
				category     = "fractionPathol",
				errorName    = errorParam, 
				outline      = outline, 
				withMean     = withMean, 
				withDirect   = withDirect, 
				ylab         = ylab,
				outputDir    = outputDir, 
				filenamePart = "performance_PathologicalFraction_DistTypes"
		)
		
		
		message("Distribution types and split by sample size")
		
		# generate boxplots per distribution type split by sample size
		error_subsetDistN <- list()
		error_subsetDistN[[1]] <- error_subsetDist[[1]]
		
		for (a in 2:length(names(errorListRIs))){
			error_subsetDistN[[a]] <- getSubset(subsetDef = catLabelsDist, distType =TRUE, tableTCs = tableTCs, 
					errorList = error_subsetDist[[a]], 	category ="N", restrict = NULL)
		}
		names(error_subsetDistN) <- names(errorListRIs)
		
		NSubset <- unique(tableTCs$N)
		
		# if subset is restricted to some sample sizes, remove the remaining ones
		if(is.data.frame(subset) & length(NSubset) != 4){
			nCats <- length(NSubset)
			
			if(5e+05 %in% NSubset)
				NSubset[NSubset == 5e+05] <- as.character("500000")
			
			catLabelsN <- paste0("N=",as.character(NSubset))
						
			for (a in 2:length(names(error_subsetDistN))){
				for(d in 1:length(names(error_subsetDistN[[a]]))){
					toRemove <-	names(error_subsetDistN[[a]][[d]])[!(names(error_subsetDistN[[a]][[d]]) %in% catLabelsN)]
					error_subsetDistN[[a]][[d]] <- error_subsetDistN[[a]][[d]][-which(names(error_subsetDistN[[a]][[d]]) %in% toRemove)]			
				}
			}
			
		}else {
			nCats 		<- length(error_subsetDistN[[2]][[1]])
			catLabelsN 	<- names(error_subsetDistN[[2]][[1]])
		}
		
		
		nameListN <- unlist(lapply(names(errorListRIs)[-1], function(x) {paste(x, catLabelsN)}))
		colListN  <- unlist(lapply(cols[-1], function(x) {rep(x, nCats)}))
		
		generateBoxplotsMultipleCats(analytes = catLabelsDist, 
				errorListAll = error_subsetDistN,
				colList      = c(colDirect, colListN),
				nameList     = c("direct 120", nameListN),
				category     = "N",
				errorName    = errorParam, 
				outline      = outline, 
				withMean     = withMean, 
				withDirect   = withDirect, 
				ylab         = ylab,
				outputDir    = outputDir, 
				filenamePart = "performance_SampleSize_DistTypes"
		)
		
		message("benchmark score ")
		
		# compute benchmark score results table
		ovRes <- getBenchmarkResults(errorList = errorList[-1], nameVec = names(errorList)[-1], tableTCs = tableTCs) 
		
		# generate bar plot 
		plotBarplot(benchmarkRes = ovRes, perDistType = FALSE, colList = cols[-1], nameList = algoNames,
				xlim = c(0,2.5), legendLoc ="topright", 
				outputDir = outputDir, 
				filename ="Barplot_BenchmarkScoreResults.png"
		)
	} else if (subset %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH")){
	
		
		message("Plots for analytes")
		# get results for analytes
		analytes <- unique(tableTCs$Analyte)
		
			
		# generate boxplots per distribution type - overall performance
		generateBoxplotsDistTypes(errorListAll = errorListRIs, 
				colList = cols,
				nameList = names(errorListRIs),
				catList = analytes, 
				catLabels = analytes, 
				errorName = errorParam, 
				outline = outline, 
				withMean = withMean, 
				withDirect = withDirect, 
				withCats = TRUE, 
				ylab = ylab,
				outputDir = outputDir, 
				filenamePart = "overallPerformance"
		)
		
		message("Plot for analyte split by pathological fraction")
# generate boxplots per distribution type split by pathological fraction
		
		error_subsetP <- list()
		error_subsetP[[1]] <- errorListRIs[[1]]
		
		analytes <- unique(tableTCs$Analyte)
		
		for (a in 2:length(names(errorListRIs))){
			error_subsetP[[a]] <- getSubset(subsetDef = analytes, distType = FALSE, tableTCs = tableTCs, 
					errorList = errorListRIs[[a]], category ="fractionPathol", restrict = NULL)
		}
		
		names(error_subsetP) <- names(errorListRIs)
		
		nCats      <- length(error_subsetP[[2]][[1]])
		catLabelsP <- names(error_subsetP[[2]][[1]])
		
		nameListP <- unlist(lapply(names(errorListRIs)[-1], function(x) {paste0("", catLabelsP)}))
		colListP  <- unlist(lapply(cols[-1], function(x) {rep(x, nCats)}))
		
		
		generateBoxplotsMultipleCats(analytes = analytes, 
				errorListAll = error_subsetP,
				colList    = c(colDirect, colListP),
				nameList   = c("direct 120", nameListP),
				category   = "fractionPathol",
				errorName  = errorParam, 
				outline    = outline, 
				withMean   = withMean, 
				withDirect = withDirect, 
				ylab       = ylab,
				outputDir  = outputDir, 
				filenamePart = "performance_PathologicalFraction" 
		
		)	
		
		message("Plot for analyte split by sample size")
		
		# generate boxplots per distribution type split by sample size
		error_subsetN 		<- list()
		error_subsetN[[1]] 	<- errorListRIs[[1]]
		
		for (a in 2:length(names(errorListRIs))){
			error_subsetN[[a]] <- getSubset(subsetDef = analytes, distType = FALSE, tableTCs = tableTCs, 
					errorList = errorListRIs[[a]], 	category = "N", restrict = NULL)
		}
		names(error_subsetN) <- names(errorListRIs)
		
		nCats 		<- length(error_subsetN[[2]][[1]])
		catLabelsN 	<- names(error_subsetN[[2]][[1]])
		
		nameListN <- unlist(lapply(names(errorListRIs)[-1], function(x) {paste0("", catLabelsN)}))
		colListN  <- unlist(lapply(cols[-1], function(x) {rep(x, nCats)}))
		
		generateBoxplotsMultipleCats(analytes = analytes, 
				errorListAll = error_subsetN,
				colList      = c(colDirect, colListN),
				nameList     = c("direct 120", nameListN),
				category     = "N",
				errorName    = errorParam, 
				outline      = outline, 
				withMean     = withMean, 
				withDirect   = withDirect, 
				ylab         = ylab,
				outputDir    = outputDir, 
				filenamePart = "performance_SampleSize",
				mar = c(5.1, 4.1, 0, 1), width = 1500, height = 700, res = 140
		
		)
		
		ovRes <- NULL
	}
	
	return(ovRes)
	
}



#' Function for reading in the result files for one marker 
#' 
#' @param analyte		(character) specifying analyte  
#' @param algo			(character)	specifying used algorithm
#' @param path			(character) specifying path to Results directories 
#' @param tableTCs		(data frame) containing all information about the simulated test sets
#' 
#' @return list with caluclated results as RWDRI objects 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

readResultFiles <- function(analyte, algo, path = NULL,  tableTCs = NULL){
	
	inputDir 	<- file.path(path, algo, analyte)
	
	# check if input directory exists
	if(!dir.exists(inputDir))
		stop(paste0("Directory '", inputDir, "' does not exist. Please check 'workingDir' and 'subset' parameter."))
		
	results     <- list()
	
	# traverse test set specification and read result files 
	for (rr in 1:nrow(tableTCs)){
		testDef <- tableTCs[rr,]
		filename <- paste0(testDef$Index, "_", testDef$Analyte,"_seed_", testDef$startSeed, "_", algo, ".Rdata" )
		
		# check if file exists
		if(!file.exists(file.path(inputDir, filename)))
			stop(simpleError(paste0("Algorithm result '", filename, "' does not exist. Please check 'workingDir' and 'subset' parameter.")))
		
		# load file and save result
		tmp 			 <- get(load(file.path(inputDir, filename)))
		tmp$Index 		 <- testDef$Index
		results[[as.character(testDef$Index)]] <- tmp	
	}
	
	
	return(results)
}


#' Function for reading all results files. 
#' 
#' @param analytes		(character) listing all analytes for which the result files should be parsed  
#' @param algo			(character)	specifying used algorithm
#' @param baseDir		(character) specifying the baseDir: Results will be used from baseDir/Results/algo/marker 
#' 								if baseDir is set, inputDir will be ignored; if baseDir is NULL, the current working directory will be used
#' @param inputDir		(character) specifying path directly to Results directories 
#' @param tableTCs		(data frame) containing all information about the simulated test sets
#' 
#' @return list with all caluclated results as RWDRI objects for each marker 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

readResultFilesAll <- function(analytes, algo, baseDir =NULL, inputDir = NULL, tableTCs){
	
	stopifnot(is.character(analytes))
	stopifnot(is.character(algo))
	stopifnot(is.null(baseDir) | is.character(baseDir))
	stopifnot(is.null(inputDir) | is.character(inputDir))
	stopifnot(is.data.frame(tableTCs))
	
	# get inputDir
	if(is.null(baseDir) & is.null(inputDir)){
		inputDir = getwd()
	}else if(!is.null(baseDir)){
		inputDir = file.path(baseDir, "Results")
	}
	
	# traverse analytes and read result files
	resIn <- list()
	for(aa in 1:length(analytes)){
		message(analytes[aa])
		resIn[[aa]] 	<- readResultFiles(analyte = analytes[aa], algo = algo, path = inputDir, tableTCs = tableTCs[tableTCs$Analyte == analytes[aa],])
	}

	names(resIn) <- analytes
	return(resIn)
}


#' Function for computing reference intervals
#' 
#' @param analyte		(character) specifiyng analyte
#' @param algo			(character)	specifying used algorithm
#' @param results 		(list) with all calculated results as RWDRI objects 
#' @param tableTCs		(data frame) containing all information about the simulated test sets 
#' @param RIperc		(numeric) vector specifying the percentiles for the reference interval, default: 0.025 and 0.975
#' @param truncNormal	(logical) specifying if a normal distribution truncated at zero shall be assumed 
#' 
#' @return data frame with computed reference intervals 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computeRIs <- function(analyte, algo, results, tableTCs, RIperc = c(0.025, 0.975), truncNormal =FALSE){
	
	# set up resulting data frame 
	resDf 			<- data.frame(matrix(nrow = length(results), ncol = 21))
	colnames(resDf) <- c("Index","Analyte", "Algorithm", "GT_LRL", "GT_URL", "LRL","URL", 
			"GT_Lambda", "GT_Mu", "GT_Sigma", "GT_Shift","Est_Lambda", "Est_Mu", "Est_Sigma", "Est_Shift", "Est_A", "Est_B", 
			"Est_Cost", "Est_P",
			"Runtime", "Status")
	
	# get table for analyte
	subTable 		<- tableTCs[tableTCs$Analyte == analyte, ]
	
	# get info that is shared for all test sets
	resDf$Analyte	<- analyte
	resDf$Algorithm <- algo
	resDf$GT_LRL	<- subTable$GT_LRL[1]
	resDf$GT_URL	<- subTable$GT_URL[1]
	resDf$GT_Lambda	<- subTable$nonp_lambda[1]
	resDf$GT_Mu		<- subTable$nonp_mu[1]
	resDf$GT_Sigma	<- subTable$nonp_sigma[1]
	resDf$GT_Shift	<- subTable$nonp_shift[1]
	
	# traverse results and compute reference intervals
	for (i in 1:length(results)){
		model <- results[[i]]
		index <- model$Index
		
		if(inherits(x = model, what = "RWDRI") & is.data.frame(model) && is.na(model$PointEst)){
			# if no model was found
			model 			<- NULL
			model		 	<- list()
			model$Lambda 	<- NA
			model$Mu 	 	<- NA
			model$Sigma	 	<- NA
			model$Shift	 	<- NA
			model$Status 	<- NA
			model$P		 	<- NA
			model$abOr 	 	<- c(NA,NA)
			model$Runtime 	<- c(NA,NA,NA)
			resDf[i,]$Index <- index[1]
			resDf[i,c("LRL","URL")]	<- ris$PointEst
			
			
		}else {
			# if a model was found, compute the reference intervals
			resDf[i,]$Index <- index
			ris				<- getRI(x = model, RIperc = RIperc, truncNormal = truncNormal)
			resDf[i,c("LRL","URL")]	<- ris$PointEst
			
			if(is.null(model$abOr))
				model$abOr <- c(NA,NA)
			
			if(is.null(model$Cost))
				model$Cost <- NA
			
			if(is.null(model$Runtime)){
				model$Runtime <- c(NA,NA,NA)
			}else if(length(model$Runtime) ==1 ){
				model$Runtime <- c(NA,NA, model$Runtime)
			}
			
			if(is.null(model$Status))
				model$Status  <- NA
			
			if(!is.na(model$Status) & model$Status != "timeout" & model$Status != "ResultFound")
				model$Runtime <- c(NA,NA,NA)
			
			if(is.null(model$P))
				model$P <- NA
		}
		
	
		# save all estimated parameters into the resulting data frame 
		resDf[i,c("Est_Lambda", "Est_Mu", "Est_Sigma", "Est_Shift", "Est_A", "Est_B","Est_Cost", "Est_P", "Runtime")] <- c(model$Lambda, model$Mu, model$Sigma, model$Shift, model$abOr[1], model$abOr[2], model$Cost, model$P, model$Runtime[3])
		resDf[i, "Status"] <- model$Status
	}
	
	return(resDf)
	
}


#' Function for computing reference intervals for all markers
#' 
#' @param analytes		(character) listing all markers for which the result files should be parsed  
#' @param algo			(character)	specifying used algorithm
#' @param resIn 		(list) with all calculated results for all markers as RWDRI objects 
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param truncNormal	(logical) specifying if a normal distribution truncated at zero shall be assumed 
#' 
#' @return list with the calculated reference intervals as data frame for each marker 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computeRIsAll <- function(analytes, algo, resIn, tableTCs, truncNormal =FALSE){
	
	stopifnot(is.character(analytes))
	stopifnot(is.character(algo))
	stopifnot(is.list(resIn))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.logical(truncNormal))
	
	# traverse analytes and compute reference intervals 
	risList <- list()
	for(a in 1:length(analytes)){
		aName <- analytes[a]
		
		if(aName == "CRP")
			RIperc = (0.95)
		else 
			RIperc = c(0.025, 0.975)
		
		risList[[aName]] <- computeRIs(analyte = aName, algo = algo, results = resIn[[aName]], tableTCs = tableTCs, RIperc = RIperc, truncNormal=truncNormal)
	}
	names(risList) <- analytes
	return(risList)
}



#' Function for retrieving reference intervals if directly computed
#' 
#' @param analytes		(character) listing all markers for which the result files should be parsed  
#' @param algo			(character)	specifying used algorithm
#' @param resIn 		(list) with all calculated results for all markers as RWDRI objects 
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' 
#' @return list with the calculated reference intervals as data frame for each marker 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

getRIsAllwithoutModel <- function(analytes, algo, resIn, tableTCs){
	
	risList <- list()
	
	# traverse analytes and get directly computed reference intervals into resulting data frame 
	for(aa in 1:length(analytes)){
		aName <- analytes[aa]
		results = resIn[[aName]]
		
		resDf 			<- data.frame(matrix(nrow = length(results), ncol = 19))
		colnames(resDf) <- c("Index","Analyte", "Algorithm", "GT_LRL", "GT_URL", "LRL","URL", 
				"GT_Lambda", "GT_Mu", "GT_Sigma", "GT_Shift","Est_Lambda", "Est_Mu", "Est_Sigma", "Est_Shift", "Est_A", "Est_B", 
				"Est_Cost","Runtime")
		
		subTable 		<- tableTCs[tableTCs$Analyte == aName, ]
		
		resDf$Analyte	<- aName
		resDf$Algorithm <- algo
		resDf$GT_LRL	<- subTable$GT_LRL[1]
		resDf$GT_URL	<- subTable$GT_URL[1]
		resDf$GT_Lambda	<- subTable$nonp_lambda[1]
		resDf$GT_Mu		<- subTable$nonp_mu[1]
		resDf$GT_Sigma	<- subTable$nonp_sigma[1]
		resDf$GT_Shift	<- subTable$nonp_shift[1]
		
		
		for (i in 1:length(results)){
			model <- results[[i]]
			index <- model$Index[1]
			
			resDf[i,]$Index <- index
			
			if(aName == "CRP")
				resDf[i,c("LRL","URL")] <- c(0, model$PointEst)
			else
				resDf[i,c("LRL","URL")]	<- model$PointEst
#			
			abOr <- model$abOr
			
			if(is.null(model$abOr))
				abOr <- c(NA,NA)
			
			resDf[i,c("Est_Lambda", "Est_Mu", "Est_Sigma", "Est_Shift", "Est_A", "Est_B")] <- c(model$Lambda, model$Mu, model$Sigma, model$Shift, abOr[1], abOr[2])
			
			if(is.null(model$Runtime))
				resDf[i,"Runtime"] <- NA	
			else 
				resDf[i,"Runtime"] <- model$Runtime[1]
			
			if(is.null(model$Cost))
				resDf[i, "Est_Cost"] <- NA
			else 
				resDf[i, "Est_Cost"] <- model$Cost
		}
		
		risList[[aName]] <- resDf
		
	}
	names(risList) <- analytes
	return(risList)
}


#' Function for computing performance measurements
#' 
#' @param analyte		(character) specifiyng current analyzed analyte
#' @param algo			(character)	specifying used algorithm
#' @param resRIs 		(data.frame) with all calculated reference intervals 
#' @param subTable		(data.frame) containing all information about the simulated test sets 
#' @param RIperc		(numeric) vector specifying the percentiles for the reference interval, default: 0.025 and 0.975
#' @param cutoffZ		(numeric) specifying if a cutoff should be used to classify results as implausible and exclude from analysis 
#' 
#' 
#' @return updated data frame with computed performance measures
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computePerfMeas <- function(analyte, algo, resRIs, subTable, RIperc = c(0.025, 0.975), cutoffZ = 5){
	
	# compute error/ bias
	resRIs$Error_LRL <- resRIs$GT_LRL - resRIs$LRL
	resRIs$Error_URL <- resRIs$GT_URL - resRIs$URL
	resRIs$Error_Ov	 <- (resRIs$Error_LRL + resRIs$Error_URL)/2
	
	# compute percentage error
	resRIs$PercError_LRL <- (resRIs$Error_LRL/resRIs$GT_LRL)*100
	resRIs$PercError_URL <- (resRIs$Error_URL/resRIs$GT_URL)*100
	resRIs$PercError_Ov  <- (resRIs$PercError_LRL + resRIs$PercError_URL)/2
	
	# compute absolute error
	resRIs$AbsError_LRL <- abs(resRIs$Error_LRL)
	resRIs$AbsError_URL <- abs(resRIs$Error_URL)
	resRIs$AbsError_Ov	<- (resRIs$Error_LRL + resRIs$Error_URL)/2
	
	# compute absolute percentage error
	resRIs$AbsPercError_LRL <- abs(resRIs$PercError_LRL)
	resRIs$AbsPercError_URL <- abs(resRIs$PercError_URL)
	resRIs$AbsPercError_Ov	<- (resRIs$AbsPercError_LRL + resRIs$AbsPercError_URL)/2
	
	
	# compute absolute percentage error based on width of RI
	denom <- resRIs$GT_URL - resRIs$GT_LRL
	resRIs$AbsPercErrorWidth_LRL <- (abs(resRIs$Error_LRL)/denom)*100
	resRIs$AbsPercErrorWidth_URL <- (abs(resRIs$Error_URL)/denom)*100
	resRIs$AbsPercErrorWidth_Ov		 <- (resRIs$AbsPercErrorWidth_LRL+resRIs$AbsPercErrorWidth_URL)/2
	
#	compute z-scores and error in z-score scale 
	resRIs$zzLRL 	 <- (BoxCox(resRIs$GT_LRL - subTable$nonp_shift[1], lambda =subTable$nonp_lambda[1])- subTable$nonp_mu[1])/subTable$nonp_sigma[1]
	resRIs$zzURL 	 <- (BoxCox(resRIs$GT_URL - subTable$nonp_shift[1], lambda =subTable$nonp_lambda[1])- subTable$nonp_mu[1])/subTable$nonp_sigma[1]
	
	if(any(resRIs$LRL == 0 & !is.na(resRIs$LRL)) & subTable$nonp_shift[1] == 0){
		tmp <- resRIs
		tmp[tmp$LRL == 0 & !is.na(tmp$LRL),]$LRL <- 1e-20
		resRIs$zzLRL_Est <- (BoxCox(tmp$LRL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
		
	} else if(any(resRIs$LRL - subTable$nonp_shift[1] <= 0 & !is.na(resRIs$LRL))){
		tmp <- resRIs
		tmp[(tmp$LRL - subTable$nonp_shift[1] <= 0) & !is.na(tmp$LRL),]$LRL <- subTable$nonp_shift[1]+1e-20
		resRIs$zzLRL_Est <- (BoxCox(tmp$LRL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
				
	} else {
		resRIs$zzLRL_Est <- (BoxCox(resRIs$LRL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
	}
	
	
	if(any(resRIs$URL == 0 & !is.na(resRIs$URL))){
		tmp <- resRIs
		tmp[tmp$URL == 0 & !is.na(tmp$URL),]$URL <- 1e-20
		resRIs$zzURL_Est <- (BoxCox(tmp$URL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
		
	}else if(any(resRIs$URL - subTable$nonp_shift[1] <= 0 & !is.na(resRIs$URL))){
		tmp <- resRIs
		tmp[(tmp$URL - subTable$nonp_shift[1] <= 0) & !is.na(tmp$URL),]$URL <- subTable$nonp_shift[1]+1e-20
		resRIs$zzURL_Est <- (BoxCox(tmp$URL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
		
	} else {
		resRIs$zzURL_Est <- (BoxCox(resRIs$URL - subTable$nonp_shift[1], lambda = subTable$nonp_lambda[1])-subTable$nonp_mu[1])/subTable$nonp_sigma[1]
	}
	
	resRIs$zzDev_LRL <- resRIs$zzLRL - resRIs$zzLRL_Est
	resRIs$zzDev_URL <- resRIs$zzURL - resRIs$zzURL_Est
	resRIs$zzDev_Ov	 <- (resRIs$zzDev_LRL + resRIs$zzDev_URL)/2
	
	if(analyte == "CRP")
		resRIs$zzDev_Ov <- resRIs$zzDev_URL 

	
	resRIs$zzDevAbs_LRL <- abs(resRIs$zzDev_LRL)
	resRIs$zzDevAbs_URL <- abs(resRIs$zzDev_URL)
	resRIs$zzDevAbs_Ov  <- (resRIs$zzDevAbs_LRL + resRIs$zzDevAbs_URL)/2
	
	if(analyte == "CRP")
		resRIs$zzDevAbs_Ov <- resRIs$zzDevAbs_URL 
	
	
	resRIs$zzDevAbsCutoff_Ov <- resRIs$zzDevAbs_Ov
	resRIs$zzDevAbsCutoff_Ov[resRIs$zzDevAbsCutoff_Ov > cutoffZ & !is.na(resRIs$zzDevAbsCutoff_Ov)] <- NA
	
	summaryErrors <- data.frame(matrix(nrow = 1, ncol = 61))
	colnames(summaryErrors) <- c("Analyte", "Algorithm", "MeanErrorLRL", "MeanErrorURL", "MeanErrorOV",
			"MeanPercErrorLRL", "MeanPercErrorURL", "MeanPercErrorOv",
			"MeanAbsErrorLRL", "MeanAbsErrorURL", "MeanAbsErrorOV", 
			"MeanAbsPercErrorLRL", "MeanAbsPercErrorURL", "MeanAbsPercErrorOV", 
			"MeanAbsPercErrorWidthLRL", "MeanAbsPercErrorWidthURL", "MeanAbsPercErrorWidthOV",
			"MeanZDevLRL", "MeanZDevURL", "MeanZDevOV", 
			"MeanZDevAbsLRL", "MeanZDevAbsURL", "MeanZDevAbsOV",
			"MeanZDevAbsCutoffOV",
			
			"MedianErrorLRL", "MedianErrorURL", "MedianErrorOV",
			"MedianPercErrorLRL", "MedianPercErrorURL", "MedianPercErrorOv",
			"MedianAbsErrorLRL", "MedianAbsErrorURL", "MedianAbsErrorOV", 
			"MedianAbsPercErrorLRL", "MedianAbsPercErrorURL", "MedianAbsPercErrorOV",
			"MedianAbsPercErrorWidthLRL", "MedianAbsPercErrorWidthURL", "MedianAbsPercErrorWidthOV",
			"MedianZDevLRL", "MedianZDevURL", "MedianZDevOV",
			"MedianZDevAbsLRL", "MedianZDevAbsURL", "MedianZDevAbsOV",
			"MedianZDevAbsCutoffOV",
			
			"SumZDevLRL", "SumZDevURL", "SumZDevOv", 
			"SumZDevAbsLRL", "SumZDevAbsURL", "SumZDevAbsOv",
			"SumZDevAbsCutoffOv",
			
			"QQAbsPercErrorLRL", "QQAbsPercErrorURL", "QQAbsPercErrorOV",
			"QQZDevAbsLRL", "QQZDevAbsURL", "QQZDevAbsOV",
			
			"NrAboveCutoff",
			"NrNAs")
	
	summaryErrors[1,] <- c(analyte, algo, 
			mean(resRIs$Error_LRL, na.rm =TRUE), mean(resRIs$Error_URL, na.rm =TRUE), mean(resRIs$Error_Ov, na.rm =TRUE), 
			mean(resRIs$PercError_LRL, na.rm =TRUE), mean(resRIs$PercError_URL, na.rm =TRUE), mean(resRIs$PercError_Ov, na.rm =TRUE), 
			mean(resRIs$AbsError_LRL, na.rm =TRUE), mean(resRIs$AbsError_URL, na.rm =TRUE), mean(resRIs$AbsError_Ov, na.rm =TRUE), 
			mean(resRIs$AbsPercError_LRL, na.rm =TRUE), mean(resRIs$AbsPercError_URL, na.rm =TRUE), mean(resRIs$AbsPercError_Ov, na.rm =TRUE), 
			mean(resRIs$AbsPercErrorWidth_LRL, na.rm =TRUE), mean(resRIs$AbsPercErrorWidth_URL, na.rm =TRUE), mean(resRIs$AbsPercErrorWidth_Ov, na.rm =TRUE),
			mean(resRIs$zzDev_LRL, na.rm =TRUE), mean(resRIs$zzDev_URL, na.rm =TRUE), mean(resRIs$zzDev_Ov, na.rm =TRUE),
			mean(resRIs$zzDevAbs_LRL, na.rm =TRUE), mean(resRIs$zzDevAbs_URL, na.rm =TRUE), mean(resRIs$zzDevAbs_Ov, na.rm =TRUE),
			mean(resRIs$zzDevAbsCutoff_Ov, na.rm =TRUE),
			
			
			median(resRIs$Error_LRL, na.rm =TRUE), median(resRIs$Error_URL, na.rm =TRUE), median(resRIs$Error_Ov, na.rm =TRUE), 
			median(resRIs$PercError_LRL, na.rm =TRUE), median(resRIs$PercError_URL, na.rm =TRUE), median(resRIs$PercError_Ov, na.rm =TRUE), 
			median(resRIs$AbsError_LRL, na.rm =TRUE), median(resRIs$AbsError_URL, na.rm =TRUE), median(resRIs$AbsError_Ov, na.rm =TRUE), 
			median(resRIs$AbsPercError_LRL, na.rm =TRUE), median(resRIs$AbsPercError_URL, na.rm =TRUE), median(resRIs$AbsPercError_Ov, na.rm =TRUE),
			median(resRIs$AbsPercErrorWidth_LRL, na.rm =TRUE), median(resRIs$AbsPercErrorWidth_URL, na.rm =TRUE), median(resRIs$AbsPercErrorWidth_Ov, na.rm =TRUE),
			median(resRIs$zzDev_LRL, na.rm =TRUE), median(resRIs$zzDev_URL, na.rm =TRUE), median(resRIs$zzDev_Ov, na.rm =TRUE),
			median(resRIs$zzDevAbs_LRL, na.rm =TRUE), median(resRIs$zzDevAbs_URL, na.rm =TRUE), median(resRIs$zzDevAbs_Ov, na.rm =TRUE),
			median(resRIs$zzDevAbsCutoff_Ov, na.rm =TRUE),
			
			
			sum(resRIs$zzDev_LRL, na.rm =TRUE), sum(resRIs$zzDev_URL, na.rm =TRUE), sum(resRIs$zzDev_Ov, na.rm =TRUE),
			sum(resRIs$zzDevAbs_LRL, na.rm =TRUE), sum(resRIs$zzDevAbs_URL, na.rm =TRUE), sum(resRIs$zzDevAbs_Ov, na.rm =TRUE),
			sum(resRIs$zzDevAbsCutoff_Ov, na.rm =TRUE),
			
			quantile(resRIs$AbsPercError_LRL,probs = 0.95, na.rm =TRUE), quantile(resRIs$AbsPercError_URL,probs = 0.95, na.rm =TRUE), quantile(resRIs$AbsPercError_Ov, probs = 0.95,na.rm =TRUE),
			quantile(resRIs$zzDevAbs_LRL,probs = 0.95, na.rm =TRUE), quantile(resRIs$zzDevAbs_URL,probs = 0.95, na.rm =TRUE), quantile(resRIs$zzDevAbs_Ov, probs = 0.95,na.rm =TRUE),
			
			nrow(resRIs[resRIs$zzDevAbs_Ov > cutoffZ & !is.na(resRIs$zzDevAbs_Ov),]),
			nrow(resRIs[is.na(resRIs$LRL) | is.na(resRIs$URL),]))
	
	return(list("resRIs" = resRIs, "summaryErrors" = summaryErrors))
}


#' Function for computing reference intervals for all markers
#' 
#' @param analytes		(character) listing all analytes for which the result files should be parsed  
#' @param algo			(character)	specifying used algorithm
#' @param risIn	 		(list) with data frame of all calculated reference intervals
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param cutoffZ 		(integer) specifying if and if so which cutoff should be used to classify results as implausible (default: 5)
#' 
#' @return list with the calculated errors as data frame for each marker 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computePerfMeasAll <- function(analytes, algo, risIn, tableTCs, cutoffZ = 5){
	
	stopifnot(is.character(analytes))
	stopifnot(is.character(algo))
	stopifnot(is.list(risIn))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.numeric(cutoffZ))
	
	
	tmp 	  <- list()
	errorList <- list()
	errorDF   <- NULL
	RIperc 	  <- c(0.025, 0.975)
	
	# traverse analytes and compute performance measures
	for(a in 1:length(analytes)){
		RIperc 	 <- c(0.025, 0.975)
		subTable <- tableTCs[tableTCs$Analyte == analytes[a],]
		ris 	 <- risIn[[a]]
		
		if(analytes[[a]] =="CRP")
			RIperc <- 0.95
		
		tmp[[a]] 		<- computePerfMeas(analyte = as.character(analytes[[a]]), algo = algo, resRIs = ris, subTable = subTable, RIperc = RIperc, cutoffZ = cutoffZ)
		errorDF			<- rbind(errorDF, tmp[[a]]$summaryErrors)
		errorList[[a]] 	<- tmp[[a]]$resRIs
		
	}
	names(errorList) <- analytes
	
	errorDF[,3:45] <- sapply(errorDF[,3:45], as.numeric)
	
	return(list("resRIs" = errorList, "summaryError" = errorDF))
}


#' Helper function to compute runtime statistics 
#' 
#' @param x 		(data.frame) with one column specifying the Runtime
#' @param analyte	(character) specifying current analyzed marker
#' 
#' @return (data.frame) containing runtime statistics (min, mean, median, max) 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

getRuntime <- function(x, analyte){
	
	
	runtimeDf <- data.frame("Analyte" = analyte, "Min" = min(x$Runtime, na.rm =TRUE), "Mean" = mean(x$Runtime, na.rm =TRUE), 
			"Median" = median(x$Runtime, na.rm =TRUE),"Max" = max(x$Runtime, na.rm =TRUE))
	
	return (runtimeDf)
}


#' Function to compute runtime statistics for all analytes
#' 
#' @param analytes		(character) listing all analytes for which the result files should be parsed  
#' @param algo			(character)	specifying used algorithm
#' @param risIn	 		(list) with data frame of all calculated reference intervals and runtime 
#' @param tableTCs		(data.frame) containing all information about the simulated test cases 
#' 
#' @return (list) wit runtime statistics per analyte and data frames with raw runtime overall and per analyte 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computeRuntimeAll <- function(analytes, algo, risIn, tableTCs){
	
	tmp <- list()
	runtimeDf <- list()
	runtimeMerge <- NULL
	runtimeMarker <- NULL
	
	for(a in 1:length(analytes)){
		message(analytes[[a]])
		
		tmp[[a]]  		<- getRuntime(x = risIn[[a]], analyte = as.character(analytes[[a]]))
		runtimeDf 		<- rbind(runtimeDf, tmp[[a]])
		runtimeMerge 	<- c(runtimeMerge,risIn[[a]]$Runtime)
		runtimeMarker 	<- cbind(runtimeMarker, risIn[[a]]$Runtime)
	}
	
	colnames(runtimeMarker) <- analytes
	
	return(list("runtimeAnalyte" = runtimeDf, "runtimeAll" = runtimeMerge, "runtimeMarker" = runtimeMarker))
}


#' Function to group the data according to a specified feature. 
#' 
#' The feature can either be the pathological fraction, the sample size or the overlap (category) individually or cumulative (_cum). 
#' For an individualized categorisation see \code{getSubsetForDefinedCats}.
#' 
#' @param subsetDef		(character) listing either the analytes or distribution types for which the result files should be parsed  
#' @param distType		(logical) indicating if parameter subsetDef refers to analytes (FALSE, default) or distribution types (TRUE) 
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param errorList		(list) containing for each method the table with the computed error measurements 
#' @param category		(character) defining the category used for creating the subsets. All defined sub-features are used for the categorization. 
#' 							Choose from "fractionPathol" (default), "N", or "OvFreq", individual or cumulative ("_cum")
#' @param restrict		(character) indicating whether test sets should be filtered according to specified restriction, default NULL, 
#' 							e.g. fractionPathol <= 0.30 
#' 
#' @return	(list) containing the performance measurements grouped according to specified subset definition and categories 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

getSubset <- function(subsetDef, distType =FALSE, tableTCs, errorList, category = c("fractionPathol","fractionPathol_cum", "N", "N_cum", "OvFreq", "OvFreq_cum"), restrict =NULL){
	
	# check input parameters
	stopifnot(is.character(subsetDef))
	stopifnot(subsetDef %in% c("normal","skewed", "heavilySkewed", "shifted", "Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH"))
	stopifnot(is.logical(distType))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.list(errorList))
	stopifnot(is.null(restrict) | is.character(restrict))
	
	category <- match.arg(category[1], choices = c("fractionPathol","fractionPathol_cum", "N", "N_cum", "OvFreq", "OvFreq_cum"))
	
	# define partition for specified category
	if (category == "fractionPathol") {
		
		catList <- c("fractionPathol == 0.0", "fractionPathol == 0.05","fractionPathol == 0.1", "fractionPathol == 0.2", "fractionPathol == 0.3", "fractionPathol == 0.4", "fractionPathol == 0.5", "fractionPathol == 0.6")
		catLabels <- c("p=0.0", "p=0.05","p=0.1", "p=0.2", "p=0.3", "p=0.4", "p=0.5", "p=0.6")
		
	} else if (category == "fractionPathol_cum") {
		
		catList 	<- c("fractionPathol <= 0.1", "fractionPathol <= 0.2", "fractionPathol <= 0.3", "fractionPathol <= 0.4", "fractionPathol <= 0.5", "fractionPathol <= 0.6")
		catLabels 	<- c("p<=0.1", "p<=0.2", "p<=0.3", "p<=0.4", "p<=0.5", "p<=0.6")
		
	} else if (category == "N") {
		
		catList 	<- c("N == 1000", "N == 5000", "N == 50000", "N == 500000")
		catLabels 	<- c("N=1000", "N=5000", "N=50000", "N=500000")
		
	} else if (category == "N_cum") {
		
		catList 	<- c("N <= 1000", "N <= 5000", "N <= 50000", "N <= 500000")
		catLabels 	<- c("N<=1000", "N<=5000", "N<=50000", "N<=500000")
		
	} else if (category == "OvFreq") {
		
		catList	  <- c("OvFreq <= 5", "OvFreq <= 10", "OvFreq <= 20", "OvFreq <= 30", "OvFreq <= 60")
		catLabels <- c("OvFreq <=5", "OvFreq <=10", "OvFreq <=20", "OvFreq <=30", "OvFreq <=60")
		
	} else if (category == "OvFreq_cum") {
		
		catList	  <- c("OvFreq <= 5", "OvFreq > 5 & OvFreq <= 10", "OvFreq > 10 & OvFreq <= 20", "OvFreq > 20 & OvFreq <= 30", "OvFreq > 30")
		catLabels <- c("OvFreq <=5", "OvFreq >5 & <=10", "OvFreq >10 & <=20", "OvFreq >20 & <=30", "OvFreq >30")
		
	}
	
	
	subsetList <- getSubsetForDefinedCats(subsetDef = subsetDef, distType =distType, tableTCs = tableTCs, 
			errorList = errorList, catList = catList, catLabels = catLabels, restrict = restrict)
		
	

	return(subsetList)
	
	
}


#' Function to group the data according to a specified feature. 
#' 
#' @param subsetDef		(character) listing either the analytes or distribution types for which the result files should be parsed  
#' @param distType		(logical) indicating if 'subsetDef' refers to analytes (FALSE, default) or distribution types (TRUE) 
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param errorList		(list) containing the table with the computed error measurements 
#' @param catList		(list) containing the categories to split the dataset
#' @param catLabels		(list) containing the labels that will be used for the categories
#' @param restrict		(character) indicating whether testcases should be filtered according to specified restriction, default NULL, 
#' 							e.g. fractionPathol <= 0.30 
#' 
#' @return	(list) containing the performance measurements grouped according to specified subset definition and categories 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

getSubsetForDefinedCats <- function(subsetDef, distType =FALSE, tableTCs, errorList, catList = NULL, catLabels = NULL, restrict = NULL){
	
	
	# check input parameters
	stopifnot(is.character(subsetDef))
	stopifnot(subsetDef %in% c("normal","skewed", "heavilySkewed", "shifted", "Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH"))
	stopifnot(is.logical(distType))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.list(errorList))
	stopifnot(!is.null(catList))
	stopifnot(length(catList) == length(catLabels))
	stopifnot(is.null(restrict) | is.character(restrict))
	
	
	subsetList <- NULL

	# traverse either analytes or distribution types 
	for (a in 1:length(subsetDef)){
		
		# get subtable for specified subset
		if(distType)
			subTable 	<- tableTCs[tableTCs$Distribution == subsetDef[a],]
		else 
			subTable 	<- tableTCs[tableTCs$Analyte == subsetDef[a],]
		
		# get sublist of overall errorlist for specified subset
		subList 	<- errorList[[as.character(subsetDef[[a]])]]
		
		tmpList		<- NULL 
		
		# traverse categories and get subset of the errorlist 
		for (cc in 1:length(catList)){
			
			tmpCat <- subset(subTable, subset = eval(parse(text = catList[cc])))
			
			if(!is.null(restrict))
				tmpCat <- subset(tmpCat, subset = eval(parse(text = restrict)))
			
			errorCat <- subset(subList, subset = subList$Index %in% tmpCat$Index)
			
			tmpList[[cc]] <- errorCat
		}
		names(tmpList) <- catLabels
		
		subsetList[[a]] <- tmpList	
	}
	
	names(subsetList) <- subsetDef
	
	return(subsetList)
	
}


#' Function to get error subsets for defined category and restriction.
#' 
#' @param overallCat	(list) containing the categories to split the dataset
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param errorList		(list) containing for each method the table with the computed error measurements 
#' @param distType		(logical) indicating if 'overallCat' refers to analytes (FALSE, default) or distribution types (TRUE) 
#' @param restrict		(character) indicating whether testcases should be filtered according to specified restriction, default NULL, 
#' 							e.g. fractionPathol <= 0.30 
#'  
#' @return	(list) containing the merged performance measurements grouped according to specified category 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
restrictSet <- function(overallCat, tableTCs, errorList, distType =TRUE, restrict = NULL){
	
	subsetList <- NULL
	
	# traverse categories
	for (a in 1:length(overallCat)){
	
		# get specified subset of test set definition
		if(distType)
			subTable 	<- tableTCs[tableTCs$Distribution == overallCat[a],]
		else 
			subTable 	<- tableTCs[tableTCs$Analyte == overallCat[a],]
		
		# get according error subset
		subList 	<- errorList[[as.character(overallCat[[a]])]]
		
		# restrict test set definitions and error subset as specified
		subTable <- subset(subTable, subset = eval(parse(text = restrict)))
		
		errorCat <- subset(subList, subset = subList$Index %in% subTable$Index)
		
		subsetList[[a]] <- errorCat	
	}
	
	names(subsetList) <- overallCat
	
	return(subsetList)
	
}


#' Function to combine analytes for defined categories 
#' 
#' The feature can either be the pathological fraction, the sample size or the overlap (category) individually or cumulative (_cum). 
#' For a individualized categorisation see \code{getSubsetForDefinedCats}.
#' 
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param errorList		(list) containing for each method the table with the computed error measurements 
#' @param catList		(list) containing the categories to split the dataset
#' @param catLabels		(list) containing the labels that will be used for the categories
#' @param distTypes		(logical) indicating if 'catList' refers to analytes (FALSE, default) or distribution types (TRUE) 
#' 
#' @return	(list) containing the merged performance measurements grouped according to specified category 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

mergeAnalytes <- function(tableTCs, errorList, catList = NULL, catLabels = NULL, distTypes =TRUE){
	
	# check input parameters	
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.list(errorList))
	stopifnot(!is.null(catList))
	stopifnot(length(catList) == length(catLabels))
	stopifnot(is.logical(distTypes))

	
	subsetList <- NULL
	
	if(distTypes & is.null(catList) & is.null(catLabels)){
		
		catListDist <- paste0("'", unique(tableTCs$Distribution), "'")
		catList <- paste0("Distribution == ",catListDist)
		
		catLabels <- unique(tableTCs$Distribution)
		
	}
	
	# traverse categories 
	for (cc in 1:length(catList)){
		
		# get subtable for specified category
		tmpCat	 <- subset(tableTCs, subset = eval(parse(text = catList[[cc]])))
		
		# get analytes part of the overall category
		catAnalytes <- unique(tmpCat$Analyte)
		
		errorCat <- NULL
		# traverse respective analytes and combine error data frame
		for (a in 1:length(catAnalytes)){
			if(distTypes)
				errorCat <- rbind(errorCat, errorList[[as.character(catAnalytes[a])]])
			else 
				errorCat <- rbind(errorCat, subset(errorList[[as.character(catAnalytes[a])]], subset = errorList[[as.character(catAnalytes[a])]]$Index %in% tmpCat$Index))
		}
		
		subsetList[[cc]] <- errorCat
		
	}
	names(subsetList) <- catLabels
	
	return(subsetList)
	
}


#'Helper function to combine all computed summary errors 
#' 
#' @param errorList		(list) of the error lists for the different methods for which the summary errors should be combined
#' @param nameVec		(character) vector specifying the names of the methods 
#' @param errorParam 	(character) specifying for which error parameter the data frame should be generated
#' @param cutoffZ 		(logical) indicating if a cutoff was set, needed for CRP case
#' 
#' @return (data frame) containing the summary errors per analyte per method
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

mergeSummaryErrors <- function(errorList, nameVec, errorParam = "MedianAbsPercErrorOV", cutoffZ = FALSE){
	
	errorAll <- merge(errorList[[1]]$summaryError[,c("Analyte", errorParam )], errorList[[2]]$summaryError[,c("Analyte", errorParam)],by ="Analyte")
	colnames(errorAll)[2:3] <- c(nameVec[1], nameVec[2])
	
	for(nn in 3:length(nameVec)){
		errorAll <- merge(errorAll,errorList[[nn]]$summaryError[,c("Analyte", errorParam)],by ="Analyte",all.x =TRUE)
		colnames(errorAll)[nn+1] <- nameVec[nn]
	}
	
	crp_errorParam = errorParam 
	
	if(length(grep("OV", x = errorParam, ignore.case = TRUE)) > 0 & !cutoffZ){
		crp_errorParam <- gsub(pattern = "OV", replacement = "URL", x = errorParam, ignore.case =TRUE)  
		
		if(length(grep("width", x = errorParam, ignore.case =TRUE)) >0){
			crp_errorParam <- gsub(pattern = "width", replacement = "", x = errorParam, ignore.case =TRUE)
			crp_errorParam <- gsub(pattern = "OV", replacement = "URL", x = crp_errorParam, ignore.case =TRUE)  
		}
	}
	crpVec <- NULL
	for(nn in 1:length(nameVec)){
		crpVec <-  c(crpVec, errorList[[nn]]$summaryError[errorList[[nn]]$summaryError$Analyte =="CRP",crp_errorParam])
	}
	if(!cutoffZ)
		errorAll[errorAll$Analyte =="CRP",-1] <- crpVec
	
	errorAll[,-1] <- lapply(errorAll[,-1],as.numeric)
		
	return(errorAll)
}



#' Computing benchmark table with the mean overall results.
#' 
#' @param errorList 		(list) containing the the computed errors for the different (indirect) methods/algorithms
#' @param nameVec			(character) vector specifying the names of the different (indirect) methods/algorithms
#' @param tableTCs			(data.frame) containing all information about the simulated test sets 
#' @param errorParam 		(character) specifying for which error parameter the data frame should be generated
#' @param cutoffZ			(integer) specifying if and if so which cutoff for the absolute z-score deviation should be used to 
#' 								classify results as implausible and exclude them from the overall benchmark score (default: 5)
#' @param catList			(character) vector containing the categories to split the dataset
#' @param catLabels			(character) vector containing the labels that will be used for the categories
#' @param perfCombination 	(character) specifying which measure should be used to compute the overall benchmark score; 
#' 								choose from "mean" (default), "median", or "sum"
#' 
#' @return (data frame) containing the computed benchmark results 
#'
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

getBenchmarkResults <- function(errorList, nameVec, tableTCs, errorParam ="zzDevAbsCutoff_Ov", cutoffZ = 5, 
		catList = c("fractionPathol <= 0.20 & N <= 5000", "fractionPathol <= 0.20 & N > 5000", 
					"fractionPathol >  0.20 & N <= 5000", "fractionPathol >  0.20 & N > 5000"), 
		catLabels = c("lowPlowN", "lowPhighN", "highPlowN", "highPhighN"), perfCombination = c("mean", "median", "sum")){
	
	# check input parameters
	stopifnot(is.list(errorList))
	stopifnot(is.character(nameVec))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.character(errorParam))
	stopifnot(is.numeric(cutoffZ))
	stopifnot(!is.null(catList))
	stopifnot(length(catList) == length(catLabels))
	
	perfCombination <- match.arg(perfCombination[1], choices = c("mean", "median", "sum"))
	
	# initialize result data frame 
	benchmarkRes <- data.frame(matrix(nrow = 27, ncol = 2+length(nameVec)))
	colnames(benchmarkRes) <- c("DistributionType", "Subcategory", nameVec)
	
	benchmarkRes[,1] <- c(rep("normal", 6), rep("skewed",6), rep("heavilySkewed", 6), 
			rep("shifted",6), "Overall", "FailureRate", "ImplausibleResults")

	# traverse algorithms and compute results 
	for (nn in 1:length(nameVec)){
		errorDf <- errorList[[nn]]$resRIs
		errorDf <- do.call(rbind, errorDf)
		
		## normal distribution 
		normalRes <- computeSubResults(errorDf = errorDf, tableTCs = tableTCs, distCat ="normal", errorParam = errorParam,
				catList = catList, catLabels = catLabels, perfCombination = perfCombination)
				
		## skewed 
		skewedRes <- computeSubResults(errorDf = errorDf, tableTCs = tableTCs, distCat ="skewed", errorParam = errorParam,
				catList = catList, catLabels = catLabels, perfCombination = perfCombination)
		
		## heavily skewed 
		heavilySkewedRes <- computeSubResults(errorDf = errorDf, tableTCs = tableTCs, distCat ="heavilySkewed", errorParam = errorParam,
				catList = catList, catLabels = catLabels, perfCombination = perfCombination)
		
		# shifted 
		shiftedRes <- computeSubResults(errorDf = errorDf, tableTCs = tableTCs, distCat ="shifted", errorParam = errorParam,
				catList = catList, catLabels = catLabels, perfCombination = perfCombination)
		
		# overall
		ovRes <- c("",do.call(perfCombination, args = list(errorDf[,errorParam], na.rm =TRUE)))
		
		# failure rate 
		failRate <- c("",100*nrow(errorDf[is.na(errorDf$LRL) | is.na(errorDf$URL),])/nrow(errorDf))
		# implausible results
		if(!is.null(cutoffZ))
			implResults <- c("",100*nrow(errorDf[errorDf$zzDevAbs_Ov > cutoffZ & !is.na(errorDf$zzDevAbs_Ov),])/nrow(errorDf))
		else 
			implResults <- c("",NA)
		
		if(nn == 1)
			benchmarkRes[,c(2,nn+2)] <- rbind(normalRes, skewedRes, heavilySkewedRes, shiftedRes, ovRes, failRate, implResults)
		else 
			benchmarkRes[,nn+2] <- rbind(cbind(normalRes[,2]), cbind(skewedRes[,2]), cbind(heavilySkewedRes[,2]), cbind(shiftedRes[,2]), ovRes[2], failRate[2], implResults[2])
	}

	return(benchmarkRes)
}


#' Helper function to compute the subscores for the distribution types and the mentioned categories
#' 
#' @param errorDf 		(data frame) containing the estimate reference intervals and all computed error measures
#' @param tableTCs		(data.frame) containing all information about the simulated test sets 
#' @param distCat		(character) specifying the distribution category
#' @param errorParam 	(character) specifiying for which error parameter the data frame should be generated
#' @param catList		(character) vector containing the categories to split the dataset
#' @param catLabels		(character) vector containing the labels that will be used for the categories
#' @param perfCombination (character) specifying if mean (default), median or sum should be computed
#' 
#' @return (data frame) containing the computed subscores
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computeSubResults <- function(errorDf, tableTCs, distCat, errorParam, catList, catLabels, perfCombination = "mean"){
	
	# get subset for defined category
	subTable  <- tableTCs[tableTCs$Distribution == distCat,]
	errorDist <- errorDf[errorDf$Index %in% subTable$Index,]
	
	subResults <- data.frame(matrix(nrow = length(catList)+2, ncol = 2))
	
	# compute subscores for defined category
	subResults[1,] <- c("PercCases", 100*length(errorDist[,errorParam][!is.na(errorDist[,errorParam])])/nrow(errorDf))
	subResults[2,] <- c("Overall", do.call(perfCombination, args = list(errorDist[,errorParam], na.rm =TRUE)))
			
	for (cc in 1:length(catList)){
		subsub <- subset(subTable, subset = eval(parse(text = catList[cc])))
		tmp    <- errorDist[errorDist$Index %in% subsub$Index,]
		subResults[cc+2,] <- c(catLabels[cc], do.call(perfCombination,args = list(tmp[,errorParam], na.rm =TRUE)))
	}
	
	return(subResults)
}


#'Function to read the result files and compute performance measures to create customized plots afterwards
#' 
#' @param workingDir		(character) specifying the working directory: Plots will be stored in workingDir/evalFolder and results will be used from workingDir/Results/algoName/biomarker; 
#' @param algoName			(character) vector specifying one algorithm for which the performance measures should be evaluated
#' @param subset			(character, numeric, or data.frame) to specify for which subset the algorithm should be executed. 
#' 							character options:	'all' (default) for all test sets,
#' 												a distribution type: 'normal', 'skewed', 'heavilySkewed', 'shifted';
#' 												a biomarker: 'Hb', 'Ca', 'FT4', 'AST', 'LACT', 'GGT', 'TSH', 'IgE', 'CRP', 'LDH'; 
#' 												'runtime' for runtime analysis subset; 							
#' 							numeric option: number of test sets per biomarker, e.g. 10;
#' 							data.frame: customized subset of table with test set specifications   
#' @param cutoffZ			(integer) specifying if and if so which cutoff for the absolute z-score deviation should be used to 
#' 								classify results as implausible and exclude them from the overall benchmark score (default: 5)
#' @param ...				additional arguments to be passed to the method
#' 								truncNormal 	(logical) specifying if a normal distribution truncated at zero shall be assumed 

#' @return (list) with (data frame) and a (list) with the computed performance measures  
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
readResultsAndComputeErrors <- function(workingDir = getwd(), algoName = NULL, subset="all", cutoffZ = 5, ...){
	
	args = list(...)
	# check input parameters 
	stopifnot(is.character(workingDir))
	stopifnot(is.character(algoName))
	stopifnot(is.character(subset) | is.numeric(subset) | is.data.frame(subset))
	stopifnot(is.numeric(cutoffZ))
	
	stopifnot(names(args) %in% c("truncNormal"))	# stop if args contains something else than truncatedNormal 
	
	truncNormal <- FALSE
	if(!is.null(args$truncNormal)){
		stopifnot(is.logical(args$truncNormal))
		truncNormal <- args$truncNormal
	}
		
	# check specified subset and load testcase definition for it
	if(is.character(subset)){
		tableTCs <- loadTestsetDefinition()
		
		if(subset == "all"){
			# all
			analytes <- unique(as.character(tableTCs$Analyte))
			
		}else if(subset =="normal" | subset =="skewed" | subset == "heavilySkewed" | subset == "shifted"){
			# distribution type
			tableTCs <- tableTCs[tableTCs$Distribution == subset,]			
			analytes <- unique(as.character(tableTCs$Analyte))
			
		}else if(subset %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH")){
			tableTCs <- tableTCs[tableTCs$Analyte == subset,]
			analytes <- subset
			
		} else if(subset =="Runtime"){
			tableTCs <- tableTCs[tableTCs$RuntimeSet == 1,]
			analytes <- unique(as.character(tableTCs$Analyte))
			
		} else {
			stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
							a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), a number to randomly assign n test sets per biomarker, 
							or the filtered table with the test sets that wished to be generated and later evaluated.")
		}
	} else if(is.numeric(subset)){
		# define subset with N 
		tableTCs <- loadTestsetDefinition()
		tableTCs <- defineSubset(tableTCs,N = subset)
		tableTCs <- tableTCs[tableTCs$Subset == 1,]
		analytes <- unique(as.character(tableTCs$Analyte))
		
	} else if(is.data.frame(subset)){
		## use specified table
		analytes <- unique(as.character(subset$Analyte))
		tableTCs <- subset
	}else {
		stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
						a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), a number to randomly assign n test sets per biomarker, 
						'Runtime' for the specified subset defined for runtime analysis, 
						or the filtered table with the test sets that wished to be generated and later evaluated.")
	}	
	
	message("Read result files and compute performance measures ...")
	# read result files and compute RIs / use pre-calculated RIs and calculate performance measures 
	resIn <- readResultFilesAll(analytes = analytes, algo = algoName, baseDir = workingDir, tableTCs = tableTCs)

	if(inherits(x = resIn[[1]][[1]], what = "RWDRI")){		
			allRI <- computeRIsAll(analytes = analytes, algo = algoName, resIn = resIn, tableTCs = tableTCs, truncNormal = truncNormal)
	}else {
		allRI <- getRIsAllwithoutModel(analytes = analytes, algo = algoName, resIn = resIn, tableTCs = tableTCs)
	}

	# compute performance measures
	errorAlgo 			<- computePerfMeasAll(analytes = analytes, algo =algoName, risIn = allRI, tableTCs =tableTCs, cutoffZ = cutoffZ)

	# convert list to data frame 
	errorAlgoDf <- do.call(rbind.data.frame, errorAlgo$resRIs)
	
	# merge data frame with computed performance measures with test set info
	perfMeasAlgo <- merge(errorAlgoDf, tableTCs, by = "Index")
	
	return(list(perfDf = perfMeasAlgo, perfList = errorAlgo))
}
