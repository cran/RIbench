#' Function for running test sets per algorithm per marker with calling Rscript for each test set
#' 
#' @param biomarker			(character) specifying the biomarker for which the algorithm should calculate RIs
#' @param algoName			(character) specifying the algorithm that should be called
#' @param algoFunction		(character) specifying the name of the function needed for estimating RIs
#' @param libs				(list) containing all libraries needed for executing the algorithm
#' @param sourceFiles 		(list) containing all source files needed for executing the algorithm
#' @param params			(list) with additional parameters needed for calling algoFunction 
#' @param decimals			(logical) indicating whether the algorithm needs the number of decimal places (TRUE) or not (FALSE, default)
#' @param ris 				(logical) indicating whether only percentiles and no model is estimated 
#' @param RIperc			(numeric) value specifying the percentiles, which define the reference interval
#' @param tableTCs			(data.frame) with the information about the simulated test sets
#' @param outputDir			(character) specifying the outputDir: Results will be stored in outputDir/Results/algo/biomarker
#' @param inputDir			(character) specifying the inputDir: Data files should be stored in inputDir/Data/biomarker
#' @param timeLimit 		(integer) specifying the maximum amount of time in seconds allowed to execute one single estimation (default: 14400 sec (4h))
#' @param subsetDef			(character) describing the specified subset of all test sets the algorithm is applied to, used for naming the progress file 
#' @param verbose			(logical) indictaing if the progress counter should be shown (default: TRUE)
#' @param showWarnings		(logical) indicating whether warnings from the call to the indirect method/algorithm should be shown (default: FALSE)
#' @param ...				additional arguments to be passed to the method
#' 
#' @return (data frame) containing information about the test sets where the algorithm terminated the R session or failed to report a result
#' 

#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

runTC_usingRscript <- function(biomarker = NULL, algoName = "myOwnAlgo", algoFunction = "estimateModel", sourceFiles = NULL, libs =NULL, params = NULL, decimals = FALSE, ris = FALSE,
		RIperc = c(0.025, 0.975),tableTCs = NULL, outputDir = NULL, inputDir = NULL, timeLimit = 14400, subsetDef = 'all', verbose = TRUE, showWarnings =FALSE, ...){
	
	args = list(...)
	
	if(is.null(outputDir))
		outputDir = getwd()
	
	if(is.null(inputDir))
		inputDir = getwd()

	# convert list into string for libraries
	if(length(libs) > 1)
		libs <- paste0(libs, collapse=",")
	
		
	# convert list into string for params
	if(is.list(params))
		params <- unlist(params)
	
	if(!is.null(names(params)))
		params = paste0(names(params), "=", params)
		
	if(length(params) > 1)
		params <- paste0(params, collapse=",")
	
	
	transform <- args$transform 
	if(is.null(transform))
		transform = FALSE
		
	# get path to runner script 
	scriptPath <- system.file("extdata","runIndMethod.R", package = "RIbench")

	# get sub table for specified marker and sort according to index
	tableSub		<- tableTCs[tableTCs$Analyte == biomarker,]
	tableSub		<- tableSub[order(tableSub$Index),]
	
	# get number of decimal places if needed for the algorithm 
	if(decimals){
		dec <- tableSub$dec[1]
		if(is.null(params))
			params <- paste0("decimals=",dec)
		else
			params <- paste0("decimals=",dec,",", params)
	}
	
	progressOut 	<- NULL	
	
	# write or update progress file 
	fileEnding = "_progressFile.csv"
	
	#if progress file already exists, set startInd according to progress index
	if (file.exists(file.path(outputDir, paste0(subsetDef, "_", algoName, fileEnding))) ){
		progress <- fread(file = file.path(outputDir, paste0(subsetDef, "_", algoName, fileEnding)))
		pgInd    <- progress$Index
		pgIter	 <- progress$Iteration
		startInd <- which(tableSub$Index >=pgInd)[1]
		
	} else {
		# if progress file doesn't exist, generate file and set startInd to 1
		fwrite(list("Index" = tableSub$Index[1],"Iteration"= 0), file = file.path(outputDir, paste0(subsetDef,"_", algoName, fileEnding)))
		
		startInd <- 1
	}
	
	# traverse sub table and call algo script for each test case
	counter 	<- 0
	seed 		<- tableSub$startSeed[1]
	
	subCall =""
	
	# if reference intervals/percentiles are directly estimated, add this to the function call
	if(ris){
		subCall <-paste0("-r ", ris, " --percentiles=", "'", paste0(RIperc, collapse =","),"'")
	}
	
	# initialize progress indicator
	msgP 		<- gettext("Progress:")
	nCharMsg 	<- 0
	
	maxValue = length(tableSub$Index)
	startValue = 0
	pValPrev = 0
	if(!is.null(args$maxValue) & !is.null(args$startValue)){
		maxValue = args$maxValue
		startValue = args$startValue
		pValPrev = startValue/maxValue

	}
				
	# for subsets, if the progress file already indicates a later stage, return the current progress and continue with the next biomarker
	if(is.na(startInd))
		return(as.data.frame(progressOut))
	
	# traverse table indices and run algorithm for each specified test set
	for(iii in (startInd:length(tableSub$Index))){
		
		index 		<- tableSub$Index[iii]
		
		# get hashes to forward to Rscript call
		uniqueHashRounded 		<- tableSub$HashCode[iii]
		uniqueHashNotRounded 	<- tableSub$HashCodeNotRounded[iii]
		
		# progress indicator
		pVal 		<- formatNumber(100*((iii+ startValue)/maxValue),1)
		backspaces 	<-  paste(rep("\b", nCharMsg), collapse ="")
			
		if(as.numeric(pVal) - as.numeric(pValPrev) >= 0.1){
			if(pValPrev == 0 || pValPrev == startValue/maxValue){
				message 	<-  paste(msgP, " ", pVal, "% ", sep ="", collapse ="")
				nCharMsg 	<-  nchar(message)- nchar(paste(msgP))
				
			}else{ 
				message 	<-  paste(" ", pVal, "% ", sep ="", collapse ="")
				nCharMsg 	<-  nchar(message)
			}
			
			if(verbose) cat(backspaces, message, sep ="")
			pValPrev 	<- pVal
		}
		
		counter <- counter + 1	
		filename <- paste0(index, "_", biomarker, "_seed_", seed, ".csv")
		
		# check if input file exists 
		if(!file.exists(file.path(inputDir, "Data", biomarker, filename)))
			stop(simpleError("Simulated test set data not found. Please check 'workingDir' and 'subset' parameter."))
				
		# call algorithm for different parameter combinations
		if(is.null(params) & transform){
			
			callAlgo <- paste("Rscript", scriptPath, "-x", index, "-f", paste0("\"",filename,"\""), 
					"-u", paste0("\"", uniqueHashRounded, "\""),"-e", paste0("\"", uniqueHashNotRounded, "\""), 
					"-i", paste0("\"",inputDir, " \""), "-o", paste0("\"", outputDir," \""), "-b", subsetDef,
					"-a", algoFunction, "-n", algoName, paste0("--libraries=",libs), paste0("--sourceFiles=", "\"",sourceFiles,"\""), 
					"-r", ris, "-t", tableSub[tableSub$Index == index,]$nonp_lambda, subCall, "-w", showWarnings )
			
		}else if(is.null(params)){
			
			callAlgo <- paste("Rscript", scriptPath, "-x", index, "-f", paste0("\"",filename,"\""), 
					"-u", paste0("\"", uniqueHashRounded, "\""),"-e", paste0("\"", uniqueHashNotRounded, "\""), 
					"-i", paste0("\"",inputDir, " \""), "-o", paste0("\"", outputDir," \""), "-b", subsetDef,
					"-a", algoFunction, "-n", algoName, paste0("--libraries=",libs), paste0("--sourceFiles=", "\"",sourceFiles,"\""), 
					"-r", ris, subCall, "-w", showWarnings)
			
		}else if(transform){
			
			callAlgo <- paste("Rscript", scriptPath, "-x", index, "-f", paste0("\"",filename,"\""),
					"-u", paste0("\"", uniqueHashRounded, "\""),"-e", paste0("\"", uniqueHashNotRounded, "\""), 
					"-i", paste0("\"",inputDir, " \""),"-o", paste0("\"",outputDir," \""), "-b", subsetDef,
					"-a", algoFunction, "-n", algoName, paste0("--libraries=",libs), paste0("--params=", "",params, ""), paste0("--sourceFiles=", "\"",sourceFiles,"\""), 
					subCall, "-t",tableSub[tableSub$Index == index,]$nonp_lambda , "-w", showWarnings)
		}else{
			
			callAlgo <- paste("Rscript", scriptPath, "-x", index, "-f", paste0("\"",filename,"\""),
					"-u", paste0("\"", uniqueHashRounded, "\""),"-e", paste0("\"", uniqueHashNotRounded, "\""), 
					"-i", paste0("\"",inputDir, " \""),"-o", paste0("\"",outputDir," \""), "-b", subsetDef,
					"-a", algoFunction, "-n", algoName, paste0("--libraries=",libs), paste0("--params=", "",params, ""), 
					paste0("--sourceFiles=", "\"",sourceFiles,"\""), subCall, "-w", showWarnings)
			
		}
		

		# start calculation, terminate after certain time 
		exit <- tryCatch({
					system(command = callAlgo, timeout = timeLimit)
				},
				warning = function(w){
					message(paste(algoName, "timed out. Here is the original warning message:"))
					message(w)
					writeResFile(algoName = algoName, biomarker = biomarker, N = tableSub[tableSub$Index == index,]$N, 
								error ="timeout", runtime = timeLimit, filename = filename, outputDir = outputDir)
					return (-1)
				})
		
		# if system command failed, i.e. R session terminated, update result file and update progress variable
		if(exit != 0){
			progressOut <- rbind(progressOut, cbind(index, 1))
			if(exit != -1){
				writeResFile(algoName = algoName, biomarker = biomarker, N = tableSub[tableSub$Index == index,]$N, 
							error ="RSessionTerminated", runtime = timeLimit, filename = filename, outputDir = outputDir)
			}
		}
		
	}
	
	progressOut <- as.data.frame(progressOut)

	return(progressOut)
	
}


#' Wrapper function to evaluate all test sets or a specified subset for a specified algorithm. 
#' 
#' @param workingDir			(character) specifying the working directroy: Results will be stored in workingDir/Results/algo/biomarker and data will be used from workingDir/Data/biomarker 
#' @param algoName				(character) specifying the algorithm that should be called
#' @param algoFunction			(character) specifying the name of the function needed for estimating RIs
#' @param libs					(list) containing all libraries needed for executing the algorithm
#' @param sourceFiles 			(list) containing all source files needed for executing the algorithm
#' @param params				(list) with additional parameters needed for calling algoFunction 
#' @param requireDecimals		(logical) indicating whether the algorithm needs the number of decimal places (TRUE) or not (FALSE, default)
#' @param requirePercentiles 	(logical) indicating whether only percentiles and no model is estimated 
#' @param subset				(character, numeric, or data.frame) to specify for which subset the algorithm should be executed. 
#' 								character options:	'all' (default) for all test sets;
#' 												a distribution type: 'normal', 'skewed', 'heavilySkewed', 'shifted';
#' 												a biomarker: 'Hb', 'Ca', 'FT4', 'AST', 'LACT', 'GGT', 'TSH', 'IgE', 'CRP', 'LDH'; 
#' 												'Runtime' for runtime analysis subset; 							
#' 								numeric option: number of test sets per biomarker, e.g. 10;
#' 								data.frame: customized subset of table with test set specifications 
#' @param timeLimit 			(integer) specifying the maximum amount of time in seconds allowed to execute one single estimation (default: 14400 sec (4h))
#' @param verbose		(logical) indictaing if the progress counter should be shown (default: TRUE)
#' @param showWarnings		(logical) indicating whether warnings from the call to the indirect method/algorithm should be shown (default: FALSE)
#' @param ...					additional arguments to be passed to the method, e.g. specified in- and output directory ('inputDir', 'outputDir')
#' 
#' @return (data frame) containing information about the test sets where the algorithm terminated the R session or failed to report a result
#' 
#' 
#' @examples
#' 
#'
#' \dontrun{
#' # The evaluation of all test sets can take several hours depending on 
#' #   the computation time of the algorithm.
#' # Wrapper function for indirect method required, see vignette("RIbench_package")
#' # Ensure that 'generateBiomarkerTestSets()' is called with the same workingDir 
#' #    before calling this function. 
#' 
#' # first generic example
#' evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModel', libs = c('myOwnAlgo'), 
#' 		sourceFiles = list("C:\\Temp\\MyAlgoWrapper.R"), 
#'		requireDecimals = FALSE, requirePercentiles = FALSE,
#'		subset ='all', timeLimit = 14400)
#' 
#' 
#' # second example, evaluation for only 'Calcium' test sets.
#' progress <- evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 			algoFunction = 'estimateModel', libs = c('myOwnAlgo'), subset = "Ca")
#' 
#' 
#' # third example, evaluation for only a subset testsets that follow a skewed distribution.
#' progress <- evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModel', libs = c('myOwnAlgo'), subset = "skewed") 
#' 
#' 
#' # forth example, evaluation for a subset of 3 testsets per biomarker. 
#' progress <- evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModel', libs = c('myOwnAlgo'), subset = 3)
#' 
#' 
#' # fifth example, evaluation for a customized subset with all test sets that have 
#' # 	a pathological fraction <= 30%. 
#' testsets <- loadTestsetDefinition()
#' progress <- evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModel', libs = c('myOwnAlgo'), 
#' 		subset = testsets[testsets$fractionPathol <= 0.3,] )
#' 
#' 
#' # sixth example, evaluation forwarding additional parameters to the 'algoFunction'
#' progress <- evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModel', libs = c('myOwnAlgo'), 
#' 		sourceFiles = list("Test_RIEst_2pBoxCox"), params = list("model='2pBoxCox'"))
#' 
#'
#' # seventh example, evaluation for indirect method that requires the number of 
#' #	decimal points as input 
#' evaluateBiomarkerTestSets(workingDir = tempdir(), algoName = 'myOwnAlgo', 
#' 		algoFunction = 'estimateModelDec', libs = c('myOwnAlgo'), 
#' 		sourceFiles = "C:\\Temp\\Test_RIEst_dec.R", requireDecimals = TRUE)
#' 
#' 
#' # eigth example, evaluation for indirect method that directly estimates the percentiles
#' evaluateBiomarkerTestSets(workingDir = tempdir(), algoName="myOwnAlgo", 
#' 		algoFunction="estimateRIs", libs="myOwnAlgo", 
#' 		sourceFiles = "C:\\Temp\\Test_RIEst.R", requirePercentiles=TRUE) 
#' }
#' 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

evaluateBiomarkerTestSets <- function(workingDir = "", algoName = "refineR", algoFunction = "findRI", libs = "refineR", sourceFiles = NULL, params = NULL, requireDecimals = FALSE, requirePercentiles = FALSE, 
		subset = "all", timeLimit = 14400, verbose = TRUE, showWarnings = FALSE, ... ){
		
	
	args <- list(...)
	
	# check input parameters 	
	stopifnot(is.character(workingDir))	
	stopifnot(is.character(algoName))	
	stopifnot(is.character(algoFunction))	
	stopifnot(is.character(libs))
	stopifnot(is.null(sourceFiles) | is.character(sourceFiles))
	stopifnot(is.null(params) | is.list(params))
	stopifnot(is.logical(requireDecimals))
	stopifnot(is.logical(requirePercentiles))	
	stopifnot(is.character(subset) | is.numeric(subset) | is.data.frame(subset))
	stopifnot(is.numeric(timeLimit))
	stopifnot(is.logical(verbose))	
	stopifnot(names(args) %in% c("outputDir","inputDir"))	# stop if args contains something else than outputDir or inputDir 
	
	# if input and output directory are not specified separately, use working directory 
	outputDir <- args$outputDir
	if(is.null(outputDir))
		outputDir = workingDir
	
	inputDir <- args$inputDir
	if(is.null(inputDir))
		inputDir = workingDir

	
	if(is.character(subset)){
		tableTCs <- loadTestsetDefinition()
		subsetDef <- subset
		
		if(subset == "all" | subset == "normal"| subset == "skewed" | subset == "heavilySkewed" | subset == "shifted" | subset == "Runtime"){
			
			if(subset == "normal" | subset == "skewed" | subset == "heavilySkewed" | subset == "shifted")
				tableTCs <- tableTCs[tableTCs$Distribution == subset,]	
			else if(subset == "Runtime")
				tableTCs <- tableTCs[tableTCs$RuntimeSet == 1,]
				
			
			# else all
			# setup Results directory structure
			for(m in unique(tableTCs$Analyte)){
				if(!dir.exists(file.path(workingDir,"Results",algoName, m))){
					dir.create(path =file.path(workingDir, "Results", algoName,m),recursive =TRUE)
				}
			}
			progress_list <- NULL
			analytes <- unique(as.character(tableTCs$Analyte))
			
			maxValue <- nrow(tableTCs)
			startValue <- 0
			
			for (i in 1:length(analytes)){
				
				if(analytes[i] == "CRP")
					RIperc = 0.95
				else 
					RIperc = c(0.025, 0.975)
				
				# run model/reference interval estimation for the specified algorithm and analyte
				progress_list[[i]] <- runTC_usingRscript(biomarker = analytes[i], algoName = algoName,algoFunction = algoFunction, 
						libs = libs, sourceFiles = sourceFiles,params = params, decimals = requireDecimals, ris = requirePercentiles,
						RIperc = RIperc, tableTCs = tableTCs, outputDir = outputDir, inputDir = inputDir, 
						timeLimit = timeLimit, subsetDef = subsetDef, verbose = verbose, showWarnings = showWarnings, ...)
				
				# update progress counter
				backspaces 	<-  paste(rep("\b", 20), collapse ="")
				if(verbose) cat(backspaces, sep ="")
			}
			
		}else if(subset %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH")){
			biomarker <- subset
			subsetDef <- subset
			tableTCs <- tableTCs[tableTCs$Analyte == biomarker,]
			
			# setup Results directory structure
			for(m in unique(tableTCs$Analyte)){
				if(!dir.exists(file.path(workingDir,"Results",algoName, m))){
					dir.create(path =file.path(workingDir, "Results", algoName,m),recursive =TRUE)
				}
			}
			
			progress_list <- NULL
			
			# update progress counter
			maxValue <- nrow(tableTCs)
			startValue <- 0
			
			if(biomarker =="CRP")
				RIperc = 0.95
			
			# run model/reference interval estimation for the specified algorithm and analyte
			progress_list <- runTC_usingRscript(biomarker = biomarker, algoName = algoName,algoFunction = algoFunction, 
					libs = libs, sourceFiles = sourceFiles, params = params, decimals = requireDecimals, ris = requirePercentiles, 
					RIperc = RIperc, tableTCs = tableTCs, outputDir = outputDir, inputDir = inputDir, 
					timeLimit = timeLimit,subsetDef = subsetDef, verbose = verbose, showWarnings = showWarnings, ...)
			
		} else {
			stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
							a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), 'Runtime' for the subset used to evaluate the runtime, 
							a number to randomly assign N test sets per biomarker, 
							or the filtered table with the test sets that wished to be evaluated.")
		}
		
	} else if(is.numeric(subset)){
		# check input parameter
		stopifnot(subset >= 1 & subset <= 576)
		
		subsetDef = paste0("N_", subset)
		# define subset with N 
		tableTCs <- loadTestsetDefinition()
		tableTCs <- defineSubset(tableTCs,N = subset)
		tableTCs <- tableTCs[tableTCs$Subset == 1,]
		
		# setup Results directory structure
		for(m in unique(tableTCs$Analyte)){
			if(!dir.exists(file.path(workingDir,"Results",algoName, m))){
				dir.create(path =file.path(workingDir, "Results", algoName,m),recursive =TRUE)
			}
		}
		
		progress_list <- NULL
		analytes <- unique(as.character(tableTCs$Analyte))
		
		maxValue <- nrow(tableTCs)
		startValue <- 0
		for (i in 1:length(analytes)){
			
			if(analytes[i] == "CRP")
				RIperc = 0.95
			else 
				RIperc = c(0.025, 0.975)
			
			# run model/reference interval estimation for the specified algorithm and analyte
			progress_list[[i]] <- runTC_usingRscript(biomarker = analytes[i], algoName = algoName,algoFunction = algoFunction, 
					libs = libs, sourceFiles = sourceFiles, params = params, decimals = requireDecimals, ris = requirePercentiles,
					RIperc = RIperc, tableTCs = tableTCs, outputDir = outputDir, inputDir = inputDir,					
					timeLimit = timeLimit, maxValue = maxValue, startValue = startValue, subsetDef = subsetDef, verbose = verbose,
					showWarnings = showWarnings, ...)
			
			# update progress counter
			startValue <- startValue + nrow(tableTCs[tableTCs$Analyte == analytes[i],])
			
			backspaces 	<-  paste(rep("\b", 20), collapse ="")
			if(verbose) cat(backspaces, sep ="")
		}
	} else if(is.data.frame(subset)){
		
		subsetDef = "Customized"
		## use specified table
		# setup Results directory structure
		for(m in unique(subset$Analyte)){
			if(!dir.exists(file.path(workingDir,"Results",algoName, m))){
				dir.create(path =file.path(workingDir, "Results", algoName,m),recursive =TRUE)
			}
		}
		progress_list <- NULL
		analytes <- unique(as.character(subset$Analyte))
		maxValue <- nrow(subset)
		startValue <- 0
		for (i in 1:length(analytes)){
			
			if(analytes[i] == "CRP")
				RIperc = 0.95
			else 
				RIperc = c(0.025, 0.975)
			
			# run model/reference interval estimation for the specified algorithm and analyte
			progress_list[[i]] <- runTC_usingRscript(biomarker = analytes[i], algoName = algoName,algoFunction = algoFunction, 
					libs = libs, sourceFiles = sourceFiles, params = params, decimals = requireDecimals, ris = requirePercentiles,RIperc = RIperc,
					tableTCs = subset, outputDir = outputDir, inputDir = inputDir,
					timeLimit = timeLimit, maxValue = maxValue, startValue = startValue,subsetDef = subsetDef, verbose = verbose,
					showWarnings = showWarnings, ...)
			
			# update progress counter
			startValue <- startValue + nrow(subset[subset$Analyte == analytes[i],])
			
			backspaces 	<-  paste(rep("\b", 20), collapse ="")
			if(verbose) cat(backspaces, sep ="")
			
		}
	}else {
		stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
						a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), a number to randomly assign n test sets per biomarker, 
						'Runtime' for the specified subset defined for runtime analysis, 
						or the filtered table with the test sets that wished to be generated and later evaluated.")
	}	
	
	if(verbose) cat("\n")
	
	fileEnding = "_progressFile.csv"
	
	# remove progress file if one reaches this point of the computation
	if (file.exists(file.path(outputDir, paste0(subsetDef, "_", algoName, fileEnding))) )
		file.remove(file.path(outputDir, paste0(subsetDef, "_", algoName, fileEnding)))

		
	return(progress_list)
	
}


#' Helper function to write result file when time out occured or R session terminated
#' 
#' @param algoName			(character) specifying the algorithm that should be called
#' @param biomarker			(character) specifying the biomarker for which the algorithm should calculate RIs
#' @param N 				(numeric) specifying the number of input data points
#' @param error				(character) specifying the type of error (e.g. timeout, RSessionTerminated)
#' @param runtime			(numeric) specifying the computation time up until the error occured
#' @param filename			(character) specifying the filename for which the algorithm failed 
#' @param outputDir			(character) specifying the outputDir: Data files should be stored in outputDir/Data/biomarker and Results will be stored in outputDir/Results/algo/biomarker
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
writeResFile <- function(algoName, biomarker, N = 0, error = NULL, runtime = NULL, filename = NULL, outputDir = NULL){
	
	obj 				<- list()
	obj$roundingBase 	<- NA
	obj$Lambda			<- NA
	obj$Mu				<- NA
	obj$Sigma			<- NA
	obj$P				<- NA
	obj$Cost			<- NA
	obj$Method			<- algoName
	obj$Shift			<- NA
	obj$Time			<- NA
	obj$Runtime			<- runtime
	obj$Data			<- NA
	obj$Analyte			<- biomarker
	obj$N				<- N 
	
	obj$Status			<- error
	
	class(obj)			<- "RWDRI"
	
	outFile <- gsub(".csv", replacement = paste0("_", algoName,".Rdata"), filename)
	save(obj, file = file.path(outputDir, "/Results/", algoName, biomarker, outFile))
	
}
