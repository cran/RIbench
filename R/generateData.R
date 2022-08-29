#' Convenience function to generate simulated data with one start seed for each biomarker and save each test set as a separate file
#' 
#' @param workingDir	(character) specifying the working directory from which a ./Data directory will be generated
#' @param subset		(character, numeric, or data.frame) to specify for which subset the data should be generated and the algorithms later applied to.
#' 								character options:	'all' (default) for all test sets;
#' 												a distribution type: 'normal', 'skewed', 'heavilySkewed', 'shifted'; 
#' 												a biomarker: 'Hb', 'Ca', 'FT4', 'AST', 'LACT', 'GGT', 'TSH', 'IgE', 'CRP', 'LDH'; 
#' 												'Runtime' for runtime analysis subset; 					
#' 								numeric option: number of test sets per biomarker, e.g. 10;
#' 								data.frame: customized subset of table with test set specifications 
#' @param rounding		(logical) indicating whether decimal places stated in test set specifications should be applied (default, TRUE), 
#' 							if FALSE, data will be rounded to 5 decimal places to mimic not rounded data
#' @param verbose		(logical) indictaing if the progress counter should be shown (default: TRUE)
#' 
#' @return  No return value, instead the data files are generated amd saved in the workingDir
#'
#' 
#' @examples
#' 
#' \dontrun{
#' workingDir <- "C:\\Temp\\RIbench\\"
#' generateBiomarkerTestSets(workingDir = workingDir)
#' }
#' 
#' \donttest{
#' # example generating a subset of 2 test sets per biomarker 
#' generateBiomarkerTestSets(workingDir = tempdir(), subset =  2)
#' }
#' 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

generateBiomarkerTestSets <- function(workingDir = "", subset = "all", rounding = TRUE, verbose =TRUE){
	
	# check input parameters
	stopifnot(is.character(workingDir))
	stopifnot(is.character(subset) | is.numeric(subset) | is.data.frame(subset))
	stopifnot(is.logical(rounding))
	stopifnot(is.logical(verbose))
	
	
	# get testset definitions for the specified subset
	if (is.character(subset)) {
		
		tableTCs <- loadTestsetDefinition()

		if (subset == "normal" | subset == "skewed" | subset == "heavilySkewed" | subset == "shifted" | subset == "Runtime") {
			
			if(subset == "normal" | subset == "skewed" | subset == "heavilySkewed" | subset == "shifted")  # distribution type
				tableTCs <- tableTCs[tableTCs$Distribution == subset,]
			else	# runtime set
				tableTCs <- tableTCs[tableTCs$RuntimeSet == 1,]
		
			
		}else if (subset %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH")) {
			# biomarker
			tableTCs <- tableTCs[tableTCs$Analyte == subset,]

		}else if (subset != "all") {
			stop("Parameter subset should either be 'all', or specify a distribution type (normal, skewed, heavilySkewed, shifted), 
					a biomarker (Hb, Ca, FT4, AST, LACT, GGT, TSH, IgE, CRP, LDH), 'Runtime' for the subset used to evaluate the runtime, 
					a number to randomly assign N test sets per biomarker, 
					or the filtered table with the test sets that wished to be generated and later evaluated.")
		}
		
		# set up dir structure data
		for (m in unique(tableTCs$Analyte)) {
			if (!dir.exists(file.path(workingDir,"Data", m))) {
				dir.create(path = file.path(workingDir, "Data", m),recursive =TRUE)
			}
		}
		
		# generate files
		generateDataFiles(tableTCs = tableTCs, outputDir = workingDir, rounding = rounding)
		
		
	} else if (is.numeric(subset)) {
		stopifnot(subset >= 1 & subset <= 576)
		# define subset with N 
		tableTCs <- loadTestsetDefinition()
		tableTCs <- defineSubset(tableTCs = tableTCs,N = subset)
	
		# set up dir structure data
		for (m in unique(tableTCs$Analyte)) {
			if (!dir.exists(file.path(workingDir,"Data", m))) {
				dir.create(path = file.path(workingDir, "Data", m),recursive =TRUE)
			}
		}
		
		# generate files
		generateDataFiles( tableTCs = tableTCs, outputDir = workingDir, rounding = rounding, verbose = verbose)		
		
	} else if (is.data.frame(subset)) {
		## use specified table
		
		# set up dir structure data
		for (m in unique(subset$Analyte)) {
			if (!dir.exists(file.path(workingDir,"Data", m))) {
				dir.create(path = file.path(workingDir, "Data", m),recursive =TRUE)
			}
		}
		# generate files
		generateDataFiles( tableTCs = subset, outputDir = workingDir, rounding = rounding, verbose = verbose)		
	}
		
}



#' Generate simulated data with one start seed for each biomarker and save each testcase as separate file
#' 
#' @param tableTCs		(data.frame) containing all information about the simulated test cases 
#' @param outputDir		(character) specifying the output directory where the data files should be written to
#' @param rounding		(logical) indicating whether decimal places stated in tableTCs should be applied (default, TRUE), 
#' 							if FALSE, data will be rounded to 5 decimal places to mimic not rounded data
#' @param verbose		(logical) indictaing if the progress counter should be shown (default: TRUE)
#' 
#' @return  No return value, instead the data files are generated
#'
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
generateDataFiles <- function( tableTCs = NULL, outputDir = NULL, rounding = TRUE, verbose = TRUE){

	# check input parameters
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.character(outputDir))
	stopifnot(is.character(outputDir))
	stopifnot(is.logical(rounding))
	stopifnot(is.logical(verbose))
	
	# get output path
	outputDir = file.path(outputDir, "Data")

	oldMarker <- tableTCs[1,]$Analyte

	# load complete table 
	allTestsets <- loadTestsetDefinition()
	
	# get and set seed for new marker
	seed <- tableTCs[1,]$startSeed
	set.seed(seed, kind = "default")

	#set up progress indicator 
	msgP 		<- gettext("Progress:")
	nCharMsg 	<- 0
	pValPrev    <- 0
	# traverse table and simulate datasets depending on the specifications for non-pathol and pathol distribution
	for (r in 1:nrow(allTestsets)) {
			
		# progress indicator
		pVal 		<- formatNumber(100*(r/nrow(allTestsets)),0)
		backspaces 	<- paste(rep("\b", nCharMsg), collapse ="")
		
		if(as.numeric(pVal) - as.numeric(pValPrev) >= 1){
			if(pValPrev == 0){
				message 	<-  paste(msgP, " ", pVal, "% ", sep ="", collapse ="")
				nCharMsg 	<-  nchar(message)- nchar(paste(msgP))
				
			}else{ 
				message 	<-  paste(" ", pVal, "% ", sep ="", collapse ="")
				nCharMsg 	<-  nchar(message)
			}
			if(verbose) cat(backspaces, message, sep ="")
			pValPrev 	<- pVal
		}
		
		tmp 	<- allTestsets[r,]
		
		marker 	<- tmp$Analyte
		index   <- tmp$Index
		N 		<- tmp$N
		
		if(marker != oldMarker){
			gc() 
			# get and set seed for new marker
			seed = allTestsets[allTestsets$Analyte ==as.character(marker), ]$startSeed[1]
			set.seed(seed, kind = "default")
			oldMarker <- marker
		}
		
		# simulate non-pathological distribution
		physTmp <- invBoxCox(rnorm(n = N, mean = tmp$nonp_mu, sd = tmp$nonp_sigma), lambda = tmp$nonp_lambda) + tmp$nonp_shift
		
		# values smaller than 0 are removed, thus if remaining values are not enough, simulate 3 times the amount and 
		# sample appropriate data points
		if (length(physTmp[physTmp >= 0]) < N*(1-tmp$fractionPathol-tmp$bg_fraction)) {
			physTmp <- invBoxCox(rnorm(n = N*3, mean = tmp$nonp_mu, sd = tmp$nonp_sigma), lambda = tmp$nonp_lambda)+tmp$nonp_shift
		}
		
		phys 	<- sample(physTmp[physTmp >=0], size = N * (1-tmp$fractionPathol-tmp$bg_fraction), replace = FALSE)
		
		# simulate background distribution, mimicking inconsistencies in RWD
		backgroundDist <- runif(n = N*tmp$bg_fraction, min = tmp$bg_min, max = tmp$bg_max)
		
		if(marker =="CRP"){
			# simulate left pathological distribution
			patho_low 	<- NULL
		}else {
			# simulate left pathological distribution
			patho_low 	<- rnorm(n = N, mean = tmp$left_mu, sd = tmp$left_sigma)
			
			if (length(patho_low[patho_low >= 0]) < N*(tmp$fractionPathol*(tmp$left_ratio/tmp$ratio_sum))) {
				patho_low 	<- rnorm(n = N*3, mean = tmp$left_mu, sd = tmp$left_sigma)
			}
			
			# if number of data points >= 0 is still smaller than needed number, sample with replacement
			if (length(patho_low[patho_low >= 0]) < N*(tmp$fractionPathol*(tmp$left_ratio/tmp$ratio_sum))) {
				patho_low   <- sample(patho_low[patho_low >= 0], size = N*(tmp$fractionPathol*(tmp$left_ratio/tmp$ratio_sum)), replace = TRUE)
			}else {
				patho_low   <- sample(patho_low[patho_low >= 0], size = N*(tmp$fractionPathol*(tmp$left_ratio/tmp$ratio_sum)), replace = FALSE)
			}
		}
		# simulate right pathological distribution 
		patho_high   <- rnorm(n = N, mean = tmp$right_mu, sd = tmp$right_sigma)
		
		if (length(patho_high[patho_high >= 0]) < N*(tmp$fractionPathol*(tmp$right_ratio/tmp$ratio_sum))) {
			patho_high   <- rnorm(n = N*3, mean = tmp$right_mu, sd = tmp$right_sigma)
		}
		
		patho_high   <- sample(patho_high[patho_high >= 0], size = N*(tmp$fractionPathol*(tmp$right_ratio/tmp$ratio_sum)), replace = FALSE)
		
		# concatenate simulates sub datasets and shuffle
		dd <- NULL
		dd <- c(phys, patho_low, patho_high, backgroundDist)
		dd <- sample(dd)
		
		# round data points for rounded testcases
		if (rounding) {	
			dd <- round(dd, digits = tmp$decimals)
		}else{ 
			dd <- round(dd, digits = 5)
		}
		
		# only write files that are part of the specified subset (for reproducibility, all test sets are generated, but only those specified are saved)
		if(tmp$Index %in% tableTCs$Index)
			fwrite(list(dd), file = file.path(outputDir,marker, paste0(index, "_", marker, "_seed_", seed, ".csv")))
		
	}
	if(verbose) cat("\n")
}

