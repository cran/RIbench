#' Convenience function to load the table with the information about the pre-defined test sets
#' 
#' @return (data frame) containing the pre-defined parameter combinations to generate the simulations 
#' 
#' @examples
#' testsets <- loadTestsetDefinition()
#' str(testsets)
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
loadTestsetDefinition <- function(){
	fpath <- system.file("extdata","SpecificationTestSets.csv", package = "RIbench")
	# read pre-defined table
	tableTCs <- read.csv(file = fpath, stringsAsFactors = FALSE)
	# check if table was modified	
	if(digest(tableTCs, algo ="md5", serializeVersion = 2, ascii = TRUE) != "596622dd802d99452d161efab0094fd5")
		stop("Modification of test set definitions detected.")
	
	return(tableTCs)
}


#' Convenience function to set up the directory structure used for storing data and results.
#'  
#' @param outputDir		(character) specifying the base output directory. From here, Data/biomarker and Result/algoName/biomarker directories are generated			
#' @param onlyData		(logical) if set to TRUE, only the biomarker subdirectories are generated, name of output directory is used as it is (default:FALSE)
#' @param onlyResults 	(logical) if set to TRUE, only the algoName/biomarker subdirectories are generated, name of output directory is used as it is (default:FALSE)
#' @param tableTCs		(data frame) containing the pre-defined parameter combinations to generate the simulations
#' @param algoName		(character) specifying the name of the algorithm used for creating the subdirectory
#' 
#' @return				No return value. Instead, the directory structure is set up. 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
setupDirStructure <- function(outputDir = NULL, onlyData = FALSE, onlyResults = FALSE, tableTCs = NULL, algoName = NULL){
	
	# check input parameters
	stopifnot(is.character(outputDir))
	stopifnot(is.logical(onlyData))
	stopifnot(is.logical(onlyResults))
	stopifnot(is.null(tableTCs) | is.data.frame(tableTCs))
	stopifnot(is.null(algoName) | is.character(algoName))
	
	# set up only Results folder and one folder for each analyte
	if(onlyResults & !is.null(algoName)){
		for(m in unique(tableTCs$Analyte)){
			if(!dir.exists(file.path(outputDir,algoName, m))){
				dir.create(path =file.path(outputDir, algoName,m),recursive =TRUE)
			}
		}
	}else if(onlyData){ # set up only specified data folder
		for(m in unique(tableTCs$Analyte)){
			if(!dir.exists(file.path(outputDir, m))){
				dir.create(path =file.path(outputDir, m),recursive =TRUE)
			}
		}
	}else{
		# set up both Results and Data folder for each analyte (and algorithm)
		for(m in unique(tableTCs$Analyte)){
			if(!dir.exists(file.path(outputDir,"Results",algoName, m))){
				dir.create(path =file.path(outputDir, "Results", algoName,m),recursive =TRUE)
			}
			if(!dir.exists(file.path(outputDir,"Data", m))){
				dir.create(path =file.path(outputDir, "Data", m),recursive =TRUE)
			}
		}
	}
}


#' One-parameter Box-Cox transformation.
#' 
#' @param x				(numeric) data to be transformed
#' @param lambda		(numeric) Box-Cox transformation parameter
#' 
#' @return (numeric) vector with Box-Cox transformation of x
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

BoxCox <- function(x, lambda){
	
	if(abs(lambda) < 1e-20)
		x <- log(x)
	else
		x <- (x^lambda-1) / lambda
	
	x
}


#' Inverse of the one-parameter Box-Cox transformation.
#' 
#' @param x				(numeric) data to be transformed
#' @param lambda		(numeric) Box-Cox transformation parameter
#' 
#' @return (numeric) vector with inverse Box-Cox transformation of x
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

invBoxCox <- function(x, lambda) {	
	
	if(abs(lambda) < 1e-20)
		x <- exp(x)
	else
		x <- (lambda*x+1)^(1/lambda) # if(lambda*x+1) is negative -> result is NaN
	
	x
}


#' Method to calculate reference intervals (percentiles) for objects of class 'RWDRI'
#' 
#' @param x				(object) of class 'RWDRI'
#' @param RIperc		(numeric) value specifying the percentiles, which define the reference interval
#' @param CIprop		(numeric) value specifying the central region for estimation of confidence intervals
#' @param pointEst		(character) specifying the point estimate determination: (1) using the full dataset ("fullDataEst"),
#' 						(2) calculating the median from all bootstrap samples ("medianBS"), (2) works only if NBootstrap > 0
#' 						(3) calculating the mean from all bootstrap samples ("meanBS"), (3) works only if NBootstrap > 0
#' @param truncNormal	(logical) specifying if a normal distribution truncated at zero shall be assumed 
#' @param Scale			(character) specifying if percentiles are calculated on the original scale ("Or") or the transformed scale ("Tr")
#' 
#' @return				(data.frame) with columns for percentile, point estimate and confidence intervals. 
#' 
#' @author Christopher Rank \email{christopher.rank@@roche.com}, Tatjana Ammer \email{tatjana.ammer@@roche.com}

getRI <- function(x, RIperc = c(0.025, 0.975), CIprop = 0.95, pointEst = c("fullDataEst", "medianBS", "meanBS"), truncNormal = FALSE, Scale = c("original", "transformed")){
	
	stopifnot(class(x) == "RWDRI")
	stopifnot(is.numeric(RIperc) & min(RIperc)>=0 & max(RIperc)<=1)
	stopifnot(is.numeric(CIprop) & length(CIprop)==1 & CIprop>=0 & CIprop<=1)
	pointEst <- match.arg(pointEst[1], choices = c("fullDataEst", "medianBS", "meanBS"))
	stopifnot(is.logical(truncNormal))
	Scale    <- match.arg(Scale[1], choices = c("original", "transformed"))
	
	RIperc	 <- sort(RIperc)
	RIResult <- data.frame(Percentile = RIperc, PointEst = NA, CILow = NA, CIHigh = NA)
	
	if (!is.na(x$Mu) & !is.na(x$Sigma) & !is.na(x$Lambda) & !is.na(x$Shift)) {
		
		if(truncNormal)
		{		
			# formula for truncated normal distribution
			RI <- pnorm(-1/x$Lambda, mean=x$Mu, sd=x$Sigma) + RIperc*(1 - pnorm(-1/x$Lambda, mean=x$Mu, sd=x$Sigma))
			RI <- qnorm(RI, mean=x$Mu, sd=x$Sigma)
			
		} else	
		{
			RI <- qnorm(p = RIperc, mean = x$Mu, sd = x$Sigma)
		}			
		
		if (Scale == "original") {
			RI <- invBoxCox(RI, lambda = x$Lambda)
			RI <- RI + x$Shift
			
			RI[RI<0] <- 0		
			RI[is.na(RI)] <- 0 
		}
		
		RIResult$PointEst <- RI
		
		# reference intervals for Bootstrap samples
		if (Scale == "original" & length(x$MuBS) > 0 & length(x$SigmaBS) > 0 & length(x$LambdaBS) > 0 & length(x$ShiftBS) > 0 & length(x$CostBS) > 0) {
			
			for (i in 1:length(RIperc)) {
				
				if(truncNormal)
				{					
					# formula for truncated normal distribution
					RIBS <- pnorm(-1/x$LambdaBS, mean=x$MuBS, sd=x$SigmaBS) + RIperc[i]*(1 - pnorm(-1/x$LambdaBS, mean=x$MuBS, sd=x$SigmaBS))
					RIBS <- qnorm(RIBS, mean=x$MuBS, sd=x$SigmaBS)					
					
				} else	
				{
					RIBS <- qnorm(p = RIperc[i], mean = x$MuBS, sd = x$SigmaBS)
				}				
				
				
				for (l in 1:length(RIBS)) {
					if (!is.na(x$MuBS[l]) & !is.na(x$SigmaBS[l]) & !is.na(x$LambdaBS[l]) & !is.na(x$ShiftBS[l])) {
						RIBS[l]	<- max(0, invBoxCox(RIBS[l], x$LambdaBS[l]) + x$ShiftBS[l], na.rm = TRUE)
						
					} else {
						RIBS[l] <-NA
					}
				}				
				
				RIResult$CILow[i]  <- as.numeric(quantile(x = RIBS, probs = (1-CIprop)/2, na.rm = TRUE))
				RIResult$CIHigh[i] <- as.numeric(quantile(x = RIBS, probs = 1-(1-CIprop)/2, na.rm = TRUE))
				
				if(pointEst == "medianBS") {
					RIResult$PointEst[i] <- median(RIBS, na.rm = TRUE)					
				} 		
				if(pointEst == "meanBS") {
					RIResult$PointEst[i] <- mean(RIBS, na.rm = TRUE)					
				}
			}
		}
	}
	
	return(RIResult)	
}


#' Standard print method for objects of class 'RWDRI'
#' 
#' @param x				(object) of class 'RWDRI'
#' @param RIperc		(numeric) value specifying the percentiles, which define the reference interval
#' @param CIprop		(numeric) value specifying the central region for estimation of confidence intervals
#' @param pointEst		(character) specifying the point estimate determination: (1) using the full dataset ("fullDataEst"),
#' 						(2) calculating the median from the bootstrap samples ("medianBS"), (2) works only if NBootstrap > 0
#' 						(3) calculating the mean from the bootstrap samples ("meanBS"), (3) works only if NBootstrap > 0
#' @param truncNormal	(logical) specifying if a normal distribution truncated at zero shall be assumed 
#' @param ...			additional arguments passed forward to other functions.
#' 
#' @return				No return value. Instead, a summary is printed.
#' 
#' @author Christopher Rank \email{christopher.rank@@roche.com}
#' 
#' @method print RWDRI

print.RWDRI <- function(x, RIperc = c(0.025, 0.975), CIprop = 0.95, pointEst = c("fullDataEst", "medianBS", "meanBS"), truncNormal = FALSE, ...){
	
	stopifnot(class(x) == "RWDRI")
	stopifnot(is.numeric(RIperc) & min(RIperc)>=0 & max(RIperc)<=1)
	stopifnot(is.numeric(CIprop) & length(CIprop)==1 & CIprop>=0 & CIprop<=1)
	stopifnot(is.logical(truncNormal))
	
	pointEst <- match.arg(pointEst[1], choices = c("fullDataEst", "medianBS", "meanBS"))
	
	# calculate reference intervals
	RI <- getRI(x = x, RIperc = RIperc, CIprop = CIprop, pointEst = pointEst, truncNormal = truncNormal)
	
	cat("\nReference Intervals\n")
	cat("------------------------------------------------\n")
	
	#check if reference intervals are na
	for (i in 1:length(RIperc)) {
		limit     <- "     median ["
		if(RIperc[i] < 0.5)
			limit <- "lower limit ["
		if(RIperc[i] > 0.5)
			limit <- "upper limit ["
		
		cat(paste0(limit, ifelse(RIperc[i]*100<10, " ", ""), format(round(RIperc[i]*100, 1), nsmall = 1), "% perc]: ", signif(RI$PointEst[i], 3))) 		
		if(!is.na(RI$CILow[i]) & !is.na(RI$CIHigh[i]))
			cat(paste0(" (", signif(RI$CILow[i], 3), "; ", signif(RI$CIHigh[i], 3), ")\n"))	
		else
			cat("\n")			
	}		
	
	cat("\nModel Parameters\n")
	cat("------------------------------------------------\n")	
	
	cat(paste0("     method: ", x$Method, " (v", x$PkgVersion, ")\n"))
	cat(paste0("      model: ", x$Model, "\n"))
	cat(paste0("     N data: ", length(x$Data), " (data points)\n"))
	
	if(!is.na(x$roundingBase))
		cat(paste0("    rounded: yes (base: ", x$roundingBase, ")\n"))
	else
		cat(paste0("    rounded: no\n"))
	
	if (!is.null(x$AgeMin) & !is.null(x$AgeMax))
		cat(paste0("  Age range: ", x$AgeMin, " to ", x$AgeMax, " years\n"))
	
	if (!is.null(x$Group))
		cat(paste0("     Gender: ", paste(x$Group, collapse=", "), "\n"))		
	
	cat(paste0("     lambda: ", signif(x$Lambda, 3), "\n"))
	cat(paste0("         mu: ", signif(x$Mu, 3), "\n"))
	cat(paste0("      sigma: ", signif(x$Sigma, 3), "\n"))
	cat(paste0("      shift: ", signif(x$Shift, 3), "\n"))
	cat(paste0("       cost: ", signif(x$Cost, 3), "\n"))
	cat(paste0("NP fraction: ", signif(x$P, 3), "\n"))	
}


#' Generate an MD5 hash sum for any R object.
#' 
#' @param x                (object) any R object.
#'
#' @return (character) MD5 hash sum of the input object.
#' 
#' @author Christopher Rank \email{christopher.rank@@roche.com}

generateMD5 <- function(x){
	
	return(digest(x, algo="md5", serializeVersion = 2, ascii = TRUE))
}


#' Function to simulate the direct method
#' 
#' @param N 		(integer) specifying the number of samples used as sample size for the direct method, default: 120
#' @param analyte	(character) specifying the biomarker that is currently simulated
#' @param params	(list) of parameters for non-pathological distribution (nonp_mu, nonp_sigma, nonp_lambda, and nonp_shift)
#' @param seed		(integer) specifying the seed used for the simulation, default: 123
#' @param NIter		(integer) specifiyng the number of times N samples should be drawn out of the simulated non-pathological distribution (default: 10,000)
#' @param RIperc	(numeric) value specifying the percentiles, which define the reference interval
#'  
#' @return 		(data frame) with the estimated reference intervals for NIter iterations
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

computeDirect <- function(N=120, analyte, params, seed = 123, NIter = 10000, RIperc = c(0.025, 0.975)){
	
	df <- NULL
	set.seed(seed, kind = "default")
	# sample N values using the defined parameters for the non-pathological distribution
	# and compute specified percentiles
	for(s in 1:NIter){
		physValue <- rnorm(n = N, mean = params$nonp_mu, sd = params$nonp_sigma)
		physValue <- invBoxCox(physValue, lambda = params$nonp_lambda)+ params$nonp_shift
		rr 		  <- quantile(physValue,probs = RIperc, type = 2)
		df 		  <- rbind(df,unname(rr))
	}
	
	df_rr <- data.frame(df)
	if(analyte =="CRP"){
		colnames(df_rr) <- "URL"
		df_rr$LRL <- 0
	}else{ 
		colnames(df_rr)<- c("LRL", "URL")
	}
	
	df_rr$N 	   <- N
	df_rr$Analyte  <- analyte
	return(df_rr)
}


#'Convenience function to simulate the direct method for the specified subset
#' 
#' @param tableTCs		(data frame) containing the pre-defined parameter combinations to generate the simulations
#' @param N 			(integer) specifying the number of samples used as sample size for the direct method, default: 120
#' @param cutoffZ		(numeric) specifying if a cutoff should be used to classify results as implausible and exclude from analysis 
#' 
#' @return  (data frame) with computed performance measures
#'
#' @examples 
#' 
#' \donttest{
#' # example to run direct method only for test sets for hemoglobin (Hb)
#' testsets <- loadTestsetDefinition()
#' directRes <- runDirectMethod(tableTCs = testsets[testsets$Analyte =="Hb",], N = 120, cutoffZ = 5)
#' } 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

runDirectMethod <- function(tableTCs = NULL, N = 120, cutoffZ = 5) {
	
	stopifnot(is.null(tableTCs) | is.data.frame(tableTCs))
	stopifnot(is.numeric(N))
	stopifnot(is.numeric(cutoffZ))
	
	# if test set specification is not defined, load the whole table and use it
	if(is.null(tableTCs))
		tableTCs <- loadTestsetDefinition()
	
	
	analytes 		<- unique(tableTCs$Analyte)
	
	dfs 	<- list()
	
	# traverse analytes and simulate direct method for each analyte
	for(a in analytes){
		message(a)
		params <- tableTCs[tableTCs$Analyte == a,][1,]
		RIperc = c(0.025, 0.975)
		
		if(a == "CRP")
			RIperc = (0.95)
		
			df_rr 		 <- computeDirect(N = N, analyte = a, params = params, NIter = 10000, RIperc = RIperc )
			df_rr$GT_LRL <- params$GT_LRL
			df_rr$GT_URL <- params$GT_URL
			
			dfs[[a]] <- df_rr

	}
	# compute performance measures for the direct method
	directErr <- computePerfMeasAll(analytes = analytes, algo = paste0("direct_", N), risIn = dfs, tableTCs = tableTCs, cutoffZ = cutoffZ)
	
	return(directErr)
}


#' Function for setting up the progress indicator. 
#' 
#' @param value			(integer) indicating the current number  
#' @param maxValue		(integer) indicating the maximum number 
#' @param nCharMsg		(integer) indicating the number of characters the message already has
#' 
#' @return (character) returing generated progress message
#' 
#' @author  Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
progressInd <- function(value, maxValue, nCharMsg = 0){
	msgP 		<- gettext("Progress:")
	nCharMsg 	<- 0
	
	# compute percentage of current progress
	pVal 		<- 100*(value/maxValue)
	
	# set up message and how many characters to delete
	backspaces 	<- paste(rep("\b", nCharMsg), collapse = "")
	message 	<- paste(msgP, " ", pVal, "% ", sep = "", collapse = "")
	nCharMsg 	<- nchar(message)
	
	cat(backspaces, message, sep = "")

	return(nCharMsg)
}


#' Function for defining a subset that is used for analyizing the computation time and can be used for other subanalyses. 
#' 
#' @param tableTCs 		(data frame) describing the pre-defined testcases 
#' @param N 			(integer) describing the number of testcases per biomarker contained in the subset (default: 50) 
#' @param seed			(integer) specifying the seed used for defining the subset, default: 123
#' 
#' @return (data frame) describing the updated table with all test case definitions. 
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

defineSubset <- function(tableTCs = NULL, N = 50, seed = 123){
	
	analytes <- as.character(unique(tableTCs$Analyte))
	
	tableTCs$Subset <- 0
	
	set.seed(seed, kind = "default")
	# traverse analytes and choose N cases for the subset
	for ( a in analytes){
		allInd <- tableTCs$Index[tableTCs$Analyte == a]
		indSub <- sample(allInd, size = N, replace = FALSE)
		tableTCs[tableTCs$Index %in% indSub,]$Subset <- 1
	}
	
	return(tableTCs[tableTCs$Subset ==1,])
}


#' Rounding method with trailing zeros. 
#' 
#' @param x				(numeric) value that is rounded 
#' @param digits		(integer) indicating the number of decimal places to be used 
#' 
#' @return				Rounded value with trailing zeros
#' 
#' @author Christopher Rank \email{christopher.rank@@roche.com}

formatNumber <- function(x, digits) {    
	#format value
	out <- round(x, digits = digits)        
	
	return(formatC(out, digits = digits, format = "f", drop0trailing = FALSE))            
}