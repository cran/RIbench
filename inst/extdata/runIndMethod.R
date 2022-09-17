#! usr/bin/env Rscript

# load libraries needed to execute this script
library(optparse)
library(data.table)
library(digest)


# define command line arguments
option_list = list(
		make_option(c("-x", "--index"), type="character", default=NULL, 
				help="dataset index", metavar="integer"),
		make_option(c("-f", "--filename"), type="character", default=NULL, 
				help="dataset file name", metavar="character"),
		make_option(c("-u", "--uniqueHashRounded"), type ="character", default =NULL, 
				help="dataset hash code for rouned data", metavar ="character"),
		make_option(c("-e", "--uniqueHashNotRounded"), type ="character", default =NULL, 
				help="dataset hash code for not rouned data", metavar ="character"),
		make_option(c("-o", "--outputBaseDir"), type="character", default="./", 
				help="output directory", metavar="character"),
		make_option(c("-i", "--inputBaseDir"), type="character", default="./", 
				help="input directory", metavar="character"), 
		make_option(c("-b", "--subsetDef"), type ="character", default = NULL, 
				help ="subset definition for name of progress file", metavar ="character"),
		make_option(c("-a", "--algoFunction"), type ="character", default = NULL, 
				help ="name of function to be called for RI estimation", metavar = "character"), 
		make_option(c("-p", "--params"), type ="character", default = NULL, 
				help ="Additional parameters needed for calling the algorithm", metavar = "list(args)"),
		make_option(c("-n", "--algoName"), type ="character", default = NULL, 
				help ="name of the algorithm used in the output files", metavar = "character"), 
		make_option(c("-l", "--libraries"), type ="character", default = NULL, 
				help ="Libraries that need to be loaded for the indirect method", metavar = "list(libs)"),
		make_option(c("-s", "--sourceFiles"), type ="character", default = NULL, 
				help ="source file(s) that contains the algoFunction, if it is not in a package", metavar = "list(sourceFiles)"),
		make_option(c("-r", "--ris"), type ="logical", default =FALSE, 
				help ="logical indicating whether model parameters (default, FALSE) or directly the RIs are returned (TRUE)", 
				metavar = "logical"), 
		make_option(c("-c", "--percentiles"), type ="character", default =NULL, 
				help ="if RIs are directly returned, percentiles need to be specified", metavar = "list(RIs)"), 
		make_option(c("-t", "--transformParam"), type ="numeric", default = NULL, 
				help ="provide a lambda if method only estimates normal distributions, e.g. for Hoffmann method", metavar ="numeric"),
		make_option(c("-w", "--warning"), type ="logical", default = FALSE, 
				help ="logical indicating whether warnings of function call should be shown or not (default, FALSE)", metavar ="logical")
)

# parse command line arguments and check parameter
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if(length(grep(",",opt$libraries)) > 0)
	opt$libraries = unlist(strsplit(opt$libraries, ","))

if(length(grep(",", opt$sourceFiles)) > 0)
	opt$sourceFiles = unlist(strsplit(opt$sourceFiles, ","))

if(length(grep(",",opt$params)) > 0)
	opt$params = trimws(unlist(strsplit(opt$params, ",")))


# load all specified libraries 
if(length(opt$libraries) != 0)
	invisible(lapply(opt$libraries, library, character.only = TRUE))

# source all specified files 
if(length(opt$sourceFiles) != 0 & opt$sourceFiles != "")
	invisible(lapply(opt$sourceFiles, source))

opt$inputBaseDir  <- trimws(opt$inputBaseDir)
opt$outputBaseDir <- trimws(opt$outputBaseDir)

biomarker <- strsplit(opt$filename, "_", perl = TRUE)[[1]][2]

outFile <- gsub(".csv", replacement = paste0("_", opt$algoName,".Rdata"), opt$filename)

# read data
data <- fread(file = file.path(opt$inputBaseDir, "Data", biomarker,opt$filename))$V1

# check if file was modified 
if(digest(data, algo = "md5", serializeVersion = 2, ascii = TRUE) != opt$uniqueHashRounded & digest(data, algo = "md5", serializeVersion = 2, ascii =TRUE) != opt$uniqueHashNotRounded)  
	stop("Modification of simulated test set data detected.")

# convert params into list with data 
if(!is.null(opt$params)){
	if(length(grep("=",opt$params)) > 0 ){
		
		tmp 	<- strsplit(opt$params, split ="=")
		vals 	<- lapply(tmp, '[[', 2)
		nVals 	<- lapply(tmp, '[[',1)

		vals <- lapply(vals, function(x){
			if(grepl(pattern ="^[0-9]+$", x = x, perl =TRUE))
				x = as.numeric(x)
			else 
				x = x
				})
		
		if(length(grep("RIperc",nVals)>1)){
			ind <- grep("RIperc", nVals)
			
			rp  <- unlist(vals[ind])
			
			vals <- vals[-ind]
			vals <- c(vals, list(as.numeric(rp)))
			
			nVals <- nVals[-ind]
			nVals <- c(nVals, "percentiles")
		}
		
		ld 	 	<- list(data)
		args 	<- c(ld, vals)
		
		names(args) <- c("", unlist(nVals))
				
	}else {
		ld 	 	<- list(data)
		args 	<- c(ld, opt$params)

	}
	
}else{ 
	args <- list(data)
}


# add percentiles to function argument if required
if(opt$ris){
	opt$percentiles <- as.numeric(trimws(gsub("'","", unlist(strsplit(opt$percentiles, ",")))))
	if(!"RIperc" %in% names(args) )
		args <- c(args, list(percentiles = opt$percentiles))
	
}

# add lambda to function argument if the transformation parameter required
if(!is.null(opt$transformParam))
	args <- c(args, list(lambda =opt$transformParam))


# run algorithm
runtime <- system.time({
			fit <- tryCatch({
						if(opt$warning)
							do.call(opt$algoFunction, args = args)
						else 
							suppressWarnings(do.call(opt$algoFunction, args = args))
					},
					error = function(cond){
						message(paste(opt$algoName, "failed. Here is the original error message:"))
						if(opt$ris){
							obj <- 	data.frame(Percentile = opt$percentiles, PointEst = NA)
							
						}else {
							obj 				<- list()
							obj$Data			<- as.numeric(data)
							obj$roundingBase 	<- NA
							obj$Lambda			<- NA
							obj$Mu				<- NA
							obj$Sigma			<- NA
							obj$P				<- NA
							obj$Cost			<- NA
							obj$Method			<- opt$algoName
							obj$Shift			<- NA
							obj$Time			<- NA
							obj$Status			<- "noResultFound"
							
							class(obj)			<- "RWDRI"
							
						}
						
						return(obj)
					})
		})

pkgVersion = NA 

# if a model was estimated: set runtime, biomarker, N, packageVersion, Shift and status
if(!opt$ris){
	
	fit$Runtime 	<- runtime
	fit$Data		<- NULL
	fit$Analyte 	<- biomarker
	fit$N 			<- length(data)
	
	
	pkgVersion <- tryCatch({
				packageVersion(opt$algoName)
			},
			error = function(cond){
				return(NA)
			})
	
	fit$PkgVersion	<- pkgVersion
	
	if(is.null(fit$Shift))
		fit$Shift <- 0 
	
	if(is.null(fit$Status))
		fit$Status <- "ResultFound"
	
}else { # else: only set runtime
	
	if(biomarker =="CRP"){
		fit$Runtime <- runtime[3]
	}else{ 
		fit$Runtime <- c(runtime[3],NA)
	}
	
}


# save file 
save(fit, file = file.path(opt$outputBaseDir, "/Results/", opt$algoName, biomarker, outFile))
fwrite(list("Index" = as.numeric(opt$index)+1, "Iteration" = 0), file.path(opt$outputBaseDir, paste0(opt$subsetDef,"_", opt$algoName,"_progressFile.csv")))
