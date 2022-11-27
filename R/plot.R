#' Plot method for generating a boxplot of the benchmark results 
#' 
#' @param errorList 		 containing the overall benchmark results 
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param outline			(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)
#' @param withMean			(logical) indicating whether the mean should be plotted as well (default: TRUE) 
#' @param withCats			(logical) set to TRUE if categories (e.g. pathological fraction) should be plotted (default: FALSE) 
#' @param withDirect		(logical) indicating whether the box of the direct method should be elongated to facilitate comparison (default:TRUE)
#' @param title				(character) specifying plot title
#' @param outputDir			(character) specifying a output directory
#' @param filename			(character) specifying a filename for the plot
#' @param ylim1				(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param ylim2				(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author  Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
plotBoxplot <- function(errorList, colList, nameList, outline = TRUE, withMean = TRUE, withCats = FALSE, withDirect = TRUE,
		title = "", outputDir = NULL, filename = NULL, ylim1 = c(0,100), ylim2 = c(100,1000), ...){
	
	# check input parameters
	stopifnot(length(errorList) == length(nameList))
	stopifnot(length(errorList) == length(colList))
	stopifnot(is.logical(outline))
	stopifnot(is.logical(withMean))
	stopifnot(is.logical(withCats))
	stopifnot(is.logical(withDirect))
	stopifnot(is.null(title) | is.character(title))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filename) | is.character(filename))
	stopifnot(is.numeric(ylim1))
	stopifnot(is.numeric(ylim2))
	
	args = list(...)
	
	colDirect = "grey"
	
	oldpar <- par(no.readonly = TRUE) 
	on.exit(suppressWarnings(par(oldpar)))
	
	# set plot device settings
	if(!is.null(outputDir) & !is.null(filename)){
		if(!is.null(args$width) & !is.null(args$height) & !is.null(args$res)){
			stopifnot(is.numeric(args$width))
			stopifnot(is.numeric(args$height))
			stopifnot(is.numeric(args$res))
			png(filename = file.path(outputDir, filename), width = args$width, height = args$height, res = args$res)
		}else if(withCats){
			png(filename = file.path(outputDir, filename), width = 2000, height = 900, res = 140)
		} else { 
			png(filename = file.path(outputDir,filename), width = 1000, height = 800, res = 140)
		}
	}
	
	# set ylab
	if(is.null(args$ylab))
		ylab = "percentage error"
	else 
		ylab  = args$ylab
	
	if( !is.null(args$yticks1) & !is.null(args$yticks2)){
		
		ticks  <- args$yticks1
		ticks2 <- args$yticks2
		
		bp <- boxplot(x = errorList, outline =outline, plot =FALSE)
		
		layout(mat = matrix(c(1,2), nrow = 2, ncol =1), widths = c(1,2), heights = c(1,2))
		par(mar = c(0, 4.1, 4.1, 1))
		
		# generate upper boxplot part
		bxp(bp, show.names =FALSE, axes =FALSE, ylim = ylim2, boxfill = colList, yaxs ="i", 
				at = 1:length(errorList), xaxs ="i", main =title)
		
		axis(side =2,las =1, at = ticks2, col ="grey44", col.ticks = "grey44", col.axis ="grey44")
		
		addGrid(y = ticks2, x= 0, col ="grey44")
		
		# generate lower boxplot part
		bxp(bp, show.names =FALSE, axes =FALSE, ylim = ylim2, add =TRUE,boxfill = colList, yaxs ="i", 
				at = 1:length(errorList), xaxs ="i")
				
		# add mean to plot
		if(withMean){
			mean <- lapply(errorList, mean, na.rm =TRUE)
			
			for(m in 1:length(mean)){
				if(!is.na(mean[[m]]) & mean[[m]] >= ylim2[1])
					points(m, mean[m], pch = 4, col ="black", lwd = 2, cex = 1)
			}
		}
		
		# set margins
		if(!is.null(args$mar)){
			par(mar = args$mar)
		}else if(withCats){
			par(mar = c(8, 4.1, 0, 1))
		}else{ 
			par(mar = c(5.1, 4.1, 0, 1))
		}
		
		# add axis labels and ticks 
		bxp(bp, axes =FALSE, ylim = ylim1, boxfill = colList, yaxs ="i",at = 1:length(errorList))
		axis(side = 2,las = 1, at = ticks)
		axis(side = 1, at = seq(1, length(errorList), by = 1), labels = nameList, las = 2)
		
		mtext(text = ylab, side = 2,  line = 3,las =3)
		box(bty ="l")
		
		# elongate box of direct method
		if(withDirect){
			qqDirect <- quantile(errorList[[1]], p = c(0.25, 0.5, 0.75), na.rm =TRUE)
			rect(xleft = -1, xright = length(errorList)+2, ybottom = qqDirect[1], ytop = qqDirect[3], col = as.rgb(colList[1], 0.8), border =NA )
		}
		
		# add grid and draw boxplot again
		addGrid(y = ticks,x = 0, "black")
		bxp(bp, axes =FALSE, ylim = ylim1, boxfill = colList, add =TRUE, yaxs ="i", at = 1:length(errorList))
		
		# add mean cross 
		if(withMean){
			for(m in 1:length(mean)){
				if(!is.na(mean[[m]]) & mean[[m]] <= ylim2[1])
					points(m, mean[m], pch = 4, col ="black", lwd = 2, cex = 1)
			}
		}		
		
	}else {
		
		layout(mat = matrix(c(1,2), nrow = 2, ncol =1), widths = c(1,2), heights = c(1,2))
		par(mar = c(0, 4.1, 4.1, 1))
		
		boxplot(x = errorList,
				names = NULL, ylab ="", main =title, las = 1,
				col = colList, fill =colList, 
				ylim = ylim2, ann =FALSE, xaxt ="n", 
				yaxs ="i", outline =outline)
		
		addGrid()
		
		boxplot(x = errorList, names = NULL, ylab ="", main =title, las = 1,
				col = colList, fill = colList, 
				ylim = ylim2, ann =FALSE, xaxt ="n", 
				yaxs ="i",add=TRUE,outline =outline)
				
		if(withMean){
			mean <- lapply(errorList, mean, na.rm =TRUE)
			
			for(m in 1:length(mean)){
				if(!is.na(mean[[m]]) & mean[[m]] >= ylim2[1])
					points(m, mean[m], pch = 4, col ="black", lwd = 2, cex = 1)
			}
		}
		
		if(!is.null(args$mar)){
			par(mar = args$mar)
		}else if(withCats){
			par(mar = c(8, 4.1, 0, 1), mgp = c(3,1.5,0))
		}else{ 
			par(mar = c(5.1, 4.1, 0, 1))
		}
		boxplot(x = errorList,
				names = nameList, ylab = ylab,  las = 2, at = seq(1, length(errorList),by = 1), 
				col = colList, fill = colList, 
				ylim = ylim1, yaxs = "i", outline =outline)
		
		if(withDirect){
			qqDirect <- quantile(errorList[[1]], p = c(0.25, 0.5, 0.75), na.rm =TRUE)
			rect(xleft = -1, xright = length(errorList)+2, ybottom = qqDirect[1], ytop = qqDirect[3], col = as.rgb(colList[1], 0.8), border =NA )
			
		}		
		
		addGrid()		
		
		boxplot(x = errorList,
				names = nameList, ylab =ylab,  las = 2, at = seq(1, length(errorList),by = 1), 
				col = colList, fill = colList, 
				ylim = ylim1, yaxs = "i", add=TRUE, outline =outline)
		
		if(withMean){
			for(m in 1:length(mean)){
				if(!is.na(mean[[m]]) & mean[[m]] <= ylim2[1])
					points(m, mean[m], pch = 4, col ="black", lwd = 2, cex = 1)
			}
		}
	}	
	
	if(!is.null(outputDir) & !is.null(filename))	
		dev.off()	
}


#' Wrapper function to generate all boxplots for the specified analytes split by defined categories 
#' 
#' @param analytes 			(character) vector specifying for which analytes the plots should be generated
#' @param errorListAll		(named list) containing the overall benchmark results per algorithm (names of list elements should be the names of the algorithms)
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param category			(character) defining the category used for creating the subsets. All defined sub-features are used for the categorization. 
#' 								Choose from "fractionPathol" (default), "N", or "OvFreq", individual or cumulative ("_cum"); if category is set this
#' 								will be used to define catList and catLabels
#' @param catList			(character) vector specifying the categories for which the boxes should be drawn
#' @param catLabels			(character) vector specifying the labels to the associated categories used for the x-axis
#' @param errorParam			(charcter) specifying for which error measure the plot should be generated 
#' @param outline			(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)
#' @param withMean			(logical) indicating whether the mean should be plotted as well (default: TRUE) 
#' @param withCats			(logical) set to TRUE if categories (e.g. pathological fraction) should be plotted (default: FALSE) 
#' @param withDirect		(logical) indicating whether the box of the direct method should be elongated to facilitate comparison (default:TRUE)
#' @param titlePart 		(character) specifying the latter part of the title
#' @param outputDir			(character) specifying an output directory
#' @param filenamePart		(character) specifying a filename for the plot
#' @param ylim1Vec			(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param ylim2Vec			(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param yticks1Vec		(numeric) vector specifying the ticks in y-direction for the first granular scale
#' @param yticks2Vec		(numeric) vector specifying the ticks in y-direction for the second less detailed scale
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

generateBoxplotsMultipleCats <- function(analytes, errorListAll, colList, nameList, category = c("fractionPathol","fractionPathol_cum", "N", "N_cum", "OvFreq", "OvFreq_cum"),
		catList = NULL, catLabels = NULL, errorParam = "zzDevAbs_Ov", outline = TRUE, withMean = TRUE, withDirect = TRUE, withCats = TRUE,
		titlePart = NULL, outputDir = NULL, filenamePart = NULL, ylim1Vec = NULL,ylim2Vec = NULL, yticks1Vec = NULL, yticks2Vec = NULL,...){
	
	# check input parameters 
	stopifnot(is.character(analytes))
	stopifnot(is.list(errorListAll))
	stopifnot(is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.character(category))
	stopifnot(is.null(catList) | is.character(catList))
	stopifnot(is.null(catLabels) | is.character(catLabels))
	stopifnot(is.character(errorParam))
	stopifnot(is.logical(outline))
	stopifnot(is.logical(withMean))
	stopifnot(is.logical(withCats))
	stopifnot(is.logical(withDirect))
	stopifnot(is.null(titlePart) | is.character(titlePart))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filenamePart) | is.character(filenamePart))
	stopifnot(is.null(ylim1Vec) | is.list(ylim1Vec))
	stopifnot(is.null(ylim2Vec) | is.list(ylim2Vec))
	stopifnot(is.null(yticks1Vec) | is.list(yticks1Vec))
	stopifnot(is.null(yticks2Vec) | is.list(yticks2Vec))
	
	# if ylim and ticks are not set, set them to 0 to 2.5 and 2.5 to 100	
	if(is.null(ylim1Vec)){
		ylim1Vec <- rep(list(c(0,2.5)), length(analytes))
		names(ylim1Vec) <- analytes
	}
	
	if(is.null(ylim2Vec)){
		ylim2Vec <- rep(list(c(2.5,100)), length(analytes))
		names(ylim2Vec) <- analytes
	}
	
	if(is.null(yticks1Vec)){
		yticks1Vec <- lapply(ylim1Vec, function(x){seq(x[1], x[2], by = 0.5)})
	}
	
	if(is.null(yticks2Vec)){
		yticks2Vec <- lapply(ylim2Vec, function(x){pretty(seq(x[1], x[2], by = 20))})
		for(a in analytes){
			if(yticks2Vec[[a]][1] < ylim2Vec[[a]][2])
				yticks2Vec[[a]] <- yticks2Vec[[a]][-1]
		}
	}
	
	# if category is set this will be used to define catList and catLabels
	if(!is.null(category)){
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
			
			catList	<- c("OvFreq <= 5", "OvFreq <= 10", "OvFreq <= 20", "OvFreq <= 30", "OvFreq <= 60")
			catLabels <- c("OvFreq <=5",  "OvFreq <=10",  "OvFreq <=20", "OvFreq <=30", "OvFreq <=60")
			
		} else if (category == "OvFreq_cum") {
			
			catList	<- c("OvFreq <= 5", "OvFreq > 5 & OvFreq <= 10", "OvFreq > 10  & OvFreq <= 20", "OvFreq > 20 & OvFreq <= 30", "OvFreq > 30")
			catLabels <-  c("OvFreq <=5", "OvFreq >5 & <=10", "OvFreq >10 & <=20", "OvFreq >20 & <=30", "OvFreq >30")
			
		}
	}
	
	# traverse analytes, combine results and generate boxplot for each analyte
	for(a in 1:length(analytes)){
		message(analytes[[a]])
		aChar <- as.character(analytes[a])
		
		ylim1 <- ylim1Vec[[aChar]]
		ylim2 <- ylim2Vec[[aChar]]
		
		yticks1 <- yticks1Vec[[aChar]]
		yticks2 <- yticks2Vec[[aChar]]
		
		
		if(analytes[[a]] =="CRP")
			tmpErrorName <- gsub(pattern ="_Ov", replacement = "_URL", x = errorParam)
		else 
			tmpErrorName <- errorParam
		
		errorListAnalyte <- NULL
		counter <- 1 
		for(algoInd in 1:length(names(errorListAll))){
			
			if(grepl("direct",names(errorListAll)[[algoInd]], ignore.case =TRUE)){
				errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[aChar]][,tmpErrorName]
				counter <- counter +1
				
			}else {
				if(withCats){
					for(cc in 1:length(catLabels)){
						ccName <- catLabels[cc]
						errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[aChar]][[ccName]][,tmpErrorName]
						counter <- counter +1
					}
				} else {
					errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[aChar]][,tmpErrorName]
					counter <- counter + 1
				}
			}
		}
		
		plotBoxplot(errorList  = errorListAnalyte, 
				colList = colList, nameList = nameList, title = paste(aChar, titlePart), 
				outputDir = outputDir, filename = paste0(aChar, "_",filenamePart, ".png"), ylim1 = ylim1, 
				ylim2 = ylim2,yticks1 = yticks1, yticks2 = yticks2, 
				outline = outline, withMean = withMean, withCats = withCats, withDirect = withDirect, ...)		
	}
}


#' Wrapper function to generate all boxplots for the specified distribution types split by defined categories 
#' 
#' @param errorListAll		(list) containing the overall benchmark results per algorithm 
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param catList			(character) vector specifying the categories for which the boxes should be drawn
#' @param catLabels			(character) vector specifying the labels to the associated categories used for the x-axis
#' @param errorParam		(charcter) specifying for which error measure the plot should be generated 
#' @param outline			(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)
#' @param withMean			(logical) indicating whether the mean should be plotted as well (default: TRUE) 
#' @param withCats			(logical) set to TRUE if categories (e.g. pathological fraction) should be plotted (default: FALSE) 
#' @param withDirect		(logical) indicating whether the box of the direct method should be elongated to facilitate comparison (default:TRUE)
#' @param titlePart 		(character) specifying the latter part of the title
#' @param outputDir			(character) specifying a output directory
#' @param filenamePart		(character) specifying a filename for the plot
#' @param ylim1Vec			(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param ylim2Vec			(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param yticks1Vec		(numeric) vector specifying the ticks in y-direction for the first granular scale
#' @param yticks2Vec		(numeric) vector specifying the ticks in y-direction for the second less detailed scale
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

generateBoxplotsDistTypes <- function(errorListAll, colList, nameList, catList, catLabels, errorParam = "zzDevAbs_Ov", 
		outline = TRUE, withMean = TRUE, withDirect = TRUE, withCats = TRUE, titlePart = NULL, outputDir = NULL, filenamePart = NULL, ylim1Vec = NULL, 
		ylim2Vec = NULL, yticks1Vec = NULL, yticks2Vec = NULL, ...){
	
	# check input parameters 
	stopifnot(is.list(errorListAll))
	stopifnot(is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.character(catList))
	stopifnot(is.character(catLabels))
	stopifnot(is.character(errorParam))
	stopifnot(is.logical(outline))
	stopifnot(is.logical(withMean))
	stopifnot(is.logical(withCats))
	stopifnot(is.logical(withDirect))
	stopifnot(is.null(titlePart) | is.character(titlePart))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filenamePart) | is.character(filenamePart))
	stopifnot(is.null(ylim1Vec) | is.list(ylim1Vec))
	stopifnot(is.null(ylim2Vec) | is.list(ylim2Vec))
	stopifnot(is.null(yticks1Vec) | is.list(yticks1Vec))
	stopifnot(is.null(yticks2Vec) | is.list(yticks2Vec))
	
	# set ylim and yticks
	if(is.null(ylim1Vec)){
		ylim1Vec <- rep(list(c(0,2.5)), length(catLabels))
		names(ylim1Vec) <- catLabels
	}
	
	if(is.null(ylim2Vec)){
		ylim2Vec <- rep(list(c(2.5,100)), length(catLabels))
		names(ylim2Vec) <- catLabels
	}
	
	if(is.null(yticks1Vec)){
		yticks1Vec <- lapply(ylim1Vec, function(x){seq(x[1], x[2], by = 0.5)})
	}
	
	if(is.null(yticks2Vec)){
		yticks2Vec <- lapply(ylim2Vec, function(x){pretty(seq(x[1], x[2], by = 20))})
		for(a in catLabels){
			if(yticks2Vec[[a]][1] < ylim2Vec[[a]][2])
				yticks2Vec[[a]] <- yticks2Vec[[a]][-1]
		}
	}
	
	# traverse categories, combine results and plot boxplot for each category
	for(cc in 1:length(catList)){
		message(catLabels[[cc]])
		cChar <- as.character(catLabels[cc])
		
		ylim1 <- ylim1Vec[[cChar]]
		ylim2 <- ylim2Vec[[cChar]]
		
		yticks1 <- yticks1Vec[[cChar]]
		yticks2 <- yticks2Vec[[cChar]]
		
		errorListAnalyte <- NULL
		counter <- 1 
		
		for(algoInd in 1:length(names(errorListAll))){
			
			if(grepl("direct",names(errorListAll)[[algoInd]], ignore.case =TRUE)){
				errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[cChar]][,errorParam]
				counter <- counter +1
			}else {
				
				if("CRP" %in% errorListAll[[algoInd]][[cChar]]$Analyte & errorParam != "zzDevAbsCutoff_Ov"){
					tmpErrorName <- gsub(pattern ="_Ov", replacement = "_URL", x = errorParam)				
					errorListAll[[algoInd]][[cChar]][errorListAll[[algoInd]][[cChar]]$Analyte =="CRP",errorParam] <-	errorListAll[[algoInd]][[cChar]][errorListAll[[algoInd]][[cChar]]$Analyte =="CRP",tmpErrorName]    
				}
				
				errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[cChar]][,errorParam]
				counter <- counter +1	
			}
			
		}
		
		plotBoxplot(errorList  = errorListAnalyte, 
				colList = colList, nameList = nameList, title = paste(cChar, titlePart), 
				outputDir = outputDir, filename = paste0(cChar, "_",filenamePart, ".png"), ylim1 = ylim1, 
				ylim2 = ylim2,yticks1 = yticks1, yticks2 = yticks2, 
				outline = outline, withMean = withMean, withCats = withCats, withDirect = withDirect, ...)		
	}
}


#' Wrapper function to generate one boxplot for a specified analyte
#' 
#' @param errorListAll		(list) containing the overall benchmark results per algorithm 
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param catList			(character) vector specifying the categories for which the boxes should be drawn
#' @param catLabels			(character) vector specifying the labels to the associated categories used for the x-axis
#' @param a					(character) specifying the analyte for which the boxplot should be generated
#' @param errorParam		(charcter) specifying for which error measure the plot should be generated 
#' @param outline			(logical) indicating whether outliers should be drawn (TRUE, default), or not (FALSE)
#' @param withMean			(logical) indicating whether the mean should be plotted as well (default: TRUE) 
#' @param withCats			(logical) set to TRUE if categories (e.g. pathological fraction) should be plotted (default: FALSE) 
#' @param withDirect		(logical) indicating whether the box of the direct method should be elongated to facilitate comparison (default:TRUE)
#' @param titlePart 		(character) specifying the latter part of the title
#' @param outputDir			(character) specifying a output directory
#' @param filenamePart		(character) specifying a filename for the plot
#' @param ylim1			(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param ylim2			(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

generateBoxPlotOneAnalyte <- function(errorListAll, colList, nameList, catList, catLabels, a, errorParam, outline = TRUE, withMean = TRUE, withCats = TRUE, withDirect = TRUE, 
		titlePart = NULL, outputDir, filenamePart = NULL, ylim1 = c(0,100), ylim2 = c(100,1000),...){
		
	# check input parameters 
	stopifnot(is.list(errorListAll))
	stopifnot(is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.character(catList))
	stopifnot(is.character(catLabels))
	stopifnot(is.character(a) & a %in% c("Hb", "Ca", "FT4", "AST", "LACT", "GGT", "TSH", "IgE", "CRP", "LDH"))
	stopifnot(is.character(errorParam))
	stopifnot(is.logical(outline))
	stopifnot(is.logical(withMean))
	stopifnot(is.logical(withCats))
	stopifnot(is.logical(withDirect))
	stopifnot(is.null(titlePart) | is.character(titlePart))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filenamePart) | is.character(filenamePart))
	stopifnot(is.null(ylim1) | is.numeric(ylim1))
	stopifnot(is.null(ylim2) | is.numeric(ylim2))
	
	errorListAnalyte <- NULL

	counter <- 1 
	
	for(algoInd in 1:length(names(errorListAll))){
		
		if(grepl("direct",names(errorListAll)[[algoInd]], ignore.case =TRUE)){
			errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[a]][,errorParam]
			counter <- counter +1
		}else {
			for(cc in 1:length(catLabels)){
				ccName = catLabels[cc]
				errorListAnalyte[[counter]] <- errorListAll[[algoInd]][[a]][[ccName]][,errorParam]
				counter <- counter +1
			}
		}
	}
	plotBoxplot(errorList  = errorListAnalyte, 
			colList = colList, nameList = nameList, title = paste(a, titlePart), 
			outputDir = outputDir, filename = paste0(a, "_",filenamePart, ".png"), ylim1 = ylim1, 
			ylim2 = ylim2, 
			outline = outline, withMean = withMean, withCats =withCats, withDirect = withDirect)	
}


#' Plot method for generating a scatterplot  
#' 
#' @param errorList  		(data frame) containing the overall benchmark results 
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param withColor			(character) indicating if plot should be colored according to pathological fraction, sample size or pathological overlap left / right
#' @param cats				(character) specifying the category labels
#' @param title				(character) specifying plot title
#' @param outputDir			(character) specifying a output directory
#' @param filename			(character) specifying a filename for the plot
#' @param xlim				(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param ylim				(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param xlab				(character) specifying x-axis label
#' @param ylab 				(character) specifying y-axis label
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author  Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 

plotScatterplot <- function(errorList, colList, nameList, withColor = NULL, cats = NULL,
		title = "", outputDir = NULL, filename = NULL, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, ... ){
	
	args <- list(...)
	# check input parameters 
	stopifnot(is.list(errorList))
	stopifnot(is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.null(withColor) | is.character(withColor))
	stopifnot(is.null(cats) | is.character(cats))
	stopifnot(is.null(title) | is.character(title))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filename) | is.character(filename))
	stopifnot(is.null(xlim) | is.numeric(xlim))
	stopifnot(is.null(ylim) | is.numeric(ylim))
	stopifnot(is.null(xlab) | is.character(xlab))
	stopifnot(is.null(ylab) | is.character(ylab))
	
	stopifnot(length(errorList) == length(nameList))
	
	oldpar <- par(no.readonly = TRUE)    
	on.exit(suppressWarnings(par(oldpar))) 
	
	if(!is.null(outputDir) & !is.null(filename))
		png(filename = file.path(outputDir, filename), width = 3000, height = 480, res = 140)
	
	par(mfrow = c(1,length(errorList)), mar = c(4.1, 4.1, 4.1, 2.1))
		
	for(e in 1:length(errorList)){
		
		if(is.null(withColor) | grepl("direct",nameList[e], ignore.case =TRUE)){
			plot(errorList[[e]][,1], errorList[[e]][,2], xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = paste0(nameList[e]), 
					col = as.rgb(colList[e], 0.6), las = 1, ...)
		} else {
			plot(errorList[[e]][,3], errorList[[e]][,4], xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, main = paste0(nameList[e]), 
					col = as.rgb(errorList[[e]][,9],0.6), las = 1, pch = 16, ...)
			legend("bottomright", legend = cats,  col = unique(errorList[[e]][,9]),
					fill = unique(errorList[[e]][,9]))
		}
		
		addGrid()
	}
	
	title(title, cex.main = 1.5, font = 2, line = -1, outer =TRUE)
	
	if(!is.null(outputDir) & !is.null(filename))	
		dev.off()	
}


#' Wrapper function to generate scatterplots for the specified analytes
#' 
#' @param analytes			(character) vector specifying for which analytes the scatterplot should be generated 
#' @param errorListAll		(list) containing the overall benchmark results per algorithm 
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param tableTCs			(data frame) containing all test case information 
#' @param errorParam		(character) specifying for which error measure the plot should be generated 
#' @param withColorCat		(character) indicating if plot should be colored according to the pathological fraction ("fractionPathol"), sample size ("N"), or "overlapPatholLeft", "overlapPatholRight" 
#' @param titlePart 		(character) specifying the latter part of the title
#' @param outputDir			(character) specifying a output directory
#' @param filenamePart		(character) specifying a filename for the plot
#' @param ylim				(numeric) vector specifying the limits in y-direction for the first granular scale	
#' @param xlim				(numeric) vector specifying the limits in y-direction for the second less detailed scale
#' @param xlab				(character) specifying x-axis label
#' @param ylab 				(character) specifying y-axis label
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author Tatjana Ammer \email{tatjana.ammer@@roche.com}

generateScatterplotsAll <- function(analytes, errorListAll, colList = NULL, nameList, tableTCs, errorParam = "zzDevAbs", withColorCat = NULL,
		titlePart = NULL, outputDir = NULL, filenamePart = NULL, ylim = NULL, xlim = NULL, xlab = NULL, ylab = NULL, ... ){
	
	# check input parameters 
	stopifnot(is.character(analytes))
	stopifnot(is.list(errorListAll))
	stopifnot(is.null(colList) | is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.data.frame(tableTCs))
	stopifnot(is.character(errorParam) & errorParam %in% c("PercError", "AbsPercError", "zzDev", "zzDevAbs", "AbsPercErrorWidth","Error","AbsError"))
	stopifnot(is.null(withColorCat) | is.character(withColorCat))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(titlePart) | is.character(titlePart))
	stopifnot(is.null(filenamePart) | is.character(filenamePart))
	stopifnot(is.null(xlim) | is.numeric(xlim))
	stopifnot(is.null(ylim) | is.numeric(ylim))
	stopifnot(is.null(xlab) | is.character(xlab))
	stopifnot(is.null(ylab) | is.character(ylab))
	
	if(!is.null(withColorCat))
		stopifnot(withColorCat %in% c("fractionPathol","N", "overlapPatholLeft", "overlapPatholRight"))
	
	if(is.null(xlab) & is.null(ylab)){
		if(errorParam =="PercError"){
			xlab <- "percentage error LRL"
			ylab <- "percentage error URL"
		}else if(errorParam =="AbsPercError"){
			xlab <- "absolute percentage error LRL"
			ylab <- "absolute percentage error URL"
		}else if(errorParam =="zzDev"){
			xlab <- "z-score deviation LRL"
			ylab <- "z-score deviation URL"
		}else if(errorParam =="zzDevAbs"){
			xlab <- "abs z-score deviation LRL"
			ylab <- "abs z-score deviation URL"
		}else if(errorParam =="AbsPercErrorWidth"){
			xlab <- "abs percentage error (based on RI range) LRL"
			ylab <- "abs percentage error (based on RI range) URL"
		}else if(errorParam =="Error"){
			xlab <- "error LRL"
			ylab <- "error URL"
		}else if(errorParam =="AbsError"){
			xlab <- "abs error LRL"
			ylab <- "abs error URL"
		}
	}
	
	if(is.null(colList))
		colList = topo.colors(n = length(nameList))
		
	tmpErrorName = errorParam
	tmpColors = NULL 
	
	for(a in 1:length(analytes)){
		message(analytes[[a]])
		
		marker <- analytes[[a]] 
				
		if(marker =="CRP" & errorParam == "AbsPercErrorWidth")
			tmpErrorName = "AbsPercError"
			
		subTable <- tableTCs[tableTCs$Analyte ==marker,]
		
		subTable <- subTable[,c("Index", "Distribution", "N", "fractionPathol", "overlapPatholLeft", "overlapPatholRight")]
		
		errorListAnalyte <- NULL
		
		cats <- NULL
		
		for(algoInd in 1:length(nameList)){
			
			errorListAnalyte[[algoInd]] <- errorListAll[[algoInd]][[marker]][,c(paste0(tmpErrorName, "_LRL"), paste0(tmpErrorName, "_URL"))]
			
			if(!is.null(withColorCat) & !grepl("direct",nameList[[algoInd]], ignore.case =TRUE)){
				tmp_df <- errorListAll[[algoInd]][[marker]][,c("Index",paste0(tmpErrorName, "_LRL"), paste0(tmpErrorName, "_URL"))]
				tmp_df <- merge(tmp_df, subTable, by ="Index")
				
				# define colors for color coding category and labels
				if(withColorCat=="fractionPathol"){
					tmp_colors <- data.frame(fractionPathol = levels(as.factor(tmp_df$fractionPathol)), 
						color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$fractionPathol)), name ="Spectral"))))
				
					tmp_df 	<- merge(tmp_df, tmp_colors, by ="fractionPathol")
					
					cats <- paste0("p=",levels(as.factor(tmp_df$fractionPathol)))
					
				}else if(withColorCat =="N"){
					
					tmp_colors <- data.frame(N = levels(as.factor(tmp_df$N)), 
							color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$N)), name ="Spectral"))))
					
					tmp_df 	<- merge(tmp_df, tmp_colors, by ="N")
					
					cats <- paste0("N=",levels(as.factor(tmp_df$N)))
					
				}else if(withColorCat =="overlapPatholLeft"){
					
					tmp_colors <- data.frame(overlapPatholLeft = levels(as.factor(tmp_df$overlapPatholLeft)), 
							color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$overlapPatholLeft)), name ="Spectral"))))
					
					tmp_df 	<- merge(tmp_df, tmp_colors, by ="overlapPatholLeft")
					
					cats <- paste0("ovL=",levels(as.factor(tmp_df$overlapPatholLeft)))
					
				}else if (withColorCat =="overlapPatholRight"){
					
					tmp_colors <- data.frame(overlapPatholRight = levels(as.factor(tmp_df$overlapPatholRight)), 
							color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$overlapPatholRight)), name ="Spectral"))))
					
					tmp_df 	<- merge(tmp_df, tmp_colors, by ="overlapPatholRight")
					
					cats <- paste0("ovR=",levels(as.factor(tmp_df$overlapPatholRight)))
				}				
				
				errorListAnalyte[[algoInd]] <- tmp_df				
			}
			
			if(marker =="CRP"){
				
				errorListAnalyte[[algoInd]] <- errorListAll[[algoInd]][[marker]][,c(paste0(tmpErrorName, "_LRL"), paste0(tmpErrorName, "_URL"))]
				
				errorListAnalyte[[algoInd]][,1] <- seq(1, nrow(errorListAnalyte[[algoInd]]), by = 1)
				
				if(!is.null(withColorCat) & !grepl("direct",nameList[[algoInd]], ignore.case =TRUE)){
					
					tmp_df <- errorListAll[[algoInd]][[marker]][,c("Index",paste0(tmpErrorName, "_LRL"), paste0(tmpErrorName, "_URL"))]
					tmp_df <- merge(tmp_df, subTable, by ="Index")
					
					# define colors for color coding category and labels
					if(withColorCat=="fractionPathol"){
						tmp_colors <- data.frame(fractionPathol = levels(as.factor(tmp_df$fractionPathol)), 
								color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$fractionPathol)), name ="Spectral"))))
						
						tmp_df 	<- merge(tmp_df, tmp_colors, by ="fractionPathol")
						cats 	<- paste0("p=",levels(as.factor(tmp_df$fractionPathol)))
						
					}else if(withColorCat =="N"){
						
						tmp_colors <- data.frame(N = levels(as.factor(tmp_df$N)), 
								color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$N)), name ="Spectral"))))
						
						tmp_df 	<- merge(tmp_df, tmp_colors, by ="N")
						cats 	<- paste0("N=",levels(as.factor(tmp_df$N)))
						
					}else if(withColorCat =="overlapPatholLeft"){
						
						tmp_colors <- data.frame(overlapPatholLeft = levels(as.factor(tmp_df$overlapPatholLeft)), 
								color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$overlapPatholLeft)), name ="Spectral"))))
						
						tmp_df 	<- merge(tmp_df, tmp_colors, by ="overlapPatholLeft")
						cats 	<- paste0("ovL=",levels(as.factor(tmp_df$overlapPatholLeft)))
						
					}else if (withColorCat =="overlapPatholRight"){
						
						tmp_colors <- data.frame(overlapPatholRight = levels(as.factor(tmp_df$overlapPatholRight)), 
								color = rev(I(brewer.pal(nlevels(as.factor(tmp_df$overlapPatholRight)), name ="Spectral"))))
						
						tmp_df 	<- merge(tmp_df, tmp_colors, by ="overlapPatholRight")
						cats 	<- paste0("ovR=",levels(as.factor(tmp_df$overlapPatholRight)))
						
					}
					
					tmp_df[,paste0(tmpErrorName, "_LRL")] <- seq(1, nrow(tmp_df), by =1)
					errorListAnalyte[[algoInd]] <- tmp_df					
				}
			}
			
		}
		
		plotScatterplot(errorList  = errorListAnalyte, 
				colList = colList, nameList = nameList, title = paste(analytes[[a]], titlePart), 
				outputDir = outputDir, filename = paste0(analytes[[a]], "_",filenamePart, ".png"), ylim = ylim,  
				xlim = xlim, xlab = xlab, ylab = ylab, withColor = withColorCat, cats = cats, ...)		
	}
}


#' Plot method for generating a barplot out of the benchmark results 
#' 
#' @param benchmarkRes  	(data frame) containing the overall benchmark results 
#' @param perDistType		(logical) indicating if one overall plot should be generated or if it should be separated by the distribution type
#' @param colList			(character) vector specifying the colors used for the different algorithms (should correspond to columns of benchmark results)
#' @param nameList			(character) vector specifying the names used in the legend (should correspond to columns of benchmark results), if NULL, colnames will be used
#' @param withLabels		(logical) indicating whether the corresponding values should be plotted as well (default: FALSE) 
#' @param withHorizLines	(logical) indicating whether horizontal lines should be plotted for a better visual separation of the different categories (default:FALSE)
#' @param title				(character) specifying plot title
#' @param xlim				(numeric) vector specifying the limits in x-direction	
#' @param xlab				(character) specifying the x-axis label	
#' @param outputDir			(character) specifying a output directory
#' @param filename			(character) specifying a filename for the plot
#' @param ...				additional arguments passed forward to other functions
#' 
#' @return				No return value. Instead, a plot is generated.
#' 
#' @author  Tatjana Ammer \email{tatjana.ammer@@roche.com}
#' 
plotBarplot <- function(benchmarkRes, perDistType = FALSE, colList, nameList = NULL, withLabels = FALSE, withHorizLines = FALSE, 
		title = NULL, xlim = NULL, xlab="Mean of Absolute Z-Score Deviations", outputDir = NULL, filename = NULL, ...){
	
	args = list(...)
	
	# check input parameters 
	stopifnot(is.data.frame(benchmarkRes))
	stopifnot(is.logical(perDistType))
	stopifnot(is.character(colList))
	stopifnot(is.character(nameList))
	stopifnot(is.logical(withLabels))
	stopifnot(is.logical(withHorizLines))
	stopifnot(is.null(title) | is.character(title))
	stopifnot(is.null(outputDir) | is.character(outputDir))
	stopifnot(is.null(filename) | is.character(filename))
	stopifnot(is.null(xlim) | is.numeric(xlim))
	stopifnot(is.null(xlab) | is.character(xlab))
	
	oldpar <- par(no.readonly = TRUE)    
	on.exit(suppressWarnings(par(oldpar)))  
	
	if(!is.null(outputDir) & !is.null(filename)){
		
		if(!is.null(args$width) & !is.null(args$height) & !is.null(args$res)){
			stopifnot(is.numeric(args$width))
			stopifnot(is.numeric(args$height))
			stopifnot(is.numeric(args$res))
			png(filename = file.path(outputDir, filename), width = args$width, height = args$height, res = args$res)
			if(is.null(args$mar))
				par(mar = c(4.1, 10, 0.1, 1))
			else 
				par(mar = args$mar)
		} else { 
			png(filename = file.path(outputDir,filename), width = 1000, height = 1800, res = 150)
			par(mar = c(4.1, 10, 0.1, 1))
		}
	}	
	
	cex = args$cex
	if(is.null(cex))
		cex = 1.1
	
	legendLoc = args$legendLoc
	if(is.null(legendLoc))
		legendLoc = "topright"
	
	# convert benchmark result data frame so it is suitable for generating a barplot
	pRes 	<- t(benchmarkRes[benchmarkRes$Subcategory != "PercCases" & benchmarkRes$DistributionType !="FailureRate" & benchmarkRes$DistributionType != "ImplausibleResults",])
	colnames(pRes) <- paste(pRes[1,], pRes[2,])
	pRes 	<- pRes[-c(1,2),]
	pRes 	<- as.data.frame(as.matrix(pRes),stringsAsFactors =FALSE)
	pRes[] 	<- lapply(pRes, as.numeric)
	pRes  	<- pRes[,c(ncol(pRes), 1:ncol(pRes)-1)]
	pRes 	<- rev(pRes)
	
	# one barplot for all results
	if(!perDistType){
		if(is.null(xlim))
			xlim <- c(0, max(pRes, na.rm =TRUE))
		
		yy <- barplot(as.matrix(pRes), beside = TRUE, horiz = TRUE, col = colList,
				las = 2, xlab = "", las = 1, main = "", border = NA, xlim = xlim)
		
		# draw horizontal lines (black between distribution types and grey between sub categories)
		if(withHorizLines){
			coord <- c(yy[1,6]-1, yy[1,11]-1, yy[1,16]-1, yy[1,21]-1)
			coord_grey <- c(yy[1,-1]-1)
			
			abline(h = coord_grey, col ="grey", lwd = 1.5)
			abline(h = coord, lwd = 1.5)
		}
		
		addGrid()
		yy <- barplot(as.matrix(pRes), beside = TRUE, horiz = TRUE, col = colList,
				las = 2, xlab = xlab, las = 1, main = title, add = TRUE, border = NA, xlim = xlim)
		
		# add labels with exact value to bars
		if(withLabels){
			xx <- as.vector(as.matrix(pRes))
			text(x =xx , y = yy,label = c(round(xx,2)), pos = 4, cex = 1.1)
		}
		
		# add legend
		if(ncol(benchmarkRes) == 3)
			legend(legendLoc, legend = rev(nameList), col = rev(colList),	fill = rev(colList))
		else 
			legend(legendLoc, legend = rev(rownames(pRes)), col = rev(colList),	fill = rev(colList))
		
		box()
	}else {
		# plot 4 barplots, one for each distribution type
		par(mfrow = c(2,2),mar = c(4.1, 10, 2, 1))
		distTypes <- strsplit(colnames(pRes), " ")
		distTypes <- unique(unlist(lapply(distTypes, '[[',1)))
		distTypes <- distTypes[distTypes!="Overall"]
		distTypes <- rev(distTypes)

		for (d in distTypes){
			
			# get subtable for distribution type
			pTmp <- pRes[,grep(d, colnames(pRes))]
			colnames(pTmp) <- trimws(gsub(d,"",colnames(pTmp)))
			
			# set xlim 
			if(is.null(xlim))
				xlimTmp <- c(0, max(pTmp, na.rm =TRUE))
			else 
				xlimTmp <- xlim
				
			
			yy <- barplot(as.matrix(pTmp), beside = TRUE, horiz = TRUE, col = colList,
					las = 2, xlab = "", las = 1, main = "", border = NA, xlim = xlim)
			
			# add horizontal lines
			if(withHorizLines){
				coord_grey <- c(yy[1,-1]-1)
				
				abline(h = coord_grey, col ="grey", lwd = 1.5)
			}
			
			addGrid()
			if(is.null(title))
				titleTmp <- paste0(d, " distribution")
			else 
				titleTmp <- title
			
			yy <- barplot(as.matrix(pTmp), beside = TRUE, horiz = TRUE, col = colList,
					las = 2, xlab = xlab, las = 1, main = titleTmp, add = TRUE, border = NA)
			
			# add labels with exact benchmark scores
			if(withLabels){
				xx <- as.vector(as.matrix(pTmp))
				text(x =xx , y = yy,label = c(round(xx,2)), pos = 4, cex = 1.1)
			}
			# add legend 
			if(ncol(benchmarkRes) == 3)
				legend(legendLoc, legend = rev(nameList), col = rev(colList),	fill = rev(colList))
			else 
				legend(legendLoc, legend = rev(rownames(pTmp)), col = rev(colList),	fill = rev(colList))
		
			box()
		}				
	}
	
	if(!is.null(outputDir) & !is.null(filename))
		dev.off()
}


#' Add a grid to an existing plot.
#' 
#' It is possible to use automatically determined grid lines (\code{x=NULL, y=NULL}) or specifying the number 
#' of cells \code{x = 3, y = 4} as done by \code{grid}. Additionally, x- and y-locations of grid-lines can be specified,
#' e.g. \code{x = 1:10, y = seq(0,10,2)}.
#' 
#' @param x (integer, numeric) single integer specifies number of cells, numeric vector specifies vertical grid-lines
#' @param y (integer, numeric) single integer specifies number of cells, numeric vector specifies horizontal grid-lines
#' @param col (character) color of grid-lines
#' @param lwd (integer) line width of grid-lines
#' @param lty (integer) line type of grid-lines
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

addGrid <- function(x = NULL, y = NULL, col = "lightgray", lwd = 1L, lty = 3L) {
	if (all(is.null(c(x,y))) || all(length(c(x,y))<2))               # call grid function
		grid(nx = x, ny = y, col = col, lwd = lwd, lty = lty)
	else {
		if (length(x) == 0)                                          # NULL
			xticks <- axTicks(side=1)
		else if (length(x) == 1) {
			U <- par("usr")
			xticks <- seq.int(U[1L], U[2L], length.out = x + 1)
		} else
			xticks <- x
		
		if (length(y) == 0)                                          # NULL
			yticks <- axTicks(side = 2)
		else if (length(y) == 1) {
			U <- par("usr")
			yticks <- seq.int(U[3L], U[4L], length.out = y + 1)
		}
		else
			yticks <- y
		
		abline(v = xticks, col = col, lwd = lwd, lty = lty)
		abline(h = yticks, col = col, lwd = lwd, lty = lty)
	}                                     
}


#' Convert color-names or RGB-code to possibly semi-transparent RGB-code.
#' 
#' Function takes the name of a color and converts it into the rgb space. Parameter "alpha" allows
#' to specify the transparency within [0,1], 0 meaning completey transparent and 1 meaning completey
#' opaque. If an RGB-code is provided and alpha != 1, the RGB-code of the transparency adapted color 
#' will be returned.
#' 
#' @param col (character) name of the color to be converted/transformed into RGB-space (code). Only
#'               those colors can be used which are part of the set returned by function colors(). Defaults
#'               to "black".
#' @param alpha (numeric) value specifying the transparency to be used, 0 = completely transparent, 
#'               1 = opaque.
#' 
#' @return RGB-code
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

as.rgb <- function(col = "black", alpha = 1) {
	if (length(col) > 1 && (length(alpha) == 1 || length(alpha) < length(col))) {        # unclear which alpha to use or only one alpha specified
		
		if(length(alpha) < length(col) && length(alpha) > 1)
			warning("Multiple (but too few) 'alpha' specified! Only use 'alpha[1]' for each color!")
		return(sapply(col, as.rgb, alpha = alpha[1]))
	}
	
	if (length(col) > 1 && length(col) <= length(alpha)) {                                # process each color separately
		res <- character()
		for (i in 1:length(col))
			res <- c(res, as.rgb(col[i], alpha[i]))
		return(res)
	}
	
	if ( col %in% colors() )
		return( rgb(t(col2rgb(col))/255, alpha = alpha) )
	else {
		col <- sub("#", "", col)
		R <- as.numeric(paste("0x", substr(col, 1,2), sep = ""))
		G <- as.numeric(paste("0x", substr(col, 3,4), sep = ""))
		B <- as.numeric(paste("0x", substr(col, 5,6), sep = ""))
		return( rgb(R/255, G/255, B/255, alpha = alpha, maxColorValue = 1) )
	}        
}

