#' Threshold optimisation
#' 
#' Determines the positive, negative, false positive and false negative rates
#' of identification accuracy for a given threshold.
#' 
#' When run over a range of thresholds, this function allows the optimisation
#' of threshold values based on minimising the identification error rates. See
#' the example below for more details.
#' 
#' @param distobj Distance matrix.
#' @param sppVector Species vector (see \code{\link{sppVector}}).
#' @param threshold Threshold distance for delimiting intra- and inter-specific
#' variation. Default of 0.01.
#' @return A table giving the threshold and number of negative and positive
#' identifications, number of false negative and false positive
#' identifications, and the cumulative error.
#' @author Rupert Collins <rupertcollins@@gmail.com>
#' @seealso \code{\link{localMinima}}.
#' @references Meyer, C. P., and Paulay, G. (2005). DNA barcoding: error rates
#' based on comprehensive sampling. _PLoS Biology_ *3* (12), 2229-2238.
#' @keywords Barcoding
#' @examples
#' 
#' data(anoteropsis)
#' anoDist <- ape::dist.dna(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' threshOpt(anoDist, anoSpp)
#' 
#' data(dolomedes)
#' doloDist <- ape::dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' threshOpt(doloDist, doloSpp)
#' 
#' #Conduct the analysis over a range of values to determine the optimum threshold
#' threshVal <- seq(0.001,0.02, by = 0.001)
#' opt <- lapply(threshVal, function(x) threshOpt(doloDist, doloSpp, thresh = x))
#' optMat <- do.call(rbind, opt)
#' graphics::barplot(t(optMat)[4:5,], names.arg=optMat[,1], xlab="Threshold values", 
#'     ylab="Cumulative error")
#' graphics::legend(x = 2.5, y = 29, legend = c("False positives", "False negatives"), 
#'     fill = c("grey75", "grey25"))
#' 
#' @importFrom ape dist.dna
#' @importFrom graphics barplot
#' @importFrom graphics legend
#' @export threshOpt
threshOpt <- function(distobj, sppVector, threshold = 0.01){
	distobj <- as.matrix(distobj)
	#individual (diag) values excluded
	diag(distobj) <- NA
	#indices of singletons
	singletons <- rmSingletons(sppVector, exclude=FALSE)
	#gets vector of column rows of all non-singletons
	ZZ <- rmSingletons(sppVector, exclude=TRUE)
	#run the sensitivity loop - singleton species (MU) excluded from iteration - only uses ZZ for query
	#False positive -  no matches within x% of query --- as the "NA" value
	#Positive - only 1 sp. within x% of query --- as the "TRUE" value;
	#False negative - more than 1 spp. within x% of query --- as the "FALSE" value
	#remember for 0, we need to say ==0 rather than <0
	OUT <- NULL
	for(i in 1:dim(distobj)[1]) {
		inThresh <- sppVector[which(distobj[,i] < threshold)]
			if(length(inThresh) == 0 && i %in% singletons) OUT[i] <- "True neg" else {
				if(length(inThresh) == 0 && !i %in% singletons) OUT[i] <- "False pos" else{
					if(length(unique(inThresh)) == 1 && unique(inThresh) == sppVector[i]) OUT[i] <- "True pos" else{
						if(length(unique(inThresh)) == 1 && !unique(inThresh) == sppVector[i]) OUT[i] <- "False neg" else OUT[i] <- "False neg"}
	}}}
	OUT <- factor(OUT, levels=c("True neg", "True pos", "False neg", "False pos"))
	tab <- table(OUT)
	tab <- c(threshold, tab, sum(tab[3], tab[4]))
	names(tab)[c(1,6)] <- c("Threshold", "Cumulative error")
	tab
}
