#' Measures of identification accuracy
#' 
#' Tests of barcoding efficacy using distance-based methods.
#' 
#' These functions test barcoding efficacy. All sequences must be identified
#' prior to testing. Each sequence is considered an unknown while the remaining
#' sequences in the dataset constitute the DNA barcoding database that is used
#' for identification. If the identification from the test is the same as the
#' pre-considered identification, a correct result is returned.
#' 
#' \code{bestCloseMatch} conducts the "best close match" analysis of Meier et
#' al. (2006), considering the closest individual unless it is further than the
#' given threshold, which results in no identification. More than one species
#' tied for closest match results in an assignment of "ambiguous". When the
#' threshold is large, this analysis will return essentially the same result as
#' \code{nearNeighbour}. If \code{names = TRUE}, a list is returned containing
#' the names of all species represented by specimens within the threshold.
#' 
#' \code{nearNeighbour} finds the closest individual and returns if their names
#' are the same (TRUE) or different (FALSE). If \code{names = TRUE}, the name
#' of the closest individual is returned. Ties are decided by majority rule.
#' 
#' \code{threshID} conducts a threshold-based analysis, similar to that
#' conducted by the "Identify Specimen" tool provided by the Barcode of Life
#' Database (\url{http://www.boldsystems.org/index.php/IDS_OpenIdEngine}). It
#' is more inclusive than \code{bestCloseMatch}, considering ALL sequences
#' within the given threshold. If \code{names = TRUE}, a list is returned
#' containing the names of all species represented by specimens within the
#' threshold.
#' 
#' These functions are not recommended as identification tools, though they can
#' be used as such when \code{names = TRUE}.
#' 
#' @param distobj A distance object (usually from \code{\link{dist.dna}}).
#' @param sppVector Vector of species names. See \code{\link{sppVector}}.
#' @param threshold Distance cutoff for identifications. Default of 0.01 (1\%).
#' @param names Logical. Should the names of the nearest match be shown?
#' Default of FALSE.
#' @return \code{bestCloseMatch} and \code{threshID} return a character vector
#' giving the identification status of each individual.  \item{"correct"}{The
#' name of the closest match is the same} \item{"incorrect"}{The name of the
#' closest match is different} \item{"ambiguous"}{More than one species is the
#' closest match (\code{bestCloseMatch}), or is within the given threshold
#' (\code{threshID})} \item{"no id"}{No species are within the threshold
#' distance}
#' 
#' \code{nearNeighbour} returns a logical vector or (if \code{names = TRUE})
#' the name for the nearest individual.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{nearNeighbour}}, \code{\link{threshID}}, \code{\link{dist.dna}}, \code{\link{sppVector}} %% ~~objects to See
#' Also as \code{\link{help}}, ~~~
#' @references Meier, R., Shiyang, K., Vaidya, G., & Ng, P. (2006). DNA
#' barcoding and taxonomy in Diptera: a tale of high intraspecific variability
#' and low identification success. _Systematic Biology_ *55* (5) 715-728.
#' @keywords Barcoding
#' @examples
#' 
#' 
#' data(anoteropsis)
#' anoDist <- ape::dist.dna(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split = "_"), 
#'     function(x) paste(x[1], x[2], sep = "_"))
#' 
#' bestCloseMatch(anoDist, anoSpp)
#' bestCloseMatch(anoDist, anoSpp, threshold = 0.005)
#' nearNeighbour(anoDist, anoSpp)
#' nearNeighbour(anoDist, anoSpp, names = TRUE)
#' threshID(anoDist, anoSpp)
#' threshID(anoDist, anoSpp, threshold = 0.003)
#' 
#' data(dolomedes)
#' doloDist <- ape::dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' bestCloseMatch(doloDist, doloSpp)
#' bestCloseMatch(doloDist, doloSpp, threshold = 0.005)
#' nearNeighbour(doloDist, doloSpp)
#' nearNeighbour(doloDist, doloSpp, names=TRUE)
#' threshID(doloDist, doloSpp)
#' threshID(doloDist, doloSpp, threshold = 0.003)
#' 
#' @importFrom ape dist.dna
#' @export threshID
threshID <- function(distobj, sppVector, threshold = 0.01, names = FALSE){
	distobj <- as.matrix(distobj)
	diag(distobj) <- NA
	idConf <- rep(NA, length(sppVector))
	aa <- apply(distobj, MARGIN=2, FUN=function(x) which(x < threshold))
	if(is.matrix(aa)) aa <- c(unname(as.data.frame(aa))) else aa
	bb <- lapply(aa, function(x) unique(sppVector[x]))
	cc <- sppVector == bb
	dd <- sapply(1:length(sppVector), function(x) sppVector[x] %in% bb[[x]])
	ee <- apply(distobj, MARGIN=2, FUN=function(x) min(x, na.rm = TRUE))
	idConf[which(cc & dd)] <- "correct"
	idConf[which(!cc & !dd)] <- "incorrect"
	idConf[which(!cc & dd)] <- "ambiguous"
	idConf[which(ee > threshold)] <- "no id"
	if(names) output <- bb else output <- idConf
	output
}

######################
#Return the name of the result

#~ idBOLD <- function(distobj, sppVector, threshold = 0.01, highThreshold = 0.05){
	#~ distobj <- as.matrix(distobj)
	#~ diag(distobj) <- NA
	#~ output <- rep(NA, length(sppVector))
	#~ for(i in 1:length(sppVector)){
		#~ high <- sppVector[which(distobj[,i] < highThreshold)]
		#~ low <- sppVector[which(distobj[,i] < threshold)]

		#~ if(length(high) == 0) output[i] <- NA else {
		  #~ if(length(unique(low)) == 1) output[i] <- unique(low) else output[i] <- FALSE}
	#~ }
	#~ output
#~ }
