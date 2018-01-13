#' Sliding nucleotide diagnostics
#' 
#' Calculates the number of diagnostic nucleotides in sliding windows.
#' 
#' Determines the number of diagnostic nucleotides for each species in each
#' window.
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @param sppVector Species vector (see \code{\link{sppVector}}).
#' @param width Desired width of windows in number of base pairs.
#' @param interval Distance between each window in number of base pairs.
#' Default of 1. Giving the option of \code{"codons"} sets the size to 3.
#' @return A matrix giving the number of diagnostic nucleotides for each
#' species (rows) in each window (columns).
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{slideAnalyses}}, \code{\link{slideBoxplots}},
#' \code{\link{slidingWindow}}.
#' @keywords Sliding window
#' @examples
#' 
#' data(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' slideNucDiag(dolomedes, doloSpp, 200, interval = 3)
#' 
#' slidND <- slideNucDiag(dolomedes, doloSpp, 200, interval = 3)
#' 
#' #Number of basepairs for each species
#' graphics::matplot(t(slidND), type = "l")
#' 
#' #Number of basepairs for a single species
#' graphics::plot(slidND[4, ], type = "l")
#' 
#' #Total number of basepairs per window
#' graphics::plot(colSums(slidND), type = "l")
#' 
#' 
#' @importFrom graphics plot
#' @importFrom graphics matplot
#' @export slideNucDiag
slideNucDiag <- 
function(DNAbin, sppVector, width, interval = 1){
	nd <- nucDiag(DNAbin, sppVector)
	if(interval == "codons") interval <- 3
	win <- seq(1, dim(DNAbin)[2] - width, by = interval)
	mat <- matrix(NA, nrow = length(nd), ncol = length(win))
	for(i in 1:length(win)) mat[ ,i] <- sapply(nd, function(x) length(which(x %in% win[i]:(win[i] + width))))
	dimnames(mat)[[1]] <- unique(sppVector)
	mat
}

