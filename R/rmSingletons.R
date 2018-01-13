#' Detect and remove singletons
#' 
#' A utility to detect and remove species represented only by singletons.
#' 
#' When \code{exclude = TRUE} (the default), singletons are excluded and the
#' vector returns the index of all non-singletons in the dataset. When
#' \code{exclude = FALSE}, the indices of the singletons are presented.
#' 
#' @param sppVector Vector of species names. (see \code{\link{sppVector}}).
#' @param exclude Logical. Should singletons be removed? Default of TRUE.
#' @return Returns a numeric vector giving the indices of the selected
#' individuals.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Utilities
#' @examples
#' 
#' data(anoteropsis)
#' anoDist <- ape::dist.dna(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' 
#' rmSingletons(anoSpp)
#' rmSingletons(anoSpp, exclude=FALSE)
#' 
#' data(dolomedes)
#' doloDist <- ape::dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' rmSingletons(doloSpp)
#' rmSingletons(doloSpp, exclude=FALSE)
#' @importFrom ape dist.dna
#' @export rmSingletons
rmSingletons <- function(sppVector, exclude = TRUE){
	singletons <- names(which(table(sppVector) == 1))
	if(exclude) which(!sppVector %in% singletons) else which(sppVector %in% singletons)
}
