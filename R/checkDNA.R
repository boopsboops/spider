#' Check a DNA alignment for missing data
#' 
#' This functions counts the number of bases in an alignment that are composed
#' of missing data.
#' 
#' This function considers bases coded as '?' and 'N' as missing data. By
#' default, gaps (coded as '-') are also considered missing.
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @param gapsAsMissing Logical. Should gaps (coded as '-') be considered
#' missing bases? Default of TRUE.
#' @return A numeric vector giving the number of missing bases in each sequence
#' of the alignment.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Utilities
#' @examples
#' 
#' data(anoteropsis)
#' checkDNA(anoteropsis)
#' checkDNA(anoteropsis, gapsAsMissing=FALSE)
#' 
#' @export checkDNA
checkDNA <-
function(DNAbin, gapsAsMissing = TRUE){
	if(gapsAsMissing) bases <- c(2, 240, 4) else bases <- c(2, 240)
	if(is.list(DNAbin)) output <- sapply(DNAbin, function(x) length(which(as.numeric(x) %in% bases)))
	if(is.matrix(DNAbin)) output <- apply(DNAbin, MARGIN = 1, FUN = function(x) length(which(as.numeric(x) %in% bases)))
output
}

