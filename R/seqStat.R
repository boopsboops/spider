#' Sequence statistics
#' 
#' Utility that produces a table giving summary statistics for a `DNAbin'
#' object.
#' 
#' This function considers bases coded as '?', 'N' and '-' as missing data.
#' 
#' @param DNAbin Alignment of class `DNAbin'.
#' @param thresh Threshold sequence length. Default of 500 (minimum length for
#' official DNA barcodes).
#' @return A table giving the minimum, maximum, mean and median sequence
#' lengths, and the number of sequences with lengths below the given threshold.
#' @author Rupert Collins <rupertcollins@@gmail.com>
#' @keywords Barcoding Utilities
#' @examples
#' 
#' data(anoteropsis)
#' seqStat(anoteropsis)
#' 
#' 
#' @export seqStat
seqStat <- function(DNAbin, thresh = 500){
	rr <- sapply(DNAbin, FUN = function(x) length(x) - checkDNA(x, gapsAsMissing=TRUE))
	tab <- table(NULL)
	tab[1:5] <- c(min(rr), max(rr), mean(rr), median(rr), length(which(rr < thresh)))
	names(tab) <- c("Min", "Max", "Mean", "Median", "Thresh")
	round(tab, digits=0)
}
