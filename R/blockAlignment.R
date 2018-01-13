#' Make all sequences the same length
#' 
#' Coerces all sequences in a DNAbin object to the same length.
#' 
#' When \code{mode = "shortest"}, the alignment is truncated at the length of
#' the shortest sequence. When \code{mode = "longest"}, the alignment is
#' extended to the end of the longest sequence, with shorter sequences filled
#' in with \code{"fill"}s.
#' 
#' @param DNAbin An object of class \code{DNAbin}
#' @param mode Character vector. Options of "shortest" or "longest"
#' @param range Numeric vector of length 2. Index of the bases where the new
#' alignment should begin and end
#' @param fill Character to fill the extra bases in short sequences. Default of
#' "" (blank). Recommend that only "-" (gap) or "?" be used
#' @return A DNAbin object in matrix format.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Data Quality Protocol
#' @examples
#' 
#' data(salticidae)
#' salticidae
#' blockAlignment(salticidae)
#' blockAlignment(salticidae, mode = "longest")
#' blockAlignment(salticidae, mode = NULL, range = c(200, 600))
#' 
#' image(blockAlignment(salticidae))
#' image(blockAlignment(salticidae, mode = "longest"))
#' image(blockAlignment(salticidae, mode = NULL, range = c(200, 600)))
#'
#' @importFrom ape as.DNAbin
#' 
#' @export blockAlignment
blockAlignment <- function(DNAbin, mode = "shortest", range = NULL, fill = "") {
	
	DNAbin <- as.list(DNAbin)
	
	sizeRange <- range(sapply(DNAbin, length))
	
	if (is.null(mode)) {
		outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[range[1]:range[2]])))
	} else {
		if (mode == "shortest") {
			outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[1:sizeRange[1]])))
		}
		
		if (mode == "longest") {
			outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[1:sizeRange[2]])))
		}
	}
	
	db <- as.list(outDNAbin)
	dbs <- lapply(db, function(x) as.character(x))
	dbs <- lapply(dbs, function(x) replace(x, which(x == ""), fill))
	outDNAbin <- as.DNAbin(dbs)
	
	as.matrix(outDNAbin)
}
