#' Missing bases in alignments
#' 
#' Checks what columns in an alignment have ambiguous bases or missing data.
#' 
#' Ambiguous bases are bases that have been coded with any of the Union of Pure
#' and Applied Chemistry (IUPAC) DNA codes that are not A, C, G, or T. Missing
#' data are bases that have been coded with "-", "?" or "N".
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @return A logical vector containing TRUE if ambiguous bases or missing data
#' are present, FALSE if not. Does not differentiate between the two classes of
#' data.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{checkDNA}}
#' @keywords Utilities
#' @examples
#' 
#' data(woodmouse)
#' is.ambig(woodmouse)
#' #Columns with ambiguous bases
#' which(is.ambig(woodmouse))
#' 
#' @export is.ambig
is.ambig <- function(DNAbin){
   x <- as.matrix(DNAbin)
   bases <- c(136, 72, 40, 24)
   ambig <- apply(x, 2, FUN=function(x) sum(as.numeric(!as.numeric(x) %in% bases)))
   ambig > 0
}

