#' Calculate Tajima's K index of divergence
#' 
#' Calculates Tajima's K index of divergence.
#' 
#' 
#' @param DNAbin An object of class `DNAbin'.
#' @param prop Logical. Should the function report the number of substitutions
#' per nucleotide? Default of TRUE.
#' @return A vector of length 1. If \code{prop = FALSE}, the mean number of
#' substitutions between any two sequences is returned. If \code{prop = TRUE}
#' (the default), this number is returned as the mean number of substitutions
#' per nucleotide (i.e. the above divided by the length of the sequences).
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{dist.dna}}.
#' @references Tajima, F. (1983). Evolutionary relationship of DNA sequences in
#' finite populations. _Genetics_ *105*, 437-460.
#' @keywords Utilities
#' @examples
#' 
#' data(anoteropsis)
#' tajima.K(anoteropsis)
#' tajima.K(anoteropsis, prop = FALSE)
#' 
#' @export tajima.K
tajima.K <-
function(DNAbin, prop = TRUE){
   res <- mean(dist.dna(DNAbin, model="N"))
   if(prop) res <- res/dim(DNAbin)[2]
   res
}

