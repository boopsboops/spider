#' Population Aggregate Analysis
#' 
#' Conducts population aggregate analysis over a matrix of characters of
#' interest.
#' 
#' When used on DNA sequences, the function treats gaps as seperate characters.
#' 
#' @param data A data matrix with columns as characters and rows as
#' individuals.
#' @param sppVector The species vector. See \code{\link{sppVector}}.
#' @return A matrix with species as rows and characters as columns. Cells give
#' the character state of each species if fixed, or "poly" if the character is
#' polymorphic.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @references Sites, J. W. J., & Marshall, J. C. (2003). Delimiting species: a
#' Renaissance issue in systematic biology. _Trends in Ecology and Evolution_
#' *18* (9), 462-470.
#' @keywords Morphology
#' @examples
#' 
#' #Create some exemplar data
#' u <- sample(c(0,1), 16, replace=TRUE)
#' v <- rep(c(0,1), rep(8,2))
#' x <- rep(c(1,0), rep(8,2))
#' y <- sample(c(0,1), 16, replace=TRUE)
#' z <- rep(c(1,0), rep(8,2))
#' 
#' dat <- cbind(u,v,x,y,z)
#' popn <- rep(c("A","B", "C", "D"), rep(4,4))
#' 
#' paa(dat, popn)
#' 
#' #Use on DNA sequences
#' data(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#' 	function(x) paste(x[1], x[2], sep="_"))
#' 
#' paa(as.character(anoteropsis), anoSpp)
#' 
#' @export paa
paa <- 
function(data, sppVector){
	singPoly <- function(vec){
		aa <- unique(vec)
		bb <- length(aa)
		cc <- ifelse(bb == 1, aa, "poly")
		cc
		}
	apply(data, MARGIN = 2, FUN = function(x) tapply(x, sppVector, singPoly))
}
