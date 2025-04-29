#' Determine thresholds from a density plot
#' 
#' This function determines possible thresholds from the distance matrix for an
#' alignment.
#' 
#' This function is based on the concept of the barcoding gap, where a dip in
#' the density of genetic distances indicates the transition between intra- and
#' inter-specific distances. Understanding your data is vital to correctly
#' interpreting the output of this function, but as a start, the first local
#' minimum is often a good place to start.
#' 
#' The value of this function is that it does not require prior knowledge of
#' species identity to get an indication of potential threshold values.
#' 
#' @param distobj A distance object (usually from \code{\link[ape]{dist.dna}}).
#' @return An object of class `density', which is a list containing the values
#' calculated by \code{\link{density}}. The element \code{localMinima} has been
#' added, which contains the values of the local minima of the density plot.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link[ape]{dist.dna}}, \code{\link{density}}. %% ~~objects to See
#' Also as \code{\link{help}}, ~~~
#' @keywords Barcoding
#' @examples
#' 
#' 
#' data(anoteropsis)
#' anoDist <- ape::dist.dna(anoteropsis)
#' 
#' anoThresh <- localMinima(anoDist)
#' graphics::plot(anoThresh)
#' anoThresh$localMinima
#' #Often the first value is the one to go for:
#' anoThresh$localMinima[1]
#' 
#' @importFrom stats density
#' @importFrom ape dist.dna
#' @importFrom graphics plot
#' @export localMinima
localMinima <- function(distobj){
	den <- density(distobj)
	a <- rep(NA, length(den$y)-2)
	for(i in 2:(length(den$y)-1)) a[i-1] <- den$y[i-1] > den$y[i] & den$y[i+1] > den$y[i]
	den$localMinima <- den$x[which(a)]
	den$data.name <- deparse(substitute(distobj))
	den$call <- paste("density.default(", den$data.name, ")", sep="")
	print(den$localMinima)
	invisible(den)
}
