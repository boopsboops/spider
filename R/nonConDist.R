#' Nearest non-conspecific and maximum intra-specific distances
#' 
#' These functions give the distances to the nearest non-conspecific and
#' furthest conspecific representatives for each individual in the dataset.
#' 
#' \code{nonConDist} returns the minimum inter-specific distance for each
#' individual.
#' 
#' \code{maxInDist} returns the maximum intra-specific distance for each
#' individual.
#' 
#' These two functions can be used to create a version of the barcoding gap.
#' 
#' @aliases nonConDist maxInDist
#' @param distobj Distance matrix.
#' @param sppVector Species vector (see \code{\link{sppVector}}). Default of
#' NULL.
#' @param propZero Logical. TRUE gives the proportion of zero distances.
#' @param rmNA Logical. TRUE ignores missing values in the distance matrix.
#' Default of FALSE
#' @return If \code{propZero=FALSE}, a numeric vector giving the distance of
#' the closest non-conspecific individual (\code{nonConDist}) or the most
#' distant conspecific individual (\code{maxInDist}).
#' 
#' If \code{propZero=TRUE}, a single number giving the proportion of zero
#' distances.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Barcoding
#' @examples
#' 
#' data(anoteropsis)
#' anoDist <- dist.dna(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' 
#' nonConDist(anoDist, anoSpp)
#' nonConDist(anoDist, anoSpp, propZero=TRUE)
#' 
#' maxInDist(anoDist, anoSpp)
#' maxInDist(anoDist, anoSpp, propZero=TRUE)
#' 
#' #Barcoding gap
#' inter <- nonConDist(anoDist, anoSpp)
#' intra <- maxInDist(anoDist, anoSpp)
#' hist(inter-intra)
#' 
#' #An alternative way of plotting the gap
#' bnd <- cbind(data.frame(inter, intra))
#' ord <- bnd[order(bnd$inter),]
#' plot(ord$inter, type="n", ylab="Percent K2P distance", xlab="Individual")
#' segCol <- rep("gray50", length(ord$inter))
#' segCol[ord$inter-ord$intra < 0] <- "red"
#' segments(x0=1:length(ord$inter), y0=ord$inter, y1=ord$intra, col=segCol, lwd=6)
#' 
#' 
#' 
#' @export nonConDist
nonConDist <-
function(distobj, sppVector = NULL, propZero = FALSE, rmNA = FALSE){
	distobj <- as.matrix(distobj)
	if(length(sppVector) > 0) dimnames(distobj)[[1]] <- sppVector
	nonSpecDists <- list()
	for(i in 1:length(dimnames(distobj)[[1]])){
	  nonSpec <- dimnames(distobj)[[1]] != dimnames(distobj)[[1]][i]
	  nonSpecDists[[i]] <- min(distobj[nonSpec,i] , na.rm = rmNA)
	}	
if(propZero) output <- length(which(unlist(nonSpecDists) == 0))/length(unlist(nonSpecDists)) else output <- unlist(nonSpecDists)

output
}

