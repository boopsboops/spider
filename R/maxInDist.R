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
#' \code{minInDist} returns the minimum intra-specific distance for each
#' individual.
#' 
#' @aliases nonConDist maxInDist minInDist
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
#' anoDist <- ape::dist.dna(anoteropsis)
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
#' graphics::hist(inter-intra)
#' 
#' #An alternative way of plotting the gap
#' bnd <- cbind(data.frame(inter, intra))
#' ord <- bnd[order(bnd$inter),]
#' graphics::plot(ord$inter, type="n", ylab="Percent K2P distance", xlab="Individual")
#' segCol <- rep("gray50", length(ord$inter))
#' segCol[ord$inter-ord$intra < 0] <- "red"
#' graphics::segments(x0=1:length(ord$inter), y0=ord$inter, y1=ord$intra, col=segCol, lwd=6)
#' 
#' @importFrom ape dist.dna
#' @importFrom graphics hist
#' @importFrom graphics plot
#' @importFrom graphics segments
#' 
#' @export maxInDist
maxInDist <- 
function (distobj, sppVector = NULL, propZero = FALSE, rmNA = FALSE){
    dat <- as.matrix(distobj)
    if (length(sppVector) > 0) 
        dimnames(dat)[[1]] <- sppVector
    conSpecDists <- list()
    for (i in 1:length(dimnames(dat)[[1]])) {
        conSpec <- dimnames(dat)[[1]] == dimnames(dat)[[1]][i]
	dd <- sort(dat[conSpec, i], decreasing = TRUE)
        if(length(dd) > 1) conSpecDists[[i]] <- dd[1] else conSpecDists[[i]] <- NA
    }
    if (propZero) 
        output <- length(which(unlist(conSpecDists) == 0))/length(unlist(conSpecDists))
    else output <- unname(unlist(conSpecDists))
    output
}