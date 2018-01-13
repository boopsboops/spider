#' Intra and inter-specific distances
#' 
#' Separates a distance matrix into its inter- and intra-specific components.
#' 
#' This function can be used to produce histograms and other charts exploring
#' the `barcode gap', such as in the examples below.
#' 
#' @param distobj A distance matrix.
#' @param sppVector The species vector (see \code{\link{sppVector}}).
#' @return A list with two elements: \item{inter}{A numeric vector containing
#' ALL inter-specific pairwise distances.} \item{intra}{A numeric vector
#' containing ALL intra-specific pairwise distances.}
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{sppDistMatrix}}.
#' @keywords Barcoding
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' doloSpDist <- sppDist(doloDist, doloSpp)
#' 
#' doloSpDist
#' 
#' #Histogram of the barcode gap
#' transGreen <- rgb(0, 1, 0, 0.5) #Make a slightly transparent colour to see some overlap
#' hist(doloSpDist$inter, col="grey")
#' hist(doloSpDist$intra, col=transGreen, add=TRUE)
#' 
#' #Boxplot of the same
#' boxplot(doloSpDist)
#' 
#' @export sppDist
sppDist <-
function(distobj, sppVector){
	distobj <- as.matrix(distobj)
	attr(distobj, "dimnames")[[1]] <- sppVector
	taxa <- unique(sppVector)
	intra <- list()
	inter <- list()
	for(i in 1:length(taxa)){
		for(j in 1:length(taxa)){
			sppMat <- distobj[which(dimnames(distobj)[[1]] == taxa[i]), which(dimnames(distobj)[[1]] == taxa[j])]
			if(taxa[i] == taxa[j]) intra[[length(intra)+1]] <- sppMat[lower.tri(sppMat)] else inter[[length(inter)+1]] <- sppMat
			}
	}
list(inter = unlist(inter), intra = unlist(intra))
}

