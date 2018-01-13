#' Mean intra- and inter-specific distance matrix
#' 
#' Creates a matrix giving the mean distances within and between species.
#' 
#' 
#' @param distobj A distance matrix.
#' @param sppVector The species vector (see \code{\link{sppVector}}).
#' @return A square matrix with dimensions \code{length(sppVector)}. It
#' contains the mean intra specific distances down the diagonal, and the mean
#' pairwise distance between the species in the triangles. The two triangles
#' are identical.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Barcoding
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- ape::dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' sppDistMatrix(doloDist, doloSpp)
#' 
#' 
#' @importFrom ape dist.dna
#' @export sppDistMatrix
sppDistMatrix <-
function(distobj, sppVector){
	dat <- as.matrix(distobj)
	attr(dat, "dimnames")[[1]] <- sppVector
	taxa <- unique(sppVector)
	pair.mat <- matrix(data = NA, nrow = length(taxa), ncol = length(taxa), dimnames = list(one = taxa,two = taxa))
	for(i in 1:length(taxa)){
		for(j in 1:length(taxa)){
			sppMat <- dat[which(dimnames(dat)[[1]] == taxa[i]), which(dimnames(dat)[[1]] == taxa[j])]
			if(taxa[i] == taxa[j]) pair.mat[i,j] <- mean(sppMat[lower.tri(sppMat)]) else pair.mat[i,j] <- mean(sppMat)
		}
	}
pair.mat
}

