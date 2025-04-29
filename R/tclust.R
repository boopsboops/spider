#' Clustering by a threshold
#' 
#' Identifies clusters, excluding individuals greater than the threshold from
#' any member.
#' 
#' If two individuals are more distant than \code{threshold} from each other,
#' but both within \code{threshold} of a third, all three are contained in a
#' single cluster.
#' 
#' @param distobj A distance object (usually from \code{\link[ape]{dist.dna}}).
#' @param threshold Distance cutoff for clustering. Default of 0.01 (1\%).
#' @return A list with each element giving the index of the individuals
#' contained in each cluster.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link[ape]{dist.dna}}, \code{\link{localMinima}}. %% ~~objects to
#' See Also as \code{\link{help}}, ~~~
#' @keywords Barcoding
#' @examples
#' 
#' 
#' data(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#' 	function(x) paste(x[1], x[2], sep="_"))
#' anoDist <- ape::dist.dna(anoteropsis)
#' 
#' tclust(anoDist)
#' 
#' #Names of individuals
#' anoClust <- tclust(anoDist)
#' lapply(anoClust, function(x) anoSpp[x])
#' 
#' @export tclust
tclust <- function(distobj, threshold = 0.01){
	distobj <- as.matrix(distobj)
	out <- unique(apply(distobj, 2, function(x) which(x < threshold)))
	for(i in 1:length(out)){
	for(j in 1:length(out)){
		if(length(intersect(out[[i]], out[[j]])) > 0) out[[i]] <- union(out[[i]], out[[j]]) else out[[i]] <- out[[i]]
		}
	}
	unique(lapply(out, sort))
}
