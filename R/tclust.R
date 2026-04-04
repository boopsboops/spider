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
tclust <- function(distobj, threshold = 0.01) {
  dmat <- as.matrix(distobj)
  n <- nrow(dmat)
  
  # adjacency list: neighbors within threshold (excluding self)
  adj <- lapply(seq_len(n), function(i) {
    which(dmat[i, ] < threshold & seq_len(n) != i)
  })
  
  visited <- rep(FALSE, n)
  clusters <- list()
  
  for (i in seq_len(n)) {
    if (!visited[i]) {
      # BFS/DFS to get connected component
      stack <- c(i)
      comp <- c()
      
      while (length(stack) > 0) {
        node <- stack[1]
        stack <- stack[-1]
        
        if (!visited[node]) {
          visited[node] <- TRUE
          comp <- c(comp, node)
          stack <- c(stack, adj[[node]])
        }
      }
      
      clusters[[length(clusters) + 1]] <- sort(unique(comp))
    }
  }
  
  return(clusters)
}
