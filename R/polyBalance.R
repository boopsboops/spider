#' Balance of a phylogenetic tree with polytomies
#' 
#' This function computes the numbers of descendants for each dichotomous
#' branch of a phylogenetic tree.
#' 
#' The function extends \code{\link{balance}} to allow the balance of a tree
#' with polytomies to be calculated. When the tree is fully dichotomous, the
#' result is identical to \code{\link{balance}}.
#' 
#' @param phy A tree of class `phylo'.
#' @return A numeric matrix with two columns and one row for each node of the
#' tree. The columns give the numbers of descendants on each node.
#' Non-dichotomous nodes are reported as 'NA'.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{balance}}.
#' @keywords Utilities
#' @examples
#' 
#' 	set.seed(55)
#' 	tr <- rtree(15)
#' 	tr2 <- di2multi(tr, tol=0.02)
#' 	polyBalance(tr)
#' 	polyBalance(tr2)
#' 
#' @export polyBalance
polyBalance <- function(phy){
	nd <- node.depth(phy)
	tab <- table(phy$edge[,1])
	ind <- as.numeric(names(tab[which(tab == 2)]))
	tips <- nd[phy$edge[phy$edge[, 1] %in% ind, 2]]
	nodes <- phy$edge[phy$edge[, 1] %in% ind, 1]
	mat <- cbind(nodes, tips)
	NNodes <- unique(phy$edge[,1])
	matInd <- unique(mat[,1])
	outMat <- matrix(NA, ncol = 2, nrow = length(NNodes))
	dimnames(outMat)[[1]] <- NNodes
	for(i in 1:length(matInd)){
		outMat[NNodes == matInd[i],] <- mat[mat[,1] == matInd[i],2]
	}
	
	outMat
}
