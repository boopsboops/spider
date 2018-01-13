#' Orders tip labels by their position on the tree.
#' 
#' Provides an ordered vector of tip labels, corresponding to their position on
#' the tree.
#' 
#' 
#' @param phy A tree of class `phylo'.
#' @param labels Logical. Should labels be printed? If FALSE, the indices are
#' given. Default of TRUE.
#' @return A character or numeric vector giving the names of the tip in the
#' order of their position on the tree. The order is that from top to bottom
#' when the tree is plotted with \code{direction = "rightwards"}.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Utilities
#' @examples
#' 
#' data(anoteropsis)
#' anoTree <- ape::nj(ape::dist.dna(anoteropsis))
#' tiporder(anoTree)
#' tiporder(anoTree, labels = FALSE)
#' 
#' 
#' data(woodmouse)
#' woodTree <- ape::nj(ape::dist.dna(woodmouse))
#' tiporder(woodTree)
#' tiporder(ape::ladderize(woodTree))
#' 
#' @importFrom ape nj
#' @importFrom ape dist.dna
#' @importFrom ape ladderize
#' @export tiporder
tiporder <- function(phy, labels = TRUE){
	nn <- length(phy$tip.label) #How many tips on the tree?
	edge <- phy$edge
	nums <- rev(edge[edge[,2] %in% 1:nn, 2])
	if(labels == TRUE) phy$tip.label[nums] else nums
}
