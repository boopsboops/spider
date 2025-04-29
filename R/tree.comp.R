#' Tree comparisons
#' 
#' Compares the clades between two trees.
#' 
#' This function is a modification of the \code{\link[ape]{dist.topo}} function in
#' \code{ape} to give similarity between the two trees as a proportion, and to
#' account for the unreliable resolution of deeper nodes that affect some
#' methods of tree construction (such as NJ).
#' 
#' It is important that the tip labels of the two trees are the same. If the
#' tip labels are different between the two trees, the method will not
#' recognise any similarity between them.
#' 
#' This function does not take into account differences in branch length. The
#' \code{"score"} method in \code{\link[ape]{dist.topo}} does this if desired.
#' 
#' @param phy1,phy2 Trees of class `phylo' to compare.
#' @param method One of the following options: \itemize{ \item
#' \code{"prop"}---returns the proportion of clades that are the same between
#' the two trees \item \code{"shallow"}---returns the proportion of shallow
#' clades (clades where \code{node.depth} < median \code{node.depth}) that are
#' the same between the two trees default of \code{"prop"}.  \item
#' \code{"PH85"}---returns the topological distance of Penny and Hendy (1985).
#' }
#' @return Numeric vector of length 1.
#' 
#' If \code{method = "prop"}, the number returned is the proportion of nodes in
#' the first tree for which there is a node in the second that contains the
#' same tips. Higher number represents greater similarity. If it is 1, the
#' trees are identical. If 0, the trees have no similarity whatsoever.
#' 
#' When \code{method = "shallow"}, only those nodes tipwards of the median node
#' depth are taken into account. This will not be useful for small trees, but
#' may be helpful with larger datasets.
#' 
#' \code{"PH85"} is the Penny and Hendy (1985) distance. This measure is the
#' default of \code{\link[ape]{dist.topo}}. In this measure, the smaller the number,
#' the closer the trees are. If the trees are identical, this results in 0.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link[ape]{node.depth}}, \code{\link[ape]{dist.topo}}.
#' @references Penny, D. and Hendy, M. D. (1985) The use of tree comparison
#' metrics. _Systematic Zoology_ *34* 75-82.
#' @keywords Utilities
#' @examples
#' 
#' set.seed(15)
#' tr <- ape::rtree(15)
#' set.seed(22)
#' tr2 <- ape::rtree(15)
#' tree.comp(tr, tr2)
#' tree.comp(tr, tr2, method="PH85")
#' tree.comp(tr, tr2, method="shallow")
#' 
#' @importFrom ape unroot
#' @importFrom ape rtree
#' @importFrom ape prop.part
#' @importFrom ape node.depth
#' @importFrom stats median
#' @export tree.comp
tree.comp <-
function (phy1, phy2, method = "prop") {
	phy1 <- unroot(phy1)
	phy2 <- unroot(phy2)
	nphy1 <- length(phy1$tip.label)
	bp1 <- prop.part(phy1)
	bp1 <- lapply(bp1, function(xx) sort(phy1$tip.label[xx]))
	nphy2 <- length(phy2$tip.label)
	bp2.tmp <- prop.part(phy2)
	bp2 <- lapply(bp2.tmp, function(xx) sort(phy2$tip.label[xx]))
	bp2.comp <- lapply(bp2.tmp, function(xx) setdiff(1:nphy2, xx))
	bp2.comp <- lapply(bp2.comp, function(xx) sort(phy2$tip.label[xx]))
	q1 <- length(bp1)
	q2 <- length(bp2)
	p <- 0
        for (i in 1:q1) {
            for (j in 1:q2) {
                if (identical(bp1[[i]], bp2[[j]]) | identical(bp1[[i]], 
                  bp2.comp[[j]])) {
                  p <- p + 1
                  break
                }
            }
	}
	if(method == "shallow"){
	    shallow <- which(node.depth(phy1)[node.depth(phy1) > 1] <= median(node.depth(phy1)[node.depth(phy1) > 1]))
	    p2 <- 0
		for (i in shallow) {
		    for (j in 1:q2) {
			if (identical(bp1[[i]], bp2[[j]]) | identical(bp1[[i]], 
			  bp2.comp[[j]])) {
			  p2 <- p2 + 1
			  break
			}
		    }
		}
	dT <- p2/length(shallow)
	}
	if(method == "PH85") dT <- q1 + q2 - 2 * p
	if(method =="prop") dT <- p/q1
dT
}

