#' Rosenberg's probability of reciprocal monophyly
#' 
#' This function computes Rosenberg's probability of reciprocal monophyly for
#' each dichotomous node of a phylogenetic tree.
#' 
#' Because \code{ape} plots node labels in a different manner to the method in
#' which they are stored, when plotting the node labels made by
#' \code{rosenberg}, make sure the \code{node} argument is given as shown in
#' the examples below.
#' 
#' @param phy A tree of class `phylo'.
#' @return A numeric vector with names giving the node numbers of \code{phy}.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{nodelabels}}.
#' @references Rosenberg, N. A. (2007). Statistical tests for taxonomic
#' distinctiveness from observations of monophyly. _Evolution_ *61* (2),
#' 317-323.
#' @keywords Sampling
#' @examples
#' 
#' data(anoteropsis)
#' anoTr <- nj(dist.dna(anoteropsis))
#' anoLab <- rosenberg(anoTr)
#' plot(anoTr)
#' nodelabels(round(anoLab,3), node=as.numeric(names(anoLab)))
#' 
#' data(dolomedes)
#' doloTr <- nj(dist.dna(dolomedes))
#' doloRose <- rosenberg(doloTr)
#' plot(doloTr)
#' nodelabels(round(doloRose, 3))
#' 
#' #Colour circles for nodes with a probability < 0.005
#' doloNodes <- doloRose < 0.005
#' doloLabs <- doloRose
#' doloLabs[doloNodes] <- "blue"
#' doloLabs[!doloNodes] <- "red"
#' plot(doloTr, cex=0.7)
#' nodelabels(pch=21, bg=doloLabs, node=as.numeric(names(doloLabs)), cex=2)
#' legend(x=0.015, y=16.13, legend=c("significant", "not significant"), pch=21, 
#'     pt.bg=c("blue", "red"), bty="n", pt.cex=2)
#' 
#' 
#' @export rosenberg
rosenberg <- function(phy){	
	RosenbergP_AB <- function(a, b){
		d1 <- choose(a+b, a)
		d2 <- a+b-1
		n1 <- 2
		n2 <- 1
		(n1/d1)*(n2/d2)
	}
	mat <- polyBalance(phy)
	lab <- apply(mat, 1, function(x) RosenbergP_AB(x[1], x[2]))
	lab
}

