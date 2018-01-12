\name{tree.comp}
\alias{tree.comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tree comparisons
}
\description{
Compares the clades between two trees.
}

\usage{
tree.comp(phy1, phy2, method = "prop")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy1, phy2}{
Trees of class `phylo' to compare. 
}
  \item{method}{
One of the following options:
	\itemize{
		\item \code{"prop"}---returns the proportion of clades that are the same between the two trees
		\item \code{"shallow"}---returns the proportion of shallow clades (clades where \code{node.depth} < median \code{node.depth}) that are the same between the two trees default of \code{"prop"}.
		\item \code{"PH85"}---returns the topological distance of Penny and Hendy (1985).
		}
}
}
\details{
This function is a modification of the \code{\link{dist.topo}} function in \code{ape} to give similarity between the two trees as a proportion, and to account for the unreliable resolution of deeper nodes that affect some methods of tree construction (such as NJ).

It is important that the tip labels of the two trees are the same. If the tip labels are different between the two trees, the method will not recognise any similarity between them.

This function does not take into account differences in branch length. The \code{"score"} method in \code{\link{dist.topo}} does this if desired.
}
\value{
Numeric vector of length 1. 

If \code{method = "prop"}, the number returned is the proportion of nodes in the first tree for which there is a node in the second that contains the same tips. Higher number represents greater similarity. If it is 1, the trees are identical. If 0, the trees have no similarity whatsoever. 

When \code{method = "shallow"}, only those nodes tipwards of the median node depth are taken into account. This will not be useful for small trees, but may be helpful with larger datasets.

\code{"PH85"} is the Penny and Hendy (1985) distance. This measure is the default of \code{\link{dist.topo}}. In this measure, the smaller the number, the closer the trees are. If the trees are identical, this results in 0.

}
\references{
Penny, D. and Hendy, M. D. (1985) The use of tree comparison metrics. _Systematic Zoology_ *34* 75-82.

}
\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{node.depth}}, \code{\link{dist.topo}}.
}

\examples{
set.seed(15)
tr <- rtree(15)
set.seed(22)
tr2 <- rtree(15)
tree.comp(tr, tr2)
tree.comp(tr, tr2, method="PH85")
tree.comp(tr, tr2, method="shallow")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
