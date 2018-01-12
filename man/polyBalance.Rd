\name{polyBalance}
\alias{polyBalance}

\title{Balance of a phylogenetic tree with polytomies}

\description{This function computes the  numbers of descendants for each dichotomous branch of a phylogenetic tree.}

\usage{polyBalance(phy)}

\arguments{
  \item{phy}{A tree of class `phylo'.}
}

\details{
The function extends \code{\link{balance}} to allow the balance of a tree with polytomies to be calculated. When the tree is fully dichotomous, the result is identical to \code{\link{balance}}.

}

\value{
A numeric matrix with two columns and one row for each node of the tree. The columns give the numbers of descendants on each node. Non-dichotomous nodes are reported as 'NA'.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{balance}}.
}

\examples{
	set.seed(55)
	tr <- rtree(15)
	tr2 <- di2multi(tr, tol=0.02)
	polyBalance(tr)
	polyBalance(tr2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
