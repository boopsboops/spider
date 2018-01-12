\name{tiporder}
\alias{tiporder}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Orders tip labels by their position on the tree.
}

\description{
Provides an ordered vector of tip labels, corresponding to their position on the tree.
}

\usage{
tiporder(phy, labels = TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phy}{A tree of class `phylo'.}
  \item{labels}{Logical. Should labels be printed? If FALSE, the indices are given. Default of TRUE.}
}

\value{
A character or numeric vector giving the names of the tip in the order of their position on the tree. The order is that from top to bottom when the tree is plotted with \code{direction = "rightwards"}.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(anoteropsis)
anoTree <- nj(dist.dna(anoteropsis))
tiporder(anoTree)
tiporder(anoTree, labels = FALSE)


data(woodmouse)
woodTree <- nj(dist.dna(woodmouse))
tiporder(woodTree)
tiporder(ladderize(woodTree))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
