\name{sppDistMatrix}
\alias{sppDistMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean intra- and inter-specific distance matrix
}
\description{
Creates a matrix giving the mean distances within and between species.
}
\usage{
sppDistMatrix(distobj, sppVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distobj}{
A distance matrix.
}
  \item{sppVector}{
The species vector (see \code{\link{sppVector}}).
}
}

\value{
A square matrix with dimensions \code{length(sppVector)}. It contains the mean intra specific distances down the diagonal, and the mean pairwise distance between the species in the triangles. The two triangles are identical. 
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

sppDistMatrix(doloDist, doloSpp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
