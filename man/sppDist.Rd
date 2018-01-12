\name{sppDist}
\alias{sppDist}

\title{Intra and inter-specific distances}

\description{
Separates a distance matrix into its inter- and intra-specific components.
}
\usage{
sppDist(distobj, sppVector)
}

\arguments{
  \item{distobj}{
A distance matrix.
}
  \item{sppVector}{
The species vector (see \code{\link{sppVector}}).
}
}
\details{
This function can be used to produce histograms and other charts exploring the `barcode gap', such as in the examples below.
}
\value{
A list with two elements:
\item{inter}{A numeric vector containing ALL inter-specific pairwise distances.}
\item{intra}{A numeric vector containing ALL intra-specific pairwise distances.}
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sppDistMatrix}}.
}
\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloSpDist <- sppDist(doloDist, doloSpp)

doloSpDist

#Histogram of the barcode gap
transGreen <- rgb(0, 1, 0, 0.5) #Make a slightly transparent colour to see some overlap
hist(doloSpDist$inter, col="grey")
hist(doloSpDist$intra, col=transGreen, add=TRUE)

#Boxplot of the same
boxplot(doloSpDist)
}

\keyword{Barcoding}
