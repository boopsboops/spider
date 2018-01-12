\name{ordinDNA}
\alias{ordinDNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculates a Principal Components Ordination of genetic distances}

\description{Calculates Principical Coonrdinates Analysis on a matrix of genetic distances and plots an ordination of the first two major axes.}

\usage{
ordinDNA(distobj, sppVector, ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{distobj}{A distance matrix.}
	\item{sppVector}{The species vector (see \code{\link{sppVector}}).}
	\item{...}{Other arguments to be passed to \code{\link{plot.ordinDNA}}.}
}

\details{
This function is a wrapper for \code{\link{cmdscale}}, which performs a Principal Coordinates Analysis on the distance matrix given. In addition, it plots an ordination of the genetic distance matrix given, showing the relative distance between each of the species in the dataset. It is presented as an alternative to the neighbour-joining trees which are frequently used for the visualisation of DNA barcoding data. NJ trees show hypotheses of relationships, which are inappropriate for the questions usally asked in DNA barcoding studies.

The distance between the centroids of the clusters are roughly proportional to the genetic distances between the species. NOTE: it is important to remember that the plot shows only one plane of a multi-dimensional space. Species with overlapping circles are not necessarily conspecific. Further exploration is required.
}

\value{
Plots an ordination of the first two major axes showing the positions of each individual (squares), the centroid of each species (circular bullet and name of species), and the variation in the species (large circle, the radius of which is the distance to the furthest individual from the centroid).

Additionally returns a list of class \code{"ordinDNA"} with the following elements:
\item{pco}{Output of the Principal Coordinates Analysis.}
\item{sppVector}{Character vector giving the species vector.}

}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cmdscale}}, \code{\link{plot.ordinDNA}}
}

\examples{

data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloOrd <- ordinDNA(doloDist, doloSpp)
doloOrd
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
