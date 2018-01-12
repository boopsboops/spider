\name{localMinima}
\alias{localMinima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Determine thresholds from a density plot}

\description{This function determines possible thresholds from the distance matrix for an alignment.}

\usage{
localMinima(distobj)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distobj}{A distance object (usually from \code{\link{dist.dna}}).}
}

\details{
This function is based on the concept of the barcoding gap, where a dip in the density of genetic distances indicates the transition between intra- and inter-specific distances. Understanding your data is vital to correctly interpreting the output of this function, but as a start, the first local minimum is often a good place to start.

The value of this function is that it does not require prior knowledge of species identity to get an indication of potential threshold values.
}

\value{
An object of class `density', which is a list containing the values calculated by \code{\link{density}}. The element \code{localMinima} has been added, which contains the values of the local minima of the density plot.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.dna}}, \code{\link{density}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

data(anoteropsis)
anoDist <- dist.dna(anoteropsis)

anoThresh <- localMinima(anoDist)
plot(anoThresh)
anoThresh$localMinima
#Often the first value is the one to go for:
anoThresh$localMinima[1]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
