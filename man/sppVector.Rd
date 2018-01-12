\name{sppVector}
\alias{sppVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species Vectors
}
\description{
A grouping variable that gives an identity to the individuals in various analyses.
}
\details{
Species vectors are the key concept behind a lot of \code{spider}'s functionality. They are the method used to group data from individuals into species. It is important to note that "species" in this context can mean any cluster (real or otherwise) that is of interest. Populations, demes, subspecies and genera could be the taxa segregated by "species vectors".

The two characteristics of a species vector are UNIQUENESS between species and CONSISTENCY within them. R recognises differences of a single character between elements, leading to \code{spider} considering these elements to represent different species.

There is an easy way and a hard way to create species vectors. The hard way is to type out each element in the vector, making sure no typos or alignment errors are made.

The easy way is to add species designations into your data matrix from the beginning in such a way that it is easy to use R's data manipulation tools to create a species vector from the names of your data. See the examples for a few ways to do this.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Functions for creating species vectors:
\code{\link{strsplit}}, \code{\link{substr}}, \code{\link{sapply}}.

Functions that use species vectors:
\code{\link{nearNeighbour}}, \code{\link{monophyly}}, \code{\link{nonConDist}}, \code{\link{nucDiag}}, \code{\link{rmSingletons}}, \code{\link{slideAnalyses}}, \code{\link{slideBoxplots}}, \code{\link{sppDist}}, \code{\link{sppDistMatrix}}, \code{\link{threshOpt}}.
}

\examples{
data(dolomedes)
#Dolomedes species vector
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

data(anoteropsis)
#Anoteropsis species vector
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}

