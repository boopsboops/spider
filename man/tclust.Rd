\name{tclust}
\alias{tclust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Clustering by a threshold}

\description{Identifies clusters, excluding individuals greater than the threshold from any member.}

\usage{
tclust(distobj, threshold = 0.01)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distobj}{A distance object (usually from \code{\link{dist.dna}}).}
  \item{threshold}{Distance cutoff for clustering. Default of 0.01 (1\%).}
}

\details{
If two individuals are more distant than \code{threshold} from each other, but both within \code{threshold} of a third, all three are contained in a single cluster.
}

\value{
A list with each element giving the index of the individuals contained in each cluster. 
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.dna}}, \code{\link{localMinima}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{

data(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
	function(x) paste(x[1], x[2], sep="_"))
anoDist <- dist.dna(anoteropsis)

tclust(anoDist)

#Names of individuals
anoClust <- tclust(anoDist)
lapply(anoClust, function(x) anoSpp[x])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
