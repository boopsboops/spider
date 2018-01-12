\name{heatmapSpp}
\alias{heatmapSpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Visualise a distance matrix using a heatmap}

\description{This function plots a heatmap of the distance matrix, with shorter distances indicated by darker colours.}

\usage{heatmapSpp(distObj, sppVector, col = NULL, axisLabels = NULL)}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distObj}{A matrix or object of class \code{dist}.}
  \item{sppVector}{The species vector. See \code{\link{sppVector}}.}
  \item{col}{A vector giving the colours for the heatmap.}
  \item{axisLabels}{A character vector that provides the axis labels for the heatmap. By default the species vector is used.}
}

\details{

The default palette has been taken from the \code{colorspace} package.
}

\value{
Plots a heatmap of the distance matrix. Darker colours indicate shorter distances, lighter colours indicate greater distances.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes, model = "raw")
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
heatmapSpp(doloDist, doloSpp)
heatmapSpp(doloDist, doloSpp, axisLabels = dimnames(dolomedes)[[1]])

data(anoteropsis)
anoDist <- dist.dna(anoteropsis, model = "raw")
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))
heatmapSpp(anoDist, anoSpp)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
