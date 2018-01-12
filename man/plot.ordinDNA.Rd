\name{plot.ordinDNA}
\alias{plot.ordinDNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an 'ordinDNA' object
}
\description{
Plots an ordination of the Principal Components Analysis conducted by \code{\link{ordinDNA}}.
}
\usage{
\method{plot}{ordinDNA}(x, majorAxes = c(1,2), plotCol = "default",  trans = "CC", 
    textcex = 0.7, pchCentroid = FALSE, sppBounds = "net", sppNames = TRUE, 
    namePos = "top", ptPch = 21, ptCex = 0.5, netWd = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class `ordinDNA'.}
  \item{majorAxes}{Numeric. Gives the numbers of the major axes that should be plotted. Default of the first two major axes (\code{majorAxes = c(1,2)})}
  \item{plotCol}{A vector of RGB colours giving the colours of the points and circles. Must be in the form of a character vector with elements "#XXXXXX" where XXXXXX gives the hexadecimal value for the colours desired. Default of \code{"default"}. Colours are recycled if necessary.}
  \item{trans}{A character vector giving the hexadecimal value for the transparency of the circles. Default of "CC".}
  \item{textcex}{Numeric. Controls the size of the text giving the species value of the circles.}
  \item{pchCentroid}{Numeric. Controls the shape of the point showing the centroid of the circle for each species. Default of FALSE, no plotting of centroid position.}
  \item{sppBounds}{Option to determine the method of visualising conspecific points. Options of \code{"net"} (the default), \code{"none"} or \code{"circles"}.}
  \item{sppNames}{Logical. Should species names be plotted? Default of TRUE.}
  \item{namePos}{Character vector of length 1 giving the position where the species names should be plotted. Possible values are: "top" and "bottom", anything else plots the names at the centroid.}
  \item{ptPch}{Numeric. Number of the symbol to be used for plotting. see \code{\link{points}}. Default of 21.}
  \item{ptCex}{Numeric. Number governing the size of the points. Default of 0.5.}
  \item{netWd}{Numeric. Number governing the width of the lines in the netowk. Default of 1.}
  
   \item{...}{Other arguments to be passed to \code{plot}.}
}

\details{
\code{plot.ordinDNA} calculates the centroid and radius of the most variable individual for each species in the multivariate space of the Principal Components Analysis object given.

\code{majorAxes} plots the axes in the form \code{c(x, y)}. The maximum number of axes calculated is the number of specimens in the dataset minus one.

\code{sppBounds} has the following options: \code{"net"} (the default) creates a complete graph between all individuals within a species.  If \code{"circles"} is specified, a circle is drawn with a center fixed on the centroid, and a radius of the length to the maximally distant individual. Selecting the option of \code{"none"} means the individuals are not connected in any way.
}

\value{
Plots an ordination of the first two major axes showing the positions of each individual (squares), the centroid of each species (circular bullet and name of species), and the variation in the species (large circle, the radius of which is the distance to the furthest individual from the centroid).

}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ordinDNA}}, \code{\link{cgraph}}.
}

\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloOrd <- ordinDNA(doloDist, doloSpp)

plot(doloOrd)
plot(doloOrd, majorAxes = c(1,3))
plot(doloOrd, textcex = 0.001)
plot(doloOrd, plotCol = c("#FF0000", "#00FF00", "#0000FF"))
plot(doloOrd, namesPos = "bottom")
plot(doloOrd, namesPos = "centre")

data(anoteropsis)
anoDist <- dist.dna(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))
    
anoOrd <- ordinDNA(anoDist, anoSpp)

plot(anoOrd, sppBounds = "circles")


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Visualisation}
