\name{slideBoxplots}
\alias{slideBoxplots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Boxplots across windows
}
\description{
Calculates boxplots of genetic distances using sliding windows.
}
\usage{
slideBoxplots(DNAbin, sppVector, width, interval = 1, method = "nonCon")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
A DNA alignment of class `DNAbin'.
}
  \item{sppVector}{
A species vector (see \code{\link{sppVector}}).
}
  \item{width}{
Width of windows.
}
  \item{interval}{
Distance between each window in number of base pairs. Default of 1. Giving the option of \code{"codons"} sets the size to 3.
}
  \item{method}{
Options of \code{"overall", "interAll"} or \code{"nonCon"} (the default).
}
}
\details{
Giving \code{method="overall"} calculates the boxplot for the distance matrix of each window.

Giving \code{method="interAll"} calculates boxplots for the inter- and intra-specific distances of each window, showing the result for ALL inter-specific distances.

Giving \code{method="nonCon"} calculates boxplots for the inter- and intra-specific distances of each window, showing the result for only the nearest-conspecific distances for each individual.
}
\value{
A list with 

 \item{treeMeasures}{Logical. Tree measures calculated? Always FALSE.}
 \item{distMeasures}{Logical. Distance measures calculated? Always FALSE.}
 \item{bp_out}{If \code{method="overall"}, contains the boxplot objects of each window.}
 \item{bp_InterSpp_out}{If \code{method!="overall"}, contains the boxplot objects of the interspecific distances of each window.}
 \item{bp_IntraSpp_out}{If \code{method!="overall"}, contains the boxplot objects of the intraspecific distances of each window.}
 \item{bp_range_out}{range of y-axis values.}
 \item{pos_out}{x-axis values.}
 \item{boxplot_out}{Logical. Boxplots calculated? Always TRUE.}
 \item{method}{The method used for calculating boxplots. \code{"overall", "interAll"} or \code{"nonCon"}.}
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{boxplot}}, \code{\link{plot.slidWin}}, \code{\link{slideAnalyses}}, \code{\link{slidingWindow}}.
}
\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloNonCon <- slideBoxplots(dolomedes, doloSpp, 200, interval=10)
plot(doloNonCon)

doloOverall <- slideBoxplots(dolomedes, doloSpp, 200, interval=10, method="overall")
plot(doloOverall)

doloInterall <- slideBoxplots(dolomedes, doloSpp, 200, interval=10, method="interAll")
plot(doloInterall)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sliding window}
