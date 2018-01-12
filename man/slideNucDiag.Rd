\name{slideNucDiag}
\alias{slideNucDiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sliding nucleotide diagnostics
}
\description{
Calculates the number of diagnostic nucleotides in sliding windows.
}
\usage{
slideNucDiag(DNAbin, sppVector, width, interval = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
A DNA alignment of class `DNAbin'.
}
  \item{sppVector}{
Species vector (see \code{\link{sppVector}}).
}
  \item{width}{
Desired width of windows in number of base pairs.
}
  \item{interval}{
Distance between each window in number of base pairs. Default of 1. Giving the option of \code{"codons"} sets the size to 3.
}
}

\details{
Determines the number of diagnostic nucleotides for each species in each window.
}

\value{
A matrix giving the number of diagnostic nucleotides for each species (rows) in each window (columns).
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{slideAnalyses}}, \code{\link{slideBoxplots}}, \code{\link{slidingWindow}}.
}
\examples{
data(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

slideNucDiag(dolomedes, doloSpp, 200, interval = 3)

slidND <- slideNucDiag(dolomedes, doloSpp, 200, interval = 3)

#Number of basepairs for each species
matplot(t(slidND), type = "l")

#Number of basepairs for a single species
plot(slidND[4, ], type = "l")

#Total number of basepairs per window
plot(colSums(slidND), type = "l")


}

\keyword{Sliding window}
