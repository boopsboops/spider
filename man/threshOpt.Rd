\name{threshOpt}
\alias{threshOpt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold optimisation
}
\description{
Determines the positive, negative, false positive and false negative rates of identification accuracy for a given threshold.
}
\usage{
threshOpt(distobj, sppVector, threshold)
}

\arguments{
  \item{distobj}{
Distance matrix.
}

\item{sppVector}{
Species vector (see \code{\link{sppVector}}).
}

  \item{threshold}{
Threshold distance for delimiting intra- and inter-specific variation. Default of 0.01.
}
}

\details{
When run over a range of thresholds, this function allows the optimisation of threshold values based on minimising the identification error rates. See the example below for more details.
}

\value{
A table giving the threshold and number of negative and positive identifications, number of false negative and false positive identifications, and the cumulative error.
}
\references{
Meyer, C. P., and Paulay, G. (2005). DNA barcoding: error rates based on
comprehensive sampling. _PLoS Biology_ *3* (12), 2229-2238.

}
\author{
Rupert Collins <rupertcollins@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{localMinima}}.
}
\examples{
data(anoteropsis)
anoDist <- dist.dna(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))
threshOpt(anoDist, anoSpp)

data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
threshOpt(doloDist, doloSpp)

#Conduct the analysis over a range of values to determine the optimum threshold
threshVal <- seq(0.001,0.02, by = 0.001)
opt <- lapply(threshVal, function(x) threshOpt(doloDist, doloSpp, thresh = x))
optMat <- do.call(rbind, opt)
barplot(t(optMat)[4:5,], names.arg=optMat[,1], xlab="Threshold values", 
    ylab="Cumulative error")
legend(x = 2.5, y = 29, legend = c("False positives", "False negatives"), 
    fill = c("grey75", "grey25"))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
