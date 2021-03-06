% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmSingletons.R
\name{rmSingletons}
\alias{rmSingletons}
\title{Detect and remove singletons}
\usage{
rmSingletons(sppVector, exclude = TRUE)
}
\arguments{
\item{sppVector}{Vector of species names. (see \code{\link{sppVector}}).}

\item{exclude}{Logical. Should singletons be removed? Default of TRUE.}
}
\value{
Returns a numeric vector giving the indices of the selected
individuals.
}
\description{
A utility to detect and remove species represented only by singletons.
}
\details{
When \code{exclude = TRUE} (the default), singletons are excluded and the
vector returns the index of all non-singletons in the dataset. When
\code{exclude = FALSE}, the indices of the singletons are presented.
}
\examples{

data(anoteropsis)
anoDist <- ape::dist.dna(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))

rmSingletons(anoSpp)
rmSingletons(anoSpp, exclude=FALSE)

data(dolomedes)
doloDist <- ape::dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

rmSingletons(doloSpp)
rmSingletons(doloSpp, exclude=FALSE)
}
\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}
\keyword{Utilities}
