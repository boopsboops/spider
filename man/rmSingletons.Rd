\name{rmSingletons}
\alias{rmSingletons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detect and remove singletons
}
\description{
A utility to detect and remove species represented only by singletons.
}
\usage{
rmSingletons(sppVector, exclude = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sppVector}{Vector of species names. (see \code{\link{sppVector}}).}
  \item{exclude}{Logical. Should singletons be removed? Default of TRUE.}
}
\details{
When \code{exclude = TRUE} (the default), singletons are excluded and the vector returns the index of all non-singletons in the dataset. When \code{exclude = FALSE}, the indices of the singletons are presented.
}
\value{
Returns a numeric vector giving the indices of the selected individuals.
}

\author{Samuel Brown <s_d_j_brown@hotmail.com>}

\examples{
data(anoteropsis)
anoDist <- dist.dna(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))

rmSingletons(anoSpp)
rmSingletons(anoSpp, exclude=FALSE)

data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

rmSingletons(doloSpp)
rmSingletons(doloSpp, exclude=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Utilities}
