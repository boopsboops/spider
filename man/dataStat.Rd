\name{dataStat}
\alias{dataStat}

\title{
Taxa statistics
}

\description{
Returns the numbers of species, genera and individuals in the dataset.
}

\usage{dataStat(sppVector, genVector, thresh)
}

\arguments{
  \item{sppVector}{
Species vector (see \code{\link{sppVector}}).
}

\item{genVector}{
Genus vector that defines the genera of each individual, created in a similar manner to the species vector.
}

  \item{thresh}{
Threshold for adequate individual/species number. Default of 5.
}
}

\details{
The value \code{NULL} can be passed to \code{gen} if genera are not of interest in the dataset.
}

\value{
A table giving the number of genera and species in the dataset; giving the minimum, maximum, mean and median number of individuals per species, and the number of species below the given threshold.
}

\author{
Rupert Collins <rupertcollins@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(anoteropsis)
#Species vector
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))
#Genus vector
anoGen <-  sapply(strsplit(anoSpp, split="_"), function(x) x[1])
dataStat(anoSpp, anoGen)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
\keyword{Utilities}