\name{tajima.K}
\alias{tajima.K}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate Tajima's K index of divergence
}
\description{
Calculates Tajima's K index of divergence.
}
\usage{
tajima.K(DNAbin, prop = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
An object of class `DNAbin'.
}
  \item{prop}{
Logical. Should the function report the number of substitutions per nucleotide? Default of TRUE.
}
}

\value{
A vector of length 1. If \code{prop = FALSE}, the mean number of substitutions between any two sequences is returned. If \code{prop = TRUE} (the default), this number is returned as the mean number of substitutions per nucleotide (i.e. the above divided by the length of the sequences).
}
\references{
Tajima, F. (1983). Evolutionary relationship of DNA sequences in finite
populations. _Genetics_ *105*, 437-460.

}
\author{
Samuel Brown	<s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.dna}}.
}

\examples{
data(anoteropsis)
tajima.K(anoteropsis)
tajima.K(anoteropsis, prop = FALSE)
}

\keyword{Utilities}
