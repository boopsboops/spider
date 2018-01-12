\name{seqStat}
\alias{seqStat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sequence statistics
}
\description{
Utility that produces a table giving summary statistics for a `DNAbin' object.
}
\usage{
seqStat(DNAbin, thresh)
}

\arguments{
  \item{DNAbin}{
Alignment of class `DNAbin'.
}
  \item{thresh}{
Threshold sequence length. Default of 500 (minimum length for official DNA barcodes).
}
}

\details{
This function considers bases coded as '?', 'N' and '-' as missing data.
}

\value{
A table giving the minimum, maximum, mean and median sequence lengths, and the number of sequences with lengths below the given threshold.
}

\author{
Rupert Collins <rupertcollins@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(anoteropsis)
seqStat(anoteropsis)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
\keyword{Utilities}