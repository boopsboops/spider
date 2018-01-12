\name{checkDNA}
\alias{checkDNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check a DNA alignment for missing data}

\description{This functions counts the number of bases in an alignment that are composed of missing data.}

\usage{checkDNA(DNAbin, gapsAsMissing = TRUE)}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{A DNA alignment of class `DNAbin'.}
  \item{gapsAsMissing}{Logical. Should gaps (coded as '-') be considered missing bases? Default of TRUE.}
}

\details{
This function considers bases coded as '?' and 'N' as missing data. By default, gaps (coded as '-') are also considered missing.
}

\value{
A numeric vector giving the number of missing bases in each sequence of the alignment.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(anoteropsis)
checkDNA(anoteropsis)
checkDNA(anoteropsis, gapsAsMissing=FALSE)
}

\keyword{Utilities}