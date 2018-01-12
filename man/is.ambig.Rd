\name{is.ambig}
\alias{is.ambig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Missing bases in alignments
}

\description{
Checks what columns in an alignment have ambiguous bases or missing data.
}

\usage{
is.ambig(DNAbin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
A DNA alignment of class `DNAbin'.
}
}
\details{
Ambiguous bases are bases that have been coded with any of the Union of Pure and Applied Chemistry (IUPAC) DNA codes that are not A, C, G, or T. Missing data are bases that have been coded with "-", "?" or "N".
}
\value{
A logical vector containing TRUE if ambiguous bases or missing data are present, FALSE if not. Does not differentiate between the two classes of data.
}

\author{Samuel Brown <s_d_j_brown@hotmail.com>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{checkDNA}}
}
\examples{
data(woodmouse)
is.ambig(woodmouse)
#Columns with ambiguous bases
which(is.ambig(woodmouse))
}

\keyword{Utilities}