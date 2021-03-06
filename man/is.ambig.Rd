% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.ambig.R
\name{is.ambig}
\alias{is.ambig}
\title{Missing bases in alignments}
\usage{
is.ambig(DNAbin)
}
\arguments{
\item{DNAbin}{A DNA alignment of class `DNAbin'.}
}
\value{
A logical vector containing TRUE if ambiguous bases or missing data
are present, FALSE if not. Does not differentiate between the two classes of
data.
}
\description{
Checks what columns in an alignment have ambiguous bases or missing data.
}
\details{
Ambiguous bases are bases that have been coded with any of the Union of Pure
and Applied Chemistry (IUPAC) DNA codes that are not A, C, G, or T. Missing
data are bases that have been coded with "-", "?" or "N".
}
\examples{

data(woodmouse)
is.ambig(woodmouse)
#Columns with ambiguous bases
which(is.ambig(woodmouse))

}
\seealso{
\code{\link{checkDNA}}
}
\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}
\keyword{Utilities}
