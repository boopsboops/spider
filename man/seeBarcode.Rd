\name{seeBarcode}
\alias{seeBarcode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create illustrative barcodes}

\description{This function plots an illustrative barcode consisting of vertical bands in four colours corresponding to the DNA bases adenine (A), cytosine (C), guanine  (G) and thiamine (T).}

\usage{seeBarcode(seq, col=c("green", "blue", "black", "red"))}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq}{A single sequence of class `DNAbin'.}
  \item{col}{A character vector of length 4 giving colours to represent A, G, C and T respectively.}
}

\details{
Green, blue, black and red are the standard colours representing A, G, C and T respectively.
}

\value{
Plots an illustrative barcode.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

\examples{
layout(matrix(1:6, ncol=1))
par(mar=c(0.5, 0, 0.5, 0))
data(woodmouse)
seeBarcode(woodmouse[1,])
seeBarcode(woodmouse[1,], col=c("pink", "orange", "steelblue", "yellow"))
seeBarcode(woodmouse[1,], col=c("black", "white", "white", "black"))
apply(woodmouse[1:3,], MARGIN=1, FUN=seeBarcode)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
\keyword{Utilities}
