\name{slidingWindow}
\alias{slidingWindow}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Create windows along an alignment}

\description{
Creates windows of a specified width along a DNA alignment.
}

\usage{
slidingWindow(DNAbin, width, interval = 1)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{DNAbin}{A DNA alignment of class `DNAbin'.}
  \item{width}{Width of each window.}
  \item{interval}{Numeric or option of \code{"codons"}. This sets interval between windows. Default of 1. Setting the option to "codons" gives an interval of 3.}
}
\details{
Sliding window analyses are often used to determine the variability along sequences. This can be useful for investigating whether there is evidence for recombination, developing shorter genetic markers, or for determining variation within a gene. 

Analyses can be conducted on each window using \code{\link{lapply}}.

}

\value{A list of `DNAbin' objects, with each alignment being \code{width} bases in length. The list has length of the DNA alignment minus the width. The positions covered by each window can be retreived with \code{attr(x, "window")}.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{lapply}}, 
\code{\link{slideAnalyses}}, 
\code{\link{slideBoxplots}}.
}

\examples{
data(woodmouse)
woodmouse <- woodmouse[,1:20]
win1 <- slidingWindow(woodmouse, width = 10)
length(win1)

win2 <- slidingWindow(woodmouse, width = 10, interval = 2)
length(win2)

win3 <- slidingWindow(woodmouse, width = 10, interval = "codons")
length(win3)

win4 <- slidingWindow(woodmouse, width = 15)
length(win4)
attr(win4[[1]], "window")
attr(win4[[2]], "window")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sliding window}
