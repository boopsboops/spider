\name{rankSlidWin}
\alias{rankSlidWin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rank a 'slidWin' object.
}
\description{
Display the highest ranking windows measured by \code{\link{slideAnalyses}}.
}
\usage{
rankSlidWin(slidWin, criteria="mean_distance", num = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{slidWin}{
An object of class `slidWin', made using \code{\link{slideAnalyses}}.
}
  \item{criteria}{
Name of criteria to sort by. Can be any of the following: \code{"mean_distance", "monophyly", "clade_comparison", "clade_comp_shallow", "zero_noncon", "zero_distances", "diag_nuc"} or \code{"all"}. Default of \code{"mean_distance"} if distance measures have been calculated, otherwise \code{"monophyly"}.
}
  \item{num}{
Number of windows to return. Default of 10. 
}
}
\details{
The criteria for \code{rankSlidWin} correspond to the variables outputted by \code{\link{slideAnalyses}} and are sorted in the following manner:
\tabular{lll}{
\code{rankSlidWin} criterion: \tab \code{\link{slideAnalyses}} output:\tab Sorting method:\cr
\code{"mean_distance"} \tab \code{"dist_mean_out"} \tab Ascending\cr
\code{"monophyly"} \tab \code{"win_mono_out"} \tab Ascending\cr
\code{"clade_comparison"} \tab \code{"comp_out"} \tab Ascending\cr
\code{"clade_comp_shallow"} \tab \code{"comp_depth_out"} \tab Ascending\cr
\code{"zero_noncon"} \tab \code{"noncon_out"} \tab Descending\cr
\code{"zero_distances"} \tab \code{"zero_out"} \tab Descending\cr
\code{"diag_nuc"} \tab \code{"nd_out"} \tab Ascending\cr
}

Given a sequence of 1:10, the ascending method of sorting considers 10 as high. The descending method considers 1 as high.

The \code{"all"} criterion returns the windows that have the highest cumulative total score over all criteria.
}

\value{
A data frame giving the values of the measures calculated by \code{\link{slideAnalyses}}, ranked to show the top 10 positions based on the criterion given.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{slideAnalyses}}.
}
\examples{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloSlide <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, treeMeasures = TRUE)

rankSlidWin(doloSlide)
rankSlidWin(doloSlide, criteria = "zero_distances")

doloSlide2 <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, treeMeasures = FALSE)
rankSlidWin(doloSlide2)

doloSlide3 <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, distMeasures = FALSE, 
    treeMeasures = TRUE)
rankSlidWin(doloSlide3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sliding window}
