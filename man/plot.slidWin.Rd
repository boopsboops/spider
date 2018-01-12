\name{plot.slidWin}
\alias{plot.slidWin}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a 'slidWin' object
}
\description{
Graphical representation of the summary statistics derived from \code{\link{slideAnalyses}} and \code{\link{slideBoxplots}}
}
\usage{
\method{plot}{slidWin}(x, outliers = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An object of class `slidWin'.
}
  \item{outliers}{
Logical. When the results of \code{\link{slideBoxplots}} are being called, should the outliers be plotted? Default of FALSE.
}
\item{...}{
Other arguments to be passed to \code{plot}.
}
}
\details{
When boxplots of methods \code{nonCon} and \code{interAll}, the y-axis limits are constrained to the midpoint of the range covered by the boxplots, so that the intra-specific variation can be seen.
}
\value{
Plots graphs depending on the options given to \code{\link{slideAnalyses}} or \code{\link{slideBoxplots}}.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{slideAnalyses}}, 
\code{\link{slideBoxplots}}.
}
\examples{
data(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

doloSlide <- slideAnalyses(dolomedes,  doloSpp, 200, interval=10, treeMeasures=TRUE)

plot(doloSlide)

doloBox <- slideBoxplots(dolomedes,  doloSpp, 200, interval=10, method="overall")

plot(doloBox)


data(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
    function(x) paste(x[1], x[2], sep="_"))

anoBox <- slideBoxplots(anoteropsis,  anoSpp, 200, interval=10, method="interAll")

plot(anoBox)
plot(anoBox, outliers=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sliding window}
