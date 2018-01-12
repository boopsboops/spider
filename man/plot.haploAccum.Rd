\name{plot.haploAccum}
\alias{plot.haploAccum}

\title{Plotting haplotype accumulation curves}

\description{
Plots the accumulation curves calculated by \code{\link{haploAccum}}.
}


\usage{
\method{plot}{haploAccum}(x, add = FALSE, ci = 2, ci.type = c("bar","line","polygon"), 
    col = par("fg"), ci.col = col, ci.lty = 1, xlab, ylab = "Haplotypes", ylim,  
    main = paste(x$method, "method of haplotype accumulation", sep=" "), ...)
}
    
\arguments{
  \item{x}{A `haploAccum' object obtained from \code{\link{haploAccum}}.}
  \item{add}{Add graph to an existing graph.}
  \item{ci}{Multiplier for the calculation of confidence intervals from standard deviation. \code{ci = 0} prevents the drawing of confidence intervals.}
  \item{ci.type}{Type of confidence intervals: \code{"bar"} for vertical bars, \code{"line"} for lines, and \code{"polygon"} for a shaded area.}
  \item{col}{Colour for curve line.}
  \item{ci.col}{Colour for lines or shaded area when \code{"polygon"}.}
  \item{ci.lty}{Line type for confidence interval lines or border of the \code{"polygon"}.}
  \item{xlab}{Label for the X-axis.}
  \item{ylab}{Label for the Y-axis.}
  \item{ylim}{Y-axis limits.}
  \item{main}{Title of the plot.}
  \item{...}{Other parameters to pass to plot.}
}

\value{
Plots a haplotype accumulation curve and confidence intervals depending on the options given to \code{\link{haploAccum}}.
}

\references{
  Gotellli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in measurement and comparison of species richness. _Ecology Letters_ *4* 379--391.
}

\author{
Jagoba Malumbres-Olarte <j.malumbres.olarte@gmail.com>.
}

\examples{
data(dolomedes)
#Generate multiple haplotypes
doloHaplo <- dolomedes[sample(37, size = 200, replace = TRUE), ] 
dolocurv <- haploAccum(doloHaplo, method = "random", permutations = 100)

plot(dolocurv)
plot(dolocurv, add = FALSE, ci = 2, ci.type = "polygon", col = "blue", ci.col = "red", 
    ci.lty = 1)
}

\keyword{Sampling}