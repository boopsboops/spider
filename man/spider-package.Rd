\name{spider-package}
\alias{spider-package}
\alias{spider}
\docType{package}
\title{
Species Identity and Evolution in R
}
\description{
Spider: SPecies IDentity and Evolution in R, is an R package implementing a number of useful analyses for DNA barcoding studies and associated research into species delimitation and speciation. Included are functions for generating summary statistics from DNA barcode data, assessing specimen identification efficacy, and for testing and optimising divergence threshold limits. In terms of investigating evolutionary and taxonomic questions, techniques for sliding window, population aggregate, and nucleotide diagnostic analyses are also provided. 

The complete list of functions can be displayed with \code{library(help=spider)}.

More information, including a tutorial on the use of spider can be found at \code{http://spider.r-forge.r-project.org}.
}

\details{
\tabular{ll}{
Package: \tab spider\cr
Type: \tab Package\cr
Version: \tab 1.3-0\cr
Date: \tab 2013-12-25\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
}

A few of the key functions provided by spider: 

DNA barcoding:
\code{\link{bestCloseMatch}}, \code{\link{nearNeighbour}}, \code{\link{threshID}}, \code{\link{threshOpt}}.

Sliding window:
\code{\link{slidingWindow}}, \code{\link{slideAnalyses}}, \code{\link{slideBoxplots}}.

Nucleotide diagnostics:
\code{\link{nucDiag}}.

Morphological techniques:
\code{\link{paa}}.
}
\author{
Samuel Brown, Rupert Collins, Stephane Boyer, Marie-Caroline Lefort, Jagoba Malumbres-Olarte, Cor Vink, Rob Cruickshank

Maintainer:
Samuel Brown <s_d_j_brown@hotmail.com>
}
\references{
Brown S. D. J., Collins R. A., Boyer S., Lefort M.-C., Malumbres-Olarte J., Vink C. J., & Cruickshank R. H. In Press. SPIDER: an R package for the analysis of species identity and evolution, with particular reference to DNA barcoding. _Molecular Ecology Resources_
}

%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{ package }
\seealso{

\code{\link{ape-package}}, 
\code{\link{pegas-package}}.
}
