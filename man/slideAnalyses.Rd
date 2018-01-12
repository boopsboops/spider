\name{slideAnalyses}
\alias{slideAnalyses}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sliding window analyses
}
\description{
Wraps a number of measures used in sliding window analyses into one easy-to-use function.
}
\usage{
slideAnalyses(DNAbin, sppVector, width, interval = 1, 
    distMeasures = TRUE, treeMeasures = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
A DNA alignment of class `DNAbin'.
}
  \item{sppVector}{
Species vector (see \code{\link{sppVector}}).
}
  \item{width}{
Desired width of windows in number of nucleotides.
}
  \item{interval}{
Distance between each window in number of nucleotides. Default of 1. Giving the option of 'codons' sets the size to 3.
}
  \item{distMeasures}{
Logical. Should distance measures be calculated? Default of TRUE.
}
  \item{treeMeasures}{
Logical. Should tree-based measures be calculated? Default of FALSE.
}
}

\details{
Distance measures include the following:
proportion of zero non-conspecific distances,
number of diagnostic nucleotides,
number of zero-length distances,
 and overall mean distance.

Tree-based measures include the following: 
proportion of species that are monophyletic,
proportion of clades that are identical between the neighbour joining tree calculated for the window and the tree calculated for the full dataset,
 and the latter with \code{method="shallow"}.

Tree-based measures are a lot more time-intensive than distance measures. When dealing with lots of taxa and short windows, this part of the function can take hours.

Both distance and tree measures are calculated from a K2P distance matrix created from the data with the option \code{pairwise.deletion = TRUE}. When sequences with missing data are compared with other sequences, a \code{NA} distance results. These are ignored in the calculation of \code{slideAnalyses} distance metrics. However, the tree measures cannot cope with this missing data, and so no result is returned for windows where some sequences solely contain missing data.
}

\value{
An object of class 'slidWin' which is a list containing the following elements:
\item{win_mono_out}{Proportion of species that are monophyletic.}
\item{comp_out}{Proportion of clades that are identical between the NJ tree calculated for the window and the tree calculated for the full dataset.}
\item{comp_depth_out}{Proportion of shallow clades that are identical.}
\item{pos_tr_out}{Index of window position for tree-based analyses.}
\item{noncon_out}{Proportion of zero non-conspecific distances.}
\item{nd_out}{The sum of diagnostic nucleotides for each species.}
\item{zero_out}{The number of zero-length distances.}
\item{dist_mean_out}{Overall mean K2P distance of each window.}
\item{pos_out}{Index of window position.}
\item{dat_zero_out}{Number of zero inter-specific distances in the full dataset.}
\item{boxplot_out}{Always FALSE. Required for \code{\link{plot.slidWin}}.}
\item{distMeasures}{Value of argument. Required for \code{\link{plot.slidWin}}.}
\item{treeMeasures}{Value of argument. Required for \code{\link{plot.slidWin}}.}

}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dist.dna}}, \code{\link{plot.slidWin}}, \code{\link{rankSlidWin}}, \code{\link{slideNucDiag}}.
}
\examples{
\dontrun{
data(dolomedes)
doloDist <- dist.dna(dolomedes)
doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)

slideAnalyses(dolomedes, doloSpp, 200, interval=10, treeMeasures=TRUE)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Sliding window}
