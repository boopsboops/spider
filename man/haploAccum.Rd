\name{haploAccum}
\alias{haploAccum}

\title{Haplotype accumulation curves}

\description{
\code{haploAccum} identifies the different haplotypes represented in a set of DNA sequences and performs the calculations for plotting haplotype accumulations curves (see \code{\link{plot.haploAccum}}).
}

\usage{haploAccum(DNAbin, method = "random", permutations = 100, ...)
}
    
\arguments{
  \item{DNAbin}{A set of DNA sequences in an object of class `DNAbin'.}
  \item{method}{Method for haplotype accumulation. Method \code{"collector"} enters the sequences in the order that they appear in the sequence alignment and \code{"random"} adds the sequences in a random order.}
  \item{permutations}{Number of permutations for method \code{"random"}.}
  \item{...}{Other parameters to functions.}
}

\details{
Haplotype accumulation curves can be used to assess haplotype diversity in an area or compare different populations, or to evaluate sampling effort. \code{``random''} calculates the mean accumulated number of haplotypes and its standard deviation through random permutations (subsampling of sequences), similar to the method to produce rarefaction curves (Gotelli and Colwell 2001).
}

\value{
An object of class `haploAccum' with items:
  \item{call}{Function call.}
  \item{method}{Method for accumulation.}
  \item{sequences}{Number of analysed sequences.}
  \item{n.haplotypes}{Accumulated number of haplotypes corresponding to each number of sequences.}                                      
  \item{sd}{The standard deviation of the haplotype accumulation curve. Estimated through permutations for \code{method = "random"} and \code{NULL} for \code{method = "collector"}.}
  \item{perm}{Results of the permutations for \code{method = "random"}.}
}

\references{
  Gotellli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity: procedures and pitfalls in measurement and comparison of species richness. _Ecology Letters_ *4*, 379--391.
}

\author{
Jagoba Malumbres-Olarte <j.malumbres.olarte@gmail.com>.
}

\note{
This function is based on the functions \code{haplotype} (E. Paradis) from the package 'pegas' and \code{specaccum} (R. Kindt) from the package'vegan'. Missing or ambiguous data will be detected and indicated by a warning, as they may cause an overestimation of the number of haplotypes.
}

\examples{
data(dolomedes)
#Generate multiple haplotypes
doloHaplo <- dolomedes[sample(37, size = 200, replace = TRUE), ] 
dolocurv <- haploAccum(doloHaplo, method = "random", permutations = 100)
dolocurv
plot(dolocurv)
}

\keyword{Sampling}