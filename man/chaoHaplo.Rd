\name{chaoHaplo}
\alias{chaoHaplo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chao estimator of haplotype number
}
\description{
Calculates the Chao1 estimate of the number of haplotypes in a population based on the total number of haplotypes present, and the number of singletons and doubletons in the dataset.
}
\usage{
chaoHaplo(DNAbin)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
An object of class `DNAbin'.
}
}
\details{
The function assumes a large number of specimens have been sampled and that duplicate haplotypes have not been removed. Interpretation becomes difficult when more than one species is included in the dataset.
}
\value{
An vector of length three, giving the estimated total number of haplotypes in the population, and lower and upper 95\% confidence limits.
}
\references{
Vink, C. J., McNeill, M. R., Winder, L. M., Kean, J. M., and Phillips, C. B. (2011). PCR analyses of gut contents of pasture arthropods. In: Paddock to PCR: Demystifying Molecular Technologies for Practical Plant Protection (eds. Ridgway, H. J., Glare, T. R., Wakelin, S. A., O'Callaghan, M.), pp. 125-134. New Zealand Plant Protection Society, Lincoln.

Chao, A. (1989). Estimating population size for sparse data in capture-recapture experimnets. _Biometrics_ *45* 427-438.
}
\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{haploAccum}}
}
\examples{
data(dolomedes)
#Create dataset with multiple copies of Dolomedes haplotypes
doloSamp <- dolomedes[sample(16, 100, replace=TRUE, prob=c(0.85, rep(0.01, 15))), ]

chaoHaplo(doloSamp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Barcoding}
