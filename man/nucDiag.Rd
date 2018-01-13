\name{nucDiag}
\alias{nucDiag}
\alias{rnucDiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nucleotide diagnostics for species alignments
}
\description{
Determines the diagnostic nucleotides for each species given in \code{sppVector}.
}

\usage{
nucDiag(DNAbin, sppVector)
rnucDiag(DNAbin, sppVector, n = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
An object of class 'DNAbin'.
}
  \item{sppVector}{
The species vector (see \code{\link{sppVector}}).
}
  \item{n}{
The number of pseudoreplicates to perform. Default of 100
}
}

\details{
These functions provide a means for evaluating the presence of diagnostic nucleotides that distinguish species within an alignment. \code{nucDiag} returns the positions of bases corresponding to the definition of pure, simple diagnostic nucleotides given by Sarkar et al (2008).

\code{rnucDiag} runs a bootstrapping-style resampling test to evaluate the numbers of diagnostic nucleotides that might be expected by random assortment of specimens.

}

\value{
\code{nucDiag} returns a list giving the pure, simple diagnostic nucleotides (i.e. those nucleotides that are fixed within species and different from all other species) for each species in the species vector. A result of \code{integer(0)} indicates there are no diagnostic nucleotides for those species.

\code{rnucDiag} returns a list containing the following elements:
\item{min}{The minimum number of diagnostic nucleotides in the sample.}
\item{mean}{The mean number of diagnostic nucleotides in the sample.}
\item{median}{The median number of diagnostic nucleotides in the sample.}
\item{max}{The maximum number of diagnostic nucleotides in the sample.}
\item{rndFreq}{A list of frequency distributions of the number of diagnostic nucleotides in groups formed by 1 sequence, 2 sequences, etc.}

}
\references{
Sarkar, I., Planet, P., & DeSalle, R. (2008). CAOS software for use in character-
based DNA barcoding. _Molecular Ecology Resources_ *8* 1256-1259
}
\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{slideNucDiag}}
}
\examples{
data(anoteropsis)
anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
	function(x) paste(x[1], x[2], sep="_"))

nucDiag(anoteropsis, anoSpp)


#To view the nucleotide values 
anoNuc <- nucDiag(anoteropsis, anoSpp)
as.character(anoteropsis[ ,anoNuc[[1]][1] ])



data(sarkar)
sarkarSpp <- substr(dimnames(sarkar)[[1]], 1, 3)
nucDiag(sarkar, sarkarSpp)

\dontrun{
rnucDiag(anoteropsis, anoSpp, n = 100)
}
}
