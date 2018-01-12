\name{nucDiag}
\alias{nucDiag}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nucleotide diagnostics for species alignments
}
\description{
Determines the diagnostic nucleotides for each species given in \code{sppVector}.
}

\usage{
nucDiag(DNAbin, sppVector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DNAbin}{
An object of class 'DNAbin'.
}
  \item{sppVector}{
The species vector (see \code{\link{sppVector}}).
}
}


\value{
A list giving the pure, simple diagnostic nucleotides (i.e. those nucleotides that are fixed within species and different from all other species) for each species in the species vector. A result of \code{integer(0)} indicates there are no diagnostic nucleotides for those species.
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
}
