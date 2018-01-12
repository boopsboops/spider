\name{read.GB}
\alias{read.GB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download sequences from Genbank with metadata.
}
\description{
Downloads sequences associated with the given accession numbers into a `DNAbin' class.
}
\usage{
read.GB(access.nb, seq.names = access.nb, species.names = TRUE, gene = TRUE, 
    access = TRUE, as.character = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{access.nb}{
A character vector giving the GenBank accession numbers to download.
}
  \item{seq.names}{
A character vector giving the names to give to each sequence. Defaults to "accession number | species name".
}
  \item{species.names}{
Logical. Should species names be downloaded? Default of TRUE.
}
  \item{gene}{
Logical. Should the name of the gene region be downloaded? Default of TRUE.
}
  \item{access}{
Logical. Should the accession number be downloaded? Default of TRUE.
}
  \item{as.character}{
Logical. Should the sequences be returned as character vector? Default of FALSE, function returns sequences as a `DNAbin' object.
}
}
\details{
This function is a modification of \code{\link[ape:ape-package]{read.GenBank}} to include metadata with each sequence. Additional data currently implemented are the species names and the gene region from which sequences were derived.
}
\value{
A 'DNAbin' object with the following attributes: \code{"species"}, \code{"gene"}, and \code{"accession_num"}.
}

\author{
Samuel Brown <s_d_j_brown@hotmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.GenBank}}.
}
\examples{
\dontrun{
read.GB("AY059961")

#Download the sequences making data(anoteropsis) from Genbank
nums <- 59961:59993
seqs <- paste("AY0", nums, sep="")
dat <- read.GB(seqs)

attr(dat, "species")
attr(dat, "gene")
attr(dat, "accession_num")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Datasets}
