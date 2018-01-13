

#' Cytochrome oxidase I (COI) sequences of New Zealand _Anoteropsis_ species
#' 
#' A set of 33 sequences of the mitochondrial protein-coding gene cytochrome
#' oxidase I from 20 species of the New Zealand wolf spider genus
#' \emph{Anoteropsis} (Lycosidae) and two species of \emph{Artoria} as
#' outgroups. The sequences are available on GenBank as accession numbers
#' AY059961 through AY059993.
#' 
#' 
#' @name anoteropsis
#' @docType data
#' @format A DNAbin object containing 33 sequences with a length of 409 base
#' pairs stored as a matrix.
#' @source Vink, C. J., and Paterson, A. M. (2003). Combined molecular and
#' morphological phylogenetic analyses of the New Zealand wolf spider genus
#' _Anoteropsis_ (Araneae: Lycosidae). _Molecular Phylogenetics and Evolution_
#' *28* 576-587.
#' @keywords Datasets
NULL





#' Cytochrome oxidase I (COI) sequences of New Zealand _Dolomedes_ species
#' 
#' A set of 37 sequences of the mitochondrial protein-coding gene cytochrome
#' oxidase I from the 4 New Zealand species of the nursery-web spider genus
#' \emph{Dolomedes} (Pisauridae). These sequences are available on GenBank as
#' accession numbers GQ337328 through GQ337385.
#' 
#' 
#' @name dolomedes
#' @docType data
#' @format A DNAbin object containing 37 sequences with a length of 850 base
#' pairs stored as a matrix.
#' @source Vink, C. J., and Duperre, N. (2010). Pisauridae (Arachnida:
#' Araneae). _Fauna of New Zealand_ *64* 1-54.
#' @keywords Datasets
NULL





#' Cytochrome oxidase I (COI) sequences of world-wide species of Salticidae
#' 
#' A set of 41 sequences of the mitochondrial protein-coding gene cytochrome
#' oxidase I from 41 species of the jumping spider family Salticidae.The
#' sequences are available on GenBank as accession numbers AY297360 through
#' AY297400.
#' 
#' 
#' @name salticidae
#' @docType data
#' @format A DNAbin object containing 41 sequences with a length of 409 base
#' pairs stored as a list.
#' @source Maddison, W. P., and Hedin, M. C. (2003). Jumping spider phylogeny
#' (Araneae: Salticidae). _Invertebrate Systematics_ *17* 529-549.
#' @keywords Datasets
NULL





#' Dummy sequences illustrating the categories of diagnostic nucleotides
#' 
#' A set of 8 dummy sequences published in Sarkar et al 2008 to illustrate the
#' different categories of diagnostic nucleotides.
#' 
#' 
#' @name sarkar
#' @docType data
#' @format A DNAbin object containing 8 sequences with a length of 18 base
#' pairs stored as a matrix.
#' @source Sarkar, I., Planet, P., & DeSalle, R. (2008). CAOS software for use
#' in character- based DNA barcoding. _Molecular Ecology Resources_ *8*
#' 1256-1259
#' @keywords Datasets
NULL





#' Species Identity and Evolution in R
#' 
#' Spider: SPecies IDentity and Evolution in R, is an R package implementing a
#' number of useful analyses for DNA barcoding studies and associated research
#' into species delimitation and speciation. Included are functions for
#' generating summary statistics from DNA barcode data, assessing specimen
#' identification efficacy, and for testing and optimising divergence threshold
#' limits. In terms of investigating evolutionary and taxonomic questions,
#' techniques for sliding window, population aggregate, and nucleotide
#' diagnostic analyses are also provided.
#' 
#' The complete list of functions can be displayed with
#' \code{library(help=spider)}.
#' 
#' More information, including a tutorial on the use of spider can be found at
#' \code{http://spider.r-forge.r-project.org}.
#' 
#' \tabular{ll}{ Package: \tab spider\cr Type: \tab Package\cr Version: \tab
#' 1.4-2\cr Date: \tab 2017-05-13\cr License: \tab GPL\cr LazyLoad: \tab yes\cr
#' }
#' 
#' A few of the key functions provided by spider:
#' 
#' DNA barcoding: \code{\link{bestCloseMatch}}, \code{\link{nearNeighbour}},
#' \code{\link{threshID}}, \code{\link{threshOpt}}, \code{\link{heatmapSpp}}.
#' 
#' Sliding window: \code{\link{slidingWindow}}, \code{\link{slideAnalyses}},
#' \code{\link{slideBoxplots}}.
#' 
#' Nucleotide diagnostics: \code{\link{nucDiag}}, \code{\link{rnucDiag}}.
#' 
#' Morphological techniques: \code{\link{paa}}.
#' 
#' @name spider-package
#' @aliases spider-package spider
#' @docType package
#' @author Samuel Brown, Rupert Collins, Stephane Boyer, Marie-Caroline Lefort,
#' Jagoba Malumbres-Olarte, Cor Vink, Rob Cruickshank
#' 
#' Maintainer: Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso
#' 
#' \code{\link{ape-package}}, \code{\link{pegas-package}}.
#' @references Brown S. D. J., Collins R. A., Boyer S., Lefort M.-C.,
#' Malumbres-Olarte J., Vink C. J., & Cruickshank R. H. 2012. SPIDER: an R
#' package for the analysis of species identity and evolution, with particular
#' reference to DNA barcoding. _Molecular Ecology Resources_ 12:562-565. doi:
#' 10.1111/j.1755-0998.2011.03108.x
#' @keywords package
NULL



