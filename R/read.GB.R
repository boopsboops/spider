#' Download sequences from Genbank with metadata.
#' 
#' Downloads sequences associated with the given accession numbers into a
#' `DNAbin' class.
#' 
#' This function is a modification of
#' \code{\link[ape:ape-package]{read.GenBank}} to include metadata with each
#' sequence. Additional data currently implemented are the species names and
#' the gene region from which sequences were derived.
#' 
#' @param access.nb A character vector giving the GenBank accession numbers to
#' download.
#' @param seq.names A character vector giving the names to give to each
#' sequence. Defaults to "accession number | species name".
#' @param species.names Logical. Should species names be downloaded? Default of
#' TRUE.
#' @param gene Logical. Should the name of the gene region be downloaded?
#' Default of TRUE.
#' @param access Logical. Should the accession number be downloaded? Default of
#' TRUE.
#' @param as.character Logical. Should the sequences be returned as character
#' vector? Default of FALSE, function returns sequences as a `DNAbin' object.
#' @return A 'DNAbin' object with the following attributes: \code{"species"},
#' \code{"gene"}, and \code{"accession_num"}.
#' @section Warning: 15 Feb 2018: 'read.GB' is deprecated. Please use the rOpenSci packages 'rentrez' and 'traits', or 'ape' for better functionality.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link[ape]{read.GenBank}}.
#' @keywords Datasets
#' @examples
#' 
#' \dontrun{
#' read.GB("AY059961")
#' 
#' #Download the sequences making data(anoteropsis) from Genbank
#' nums <- 59961:59993
#' seqs <- paste("AY0", nums, sep="")
#' dat <- read.GB(seqs)
#' 
#' attr(dat, "species")
#' attr(dat, "gene")
#' attr(dat, "accession_num")}
#' 
#' @export read.GB
read.GB <-
function(access.nb, seq.names = access.nb, species.names = TRUE, gene=TRUE, access=TRUE, as.character = FALSE) {
    .Deprecated(msg="'read.GB' is deprecated. Please use the 'rentrez', 'traits' or 'ape' packages.")
}

