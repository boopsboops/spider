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
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{read.GenBank}}.
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
    N <- length(access.nb)
    nrequest <- N%/%400 + as.logical(N%%400)
    X <- character(0)
    for (i in 1:nrequest) {
        a <- (i - 1) * 400 + 1
        b <- 400 * i
        if (i == nrequest) 
            b <- N
        URL <- paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=nuccore&id=", 
            paste(access.nb[a:b], collapse = ","), "&rettype=gb&retmode=text", 
            sep = "")
        X <- c(X, scan(file = URL, what = "", sep = "\n", quiet = TRUE))
    }
    FI <- grep("^ {0,}ORIGIN", X) + 1
    LA <- which(X == "//") - 1
    obj <- list()
    length(obj) <- N
    for (i in 1:N) {
        tmp <- gsub("[[:digit:] ]", "", X[FI[i]:LA[i]])
        obj[[i]] <- unlist(strsplit(tmp, NULL))
    }
    names(obj) <- seq.names
    if (!as.character) 
        obj <- as.DNAbin(obj)
    if (species.names) {
        tmp <- character(N)
        sp <- grep("ORGANISM", X)
        for (i in 1:N) tmp[i] <- unlist(strsplit(X[sp[i]], " +ORGANISM +"))[2]
        attr(obj, "species") <- gsub(" ", "_", tmp)
    if (gene) {
        tmp2 <- character(N)
        def <- grep("DEFINITION", X)
        for (i in 1:N) tmp2[i] <- unlist(strsplit(X[def[i]], "DEFINITION +"))[2]
        attr(obj, "gene") <- gsub(" ", "_", tmp2)
    if (access) {
        tmp3 <- character(N)
        def <- grep("ACCESSION", X)
        for (i in 1:N) tmp3[i] <- unlist(strsplit(X[def[i]], "ACCESSION +"))[2]
        attr(obj, "accession_num") <- gsub(" ", "_", tmp3)
    }
    names(obj)<-paste(attr(obj,"accession_num"), "|", attr(obj,"species"), sep = "")
    obj
}
}
}

