#' Downloads DNA sequences from the Barcode of Life Database (BOLD)
#' 
#' These functions allow DNA sequences to be downloaded from the Barcode of
#' Life Database (BOLD).
#' 
#' \code{search.BOLD} retrieves BOLD process identification numbers for any
#' given taxon using the API for BOLD version 3.0. By default, it only returns
#' the first 500 process IDs for the given taxon. By selecting the option
#' \code{exhaustive = TRUE}, the function can be made to search for more than
#' 500 process IDs, but is much slower.
#' 
#' \code{stats.BOLD} retrieves the total number of records for the given taxon.
#' 
#' \code{read.BOLD} downloads the sequences associated with the process
#' identification numbers using a brute force method of downloading the
#' specimen record, then searching and splitting the HTML code to remove the
#' relevant information. This process is likely to make the function fairly
#' unstable if BOLD make any changes to their website.
#' 
#' Previous versions of \code{read.BOLD} used the eFetch web service offered by
#' BOLD to enable batch retrieval of records, however from October 2012 BOLD
#' deprecated eFetch without providing a replacement service.
#' 
#' @aliases read.BOLD search.BOLD stats.BOLD
#' @param taxon A character vector of the names of the taxa of interest.
#' @param exhaustive Logical. Should the function search for more than 500
#' process IDs? Default of FALSE.
#' @param IDs A character vector containing BOLD process ID numbers.
#' @return \code{search.BOLD} returns a character vector giving the process
#' identification numbers of the specimens found by the search.
#' 
#' \code{read.BOLD} returns an object of class `DNAbin'. This object has the
#' attributes "species", "accession_num", and "gene".
#' @section Warning: On 26 Oct 2011, attempts to access records using the
#' eFetch system through a web browser resulted in an error, saying that eFetch
#' and eSearch are offline for maintainance.
#' 
#' As of 7 March 2012, both functions have been modified to interface with the
#' new BOLD architecture, and work as expected.
#' 
#' 29 Oct 2012: It appears that BOLD has taken eFetch offline permanently,
#' rendering \code{read.BOLD} as it currently stands useless. While we may be
#' able to work out something, this will require a complete rewrite of the
#' function. \code{search.BOLD} continues to work as intended.
#' 
#' 17 Dec 2012: A new version of \code{read.BOLD} has been released that
#' appears to work (for the time being).
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{stats.BOLD}}, \code{\link{search.BOLD}}, \code{\link{read.GB}}. %% ~~objects to See Also as
#' \code{\link{help}}, ~~~
#' @references BOLD web services: \url{http://services.boldsystems.org/}.
#' 
#' BOLD version 3.0 \url{http://v3.boldsystems.org/}.
#' @keywords Barcoding Datasets
#' @examples
#' 
#' \dontrun{
#' stats.BOLD("Pisauridae")
#' 
#' search.BOLD(c("Danio kyathit", "Dolomedes", "Sitona discoideus"))
#' 
#' nn <- search.BOLD("Pisauridae")
#' pisaurid <- read.BOLD(nn)
#' 
#' ape::write.dna(pisaurid, "filename.fas", format="fasta")}
#' 
#' @importFrom ape write.dna
#' @export read.BOLD
read.BOLD <- function(IDs){
	allSeqs <- list()
	
	for(i in 1:length(IDs)){
		URL <- paste("http://v3.boldsystems.org/index.php/Public_RecordView?processid=", IDs[i], sep = "")
		res <- scan(file = URL, what = "", sep = "\n", quiet = TRUE)
		#DNA sequence
		dnaLineNo <- grep("Locus", res)
		dnaBlock <- paste(res[dnaLineNo: (dnaLineNo + 19)], collapse="")
		dnaSeq <- strsplit(dnaBlock, split = "<pre>|</pre>")[[1]][2]
		gene <- strsplit(strsplit(dnaBlock, split = "td")[[1]][4], split = "<|>")[[1]][2]
		
		#Taxonomy---Subfamily, genus, species, BIN number
		taxLineNo <- grep("TAXONOMY", res)
		taxBlock <- paste(res[taxLineNo: (taxLineNo + 30)], collapse="")
		taxFields <- strsplit(gsub("\\t", "", taxBlock), split = "<tr>|</tr>")
		sciname <- strsplit(taxFields[[1]][8], split = ">|<")[[1]][29]
		BIN <- strsplit(taxFields[[1]][10], split = ">|<")[[1]][31]
		
		seqs <- as.DNAbin(list(strsplit(tolower(dnaSeq), split="")[[1]]))
		nam <- paste(IDs[i], sciname, BIN, sep = "|")
		names(seqs) <- nam
		attr(seqs, "species") <- sciname
		attr(seqs, "BIN") <- BIN
		attr(seqs, "accession_num") <- IDs[i]
		attr(seqs, "gene") <- gene
		allSeqs[[i]] <- seqs
	}
	collSeqs <- do.call(c, allSeqs)
	attr(collSeqs, "species") <- unlist(lapply(allSeqs, function(x) attr(x, "species")))
	attr(collSeqs, "accession_num") <- unlist(lapply(allSeqs, function(x) attr(x, "accession_num")))
	attr(collSeqs, "BIN") <- unlist(lapply(allSeqs, function(x) attr(x, "BIN")))
	attr(collSeqs, "gene") <- unlist(lapply(allSeqs, function(x) attr(x, "gene")))
	collSeqs
}
