blockAlignment <- function(DNAbin, mode = "shortest", range = NULL, fill = "") {
	
	DNAbin <- as.list(DNAbin)
	
	sizeRange <- range(sapply(DNAbin, length))
	
	if (is.null(mode)) {
		outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[range[1]:range[2]])))
	} else {
		if (mode == "shortest") {
			outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[1:sizeRange[1]])))
		}
		
		if (mode == "longest") {
			outDNAbin <- as.DNAbin(t(sapply(as.character(DNAbin), function(xx) xx[1:sizeRange[2]])))
		}
	}
	
	db <- as.list(outDNAbin)
	dbs <- lapply(db, function(x) as.character(x))
	dbs <- lapply(dbs, function(x) replace(x, which(x == ""), fill))
	outDNAbin <- as.DNAbin(dbs)
	
	as.matrix(outDNAbin)
}
