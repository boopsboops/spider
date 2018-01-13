bestCloseMatch <- function(distobj, sppVector, threshold = 0.01, names = FALSE){
	distobj <- as.matrix(distobj)
	diag(distobj) <- NA
	idConf <- rep(NA, length(sppVector))
	aa <- apply(distobj, MARGIN=2, FUN=function(x) which(x == min(x, na.rm = TRUE)))
	bb <- lapply(aa, function(x) unique(sppVector[x]))
	cc <- sppVector == bb
	dd <- sapply(1:length(sppVector), function(x) sppVector[x] %in% bb[[x]])
	ee <- apply(distobj, MARGIN=2, FUN=function(x) min(x, na.rm = TRUE))
	idConf[which(cc & dd)] <- "correct"
	idConf[which(!cc & !dd)] <- "incorrect"
	idConf[which(!cc & dd)] <- "ambiguous"
	idConf[which(ee > threshold)] <- "no id"
	if(names) output <- bb else output <- idConf
	output
}