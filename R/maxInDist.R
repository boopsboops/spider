maxInDist <- 
function (distobj, sppVector = NULL, propZero = FALSE, rmNA = FALSE){
    dat <- as.matrix(distobj)
    if (length(sppVector) > 0) 
        dimnames(dat)[[1]] <- sppVector
    conSpecDists <- list()
    for (i in 1:length(dimnames(dat)[[1]])) {
        conSpec <- dimnames(dat)[[1]] == dimnames(dat)[[1]][i]
	dd <- sort(dat[conSpec, i], decreasing = TRUE)
        if(length(dd) > 1) conSpecDists[[i]] <- dd[1] else conSpecDists[[i]] <- NA
    }
    if (propZero) 
        output <- length(which(unlist(conSpecDists) == 0))/length(unlist(conSpecDists))
    else output <- unname(unlist(conSpecDists))
    output
}