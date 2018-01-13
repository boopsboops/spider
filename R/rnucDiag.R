rnucDiag <- function(DNAbin, sppVector, n = 100){
	lsv <- length(sppVector)
	psv <- table(sppVector)/lsv
	rss <- replicate(n, sample(1:length(unique(sppVector)), lsv, replace = TRUE, prob = psv), simplify = FALSE)
	rnd <- lapply(rss, function(xx) unlist(sapply(nucDiag(DNAbin, xx), length)))
	
	rndOrd <- sapply(rnd, function(xx) xx[order(names(xx))])
	rssNum <- sapply(rss, table)
	NndNss <- tapply(unlist(rndOrd), unlist(rssNum), function(xx) xx)
	rndFreq <- lapply(NndNss, table)
	
	rmin <- sapply(rnd, min)
	rmean <- sapply(rnd, mean)
	rmedian <- sapply(rnd, median)
	rmax <- sapply(rnd, max)
	#list(min = min(rndn), mean = mean(rndn), max = max(rndn))
	list(min = rmin, mean = rmean, median = rmedian, max = rmax, rndFreq = rndFreq)
}