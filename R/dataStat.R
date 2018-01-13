#' Taxa statistics
#' 
#' Returns the numbers of species, genera and individuals in the dataset.
#' 
#' The value \code{NULL} can be passed to \code{gen} if genera are not of
#' interest in the dataset.
#' 
#' @param sppVector Species vector (see \code{\link{sppVector}}).
#' @param genVector Genus vector that defines the genera of each individual,
#' created in a similar manner to the species vector.
#' @param thresh Threshold for adequate individual/species number. Default of
#' 5.
#' @return A table giving the number of genera and species in the dataset;
#' giving the minimum, maximum, mean and median number of individuals per
#' species, and the number of species below the given threshold.
#' @author Rupert Collins <rupertcollins@@gmail.com>
#' @keywords Barcoding Utilities
#' @examples
#' 
#' data(anoteropsis)
#' #Species vector
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' #Genus vector
#' anoGen <-  sapply(strsplit(anoSpp, split="_"), function(x) x[1])
#' dataStat(anoSpp, anoGen)
#' 
#' @importFrom stats median
#' @export dataStat
dataStat <- function(sppVector, genVector, thresh = 5){
unSpp <- unique(sppVector)
unGen <- unique(genVector)
sppnum <- sapply(unSpp, function(x) length(which(sppVector %in% x)))
tab <- table(NULL)
tab[1:7] <- c(length(unGen), length(unSpp), min(sppnum), max(sppnum), median(sppnum), mean(sppnum), length(which(sppnum < thresh)))
names(tab) <- c("Genera", "Species", "Min", "Max", "Median", "Mean", "Thresh" )
round(tab, digits=0)
}
