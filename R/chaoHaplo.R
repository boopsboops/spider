#' Chao estimator of haplotype number
#' 
#' Calculates the Chao1 estimate of the number of haplotypes in a population
#' based on the total number of haplotypes present, and the number of
#' singletons and doubletons in the dataset.
#' 
#' The function assumes a large number of specimens have been sampled and that
#' duplicate haplotypes have not been removed. Interpretation becomes difficult
#' when more than one species is included in the dataset.
#' 
#' @param DNAbin An object of class `DNAbin'.
#' @return An vector of length three, giving the estimated total number of
#' haplotypes in the population, and lower and upper 95\% confidence limits.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{haploAccum}}
#' @references Vink, C. J., McNeill, M. R., Winder, L. M., Kean, J. M., and
#' Phillips, C. B. (2011). PCR analyses of gut contents of pasture arthropods.
#' In: Paddock to PCR: Demystifying Molecular Technologies for Practical Plant
#' Protection (eds. Ridgway, H. J., Glare, T. R., Wakelin, S. A., O'Callaghan,
#' M.), pp. 125-134. New Zealand Plant Protection Society, Lincoln.
#' 
#' Chao, A. (1989). Estimating population size for sparse data in
#' capture-recapture experiments. _Biometrics_ *45* 427-438.
#' @keywords Barcoding
#' @examples
#' 
#' data(dolomedes)
#' #Create dataset with multiple copies of Dolomedes haplotypes
#' doloSamp <- dolomedes[sample(16, 100, replace=TRUE, prob=c(0.85, rep(0.01, 15))), ]
#' 
#' chaoHaplo(doloSamp)
#' 
#' @importFrom pegas haplotype
#' @export chaoHaplo
chaoHaplo <- function(DNAbin){
	haplo <- haplotype(DNAbin)
	i <- if(length(grep("[-|?|r|y|m|k|w|s|b|d|h|v|n]", DNAbin)) > 0) message("There are missing or ambiguous data, which may cause an overestimation of the number of haplotypes")
	nums <- sapply(attr(haplo, "index"), length)
	n <- dim(DNAbin)[1]
	h <- length(nums)
	s <- length(which(nums == 1))
	d <- length(which(nums == 2))
	#Estimated number of haplotypes (From Vink et al 2011)
	if(d > 0) est <- h + ((s^2)/(2 * d)) else est <- h + ((s * (s - 1))/2)
	
	#Confidence intervals (Modified from Chao 1989)
	varest <- est/((h / (est - h)) - n/est)
	C <- exp(1.96 * sqrt(log( 1 + (varest / ((est - h)^2)))))
	
	low <- h + (est - h)/C
	high <- h + (est - h) * C
    
	c(est, low, high)
}
	

