#' Boxplots across windows
#' 
#' Calculates boxplots of genetic distances using sliding windows.
#' 
#' Giving \code{method="overall"} calculates the boxplot for the distance
#' matrix of each window.
#' 
#' Giving \code{method="interAll"} calculates boxplots for the inter- and
#' intra-specific distances of each window, showing the result for ALL
#' inter-specific distances.
#' 
#' Giving \code{method="nonCon"} calculates boxplots for the inter- and
#' intra-specific distances of each window, showing the result for only the
#' nearest-conspecific distances for each individual.
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @param sppVector A species vector (see \code{\link{sppVector}}).
#' @param width Width of windows.
#' @param interval Distance between each window in number of base pairs.
#' Default of 1. Giving the option of \code{"codons"} sets the size to 3.
#' @param method Options of \code{"overall", "interAll"} or \code{"nonCon"}
#' (the default).
#' @return A list with
#' 
#' \item{treeMeasures}{Logical. Tree measures calculated? Always FALSE.}
#' \item{distMeasures}{Logical. Distance measures calculated? Always FALSE.}
#' \item{bp_out}{If \code{method="overall"}, contains the boxplot objects of
#' each window.} \item{bp_InterSpp_out}{If \code{method!="overall"}, contains
#' the boxplot objects of the interspecific distances of each window.}
#' \item{bp_IntraSpp_out}{If \code{method!="overall"}, contains the boxplot
#' objects of the intraspecific distances of each window.}
#' \item{bp_range_out}{range of y-axis values.} \item{pos_out}{x-axis values.}
#' \item{boxplot_out}{Logical. Boxplots calculated? Always TRUE.}
#' \item{method}{The method used for calculating boxplots. \code{"overall",
#' "interAll"} or \code{"nonCon"}.}
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{boxplot}}, \code{\link{plot.slidWin}},
#' \code{\link{slideAnalyses}}, \code{\link{slidingWindow}}.
#' @keywords Sliding window
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' doloNonCon <- slideBoxplots(dolomedes, doloSpp, 200, interval=10)
#' plot(doloNonCon)
#' 
#' doloOverall <- slideBoxplots(dolomedes, doloSpp, 200, interval=10, method="overall")
#' plot(doloOverall)
#' 
#' doloInterall <- slideBoxplots(dolomedes, doloSpp, 200, interval=10, method="interAll")
#' plot(doloInterall)
#' 
#' @export slideBoxplots
slideBoxplots <-
function(DNAbin, sppVector, width, interval = 1, method="nonCon"){
	#Produce distance matrix, number of zero cells of full sequence
	boxplot_out <- TRUE
	dat <- as.matrix(DNAbin)
	dimnames(dat)[[1]] <- sppVector
	datd <- dist.dna(dat, pairwise.deletion = TRUE)
	#Create the windows
	win <- slidingWindow(dat, width-1, interval = interval)
	pos_out <- sapply(win, function(x) attr(x, "window")[1])
	win_dist <- lapply(win, function(x) dist.dna(x, pairwise.deletion = TRUE))
	bp_range_out <- range(win_dist,na.rm=TRUE)
	if(method == "overall"){
		#Boxplots of overall distance matrix
		bp_out <- lapply(win_dist, function(x) boxplot(x, plot=FALSE))
	}
	if(method == "interAll"){
		#Boxplots of intra- and inter-specific distances
		spp_dist <- lapply(win_dist, function(x) sppDist(x, sppVector))
		bp_InterSpp_out <- lapply(spp_dist, function(x) boxplot(x$inter, plot=FALSE))
		bp_IntraSpp_out <- lapply(spp_dist, function(x) boxplot(x$intra, plot=FALSE))
	}
	if(method == "nonCon"){
		#Boxplots of closest non-conspecific distance
		spp_dist <- lapply(win_dist, function(x) sppDist(x, sppVector))
		bp_IntraSpp_out <- lapply(spp_dist, function(x) boxplot(x$intra, plot=FALSE))
		spp_nonCon <- lapply(win_dist, nonConDist)
		bp_InterSpp_out <- lapply(spp_nonCon, function(x) boxplot(x, plot=FALSE))
		bp_range_out <- c(0, max(unlist(spp_nonCon), na.rm = TRUE))
	}
	
rm(list = ls()[!ls() %in% c(ls(pattern="_out"), ls(pattern="thod"))])
distMeasures <- FALSE
treeMeasures <- FALSE
output <- as.list(environment())
class(output) <- "slidWin"
output
}
