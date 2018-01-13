#' Sliding window analyses
#' 
#' Wraps a number of measures used in sliding window analyses into one
#' easy-to-use function.
#' 
#' Distance measures include the following: proportion of zero non-conspecific
#' distances, number of diagnostic nucleotides, number of zero-length
#' distances, and overall mean distance.
#' 
#' Tree-based measures include the following: proportion of species that are
#' monophyletic, proportion of clades that are identical between the neighbour
#' joining tree calculated for the window and the tree calculated for the full
#' dataset, and the latter with \code{method="shallow"}.
#' 
#' Tree-based measures are a lot more time-intensive than distance measures.
#' When dealing with lots of taxa and short windows, this part of the function
#' can take hours.
#' 
#' Both distance and tree measures are calculated from a K2P distance matrix
#' created from the data with the option \code{pairwise.deletion = TRUE}. When
#' sequences with missing data are compared with other sequences, a \code{NA}
#' distance results. These are ignored in the calculation of
#' \code{slideAnalyses} distance metrics. However, the tree measures cannot
#' cope with this missing data, and so no result is returned for windows where
#' some sequences solely contain missing data.
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @param sppVector Species vector (see \code{\link{sppVector}}).
#' @param width Desired width of windows in number of nucleotides.
#' @param interval Distance between each window in number of nucleotides.
#' Default of 1. Giving the option of 'codons' sets the size to 3.
#' @param distMeasures Logical. Should distance measures be calculated? Default
#' of TRUE.
#' @param treeMeasures Logical. Should tree-based measures be calculated?
#' Default of FALSE.
#' @return An object of class 'slidWin' which is a list containing the
#' following elements: \item{win_mono_out}{Proportion of species that are
#' monophyletic.} \item{comp_out}{Proportion of clades that are identical
#' between the NJ tree calculated for the window and the tree calculated for
#' the full dataset.} \item{comp_depth_out}{Proportion of shallow clades that
#' are identical.} \item{pos_tr_out}{Index of window position for tree-based
#' analyses.} \item{noncon_out}{Proportion of zero non-conspecific distances.}
#' \item{nd_out}{The sum of diagnostic nucleotides for each species.}
#' \item{zero_out}{The number of zero-length distances.}
#' \item{dist_mean_out}{Overall mean K2P distance of each window.}
#' \item{pos_out}{Index of window position.} \item{dat_zero_out}{Number of zero
#' inter-specific distances in the full dataset.} \item{boxplot_out}{Always
#' FALSE. Required for \code{\link{plot.slidWin}}.} \item{distMeasures}{Value
#' of argument. Required for \code{\link{plot.slidWin}}.}
#' \item{treeMeasures}{Value of argument. Required for
#' \code{\link{plot.slidWin}}.}
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{dist.dna}}, \code{\link{plot.slidWin}},
#' \code{\link{rankSlidWin}}, \code{\link{slideNucDiag}}.
#' @keywords Sliding window
#' @examples
#' 
#' \dontrun{
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' slideAnalyses(dolomedes, doloSpp, 200, interval=10, treeMeasures=TRUE)
#' }
#' 
#' @importFrom ape dist.dna
#' @importFrom ape nj
#' @importFrom ape node.depth
#' @importFrom stats median
#' @export slideAnalyses
slideAnalyses <-
function(DNAbin, sppVector, width, interval = 1, distMeasures = TRUE, treeMeasures = FALSE){
	#Produce distance matrix, number of zero cells of full sequence
	boxplot_out <-FALSE
	dat <- as.matrix(DNAbin)
	dimnames(dat)[[1]] <- sppVector
	datd <- dist.dna(dat, pairwise.deletion = TRUE)
	dat_zero_out <- sum(as.numeric(datd == 0))/length(datd)
	#Create the windows
	win <- slidingWindow(dat, width, interval = interval)
	pos_out <- sapply(win, function(x) attr(x, "window")[1])
	win_dist <- lapply(win, function(x) dist.dna(x, pairwise.deletion = TRUE))
	#Distance metrics
	if(distMeasures){
		#Mean of distance matrix
		dist_mean_out <- sapply(win_dist, function(x) mean(x, na.rm=TRUE)) 
		#Number of zero cells
		zero_out <- sapply(win_dist, function(y) sum(as.numeric(y == 0), na.rm=TRUE)/length(y))
		##################
		#Threshold measures REMOVED
		#thres_above_out <- sapply(win_dist, function(x) sum( as.numeric(x >= thresA) ) )
		#thres_below_out <- sapply(win_dist, function(x) sum( as.numeric(x <= thresB) ) )
		##################
		#Diagnostic nucleotides 
		nd_out <- slideNucDiag(DNAbin, sppVector, width, interval)
		nd_out <- colSums(nd_out)
		#Nearest non-conspecific distance
		noncon_out <- sapply(win_dist, function(x) nonConDist(x, propZero = TRUE, rmNA=TRUE))
		
	}
	if(treeMeasures){
		#Produce NJ tree of full sequence
		dat_tr <- nj(datd)
		depth <- which(node.depth(dat_tr)[node.depth(dat_tr) > 1] <= median(node.depth(dat_tr)[node.depth(dat_tr) > 1]))
		#Remove windows with NA distances
		dist_na <- sapply(win_dist, function(x)  sum( as.numeric( is.na(x) ) ) )
		pos_tr_out <- pos_out[ dist_na < 1 ]
		win_tr <- lapply(win_dist[ dist_na < 1 ], nj)
		#Comparing clade composition with full alignment
		comp_out <- sapply(win_tr, function(x) tree.comp(dat_tr, x))
		comp_depth_out <- sapply(win_tr, function(x) tree.comp(dat_tr, x, method="shallow"))
		#Monophyly
		win_mono <- lapply(win_tr, function(x) monophyly(x, sppVector))
		win_mono_out <- sapply(win_mono, function(x) length(which(x))/length(x))
		}
rm(list = ls()[!ls() %in% c(ls(pattern="_out"), ls(pattern="res"))])
output <- as.list(environment())
class(output) <- "slidWin"
output
}

