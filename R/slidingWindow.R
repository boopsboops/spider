#' Create windows along an alignment
#' 
#' Creates windows of a specified width along a DNA alignment.
#' 
#' Sliding window analyses are often used to determine the variability along
#' sequences. This can be useful for investigating whether there is evidence
#' for recombination, developing shorter genetic markers, or for determining
#' variation within a gene.
#' 
#' Analyses can be conducted on each window using \code{\link{lapply}}.
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @param width Width of each window.
#' @param interval Numeric or option of \code{"codons"}. This sets interval
#' between windows. Default of 1. Setting the option to "codons" gives an
#' interval of 3.
#' @return A list of `DNAbin' objects, with each alignment being \code{width}
#' bases in length. The list has length of the DNA alignment minus the width.
#' The positions covered by each window can be retreived with \code{attr(x,
#' "window")}.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{lapply}}, \code{\link{slideAnalyses}},
#' \code{\link{slideBoxplots}}.
#' @keywords Sliding window
#' @examples
#' 
#' data(woodmouse)
#' woodmouse <- woodmouse[,1:20]
#' win1 <- slidingWindow(woodmouse, width = 10)
#' length(win1)
#' 
#' win2 <- slidingWindow(woodmouse, width = 10, interval = 2)
#' length(win2)
#' 
#' win3 <- slidingWindow(woodmouse, width = 10, interval = "codons")
#' length(win3)
#' 
#' win4 <- slidingWindow(woodmouse, width = 15)
#' length(win4)
#' attr(win4[[1]], "window")
#' attr(win4[[2]], "window")
#' 
#' @export slidingWindow
slidingWindow <-
function(DNAbin, width, interval = 1){
	annote <- function(x){
		width <- width - 1
		y <- align[ , x:(x+width)]
		attr(y, "window") <- c(x, x+width)
		y
	}
	if(interval == "codons") interval <- 3
	align <- as.matrix(DNAbin)
	len <- dim(align)[[2]]
	iter <- seq(1, len-width, by = interval)
	li <- lapply(iter, function(x) annote(x))
	li
}

