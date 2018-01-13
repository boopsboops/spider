#' Rank a 'slidWin' object.
#' 
#' Display the highest ranking windows measured by \code{\link{slideAnalyses}}.
#' 
#' The criteria for \code{rankSlidWin} correspond to the variables outputted by
#' \code{\link{slideAnalyses}} and are sorted in the following manner:
#' \tabular{lll}{ \code{rankSlidWin} criterion: \tab
#' \code{\link{slideAnalyses}} output:\tab Sorting method:\cr
#' \code{"mean_distance"} \tab \code{"dist_mean_out"} \tab Ascending\cr
#' \code{"monophyly"} \tab \code{"win_mono_out"} \tab Ascending\cr
#' \code{"clade_comparison"} \tab \code{"comp_out"} \tab Ascending\cr
#' \code{"clade_comp_shallow"} \tab \code{"comp_depth_out"} \tab Ascending\cr
#' \code{"zero_noncon"} \tab \code{"noncon_out"} \tab Descending\cr
#' \code{"zero_distances"} \tab \code{"zero_out"} \tab Descending\cr
#' \code{"diag_nuc"} \tab \code{"nd_out"} \tab Ascending\cr }
#' 
#' Given a sequence of 1:10, the ascending method of sorting considers 10 as
#' high. The descending method considers 1 as high.
#' 
#' The \code{"all"} criterion returns the windows that have the highest
#' cumulative total score over all criteria.
#' 
#' @param slidWin An object of class `slidWin', made using
#' \code{\link{slideAnalyses}}.
#' @param criteria Name of criteria to sort by. Can be any of the following:
#' \code{"mean_distance", "monophyly", "clade_comparison",
#' "clade_comp_shallow", "zero_noncon", "zero_distances", "diag_nuc"} or
#' \code{"all"}. Default of \code{"mean_distance"} if distance measures have
#' been calculated, otherwise \code{"monophyly"}.
#' @param num Number of windows to return. Default of 10.
#' @return A data frame giving the values of the measures calculated by
#' \code{\link{slideAnalyses}}, ranked to show the top 10 positions based on
#' the criterion given.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{slideAnalyses}}.
#' @keywords Sliding window
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' doloSlide <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, treeMeasures = TRUE)
#' 
#' rankSlidWin(doloSlide)
#' rankSlidWin(doloSlide, criteria = "zero_distances")
#' 
#' doloSlide2 <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, treeMeasures = FALSE)
#' rankSlidWin(doloSlide2)
#' 
#' doloSlide3 <- slideAnalyses(dolomedes, doloSpp, 200, interval = 10, distMeasures = FALSE, 
#'     treeMeasures = TRUE)
#' rankSlidWin(doloSlide3)
#' 
#' @export rankSlidWin
rankSlidWin <- 
function(slidWin, criteria = "mean_distance", num = 10){
	revRank <- function(xx) (length(xx)+1) - rank(xx, ties.method="min")
	list2df <- function(listObj){
		len <- sapply(listObj, length)
		mlen <- min(len)
		for(i in 1:length(listObj)) listObj[[i]] <- listObj[[i]][1:mlen]
		as.data.frame(listObj)
	}
	distCriteria <- distLabel <- treeCriteria <- treeLabel <- treeEX <- NULL
	if(!slidWin$distMeasures && !slidWin$treeMeasures) stop("Object of class`slidWin' must be created using slideAnalyses")
	if(!slidWin$distMeasures && slidWin$treeMeasures) criteria <- "monophyly"
	if(slidWin$distMeasures){
		distCriteria <- c("mean_distance", "zero_noncon", "zero_distances", "diag_nuc")
		distLabel <- c("dist_mean_out", "noncon_out", "zero_out", "nd_out")
	}
	if(slidWin$treeMeasures){ 
		treeCriteria <- c("monophyly", "clade_comparison", "clade_comp_shallow")
		treeLabel <- c("win_mono_out", "comp_out", "comp_depth_out")
		treeEX <- c("pos_tr_out")
	}
	measures <- c("position", distCriteria, treeCriteria)
	#Remove objects not of interest
	excluded <- match(c("dat_zero_out", "boxplot_out", "distMeasures", "treeMeasures", treeEX), names(slidWin))
	dFrame <- list2df(slidWin[-excluded])
	#Reorder and rename dataframe columns
	dfOrder <- match(c("pos_out", distLabel, treeLabel), names(dFrame))
	dFrame <- dFrame[ , dfOrder]
	names(dFrame) <- measures
	#Order rows
	high <- match(c("monophyly", "clade_comparison", "clade_comp_shallow", "diag_nuc", "mean_distance"), measures)
	highVal <- as.data.frame(lapply(dFrame, revRank))
	dfVals <- highVal
	if(slidWin$distMeasures){
		low <- match(c("zero_noncon", "zero_distances"), measures)
		lowVal <- as.data.frame(lapply(dFrame, function(x) rank(x, ties.method="min")))
		dfVals[ , low] <- lowVal[ , low]
	}
	if("all" %in% criteria) rowOrd <- order(apply(dfVals[ , -1], MARGIN=1, FUN=sum))
		else{
			ordNum <- which(measures %in% criteria)
			if(length(criteria) > 1) rowOrd <- order(apply(dfVals[ , ordNum], MARGIN=1, FUN=sum))
				else rowOrd <- order(dfVals[ , ordNum])
				}
	#Return top 10
	head(dFrame[ rowOrd , ], n = as.integer(num))
}
