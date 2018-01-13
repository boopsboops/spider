#' Visualise a distance matrix using a heatmap
#' 
#' This function plots a heatmap of the distance matrix, with shorter distances
#' indicated by darker colours.
#' 
#' 
#' The default palette has been taken from the \code{colorspace} package.
#' 
#' @param distObj A matrix or object of class \code{dist}.
#' @param sppVector The species vector. See \code{\link{sppVector}}.
#' @param col A vector giving the colours for the heatmap.
#' @param axisLabels A character vector that provides the axis labels for the
#' heatmap. By default the species vector is used.
#' @param triangle Which triangle of the heatmap should be plotted. Possible
#' values of "both", "upper" and "lower". Default of "both".
#' @param showData Logical. Should the data be shown on the heatmap? Default of
#' FALSE.
#' @param dataRound The number of significant figures the printed data will
#' show. Default of 3.
#' @param dataCEX Size of text for printed data. Default of 1.
#' @return Plots a heatmap of the distance matrix. Darker colours indicate
#' shorter distances, lighter colours indicate greater distances.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Utilities
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes, model = "raw")
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' heatmapSpp(doloDist, doloSpp)
#' heatmapSpp(doloDist, doloSpp, axisLabels = dimnames(dolomedes)[[1]])
#' 
#' data(anoteropsis)
#' anoDist <- dist.dna(anoteropsis, model = "raw")
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' heatmapSpp(anoDist, anoSpp)
#' heatmapSpp(anoDist, anoSpp, showData = TRUE)
#' heatmapSpp(anoDist, anoSpp, showData = TRUE, dataRound = 1, dataCEX = 0.4)
#' heatmapSpp(anoDist, anoSpp, triangle = "upper")
#' heatmapSpp(anoDist, anoSpp, triangle = "lower")
#' heatmapSpp(anoDist, anoSpp, triangle = "lower", showData = TRUE, dataRound = 1, dataCEX = 0.4)
#' 
#' 
#' @importFrom graphics image
#' @importFrom graphics text
#' @importFrom graphics axis
#' @importFrom ape dist.dna
#' @export heatmapSpp
heatmapSpp <- function(distObj, sppVector, col = NULL, axisLabels = NULL, triangle = "both", showData = FALSE, dataRound = 3, dataCEX = 1){
	if (!is.matrix(distObj)) distObj <- as.matrix(distObj)
	
	if (is.null(col)) cols <- c("#D33F6A", "#D95260", "#DE6355", "#E27449", "#E6833D", "#E89331", "#E9A229", "#EAB12A", "#E9C037", "#E7CE4C", "#E4DC68", "#E2E6BD") else cols <- col
	
	if (is.null(axisLabels)) axisLabels <- sppVector[order(sppVector)] else axisLabels <- axisLabels[order(sppVector)]
	sortedMat <- distObj[order(sppVector), order(sppVector)]
	finalMat <- sortedMat
	
	if(triangle == "lower") finalMat[upper.tri(finalMat)] <- NA
	if(triangle == "upper") finalMat[lower.tri(sortedMat)] <- NA
		
	image(finalMat, col = cols, xaxt = "n", yaxt = "n")
	
	if(showData == TRUE){
		xind <- rep(1:ncol(finalMat), rep(ncol(finalMat), nrow(finalMat)))
		yind <- rep(1:nrow(finalMat), ncol(finalMat))

		text((xind - 1)/(ncol(finalMat) - 1), (yind - 1)/(nrow(finalMat) - 1), label = mapply(function(x, y) signif(finalMat[x,y], dataRound), x = xind, y = yind), cex = dataCEX)
	}
	
	axis(1, at = seq(0, 1, length.out = dim(distObj)[1]), labels = axisLabels, las = 2)
	axis(2, at = seq(0, 1, length.out = dim(distObj)[1]), labels = axisLabels, las = 2)

}
