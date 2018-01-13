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
#' 
#' 
#' @export heatmapSpp
heatmapSpp <- function(distObj, sppVector, col = NULL, axisLabels = NULL){
	if (!is.matrix(distObj)) distObj <- as.matrix(distObj)
	
	if (is.null(col)) cols <- c("#D33F6A", "#D95260", "#DE6355", "#E27449", "#E6833D", "#E89331", "#E9A229", "#EAB12A", "#E9C037", "#E7CE4C", "#E4DC68", "#E2E6BD") else cols <- col
	
	if (is.null(axisLabels)) axisLabels <- sppVector[order(sppVector)] else axisLabels <- axisLabels[order(sppVector)]
	
	image(distObj[order(sppVector), order(sppVector)], col = cols, xaxt = "n", yaxt = "n")
	axis(1, at = seq(0, 1, length.out = dim(distObj)[1]), labels = axisLabels, las = 2)
	axis(2, at = seq(0, 1, length.out = dim(distObj)[1]), labels = axisLabels, las = 2)

}
