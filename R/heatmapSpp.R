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
