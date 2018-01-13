#' Plot an 'ordinDNA' object
#' 
#' Plots an ordination of the Principal Components Analysis conducted by
#' \code{\link{ordinDNA}}.
#' 
#' \code{plot.ordinDNA} calculates the centroid and radius of the most variable
#' individual for each species in the multivariate space of the Principal
#' Components Analysis object given.
#' 
#' \code{majorAxes} plots the axes in the form \code{c(x, y)}. The maximum
#' number of axes calculated is the number of specimens in the dataset minus
#' one.
#' 
#' \code{sppBounds} has the following options: \code{"net"} (the default)
#' creates a complete graph between all individuals within a species.  If
#' \code{"circles"} is specified, a circle is drawn with a center fixed on the
#' centroid, and a radius of the length to the maximally distant individual.
#' Selecting the option of \code{"none"} means the individuals are not
#' connected in any way.
#' 
#' @param x An object of class `ordinDNA'.
#' @param majorAxes Numeric. Gives the numbers of the major axes that should be
#' plotted. Default of the first two major axes (\code{majorAxes = c(1,2)})
#' @param plotCol A vector of RGB colours giving the colours of the points and
#' circles. Must be in the form of a character vector with elements "#XXXXXX"
#' where XXXXXX gives the hexadecimal value for the colours desired. Default of
#' \code{"default"}. Colours are recycled if necessary.
#' @param trans A character vector giving the hexadecimal value for the
#' transparency of the circles. Default of "CC".
#' @param textcex Numeric. Controls the size of the text giving the species
#' value of the circles.
#' @param pchCentroid Numeric. Controls the shape of the point showing the
#' centroid of the circle for each species. Default of FALSE, no plotting of
#' centroid position.
#' @param sppBounds Option to determine the method of visualising conspecific
#' points. Options of \code{"net"} (the default), \code{"none"} or
#' \code{"circles"}.
#' @param sppNames Logical. Should species names be plotted? Default of TRUE.
#' @param namePos Character vector of length 1 giving the position where the
#' species names should be plotted. Possible values are: "top" and "bottom",
#' anything else plots the names at the centroid.
#' @param ptPch Numeric. Number of the symbol to be used for plotting. see
#' \code{\link{points}}. Default of 21.
#' @param ptCex Numeric. Number governing the size of the points. Default of
#' 0.5.
#' @param netWd Numeric. Number governing the width of the lines in the netowk.
#' Default of 1.
#' @param ... Other arguments to be passed to \code{plot}.
#' @return Plots an ordination of the first two major axes showing the
#' positions of each individual (squares), the centroid of each species
#' (circular bullet and name of species), and the variation in the species
#' (large circle, the radius of which is the distance to the furthest
#' individual from the centroid).
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{ordinDNA}}, \code{\link{cgraph}}.
#' @keywords Visualisation
#' @examples
#' 
#' data(dolomedes)
#' doloDist <- dist.dna(dolomedes)
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' doloOrd <- ordinDNA(doloDist, doloSpp)
#' 
#' plot(doloOrd)
#' plot(doloOrd, majorAxes = c(1,3))
#' plot(doloOrd, textcex = 0.001)
#' plot(doloOrd, plotCol = c("#FF0000", "#00FF00", "#0000FF"))
#' plot(doloOrd, namesPos = "bottom")
#' plot(doloOrd, namesPos = "centre")
#' 
#' data(anoteropsis)
#' anoDist <- dist.dna(anoteropsis)
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#'     
#' anoOrd <- ordinDNA(anoDist, anoSpp)
#' 
#' plot(anoOrd, sppBounds = "circles")
#' 
#' 
#' 
#' @export plot.ordinDNA
plot.ordinDNA <- function(x, majorAxes = c(1,2), plotCol = "default", trans = "CC", textcex = 0.7, pchCentroid = FALSE, sppBounds = "net", sppNames = TRUE, namePos = "top", ptPch = 21, ptCex = 0.5, netWd = 1, ...){
	#Colours
	if(plotCol[1] == "default") {
		plotCol <- c("#D33F6A", "#D95260", "#DE6355", "#E27449", "#E6833D", "#E89331", 
		"#E9A229", "#EAB12A", "#E9C037", "#E7CE4C", "#E4DC68", "#E2E6BD", 
		"#8E063B", "#A0323E", "#B04D41", "#C06544", "#CD7B48", "#D8904D", 
		"#E0A455", "#E7B65E", "#EAC76A", "#EAD577", "#E8E188", "#E2E6BD", 
		"#023FA5", "#5868AC", "#848DBC", "#A9AECB", "#C8CAD8", "#DDDEE0", 
		"#E1DDDD", "#D9C6C9", "#CEA5AC", "#BE7E8A", "#A94F64", "#8E063B"
		)
		} else plotCol <-  rep(plotCol, length(x$sppVector)/length(plotCol))
	transCol <- paste(plotCol, trans, sep = "")


#' Species Vectors
#' 
#' A grouping variable that gives an identity to the individuals in various
#' analyses.
#' 
#' Species vectors are the key concept behind a lot of \code{spider}'s
#' functionality. They are the method used to group data from individuals into
#' species. It is important to note that "species" in this context can mean any
#' cluster (real or otherwise) that is of interest. Populations, demes,
#' subspecies and genera could be the taxa segregated by "species vectors".
#' 
#' The two characteristics of a species vector are UNIQUENESS between species
#' and CONSISTENCY within them. R recognises differences of a single character
#' between elements, leading to \code{spider} considering these elements to
#' represent different species.
#' 
#' There is an easy way and a hard way to create species vectors. The hard way
#' is to type out each element in the vector, making sure no typos or alignment
#' errors are made.
#' 
#' The easy way is to add species designations into your data matrix from the
#' beginning in such a way that it is easy to use R's data manipulation tools
#' to create a species vector from the names of your data. See the examples for
#' a few ways to do this.
#' 
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso Functions for creating species vectors: \code{\link{strsplit}},
#' \code{\link{substr}}, \code{\link{sapply}}.
#' 
#' Functions that use species vectors: \code{\link{nearNeighbour}},
#' \code{\link{monophyly}}, \code{\link{nonConDist}}, \code{\link{nucDiag}},
#' \code{\link{rmSingletons}}, \code{\link{slideAnalyses}},
#' \code{\link{slideBoxplots}}, \code{\link{sppDist}},
#' \code{\link{sppDistMatrix}}, \code{\link{threshOpt}}.
#' @keywords Utilities
#' @examples
#' 
#' data(dolomedes)
#' #Dolomedes species vector
#' doloSpp <- substr(dimnames(dolomedes)[[1]], 1, 5)
#' 
#' data(anoteropsis)
#' #Anoteropsis species vector
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' 
#' 
#' 
	sppVector <- x$sppVector
	sppVecFac <- as.factor(sppVector)
	sppVecFacNum <- as.numeric(unique(sppVecFac))

	
	#Figure out the centroid positions
	mat <- x$pco$points[, majorAxes]
	centroids <- aggregate(mat, list(sppVector), mean)
	names(centroids) <- c("spp", "x", "y")
	#Determine width of circle
	maxDist <- function(xx){
		aa <- matrix(xx, ncol = 2)
		aa <- rbind(apply(aa, 2, mean),aa)
		bb <- as.matrix(dist(aa))
		max(bb[,1])
	}
	radius <- sapply(unique(sppVector), function(xx) maxDist(mat[sppVector == xx,]))
	radius <- radius[match(sort(names(radius)), names(radius))]
	
	# net setup
	sppPoints <- lapply(unique(sppVector), function(xx) mat[sppVector == xx, , drop = FALSE])
	topPoint <- sapply(sppPoints, function(xx) xx[which.max(xx[,2]), ])
	
	#Proportion of variation in each axis
	propVar <- round(x$pco$eig/max(cumsum(x$pco$eig)) * 100, 1)
	
	if(namePos == "top") labRadius <- radius else if(namePos == "bottom") labRadius <- -radius else labRadius <- 0

	plot(mat[,1], mat[,2], type = "n", asp = 1, xlab = paste("Major axis ", majorAxes[1], " (", propVar[majorAxes[1]], "%)", sep = ""), ylab = paste("Major axis ", majorAxes[2], " (", propVar[majorAxes[2]], "%)", sep = ""), ...)
	if(sppBounds == "circles") symbols(centroids[,2], centroids[,3], circles = radius, fg = transCol[as.numeric(sort(unique(sppVecFac)))], bg = transCol[as.numeric(sort(unique(sppVecFac)))], inches = FALSE, add = TRUE)
	if(sppBounds == "net") lapply(1:length(sppPoints), function(xx) cgraph(sppPoints[[xx]], col = plotCol[sppVecFacNum[xx]], lwd = netWd))	
	if(pchCentroid) points(centroids[,2], centroids[,3], pch = ptPch, bg = plotCol[as.numeric(sort(unique(sppVecFac)))])
	if(sppNames & namePos != "topPoint") text(centroids[,2], centroids[,3] + labRadius, labels = sort(unique(sppVector)), cex = textcex, pos = 3, offset = 0.06) else if(sppNames & namePos == "topPoint") text(topPoint[1, ], topPoint[2, ], labels = unique(sppVector), cex = textcex, pos = 3, offset = 0.06)
	points(mat[,1], mat[,2], pch=22, bg = plotCol[as.numeric(sppVecFac)], cex = ptCex)
	#~ list(radius, centroids, mat)
	#sppPoints
}
