#' Complete graph
#' 
#' Creates a complete graph for the given cloud of vertices.
#' 
#' If \code{y} is not given, \code{x} is required to be a matrix containing
#' both x and y values.
#' 
#' @param x X values, or a matrix with two columns containing X and Y values.
#' @param y Y values. Can be left empty if \code{x} is a matrix.
#' @param ... Other arguments to be passed to \code{\link{segments}}.
#' @return Plots a complete graph between the given vertices.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link{plot.ordinDNA}}.
#' @keywords Visualisation
#' @examples
#' 
#' x <- runif(15)
#' y <- runif(15)
#' 
#' plot(x, y)
#' cgraph(x, y)
#' 
#' M <- cbind(x, y)
#' cgraph(M[1:10,], col = "blue")
#' 
#' 
#' @export cgraph
cgraph <- function(x, y = NULL, ...){
	if(!is.null(y)) mat <- cbind(x, y) else mat <- x
	dd <- dim(mat)[1]
	if(dd < 2) return()
	if(is.null(dd)) return()
	ddComb <- combn(1:dd, 2)
	segments(mat[ddComb[1,], 1], mat[ddComb[1,], 2], mat[ddComb[2,], 1], mat[ddComb[2,], 2], ...)
}
