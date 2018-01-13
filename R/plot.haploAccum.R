#' Plotting haplotype accumulation curves
#' 
#' Plots the accumulation curves calculated by \code{\link{haploAccum}}.
#' 
#' 
#' @param x A `haploAccum' object obtained from \code{\link{haploAccum}}.
#' @param add Add graph to an existing graph.
#' @param ci Multiplier for the calculation of confidence intervals from
#' standard deviation. \code{ci = 0} prevents the drawing of confidence
#' intervals.
#' @param ci.type Type of confidence intervals: \code{"bar"} for vertical bars,
#' \code{"line"} for lines, and \code{"polygon"} for a shaded area.
#' @param col Colour for curve line.
#' @param ci.col Colour for lines or shaded area when \code{"polygon"}.
#' @param ci.lty Line type for confidence interval lines or border of the
#' \code{"polygon"}.
#' @param xlab Label for the X-axis.
#' @param ylab Label for the Y-axis.
#' @param ylim Y-axis limits.
#' @param main Title of the plot.
#' @param ... Other parameters to pass to plot.
#' @return Plots a haplotype accumulation curve and confidence intervals
#' depending on the options given to \code{\link{haploAccum}}.
#' @author Jagoba Malumbres-Olarte <j.malumbres.olarte@@gmail.com>.
#' @references Gotellli, N.J. & Colwell, R.K. (2001). Quantifying biodiversity:
#' procedures and pitfalls in measurement and comparison of species richness.
#' _Ecology Letters_ *4* 379--391.
#' @keywords Sampling
#' @examples
#' 
#' data(dolomedes)
#' #Generate multiple haplotypes
#' doloHaplo <- dolomedes[sample(37, size = 200, replace = TRUE), ] 
#' dolocurv <- haploAccum(doloHaplo, method = "random", permutations = 100)
#' 
#' plot(dolocurv)
#' plot(dolocurv, add = FALSE, ci = 2, ci.type = "polygon", col = "blue", ci.col = "red", 
#'     ci.lty = 1)
#' 
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics segments
#' @importFrom graphics matlines
#' @importFrom graphics polygon
#' @importFrom graphics lines
#' @export plot.haploAccum
plot.haploAccum <-
    function(x, add = FALSE, ci = 2, ci.type = c("bar","line","polygon"), 
             col = par("fg"), ci.col = col, ci.lty = 1, xlab,
             ylab = "Haplotypes", ylim, main = paste(x$method, "method of haplotype accumulation", sep=" "), ...)
{
    xaxvar <- x[["sequences"]]
    if (missing(xlab)) xlab <- "Sequences"
    ci.type <- match.arg(ci.type)
    if (!add) {
        if (missing(ylim))
            ylim <- c(1, max(x$n.haplotypes, x$n.haplotypes + ci*x$sd))
        plot(xaxvar, x$n.haplotypes, xlab=xlab, ylab=ylab, ylim=ylim,
             type="n", main = main, ...)
    }
    if (!is.null(x$sd) && ci)
        switch(ci.type,
               bar = segments(xaxvar, x$n.haplotypes - ci*x$sd, xaxvar,
                  x$n.haplotypes + ci*x$sd, col=ci.col, lty=ci.lty, ...),
               line = matlines(xaxvar, x$n.haplotypes + t(rbind(-ci,ci) %*% x$sd),
                 col=ci.col, lty=ci.lty, ...),
               polygon = polygon(c(xaxvar, rev(xaxvar)),
                 c(x$n.haplotypes - ci*x$sd, rev(x$n.haplotypes + ci*x$sd)), col=ci.col,
                 lty=ci.lty, main = main,  ...)
               )
    lines(xaxvar, x$n.haplotypes,col=col, ...)
    invisible()
}

