#' Create illustrative barcodes
#' 
#' This function plots an illustrative barcode consisting of vertical bands in
#' four colours corresponding to the DNA bases adenine (A), cytosine (C),
#' guanine (G) and thiamine (T).
#' 
#' Green, blue, black and red are the standard colours representing A, G, C and
#' T respectively.
#' 
#' @param seq A single sequence of class `DNAbin'.
#' @param col A character vector of length 4 giving colours to represent A, G,
#' C and T respectively.
#' @return Plots an illustrative barcode.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Barcoding Utilities
#' @examples
#' 
#' graphics::layout(matrix(1:6, ncol=1))
#' graphics::par(mar=c(0.5, 0, 0.5, 0))
#' data(woodmouse)
#' seeBarcode(woodmouse[1,])
#' seeBarcode(woodmouse[1,], col=c("pink", "orange", "steelblue", "yellow"))
#' seeBarcode(woodmouse[1,], col=c("black", "white", "white", "black"))
#' apply(woodmouse[1:3,], MARGIN=1, FUN=seeBarcode)
#' 
#' @importFrom graphics plot
#' @importFrom graphics abline
#' @importFrom graphics layout
#' @importFrom graphics par
#' @export seeBarcode
seeBarcode <- 
function(seq, col=c("green", "blue", "black", "red")){
                if(!is.null(dim(seq))) if(dim(seq)[1] > 1) stop("Single sequences only please!")
                bases <- c(136, 40, 72, 24)
                pos <- 1:length(seq)
                ind <- match(as.numeric(seq), bases)
                plot(1, 1, xlim=c(0,max(pos)), ylim=c(0,1), xaxt="n", yaxt="n", xlab=NA, ylab=NA,
                                bty="n", type="n", main=dimnames(seq)[[1]])
                abline(v=pos, col=col[ind])
}
