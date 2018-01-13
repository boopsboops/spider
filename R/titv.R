#' Number of pairwise transitions and transversions in an alignment.
#' 
#' Calculates the number of pairwise transitions and transversions between
#' sequences.
#' 
#' 
#' @param DNAbin A DNA alignment of class `DNAbin'.
#' @return A square matrix with dimensions of \code{length(dat)}. The upper
#' triangle contains the number of transversions. The lower triangle contains
#' the number of transitions.
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @keywords Utilities
#' @examples
#' 
#' data(dolomedes)
#' 
#' subs <- titv(dolomedes)
#' 
#' #Transversions
#' subs[upper.tri(subs)]
#' tv <- t(subs)
#' tv <- tv[lower.tri(tv)]
#' 
#' #Transitions
#' ti <- subs[lower.tri(subs)]
#' 
#' 
#' #Saturation plot
#' doloDist <- ape::dist.dna(dolomedes)
#' graphics::plot(doloDist, ti, type="p", pch=19, col="blue", 
#'     main="Saturation plot of number of transitions and transversions\n
#'     against K2P distance. Red: transversions. Blue: transitions")
#' graphics::points(doloDist, tv, pch=19, col="red")
#' 
#' 
#' @importFrom ape dist.dna
#' @importFrom graphics plot
#' @importFrom graphics points
#' @export titv
titv <-
function(DNAbin){
mat<-as.matrix(DNAbin)
res<-matrix(NA, ncol=dim(mat)[1], nrow=dim(mat)[1], dimnames=list(x=names(DNAbin), y=names(DNAbin)))
for(i in 1:(dim(mat)[1] - 1)){
for(j in (i+1):dim(mat)[1]){
vec<-as.numeric(mat[i,])+as.numeric(mat[j,])-8
res[j,i]<-sum(!is.na(match(vec,c(200,56))))#Transitions
res[i,j]<-sum(!is.na(match(vec,c(152,168,88,104))))#Transversions
}
}
res
}

