#' Species monophyly over a tree
#' 
#' Determines if the species given in \code{sppVector} form monophyletic groups
#' on a given tree.
#' 
#' \code{monophyly} determines if each species is monophyletic.
#' \code{monophylyBoot} incorporates a bootstrap test to determine the support
#' for this monophyly. Species with a bootstrap support lower than
#' \code{"thresh"} are recorded as FALSE.
#' 
#' Rerooting is done on the longest internal edge in the tree returned by
#' \code{nj(dist.dna(DNAbin))}.
#' 
#' @param phy A tree of class `phylo'.
#' @param sppVector Species vector. See \code{\link{sppVector}}
#' @param pp Object of class `prop.part'. Assists in speeding up the function,
#' if it has been called already. Default of NA, calling
#' \code{\link[ape]{prop.part}} internally.
#' @param singletonsMono Logical. Should singletons (i.e. only a single
#' specimen representing that species) be treated as monophyletic? Default of
#' TRUE. Possible values of FALSE and NA.
#' @param DNAbin An object of class 'DNAbin'. Required for calculating
#' bootstrap values.
#' @param thresh Numeric between 0 and 1. Bootstrap threshold under which
#' potentially monophyletic species are negated. Default of 0.7.
#' @param reroot Logical. Should the bootstrap replicates be rerooted on the
#' longest edge? Default of TRUE.
#' @param reps Numeric. Number of bootstrap replications. Default of 1000.
#' @param block The number of nucleotides that will be resampled together.
#' Default of 3 to resample on the codon level.
#' @return \code{monophyly} returns a logical vector, stating if each species
#' is monophyletic. Values correspond to the species order given by
#' \code{unique(sppVector)}.
#' 
#' \code{monophylyBoot} returns a list with the following elements:
#' \item{results}{A logical vector, stating if each species is monophyletic
#' with a bootstrap support higher than the given threshold.} \item{BSvalues}{A
#' numeric vector giving the bootstrap proportions for each node of
#' \code{phy}.}
#' @author Samuel Brown <s_d_j_brown@@hotmail.com>
#' @seealso \code{\link[ape]{prop.part}}, \code{\link[ape]{root}},
#' \code{\link[ape]{boot.phylo}}, \code{\link{monophyly}}.
#' @examples
#' 
#' #Random trees
#' set.seed(16)
#' tr <- ape::rtree(15)
#' spp <- rep(LETTERS[1:5], rep(3,5))
#' monophyly(tr, spp)
#' 
#' tr2 <- tr
#' spp2 <- c(rep(LETTERS[1:4], rep(3,4)), LETTERS[5:7])
#' monophyly(tr2, spp2)
#' 
#' #Empirical data
#' \dontrun{
#' data(anoteropsis)
#' anoTree <- ape::nj(ape::dist.dna(anoteropsis))
#' anoSpp <- sapply(strsplit(dimnames(anoteropsis)[[1]], split="_"), 
#'     function(x) paste(x[1], x[2], sep="_"))
#' 
#' monophyly(anoTree, anoSpp)
#' monophyly(anoTree, anoSpp, singletonsMono=FALSE)
#' unique(anoSpp)
#' 
#' #To get score for each individual
#' anoMono <- monophyly(anoTree, anoSpp)
#' anoMono[match(anoSpp, unique(anoSpp))]
#' 
#' data(woodmouse)
#' woodTree <- ape::nj(ape::dist.dna(woodmouse))
#' woodSpp <- c("D", "C", "C", "A", "A", "E", "A", "F", "C", "F", "E", "D", "A", "A", "E")
#' unique(woodSpp)
#' monophyly(woodTree, woodSpp)
#' woodMono <- monophylyBoot(woodTree, woodSpp, woodmouse)
#' woodMono$results
#' woodMono$BSvalues
#' 
#' monophylyBoot(woodTree, woodSpp, woodmouse, reroot = FALSE)
#' monophylyBoot(woodTree, woodSpp, woodmouse, thresh = 0.9, reroot = FALSE)
#' }
#' 
#' @importFrom ape nj
#' @importFrom ape dist.dna
#' @importFrom ape boot.phylo
#' @importFrom ape root
#' @importFrom ape rtree
#' @importFrom ape prop.part
#' @export monophylyBoot
monophylyBoot <- 
function (phy, sppVector, DNAbin, thresh = 0.7, reroot = TRUE, pp = NA, singletonsMono = TRUE, reps = 1000, block = 3) 
{
    res <- list()
    xxx <- lapply(unique(sppVector), function(y) which(sppVector == 
        y))
    if(reroot){
	testTr <- nj(dist.dna(DNAbin))
	maxInt <- max(testTr$edge.length[testTr$edge[,2] > length(testTr$tip.label)])
	nodeRoot <- testTr$edge[which(testTr$edge.length == maxInt), 2]
	boot <- boot.phylo(phy, DNAbin, function(x) root(nj(dist.dna(x)), node = nodeRoot, resolve.root=TRUE), B = reps, block = block)/reps
	} else boot <- boot.phylo(phy, DNAbin, function(x) nj(dist.dna(x)), B = reps, block = block)/reps
    sppTab <- sapply(xxx, length)
    singletons <- which(sppTab == 1)
    nonSingletons <- which(sppTab != 1)
    ifelse(is.na(pp), yyy <- prop.part(phy), yyy <- pp)
    zzz <- sapply(yyy, length)
    defNon <- which(!sppTab %in% zzz)
    poss <- which(sppTab %in% zzz)
    bb <- lapply(sppTab, function(x) boot[which(zzz == x)])
    tt <- lapply(sppTab, function(x) which(zzz == x))
    for(i in poss){
	res[i] <- NA
	for(j in 1:length(tt[[i]])){
		res[[i]][j] <- sum(as.numeric(!xxx[[i]] %in% yyy[[ tt[[i]][j] ]]))
		}
	}
    bc <- sapply(res, function(x) which(x == 0))
    bootCheck <- sapply(1:length(bc), function(x) bb[[x]][bc[[x]][1]])
    out <- sapply(res, function(x) as.logical(sum(as.numeric(x < 
        1))))
    if(is.list(out)) out <- rep(singletonsMono, length(singletons))
    out[defNon] <- FALSE
    out[singletons] <- singletonsMono
    out[bootCheck < thresh] <- FALSE
    store <- list(results= out, BSvalues = boot)
    print(out)
    invisible(store)
}
