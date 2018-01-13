seqStat <- function(DNAbin, thresh = 500){
    cd <- checkDNA(DNAbin, gapsAsMissing = TRUE)
    rr <- sapply(DNAbin, length) - cd
    tab <- table(NULL)
    tab[1:5] <- c(min(rr), max(rr), mean(rr), median(rr), length(which(rr < thresh)))
    names(tab) <- c("Min", "Max", "Mean", "Median", "Thresh")
    tab <- round(tab, digits = 0)
    return(tab)
}
