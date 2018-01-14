[![Build Status](https://travis-ci.org/boopsboops/spider.svg?branch=master)](https://travis-ci.org/boopsboops/spider)
[![rstudio mirror](http://cranlogs.r-pkg.org/badges/grand-total/spider)](http://cran.rstudio.com/web/packages/spider/index.html)
[![cran version](http://www.r-pkg.org/badges/version/spider)](https://cran.r-project.org/package=spider)

# spider

## Overview

The official GitHub repository for the R package "SPecies IDentity and Evolution in R" (spider).

`spider` provides functions for the analysis of species limits and DNA barcoding data. Included are functions for generating important summary statistics from DNA barcode data, assessing specimen identification efficacy, testing and optimizing divergence threshold limits, assessment of diagnostic nucleotides, and calculation of the probability of reciprocal monophyly. Additionally, a sliding window function offers opportunities to analyse information across a gene, often used for marker design in degraded DNA studies. Further information on the package has been published in [Brown et al. (2012)](http://dx.doi.org/10.1111/j.1755-0998.2011.03108.x).

For an introduction to the package, visit our `spider` [tutorial](http://spider.r-forge.r-project.org/tutorial/tutorial.pdf) and [manual](http://spider.r-forge.r-project.org/docs/spider-manual.pdf). Over time, the tutorial will be expanded and moved into GitHub vignettes and project pages. 

If you are interested in previous versions (before v1.5.0) of the `spider` source code, check out our [old repository](https://r-forge.r-project.org/projects/spider/) hosted at r-forge.

## Installation

Stable CRAN version (NOT YET WORKING).

```r
install.packages("spider")
```

Or development version from GitHub (WORKING).

```r
devtools::install_github("boopsboops/spider")
```

## Examples

Here, we will do a quick "best close match" analysis [(Meier et al., 2006)](http://dx.doi.org/10.1080/10635150600969864) on a _Anoteropsis_ wolf spider dataset ([Vink & Paterson, 2003](http://dx.doi.org/10.1080/10635150600969864)) to see how well DNA barcodes can identify individuals in a simulated identification scenario.

```r
# load up the data
library("spider")
data(anoteropsis)
```

```r
# make a quick species vector (unique species name for each individual) from the taxon labels
anoSpp <- sapply(strsplit(rownames(anoteropsis), split="_"), function(x) paste(x[1], x[2]))
head(anoSpp)

#> [1] "Artoria flavimanus" "Artoria separata" "Anoteropsis adumbrata" "Anoteropsis adumbrata" "Anoteropsis aerescens" "Anoteropsis aerescens"
```

```r
# get some statistics about the sequence lengths
seqStat(anoteropsis)
```

```r
# make a distance matrix from raw p-distances
anoDist <- ape::dist.dna(anoteropsis, model="raw", pairwise.deletion=TRUE)
```

```r
# calculate identification success based on a 1% interspecific threshold
table(bestCloseMatch(distobj=anoDist, sppVector=anoSpp, threshold=0.01))

#>  correct incorrect     no id 
#>       11         2        20 
```

## Current contributors

* [Rupert A. Collins](https://github.com/boopsboops)
* [Samuel D. J. Brown](https://github.com/)
* [Stephane Boyer](https://github.com/)


## Meta

* Please [report here any issues or bugs or suggestions](https://github.com/boopsboops/spider/issues).
* License: MIT.
* Get citation information for `spider` in R by running `citation(package='spider')`.