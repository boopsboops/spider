[![Build Status](https://travis-ci.org/boopsboops/spider.svg?branch=master)](https://travis-ci.org/boopsboops/spider)
[![rstudio mirror](http://cranlogs.r-pkg.org/badges/grand-total/spider)](http://cran.rstudio.com/web/packages/spider/index.html)
[![cran version](http://www.r-pkg.org/badges/version/spider)](https://cran.r-project.org/package=spider)

# spider

---

The official GitHub repository for the R package "Species Identity and Evolution in R" (`spider`).

`spider` provides functions for the analysis of species limits and DNA barcoding data. Included are functions for generating important summary statistics from DNA barcode data, assessing specimen identification efficacy, testing and optimizing divergence threshold limits, assessment of diagnostic nucleotides, and calculation of the probability of reciprocal monophyly. Additionally, a sliding window function offers opportunities to analyse information across a gene, often used for marker design in degraded DNA studies. Further information on the package has been published in [Brown et al. (2012)](http://dx.doi.org/10.1111/j.1755-0998.2011.03108.x).

For an introduction to the package, visit our `spider` [tutorial](http://spider.r-forge.r-project.org/tutorial/tutorial.pdf) and [manual](http://spider.r-forge.r-project.org/docs/spider-manual.pdf).

Over time, the tutorial will we expanded and moved into GitHub vignettes and project pages. 

If you are interested in previous versions (before v1.5.0) of the `spider` source code, checkout our old repository hosted at [r-forge](https://r-forge.r-project.org/projects/spider/).

## Installation

Stable CRAN version (NOT YET WORKING).

```r
install.packages("spider")
```

Or development version from GitHub (WORKING).

```r
devtools::install_github("boopsboops/spider")
```

```r
library("spider")
```

## Examples

Here, we will do a quick "best close match" analysis on a dataset to see how well DNA barcodes can identify individuals in a simulated identification scenario.

Load up the _Anoteropsis_ wolf spider data.

```r
data(anoteropsis)
```

Make a quick species vector (a unique species name for each individual in the dataset) from the taxon labels.

```r
anoSpp <- sapply(strsplit(rownames(anoteropsis), split="_"), function(x) paste(x[1], x[2]))
head(anoSpp)
```

Get some statistics about the sequence lengths.

```r
seqStat(anoteropsis)
```

Load up `ape` and make a distance matrix from raw p-distances.

```r
library("ape")
anoDist <- dist.dna(anoteropsis, model="raw", pairwise.deletion=TRUE)
```

Calculate identification success with "best close match" of [Meier et al. (2006)](http://dx.doi.org/10.1080/10635150600969864).

```r
table(bestCloseMatch(distobj=anoDist, sppVector=anoSpp, threshold=0.01))
```

## Current contributors

* [Rupert A. Collins](https://github.com/boopsboops)
* [Samuel D. J. Brown](https://github.com/)
* [Stephane Boyer](https://github.com/)


## Meta

* Please [report any issues or bugs](https://github.com/boopsboops/spider/issues).
* License: MIT.
* Get citation information for `spider` in R doing `citation(package = 'spider')`.