## Submission of spider v1.5.2 (04-04-2026)

This release fixes a bug in `tclust()`.

### Changes

* Bug in `tclust` reported by killidude (https://github.com/boopsboops/spider/issues/7) fixed via pull request (https://github.com/boopsboops/spider/pull/8).
* Fixed URLs in `stats.BOLD.R`, `search.BOLD.R`, `read.BOLD.R` to use https.
* Updated `NEWS` and `DESCRIPTION`.

### Test environments

* Local: Ubuntu 22.04.5 LTS, R 4.5.3 via `R CMD check --as-cran` and `devtools::check(cran=TRUE)`
* GitHub Actions:
  * windows-latest (release)
  * ubuntu-latest (devel, release, oldrel-1)
  * macos-latest (release)

### R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES

### Downstream dependencies

* NicheBarcoding
