## Submission of spider v1.5.3 (13-07-2026)

This release fixes compatibility with R-devel changes to `aperm()` affecting `apply()`.

### Changes

* `seeBarcode()` edited to coerce input to a vector and to use a for loop rather than apply in the example.
* Updated `NEWS` and `DESCRIPTION`.

### Test environments

* Local: Ubuntu 22.04.5 LTS, R 4.6.1 and R 4.5.3 via `R CMD check --as-cran` and `devtools::check(cran=TRUE)`
* GitHub Actions:
  * windows-latest (release)
  * ubuntu-latest (devel, release, oldrel-1)
  * macos-latest (release)

### R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES

### Downstream dependencies

* NicheBarcoding
