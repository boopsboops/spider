## Submission of spider (07-05-2025)

This release addresses issues requested by CRAN related to missing package anchors in Rd files.

### Changes

* Added links to package anchors for all Rd `\link{}` targets referencing external or base packages.
* Converted `Author` field to `Authors@R`; added `URL`, `BugReports`, and `Encoding` fields to DESCRIPTION.
* Updated `CITATION` to use `bibentry()` format.
* Updated `NEWS`, `LICENSE`, and tidied `README`.

### Test environments

* Local: Ubuntu 22.04.5 LTS, R 4.4.1 via `R CMD check --as-cran` and `devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)`
* Win-builder (R-release)
* macOS (release) via `devtools::check_mac_release()`
* GitHub Actions:
  * windows-latest (release)
  * ubuntu-latest (devel, release, oldrel-1)
  * macos-latest (release)

### R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* NOTE: A non-standard file `cran-comments.md` was found at the top level. This file is included intentionally to provide CRAN with submission context, as per CRAN submission best practices.

### Downstream dependencies

* NicheBarcoding
