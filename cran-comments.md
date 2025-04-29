## Submission of spider (29-04-2025)

* Links added to package anchors for all Rd \link{} targets not in the package itself and the base packages
* Authors list in DESCRIPTION changed to "Authors@R" format, added URL, BugReports, Encoding fields
* Update NEWS
* Tidy README
* Bump version to 1.5.1

## Notes

* This is release at request of CRAN to fix errors with missing package anchors in Rd files. 

## Test environments

* local Ubuntu 22.04.5 LTS, R 4.4.1 via devtools::check()
* windows-latest (release) via GitHub Actions https://github.com/boopsboops/spider/actions
* ubuntu-latest (devel) via GitHub Actions https://github.com/boopsboops/spider/actions
* ubuntu-latest (release) via GitHub Actions https://github.com/boopsboops/spider/actions
* macos-latest (release) via GitHub Actions https://github.com/boopsboops/spider/actions
* ubuntu-latest (oldrel-1) via GitHub Actions https://github.com/boopsboops/spider/actions

## R CMD check results

0 ERRORs | 0 WARNINGs | 0 NOTES.

## Downstream dependencies

* NicheBarcoding
