# cran-comments

## R CMD check results

0 errors | 0 warnings | 0 notes (local: R 4.6.0 on macOS arm64)

## Notes for reviewers

* The package bundles a ~1.9 MB gzipped CSV
  (`inst/extdata/ACMTermPremium_replicated_monthly_3m_120m.csv.gz`),
  the ACM term-structure dataset at monthly maturity steps. This may
  surface as an installed-size NOTE on some platforms; the file is the
  package's primary dataset and is already maximally compressed.
* `Depends: R (>= 4.5.0)` is required for `tools::sha256sum()`, used to
  verify downloaded data against the publishing release's digests.
* All download functionality is opt-in and fully mocked in tests; the
  test suite runs offline against the bundled data.
