## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new submission (`estheRineq` 0.1.0).

## Test environments

* Local Windows 11 x64, R 4.6.0
* GitHub Actions:
  * `ubuntu-latest` (R-release, R-devel)
  * `windows-latest` (R-release)
  * `macos-latest` (R-release)
* CRAN Win-Builder (`R-devel`)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes for CRAN reviewers

* `estheRineq` uses zero external hard package `Imports` beyond base R (`grDevices`, `graphics`, `stats`, `utils`), keeping installation fast and self-contained across platforms.
* Proper names of methodological authors and epidemiological concepts in `DESCRIPTION` (`Gateaux`, `Wagstaff`, `Erreygers`, `Rao-Wu-Yue`, `Gini`, `Atkinson`, `Palma`, `Dobson`, `Byar`, `Pelotas`, `Spiegelhalter`, `Oaxaca`) are spelled intentionally and accompanied by `<doi:...>` references.
