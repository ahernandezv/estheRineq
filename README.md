# estheRineq

**Epidemiological Statistics Toolkit for Health Inequality Estimation and Reporting in R**

[![R-CMD-check](https://github.com/ahernandezv/estheRineq/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahernandezv/estheRineq/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://ahernandezv.github.io/estheRineq/)
[![License: GPL v3+](https://img.shields.io/badge/License-GPLv3+-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R >= 4.1](https://img.shields.io/badge/R-%3E%3D%204.1.0-276DC3.svg)](https://cran.r-project.org/)

`estheRineq` is a zero-dependency-core scientific R library (and companion package to Python [`esther-ineq`](https://pypi.org/project/esther-ineq/)) uniting **complex-survey health econometrics** (World Bank *Analyzing Health Equity*, Stata `conindex` / `lorenz` / `oaxaca_rif`, R `convey` / `rineq`) and **epidemiological health inequality surveillance & reporting** (WHO Health Inequality Monitor `healthequal` / HEAT, Pelotas ICEH `equiplot`, UKHSA `PHEindicatormethods`, Moreno-Betancur GLMs, and **STROBE-Equity 2025**).

---

## Key Capabilities (`v0.1.0`)

1. **Dual First-Class S3 Data Containers (`survey_health_data` & `grouped_health_data`):**
   - Zero external hard `Imports` beyond base R (`stats`, `graphics`, `grDevices`, `grid`, `utils`, `methods`), with automatic seamless support for `survey::svydesign`, `survey::svyrep.design`, and `ggplot2` when installed.
   - Full complex multistage survey design support: probability weights (`weights`), stratification (`strata`), primary sampling units (`psu`), finite population correction (`fpc`), lonely-PSU handling, survey replicate weights (`BRR`, `Fay`, `JK1`, `JKn`, `Bootstrap`), and unconditional domain/subpopulation comparisons (`by = ...`).
2. **Exact Influence-Function (`IF` / `RIF`) Variance Kernel:**
   - Default `vce = "if"` accounts jointly for outcome variance, mean $\hat{\mu}_h$ denominator variance, and fractional rank $\hat{R}_i$ uncertainty in $O(N \log N)$ time, alongside `vce = "convenient"` and `vce = "newey"` modes for 1-to-1 Stata `conindex` replication.
3. **Complete Concentration & Classical Inequality Suite:**
   - Standard (`CI`), Generalized (`GC` / `ACI`), Modified (`MC`), **Wagstaff (`W`)**, **Erreygers (`E`)**, **Extended (`v`)**, and **Symmetric (`beta`)** concentration indices with tie-exact interval integration + omnibus subgroup Wald $F$-tests.
   - Classical `Gini`, `S-Gini`, `Atkinson` (+ Equally Distributed Equivalent `EDE`), `Generalized Entropy` (`MLD`, `Theil`, and Shorrocks between/within subgroup decomposition), `Palma`, and `S80/S20`.
4. **All 21 WHO Health Inequality Monitor (`healthequal` / HEAT) Summary Measures:**
   - Simple (`d`, `r`), Ordered (`aci`, `rci`, `sii`, `rii`), Non-ordered (`bgv`, `bgsd`, `covar`, `mdmw`, `mdmu`, `mdbw`, `mdbu`, `mdrw`, `mdru`, `idisw`, `idisu`, `ti`, `mld`), and Population Impact (`par`, `paf`) on both disaggregated tables and complex surveys.
5. **Modern Epidemiological `SII` & `RII` Engine:**
   - Survey-weighted logistic G-computation marginal standardization (`method = "logit_marginal"`), Moreno-Betancur identity-link + modified Poisson (`method = "moreno_betancur"`), and WLS/Monte Carlo on surveillance tables, with age/stratum-specific ridits.
6. **Epidemiological & Need Standardization (`DSR`, `SMR`, `ISR`, and Horizontal Inequity `HI`):**
   - Directly and indirectly standardized rates with exact Dobson / Byar Poisson-Gamma CIs and built-in WHO (2000–2025), Segi (1960), and European (2013) standard populations.
   - Gravelle-consistent linear and nonlinear indirect need standardization (`HI`) with PSU-level Rao–Wu–Yue survey bootstrap SEs.
7. **Regression & Counterfactual Decompositions:**
   - **Wagstaff, van Doorslaer & Watanabe (2003)** linear & GLM (`AME`/`MEM`) decomposition with **automatic reference-invariant factor grouping** across multi-category factors.
   - **Bivariate `RIF-I-OLS`** (Heckley, Gerdtham & Kjellsson, 2016) and **`RIF-Oaxaca`** (Ríos-Ávila, 2020, including IPW propensity reweighting).
8. **Editorial Visualizations & Trilingual `STROBE-Equity` Reporting (`en`, `es`, `pt`):**
   - Pelotas `equiplot` (microdata, grouped, and Stata-style wide columns with automatic *top* vs. *bottom inequality* pattern diagnostics and row sorting), Concentration/Lorenz curves with simultaneous confidence bands, SII/RII ridit bubble gradients, Decomposition forest plots, and Spiegelhalter `funnel_plot`s.
   - Direct export to `data.frame`, `Markdown`, `LaTeX` (`booktabs`), `HTML`, `Excel (.xlsx)`, and `Word (.docx)` with automated **STROBE-Equity (2025)** methodological notes.

---

## Installation

```r
# Install from GitHub:
# install.packages("remotes")
remotes::install_github("ahernandezv/estheRineq")
```

---

## Quick Start

```r
library(estheRineq)

# 1. Load built-in DHS/ENDES-style complex survey dataset
df <- load_survey_mch()

svy <- survey_health_data(
  data = df,
  weights = "sample_weight",
  strata = "stratum_id",
  psu = "cluster_id"
)

# 2. Erreygers concentration index across urban vs. rural domains + Wald F-test
res_ci <- concentration_index(
  svy,
  outcome = "stunting",
  rank = "wealth_score",
  method = "erreygers",
  by = "area",
  favorable = FALSE
)
print(res_ci) # Default English; pass lang = "es" or lang = "pt" if needed

# 3. Epidemiological SII & RII + LaTeX booktabs table & STROBE-Equity note
res_sii <- sii_rii(
  svy,
  outcome = "full_immunization",
  rank = "wealth_quintile",
  favorable = TRUE,
  scale = 100
)
cat(to_latex(res_sii))

# 4. Reference-invariant Wagstaff decomposition
decomp <- decompose_wagstaff(
  svy,
  formula = stunting ~ mother_edu + area + improved_sanitation + child_age_mo,
  rank = "wealth_score",
  method = "erreygers",
  bootstrap_reps = 200
)
plot(decomp)

# 5. Pelotas equiplot directly from survey microdata (or wide subgroup columns)
equiplot(
  svy,
  outcome = "full_immunization",
  subgroup = "wealth_quintile",
  by = "region",
  scale = 100,
  diagnose_pattern = TRUE,
  sort_by = "gap"
)
```

---

## Methodological Credits & References

- **ICEH Pelotas Equiplot & Inequality Patterns:** International Center for Equity in Health (ICEH), Federal University of Pelotas (`equidade.org/equiplot`; Barros & Victora).
- **WHO Health Inequality Monitor:** Schlotheuber & Hosseinpoor (2022), *International Journal of Environmental Research and Public Health* (`healthequal` / HEAT).
- **Rank-Dependent Concentration Indices & Decompositions:** O'Donnell, van Doorslaer, Wagstaff & Lindelow (2008); O'Donnell, O'Neill, Van Ourti & Walsh (2016, `conindex`); Heckley, Gerdtham & Kjellsson (2016); Ríos-Ávila (2020, `oaxaca_rif`).
- **Epidemiological SII, RII, and Reporting:** Moreno-Betancur et al. (2015); Welch et al. (2025, `STROBE-Equity`).

## Citation

```r
citation("estheRineq")
```

```bibtex
@manual{hernandez_vasquez_estherineq_2026,
  author = {Hern{\'a}ndez-V{\'a}squez, Akram},
  title  = {{estheRineq: Epidemiological Statistics Toolkit for Health Inequality Estimation and Reporting in R}},
  year   = {2026},
  note   = {R package version 0.1.0},
  url    = {https://github.com/ahernandezv/estheRineq}
}
```

## Acknowledgments

This work was supported in part by complimentary access to Google Antigravity and Gemini 4 Argon to accelerate scientific workflows.

## License

Distributed under the **GNU General Public License v3.0 or later (`GPL (>= 3)`)**.
