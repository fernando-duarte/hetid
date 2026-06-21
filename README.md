
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hetid <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/fernando-duarte/hetid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fernando-duarte/hetid/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

The **hetid** package implements identification through
heteroskedasticity methods from Lewbel (2012) for triangular systems,
with applications to the **Volatility Financial Conditions Index
(VFCI)** developed by Adrian, DeHaven, Duarte, and Iyer.

This package provides tools for working with heteroskedasticity-based
identification methods:

- **Data Management**: Access to ACM term structure data
- **Bond Pricing**: Bond pricing calculations and moment estimators
- **Residual Computation**: Implementation of Lewbel (2012)
  identification methods

> **Breaking change (0.3.0): maturity indices are now months.** Every
> maturity argument and column suffix denotes months, not years:
> `i = 60` and `y60` are the 5-year bond, valid maturities run 3-120,
> and the old year-style names `y1`-`y5` no longer exist (they fail
> loudly). Note that `y6`-`y10` silently changed meaning: they now
> denote 6-10 **months**, where they used to denote 6-10 years.

## Key Methodology

The package implements the identification through heteroskedasticity
approach from Lewbel (2012), which exploits conditional
heteroskedasticity to identify structural parameters in triangular
systems:

    Y₁,t+1 = θ Y₂,t+1 + ε₁,t+1
    Y₂,t+1 = γ' Zₜ + ε₂,t+1

Identification is achieved through the moment condition:

    E[ε₁,t+1 ε₂,t+1 | Zₜ] = τ · Var(ε₂,t+1 | Zₜ)

This approach is particularly valuable when traditional instrumental
variables are unavailable or weak.

## Installation

You can install the development version of hetid from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("fernando-duarte/hetid")
```

## Core Features

### 📊 Data Management

- **ACM Term Structure Data**: Monthly yields, term premia, and
  risk-neutral yields at one-month maturity steps from 3 to 120 months
- **Economic Variables**: Quarterly economic and financial data
- **Verified Updates**: `download_term_premia()` fetches the latest
  GitHub release and verifies it against the release’s sha256 digest
  before caching; the official NY Fed workbook remains available as the
  opt-in `source = "nyfed"` fallback (annual maturities only)
- **Data Validation**: Built-in checks for data consistency and
  completeness

### 🧮 Bond Pricing Calculations

- **Expected Log Bond Prices** (`compute_n_hat`): Estimate
  E_t\[p\_(t+i)^(1)\]
- **Price News** (`compute_price_news`): Unexpected bond price changes
- **SDF Innovations** (`compute_sdf_innovations`): Stochastic discount
  factor innovations
- **Moment Estimators**: Supremum (`compute_c_hat`) and fourth moment
  (`compute_k_hat`) estimators
- **Variance Bounds** (`compute_variance_bound`): Empirical bounds for
  forecast errors

### 🔍 Identification Methods

- **Reduced Form Residuals**: Compute W₁ and W₂ residuals for
  identification
- **Multi-maturity Analysis**: Simultaneous estimation across the yield
  curve

#### Generalized instruments

Any collection of time-series instruments can drive the identification:
supply them (or transformations of them) to the moments step, then build
constraints from one or more linear combinations per endogenous
component — or from every instrument separately.

``` r
library(hetid)
#> Data availability:
#>   * ACM term premia: Available (updated 2026-06-21)
#> 
#> Use load_term_premia() to access the data.
set.seed(42)
w1 <- rnorm(100)
w2 <- matrix(rnorm(200), nrow = 100)
z <- matrix(rnorm(300),
  nrow = 100,
  dimnames = list(NULL, c("a", "b", "c"))
)

instruments <- build_instrument_matrix(
  z,
  transforms = list(a_sq = function(z) z[, "a"]^2)
)
moments <- compute_identification_moments(w1, w2, instruments)

# every instrument separately: one constraint per (component, instrument)
all_sep <- build_general_quadratic_system(
  separate_instruments_lambda(moments),
  tau = 0.2, moments
)
nrow(all_sep$labels)
#> [1] 8

# two combinations for component one, one for component two
lambda <- list(
  cbind(c(1, 0, 0, 0), c(0, 1, 1, 0)),
  matrix(c(1, 1, 0, 1), ncol = 1)
)
mixed <- build_general_quadratic_system(lambda, tau = 0.2, moments)
mixed$labels
#>   constraint maturity combo               name
#> 1          1        1     1 maturity_1_combo_1
#> 2          2        1     2 maturity_1_combo_2
#> 3          3        2     1         maturity_2
```

Set widths are comparable only across schemes with the same number of
constraints, and width-minimizing optimized weights are a computational
benchmark, not a confidence statement; optimized weights are reported
under the spec’s variance normalization
$\lambda^\top \widehat{\mathrm{Var}}(Z)\lambda = 1$.

## Quick Start

### Basic Workflow

``` r
library(hetid)

# Data Setup: merge ACM yields with bundled PCs by quarter
download_term_premia()
mats <- seq(12, 120, by = 12)
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  maturities = mats,
  frequency = "quarterly"
)
data("variables", package = "hetid")

# ACM and the bundled PCs share the period-end date convention, so they
# merge directly by calendar date (covers dates available in both datasets)
pc_cols <- paste0("pc", 1:4)
merged <- merge(
  variables[, c("date", pc_cols)],
  acm_data[, c("date", paste0("y", mats), paste0("tp", mats))],
  by = "date"
)
pcs <- as.matrix(merged[, pc_cols])
yields <- merged[, paste0("y", mats)]
tp <- merged[, paste0("tp", mats)]

# Compute Reduced Form Residuals
w1 <- compute_w1_residuals(n_pcs = 4)
w2 <- compute_w2_residuals(
  yields, tp,
  maturities = c(24, 60, 108),
  n_pcs = 4, pcs = pcs, dates = merged$date
)
```

### Bond Pricing Examples

``` r
# Extract ACM data
data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- data[, paste0("y", seq(12, 120, 12))]
term_premia <- data[, paste0("tp", seq(12, 120, 12))]

# Compute expected log bond prices (i = 60 is the 5-year bond)
n_hat_60 <- compute_n_hat(yields, term_premia, i = 60, dates = data$date)

# Compute price news (unexpected price changes)
price_news_60 <- compute_price_news(yields, term_premia, i = 60, dates = data$date)

# Compute SDF innovations
sdf_innovations_60 <- compute_sdf_innovations(yields, term_premia, i = 60, dates = data$date)

# Compute moment estimators
c_hat_60 <- compute_c_hat(yields, term_premia, i = 60) # Supremum estimator
k_hat_60 <- compute_k_hat(yields, term_premia, i = 60) # Fourth moment estimator

# Compute variance bound
var_bound_60 <- compute_variance_bound(yields, term_premia, i = 60)
```

## Mathematical Background

The identification strategy exploits heteroskedasticity in a triangular
system:

    Y₁,t+1 = θ Y₂,t+1 + ε₁,t+1
    Y₂,t+1 = γ' Zₜ + ε₂,t+1

The key insight from Lewbel (2012) is that identification can be
achieved through heteroskedasticity-based moment conditions.

## Function Reference

### Data Functions

- `download_term_premia()` - Download ACM term structure data
- `extract_acm_data()` - Extract and process ACM data
- `load_term_premia()` - Load cached ACM data

### Bond Pricing Functions

- `compute_n_hat()` - Expected log bond prices
- `compute_price_news()` - Unexpected bond price changes
- `compute_sdf_innovations()` - SDF innovations
- `compute_c_hat()` - Supremum estimator
- `compute_k_hat()` - Fourth moment estimator
- `compute_variance_bound()` - Variance bounds

### Identification Functions

- `compute_w1_residuals()` - Primary endogenous variable residuals
- `compute_w2_residuals()` - Secondary endogenous variable residuals
- `compute_identification_moments()` - All seven identification moments
- `build_instrument_matrix()` - Construct instrument matrices with
  optional transformations
- `build_general_quadratic_system()` - Quadratic constraints from
  arbitrary instrument combinations
- `separate_instruments_lambda()` - Per-instrument identity weight
  matrices
- `make_system_checker()` - Closure for evaluating quadratic constraint
  satisfaction

## Data Sources

The package provides access to:

- **ACM Term Structure Data**: Monthly data based on Adrian, Crump, and
  Moench (2013) including yields, term premia, and risk-neutral yields
  at one-month maturity steps from 3 to 120 months. The bundled file is
  the validated replication published at
  [fernando-duarte/ACM_term_premium](https://github.com/fernando-duarte/ACM_term_premium)
  (it matches the official NY Fed workbook to within 0.0026 basis points
  at the annual nodes and extends the maturity grid to monthly steps);
  downloads are verified against the release’s sha256 digest. The
  official NY Fed workbook is the opt-in `source = "nyfed"` fallback and
  carries annual maturities only.
- **Economic Variables**: Quarterly economic and financial variables
  including GDP, inflation, financial conditions indices, and principal
  components

## References

- Adrian, T., Crump, R. K., and Moench, E. (2013). “Pricing the term
  structure with linear regressions.” *Journal of Financial Economics*,
  110(1), 110-138.

- Adrian, T., DeHaven, M., Duarte, F., and Iyer, T. (2024). “The
  Volatility Financial Conditions Index.” *Working Paper*.

- Lewbel, A. (2012). “Using heteroscedasticity to identify and estimate
  mismeasured and endogenous regressor models.” *Journal of Business &
  Economic Statistics*, 30(1), 67-80.

## License

MIT © Fernando Duarte
