
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
  risk-neutral yields (1-10 years)
- **Economic Variables**: Quarterly economic and financial data
- **Automatic Updates**: Download latest data from Federal Reserve
  sources
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

## Quick Start

### Basic Workflow

``` r
library(hetid)

# Data Setup: merge ACM yields with bundled PCs by quarter
download_term_premia()
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  frequency = "quarterly"
)
data("variables", package = "hetid")

# Year-quarter keys for date alignment
acm_data$yq <- paste0(
  format(acm_data$date, "%Y"), "-",
  quarters(acm_data$date)
)
variables$yq <- paste0(
  format(variables$date, "%Y"), "-",
  quarters(variables$date)
)

# Merge (covers quarters available in both datasets)
pc_cols <- paste0("pc", 1:4)
merged <- merge(
  variables[, c("yq", pc_cols)],
  acm_data[, c("yq", grep("^(y[0-9]|tp)",
    names(acm_data),
    value = TRUE
  ))],
  by = "yq"
)
pcs <- as.matrix(merged[, pc_cols])
yields <- merged[, grep("^y[0-9]", names(merged))]
tp <- merged[, grep("^tp", names(merged))]

# Compute Reduced Form Residuals
w1 <- compute_w1_residuals(n_pcs = 4)
w2 <- compute_w2_residuals(
  yields, tp,
  maturities = c(2, 5, 9),
  n_pcs = 4, pcs = pcs
)
```

### Bond Pricing Examples

``` r
# Extract ACM data
data <- extract_acm_data(data_types = c("yields", "term_premia"))
yields <- data[, grep("^y", names(data))]
term_premia <- data[, grep("^tp", names(data))]

# Compute expected log bond prices
n_hat_5 <- compute_n_hat(yields, term_premia, i = 5)

# Compute price news (unexpected price changes)
price_news_5 <- compute_price_news(yields, term_premia, i = 5)

# Compute SDF innovations
sdf_innovations_5 <- compute_sdf_innovations(yields, term_premia, i = 5)

# Compute moment estimators
c_hat_5 <- compute_c_hat(yields, term_premia, i = 5) # Supremum estimator
k_hat_5 <- compute_k_hat(yields, term_premia, i = 5) # Fourth moment estimator

# Compute variance bound
var_bound_5 <- compute_variance_bound(yields, term_premia, i = 5)
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

## Data Sources

The package provides access to:

- **ACM Term Structure Data**: Monthly data from Adrian, Crump, and
  Moench (2013) including yields, term premia, and risk-neutral yields
  for 1-10 year maturities
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
