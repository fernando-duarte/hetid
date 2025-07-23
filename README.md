
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hetid <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/fernando-duarte/hetid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fernando-duarte/hetid/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `hetid` package implements identification through heteroskedasticity
methods from Lewbel (2012) for triangular systems, with a focus on
estimating the Volatility Financial Conditions Index (VFCI).

## Installation

You can install the development version of hetid from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("fernando-duarte/hetid")
```

## Features

The package provides functions for:

- **Data Access**: Download and load Federal Reserve economic data
  - ACM term structure data (yields, term premia, risk-neutral yields)
  - Quarterly economic and financial variables
- **Bond Calculations**: Compute bond-related quantities
  - Expected log bond prices (n_hat)
  - Fourth moment estimators (k_hat)
  - Supremum estimators (c_hat)
  - Variance bounds
  - Price news and SDF innovations
- **Heteroskedasticity Identification**:
  - Compute reduced form residuals for primary and secondary endogenous
    variables
  - Solve quadratic equations for identification parameters

## Basic Usage

``` r
library(hetid)

# Download and load ACM data
download_term_premia()
data <- extract_acm_data(data_types = c("yields", "term_premia"))

# Compute reduced form residuals
res_y1 <- compute_reduced_form_residual_y1(n_pcs = 4)
res_y2 <- compute_reduced_form_residual_y2(
  yields = data[, grep("^y", names(data))],
  term_premia = data[, grep("^tp", names(data))],
  maturities = c(1, 2, 5),
  n_pcs = 4
)

# Solve identification quadratic
result <- solve_gamma_quadratic(
  pc_j = variables$pc1,
  w1 = res_y1$residuals,
  w2 = res_y2$residuals[[1]],
  tau = 0.5
)
```

## Analysis Scripts

The repository includes example scripts (not part of the package) for
comprehensive analysis:

1.  **`hetid_analysis_script.R`**: Complete analysis script that
    computes γ₁ roots for all combinations of principal components
    (j=1,…,J) and maturities (i=1,…,10)

2.  **`hetid_interactive_analysis.R`**: Interactive version allowing
    user to specify parameters and explore different configurations

3.  **`example_hetid_analysis.R`**: Simple demonstration of basic
    package functionality

To run the full analysis:

``` r
source("hetid_analysis_script.R")
```

Or for interactive exploration:

``` r
source("hetid_interactive_analysis.R")
```

## References

- Adrian, T., Crump, R. K., and Moench, E. (2013). “Pricing the term
  structure with linear regressions.” Journal of Financial Economics,
  110(1), 110-138.

- Lewbel, A. (2012). “Using heteroscedasticity to identify and estimate
  mismeasured and endogenous regressor models.” Journal of Business &
  Economic Statistics, 30(1), 67-80.

- Adrian, T., DeHaven, M., Duarte, F., and Iyer, T. “The Volatility
  Financial Conditions Index (VFCI).”
