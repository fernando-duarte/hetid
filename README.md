
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
  - Federal Reserve yield curve data
  - Quarterly economic and financial variables
- **Bond Calculations**: Compute bond-related quantities
  - Expected log bond prices (n_hat)
  - Fourth moment estimators (k_hat)
  - Supremum estimators (c_hat)
  - Variance bounds
  - SDF news and innovations
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

## Analysis Script

The repository includes a comprehensive analysis script (not part of the
package) that demonstrates all package functionality:

**`hetid_analysis_enhanced.R`**: Complete analysis script that includes:
- Computation of γ₁ roots for all combinations of principal components
  (j=1,…,J) and maturities (i=1,…,10)
- Breusch-Pagan heteroskedasticity tests for W_{2,i} residuals
- Correlation calculations |corr(PC_j, W²_{2,i})| for all combinations
- Optimization of PC weights to minimize root distance
- Enhanced visualizations including:
  - Heteroskedasticity test p-values
  - Correlation heatmaps
  - Root plots for both individual PCs and optimal linear combinations

To run the analysis:

``` r
source("hetid_analysis_enhanced.R")
```

The script allows you to specify:
- `J`: Number of principal components to use (1-6)
- `tau`: Quantile parameter (0-1)

## References

- Adrian, T., Crump, R. K., and Moench, E. (2013). “Pricing the term
  structure with linear regressions.” Journal of Financial Economics,
  110(1), 110-138.

- Lewbel, A. (2012). “Using heteroscedasticity to identify and estimate
  mismeasured and endogenous regressor models.” Journal of Business &
  Economic Statistics, 30(1), 67-80.

- Adrian, T., DeHaven, M., Duarte, F., and Iyer, T. “The Volatility
  Financial Conditions Index (VFCI).”
