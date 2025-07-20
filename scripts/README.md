# hetid Scripts Directory

This directory contains analysis scripts and function tests for the hetid package.

## Structure

- **Main analysis scripts** (01-05): Complete workflows for heteroskedasticity identification analysis
- **hetid_analysis_enhanced.R**: Interactive comprehensive analysis with all features
- **tests/**: Individual function tests and data visualization

## Main Analysis Scripts

### 01_basic_analysis.R
Basic heteroskedasticity identification analysis computing gamma1 roots for all PC-maturity combinations.

### 02_heteroskedasticity_tests.R
Focused on Breusch-Pagan tests and correlation analysis between PCs and squared residuals.

### 03_optimization_analysis.R
Finds optimal linear combinations of PCs that minimize root distance.

### 04_visualization_gallery.R
Creates comprehensive visualizations from saved results.

### 05_summary_statistics.R
Generates detailed summary statistics and diagnostics.

### hetid_analysis_enhanced.R
Complete interactive analysis combining all features above. Best for RStudio use.

## Test Scripts (tests/)

### Data Visualization
- **test_acm_data_visualization.R**: Visualize and summarize ACM term structure data

### Function Tests
Each test script demonstrates one function with default parameters:

#### Bond Calculations
- **test_compute_n_hat.R**: Expected log bond price estimator
- **test_compute_k_hat.R**: Fourth moment estimator
- **test_compute_c_hat.R**: Supremum estimator
- **test_compute_variance_bound.R**: Variance bounds for bond returns

#### SDF Calculations
- **test_compute_price_news.R**: Price news (unexpected bond price changes)
- **test_compute_sdf_innovations.R**: SDF innovations with convexity adjustment

#### Residual Computations
- **test_compute_reduced_form_residual_y1.R**: Consumption growth residuals
- **test_compute_reduced_form_residual_y2.R**: SDF innovation residuals

#### Quadratic Solvers
- **test_solve_gamma_quadratic.R**: Basic gamma1 quadratic solver
- **test_solve_gamma_quadratic_lincomb.R**: Solver using PC linear combinations

#### Optimization
- **test_optimize_pc_weights.R**: Single maturity optimization
- **test_optimize_pc_weights_all_maturities.R**: Multi-maturity optimization

#### Data Functions
- **test_extract_acm_data.R**: ACM data extraction with filtering
- **test_download_functions.R**: Data download functions

## Usage

### Running Analysis Scripts
```r
# Set working directory to package root
setwd("path/to/hetid")

# Run basic analysis
source("scripts/01_basic_analysis.R")

# Or run enhanced interactive analysis
source("scripts/hetid_analysis_enhanced.R")
```

### Running Function Tests
```r
# Test a specific function
source("scripts/tests/test_compute_n_hat.R")

# Run all tests (example)
test_files <- list.files("scripts/tests", pattern = "^test_.*\\.R$",
                        full.names = TRUE)
for (file in test_files) {
  cat("\n\nRunning:", basename(file), "\n")
  source(file)
}
```

## Parameters

All main scripts have user parameters at the top of the file:
- `J`: Number of principal components (1-6)
- `tau`: Quantile parameter (0-1)
- `save_plots`: Whether to save plots as PDFs
- `output_dir`: Directory for output files

## Output

Scripts generate:
- Console output with results
- PDF plots (if `save_plots = TRUE`)
- RDS files with numerical results
- Summary text files

## Notes

- The enhanced script (`hetid_analysis_enhanced.R`) is best for interactive use in RStudio
- Scripts 01-05 are designed to be run independently with parameters set at the top
- Test scripts demonstrate individual functions and can be used to verify correct installation
