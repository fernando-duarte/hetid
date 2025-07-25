# Utility Functions for hetid Scripts

This directory contains reusable utility functions that consolidate common patterns across the analysis scripts, following the DRY (Don't Repeat Yourself) principle.

## Files

### common_settings.R
- Sets up common directories and paths
- Defines shared parameters (plot dimensions, random seed, etc.)
- Sources all utility functions
- Uses package constants from hetid where available

### plotting_utils.R
Functions for consistent plot creation and saving:
- `save_plot_svg()` - Save plots in SVG format
- `apply_theme()` - Apply consistent theme to ggplot objects
- `save_correlation_heatmap()` - Create and save correlation heatmaps
- `create_time_series_plot()` - Create multi-series time series plots
- `display_and_save_plot()` - Display in RStudio viewer and save to disk

### stats_utils.R
Functions for statistical analysis and data validation:
- `compute_summary_stats()` - Compute comprehensive summary statistics
- `perform_stationarity_tests()` - Run ADF, KPSS, and Ljung-Box tests
- `create_formatted_table()` - Create publication-ready gt tables
- `create_interactive_table()` - Create interactive DT tables
- `check_data_completeness()` - Check for missing values

### hetero_test_utils.R
Functions for heteroskedasticity testing and diagnostics:
- `perform_all_hetero_tests()` - Run comprehensive heteroskedasticity tests
- `create_hetero_diagnostic_plots()` - Create diagnostic plots
- `summarize_hetero_tests()` - Summarize test results

## Usage

All scripts in the analysis pipeline automatically source these utilities through `common_settings.R`. To use in a new script:

```r
source(here::here("scripts/utils/common_settings.R"))
```

## Benefits

1. **Consistency**: All plots and tables have consistent formatting
2. **Maintainability**: Changes to formatting only need to be made in one place
3. **Reliability**: Common functionality is tested and debugged once
4. **Efficiency**: Reduces code duplication across scripts
5. **Clarity**: Scripts focus on analysis logic rather than formatting details
