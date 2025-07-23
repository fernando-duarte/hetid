# hetid Package Refactoring Plan

## Executive Summary

This document outlines a comprehensive refactoring plan for the `hetid` R package to eliminate code duplication, implement DRY (Don't Repeat Yourself) principles, and establish single sources of truth throughout the codebase. The plan is validated against modern R package development best practices and Context7 research.

## Current State Assessment

### 🔴 Major Issues Identified

#### Code Duplication Patterns
- **Input validation** repeated across 15+ functions (`i >= 1`, maturity bounds, `n_pcs` validation)
- **Data loading logic** duplicated in download/load functions (path resolution, error handling)
- **Mathematical operations** scattered (`/ 100` conversions, NA handling, data alignment)
- **Documentation examples** repeated across README.md, README.Rmd, and roxygen2 docs

#### Hardcoded Values
- Magic numbers: `1e6` (optimization penalty), `n_pcs = 4` (default)
- URL strings in download functions
- File paths and directory structures

#### Inconsistent Error Handling
- Different error message formats across functions
- Repeated try-catch patterns
- Inconsistent validation approaches

### 🟢 Existing Good Practices
- ✅ `compute_moments_unified.R` provides centralized moment calculations
- ✅ Modular validation functions (`validate_gamma_inputs.R`)
- ✅ Clear separation of concerns in some areas

## Refactoring Strategy

### Implementation Phases

#### **Phase 1: Core Infrastructure** 🚀 *[HIGH PRIORITY]*

**Objective**: Establish foundational single sources of truth

**Tasks**:

1. **Create Constants Module** (`R/constants.R`)
   ```r
   # Package-wide constants
   HETID_CONSTANTS <- list(
     DEFAULT_N_PCS = 4L,
     MAX_N_PCS = 6L,
     MIN_MATURITY = 1L,
     MAX_MATURITY = 10L,
     OPTIMIZATION_PENALTY = 1e6,
     PERCENTAGE_DIVISOR = 100,
     MACHINE_EPSILON = .Machine$double.eps
   )

   # Data source URLs
   DATA_URLS <- list(
     ACM_TERM_PREMIA = "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls",
     FED_SVENSSON = "https://www.federalreserve.gov/data/yield-curve-tables/feds200628.csv",
     FED_SMOOTHED = "https://www.federalreserve.gov/data/yield-curve-tables/feds200533.csv"
   )
   ```

2. **Unified Validation Framework** (`R/validation_framework.R`)
   ```r
   validate_common_inputs <- function(i = NULL, n_pcs = NULL, maturity = NULL,
                                      maturities = NULL, tau = NULL) {
     # Single source for all common validation logic
   }

   validate_data_inputs <- function(yields = NULL, term_premia = NULL,
                                    pc_data = NULL) {
     # Unified data validation with consistent error messages
   }
   ```

3. **Centralized Path Management** (`R/data_paths.R`)
   ```r
   get_package_data_dir <- function() {
     # Single source for package directory logic
   }

   get_data_file_path <- function(filename) {
     # Unified file path resolution
   }
   ```

**Expected Impact**: 40% reduction in validation code duplication

#### **Phase 2: Data Operations Consolidation** 📊 *[MEDIUM PRIORITY]*

**Objective**: Unify data loading, downloading, and processing operations

**Tasks**:

1. **Unified Download Framework** (`R/download_framework.R`)
   ```r
   download_data_file <- function(url, filename, file_type = c("csv", "excel"),
                                  force = FALSE, quiet = FALSE) {
     # Template method for all data downloads
     # Handles: URL validation, file existence, error handling, progress reporting
   }
   ```

2. **Standardized Data Loading** (`R/data_loading.R`)
   ```r
   load_package_data <- function(data_type = c("term_premia", "yield_curve"),
                                 dataset = NULL, auto_download = FALSE) {
     # Unified interface for all data loading operations
   }
   ```

3. **Common Data Processing Utilities** (`R/data_utils.R`)
   ```r
   convert_percentage_to_decimal <- function(data, columns = NULL) {
     # Single source for percentage conversion
   }

   align_time_series <- function(..., remove_na = TRUE) {
     # Unified data alignment logic
   }
   ```

**Expected Impact**: 60% reduction in data handling duplication

#### **Phase 3: Computation Standardization** 🧮 *[MEDIUM PRIORITY]*

**Objective**: Abstract common computational patterns

**Tasks**:

1. **Computation Framework** (`R/computation_framework.R`)
   ```r
   compute_financial_metric <- function(yields, term_premia, i,
                                        metric_type = c("c_hat", "k_hat", "n_hat")) {
     # Template method pattern for similar computations
     validate_computation_inputs(yields, term_premia, i)

     switch(metric_type,
       "c_hat" = compute_c_hat_impl(yields, term_premia, i),
       "k_hat" = compute_k_hat_impl(yields, term_premia, i),
       "n_hat" = compute_n_hat_impl(yields, term_premia, i),
       stop("Unknown metric type: ", metric_type)
     )
   }
   ```

2. **Unified Error Handling** (`R/error_handling.R`)
   ```r
   handle_computation_error <- function(error, context, fallback = NA) {
     # Standardized error handling across functions
   }
   ```

**Expected Impact**: 35% reduction in computation code duplication

#### **Phase 4: Optimization Framework** ⚡ *[LOWER PRIORITY]*

**Objective**: Standardize optimization setup and execution

**Tasks**:

1. **Abstract Optimization Setup** (`R/optimization_framework.R`)
2. **Standardize Multi-start Logic** (`R/multistart_optimization.R`)

**Expected Impact**: 25% reduction in optimization code duplication

#### **Phase 5: Documentation Consolidation** 📚 *[MAINTENANCE]*

**Objective**: Eliminate documentation duplication

**Tasks**:

1. **Documentation Templates** (`R/doc_templates.R`)
2. **Centralized References** (`R/references.R`)

**Expected Impact**: Consistent documentation, easier maintenance

## Implementation Guidelines

### Development Workflow

1. **Create feature branch** for each phase
2. **Implement incrementally** with comprehensive testing
3. **Maintain backward compatibility** during transition
4. **Use deprecation warnings** for changed interfaces
5. **Extensive unit tests** for new centralized functions

### Quality Assurance

```r
# Automated quality checks (following devtools patterns)
devtools::document()     # Update documentation
devtools::test()         # Run all tests
devtools::check()        # Full package check
lintr::lint_package()    # Code style checking
styler::style_pkg()      # Code formatting
goodpractice::gp()       # Best practices check
```

### File Organization

```
R/
├── hetid-package.R              # Main package documentation
├── constants.R                  # Package constants (Phase 1)
├── validation_framework.R       # Input validation (Phase 1)
├── data_paths.R                # Path management (Phase 1)
├── download_framework.R         # Data downloads (Phase 2)
├── data_loading.R              # Data loading (Phase 2)
├── data_utils.R                # Data utilities (Phase 2)
├── computation_framework.R      # Computations (Phase 3)
├── error_handling.R            # Error handling (Phase 3)
├── optimization_framework.R     # Optimization (Phase 4)
├── doc_templates.R             # Documentation (Phase 5)
└── utils.R                     # General utilities
```

## Success Metrics

### Quantitative Goals
- **40% reduction** in overall code duplication
- **100% test coverage** for new centralized functions
- **Zero breaking changes** to public API
- **Improved maintainability** scores from `goodpractice`

### Qualitative Goals
- **Consistent error messages** across all functions
- **Unified parameter validation** patterns
- **Single source of truth** for all constants and configurations
- **Improved developer experience** for package maintenance

## Risk Mitigation

### Technical Risks
- **Backward compatibility**: Use deprecation warnings, maintain old interfaces initially
- **Testing complexity**: Implement comprehensive test suites for each phase
- **Performance impact**: Profile critical paths, optimize if needed

### Process Risks
- **Scope creep**: Stick to defined phases, resist feature additions during refactoring
- **Timeline pressure**: Prioritize phases, can ship incrementally
- **Team coordination**: Clear documentation, regular progress reviews

## Timeline Estimate

- **Phase 1**: 2-3 weeks (Core Infrastructure)
- **Phase 2**: 3-4 weeks (Data Operations)
- **Phase 3**: 2-3 weeks (Computation)
- **Phase 4**: 1-2 weeks (Optimization)
- **Phase 5**: 1 week (Documentation)

**Total**: 9-13 weeks for complete refactoring

## Validation

This plan has been validated against:
- ✅ **Hadley Wickham's R Packages** best practices
- ✅ **devtools** development patterns
- ✅ **tidyverse** style guidelines
- ✅ Modern R package development standards
- ✅ DRY and SOLID principles

## Next Steps

1. **Review and approve** this refactoring plan
2. **Set up development environment** with quality tools
3. **Create feature branch** for Phase 1
4. **Begin implementation** with constants module
5. **Establish testing framework** for validation

---

*This refactoring plan follows modern R package development best practices and is designed to significantly improve code maintainability while preserving all existing functionality.*
