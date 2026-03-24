# hetid Package Analysis Report

**Date:** 2026-03-23
**Package:** hetid v0.1.0
**Scope:** Full codebase analysis -- architecture, code quality, performance, testing, domain
correctness, numerical stability
**Files analyzed:** 27 R source files (2,775 lines), 17 test files, 47 man pages, NAMESPACE,
DESCRIPTION

---

## Executive Overview

**hetid** is a well-architected, early-stage (v0.1.0) pure-R econometric package implementing
Lewbel (2012) heteroskedasticity-based identification for the VFCI (Volatility Financial
Conditions Index). The codebase exhibits unusually strong discipline for a research package:
clean three-layer architecture (data, computation, and identification), thorough roxygen2
documentation with LaTeX formulas, minimal dependencies (only `stats` and `utils`), and 16 test
files including mathematical correctness verification.

R CMD check passes cleanly with 0 notes, 0 warnings, 0 errors.

### Key Risks (ordered by severity)

The analysis identified **4 confirmed correctness defects** (all 4 now fixed) and **8 code
quality / performance items** (4 now fixed, 1 rejected):

**Confirmed defects (medium severity):**

1. ~~**Subset-maturity indexing bug** in `compute_identified_set_quadratic()`~~ **FIXED** in
   commit `98ef719`. Changed `s_i_1` and `s_i_2` to use position indexing (`idx`), added input
   length validation, improved `maturities=NULL` fallback, and added regression test.
2. ~~**Incomplete maturity validation**~~ **FIXED** in commit `80564d7`. Consolidated all
   maturity validation into `validate_maturity_index()` with integrality check (`i %% 1 != 0`)
   and function-specific upper bounds (`max_maturity = MAX_MATURITY - 1` for functions needing
   `i+1` columns). Removed redundant `validate_maturity_param()` and `compute_n_hat_validated()`
   wrappers. Added regression tests for non-integer and out-of-range inputs across all compute
   functions.
3. ~~**Unguarded division by zero** in identified-set quadratic form~~ **FIXED** in commit
   `55bf368`. Added vectorized pre-loop validation rejecting non-finite/non-positive `sigma_i_sq`
   (reports all offending maturities at once), plus post-division finiteness check on `d_i` to
   catch overflow from tiny positive values. Added 7 test blocks covering zero, NA/NaN/Inf,
   negative, multiple-bad, subset maturities, small-positive acceptance, and overflow boundary.
4. ~~**Silent truncation of mismatched inputs**~~ **FIXED** in commit `98ef719`. The function
   now validates that all statistical inputs match `length(maturities)` and checks all 7 input
   lengths in the `maturities=NULL` fallback.

**Code quality and performance items (low severity):**

5. ~~Inconsistent validation dispatch (two overlapping maturity validators)~~ **FIXED** in
   commit `80564d7`
6. ~~Three for-loop vectorization opportunities~~ **FIXED** in commit `e5653be`
7. ~~Implicit data coupling via silent package data loading~~ **FIXED** (2026-03-24, message/warning on fallback, W2 deduplication)
8. ~~Over-abstraction in `apply_time_series_transform()`~~ **REJECTED** (keeping for future reuse)
9. ~~Mathematically simplifiable V_i computation~~ **FIXED** (see below)
10. ~~Deeply nested date parsing in `load_term_premia()`~~ **FIXED** (see below)
11. ~~Missing unit tests for utility functions~~ **FIXED** (2026-03-24, added tests for `build_acm_col_mapping` and `data_paths`)
12. ~~Full `lm` object returned from `compute_w1_residuals()`~~ **REJECTED** (keeping for future development)

**Additional findings discovered during adversarial plan review (2026-03-23):**

13. ~~W2 `return_df` date alignment bug with interior NA rows~~ **FIXED** (2026-03-23)
14. ~~Positional splice between bundled PCs and ACM yields~~ **FIXED** (2026-03-24, warning + docs)
15. ~~Missing `ncol(pcs)` validation against `n_pcs`~~ **FIXED** (2026-03-24)
16. ~~Missing `dates` length validation in W2~~ **CLOSED** (user-supplied dates validated; bundled dates inherently match bundled PCs since both come from same `load_w2_pcs()` call)
17. ~~`require_date_column` inflexibility in W1 when `return_df = FALSE`~~ **FIXED** (2026-03-24, date column now optional unless `return_df = TRUE`)
18. ~~Examples and README teach the broken default path~~ **FIXED** (2026-03-24, examples now show year-quarter merge workflow with explicit `pcs=`)
19. Material R-squared impact from positional splice: ~0.20 vs ~0.11 for maturity 5 (documented)

**Additional findings discovered during adversarial plan review of finding 18 (2026-03-24):**

20. ~~W2 `dates` param docs use wrong shared template~~ **FIXED** (2026-03-24)
21. ~~Test grep pattern in W2 merge test matches merge key column~~ **FIXED** (2026-03-24)
22. ~~`README.html` generation not covered by `devtools::build_readme()`~~ **FIXED** (2026-03-24, removed from git)
23. ~~Pre-existing `maturities = c(2, 5, 10)` bug in old examples~~ **FIXED** (2026-03-24, as part of finding 18)

None of the defects affect the package's correctness when used with its bundled ACM data via the
documented workflow. They would surface when the identification-layer functions are called
directly with non-standard inputs. Items 13-17 were discovered during adversarial review of the
implicit data coupling plan and are documented in detail below.

---

## Scores

| Area               | Score   | Notes                                                        |
|--------------------|---------|------------------------------------------------------------- |
| Architecture       | 8/10    | Clean data, computation, identification pipeline             |
| Code Quality       | 7/10    | Consistent style, all files under 200 lines; indexing bug    |
| Testing            | 8/10    | Good coverage with boundary and non-integer maturity tests   |
| Performance        | 8/10    | Inner loops vectorized; adequate at current scale            |
| Domain Correctness | 9/10    | Formulas verified correct; all validation gaps fixed         |
| Documentation      | 9/10    | Excellent roxygen2 with LaTeX math, parameter templates      |
| Numerical Stability| 8/10    | Zero-division guarded; post-division overflow check added    |

---

## Architecture

### Three-Layer Design

The package follows a clean three-layer architecture:

```
Data Layer                    Computation Layer                  Identification Layer
---                           ---                                ---
download_term_premia()        compute_n_hat()                    compute_scalar_statistics()
       |                           |                             compute_vector_statistics()
       v                           v                             compute_matrix_statistics()
load_term_premia()            compute_price_news()                        |
       |                           |                                      v
       v                           v                             compute_identified_set_components()
extract_acm_data()            compute_sdf_innovations()                   |
                                    |                                      v
                              compute_c_hat()                    compute_identified_set_quadratic()
                              compute_k_hat()
                              compute_variance_bound()
                              compute_w1_residuals()
                              compute_w2_residuals()
```

**Supporting layers:**

- `validation_utils.R` -- shared input validation
- `computation_utils.R` -- shared computation helpers
- `data_paths.R` -- path management for external data
- `build_acm_col_mapping.R` -- column name mapping
- `convert_to_quarterly.R` -- frequency conversion
- `constants.R` -- centralized constants (`HETID_CONSTANTS`, `DATA_URLS`)

### Architecture Strengths

- **Function-per-file organization** works well at this scale (27 files, max 197 lines)
- **Minimal dependency footprint**: only `stats` and `utils` as Imports; `readxl` suggested
- **Clean NAMESPACE** with 18 explicit exports and explicit `importFrom` declarations
- **Composable API**: each function is focused and can be used independently or chained

### Architecture Weaknesses

- No high-level convenience function chains the full identification pipeline (scalar stats +
  vector stats + matrix stats + components + quadratic form); users must call 5 functions in
  sequence
- The computation and identification layers have different conventions for maturity indexing:
  computation functions take a single `i`, while identification functions take matrices/lists
  indexed by maturity position, creating an impedance mismatch

---

## Confirmed Defects

### ~~Defect: Subset-Maturity Indexing Bug in Quadratic Form~~ FIXED

**Severity:** Medium
**File:** `compute_identified_set_quadratic.R`
**Status:** Fixed in commit `98ef719` (2026-03-23)

**What was fixed:**
- Changed `s_i_1[[i]]` and `s_i_2[[i]]` to `s_i_1[[idx]]` and `s_i_2[[idx]]`, making all
  statistical inputs use consistent position-based indexing (only `tau` remains maturity-indexed,
  as it is always full-length)
- Added input length validation: all 7 statistical inputs must match `length(maturities)`
- Improved `maturities=NULL` fallback to check all 7 input lengths (was only checking 3)
- Improved error messages to include both maturity ID and position index
- Split test file (259 lines) into two files under 200 lines each
- Fixed existing subset test to pass subsetted `s_i_1[maturities]` and `s_i_2[maturities]`
- Added regression test comparing batch subset results against individual maturity computations
- Added validation test for mismatched input lengths

---

### ~~Defect: Incomplete Maturity Validation~~ FIXED

**Severity:** Medium
**Files:** `validation_utils.R`, `computation_utils.R`, `compute_n_hat.R`, `compute_c_hat.R`,
`compute_price_news.R`, `compute_sdf_innovations.R`, `compute_variance_bound.R`,
`validate_w2_inputs.R`
**Status:** Fixed in commit `80564d7` (2026-03-23)

**What was fixed:**
- Added integrality check (`i %% 1 != 0`) to `validate_maturity_index()` so non-integer values
  like `i = 1.5` produce a clear "must be an integer" error instead of a confusing column-not-found
  error downstream
- Added function-specific upper bounds: functions needing `y(i+1)` columns (`compute_n_hat`,
  `compute_c_hat`, `compute_price_news`, `compute_sdf_innovations`, `compute_variance_bound`) now
  pass `max_maturity = HETID_CONSTANTS$MAX_MATURITY - 1` (effective max = 9). `compute_k_hat`
  retains the default max = 10 since it only needs `n_hat(i-1)`
- Eliminated `validate_maturity_param()` (minimal validator with no type/upper-bound checks) and
  `compute_n_hat_validated()` (became a pure pass-through wrapper). All call sites now use
  `validate_maturity_index()` directly
- Updated `validate_w2_inputs()` maturity cap from `MAX_MATURITY` to `MAX_MATURITY - 1`
- Added `@note` documentation to all max-9 functions warning about the effective upper bound
- Added `test-validation_utils.R` with comprehensive validator edge-case tests (non-integer,
  out-of-range, invalid types, custom max_maturity, valid inputs)
- Added smoke error tests (`i = 1.5`, `i = 10`) to all 6 compute function test files
- Verified `compute_k_hat(i = 10)` succeeds (regression test for the legitimate max-10 case)

---

### ~~Defect: Unguarded Division by Zero in Quadratic Form~~ FIXED

**Severity:** Medium
**File:** `compute_identified_set_quadratic.R`
**Status:** Fixed in commit `55bf368` (2026-03-23)

**What was fixed:**
- Added vectorized pre-loop validation (`which(!is.finite(sigma_i_sq) | sigma_i_sq <= 0)`)
  that rejects non-finite, non-positive `sigma_i_sq` values before any computation begins,
  reporting all offending maturities in a single error message
- Added post-division finiteness check on `d_i` inside the loop to catch overflow when
  `sigma_i_sq` is positive but extremely small (e.g., 1e-309), which passes the pre-loop guard
  but overflows the division to `Inf`
- Updated `@param sigma_i_sq` roxygen documentation to state the positivity requirement
- Added 7 test blocks: zero value, NA/NaN/Inf/negative values, multiple bad maturities at once,
  subset maturities (verifying original maturity indices in error), small-but-positive acceptance
  (regression test against false positives), and overflow-boundary case

**Design decisions (vetted through 3-round adversarial review):**
- Uses `<= 0` threshold instead of `.Machine$double.eps` because `sigma_i_sq` is computed from
  4th powers of residuals and its scale varies enormously; an absolute epsilon would false-positive
  on legitimately small-scale data
- Uses `!is.finite()` to catch NA/NaN/Inf which would otherwise produce cryptic base R errors
- Vectorized pre-loop check reports all bad maturities at once (fail-fast) rather than one at a
  time inside the loop
- Two-layer defense: pre-loop validates the input contract, post-division catches numerical
  overflow that isn't an input error

---

### ~~Defect: Silent Truncation of Mismatched Inputs~~ FIXED

**Severity:** Medium
**File:** `compute_identified_set_quadratic.R`
**Status:** Fixed in commit `98ef719` (2026-03-23)

**What was fixed:**
- `maturities=NULL` fallback now checks all 7 input lengths (was only 3)
- Added explicit length validation after maturities resolution: all statistical inputs must
  match `length(maturities)`, with a descriptive error message on mismatch
- Added `expect_error` test for mismatched input lengths

---

## Code Quality and Performance Findings

### ~~Finding: For-Loop Vectorization Opportunities~~ FIXED

**Severity:** Low (future-proofing; not a current bottleneck at 243 observations)
**Files:** `computation_utils.R`, `compute_k_hat.R`, `compute_vector_statistics.R`
**Status:** Fixed in commit `e5653be` (2026-03-23)

**What was fixed:**
- `compute_vector_statistics()`: Replaced inner for-loop over maturities with a single BLAS call
  (`t(pcs) %*% (w2 * w2_i) / T_obs`). The column broadcast `w2 * w2_i` and matrix multiply
  produce the entire J x I result in one operation.
- `compute_k_hat()`: Replaced scalar accumulation loop with vectorized shifted-index computation
  using `mean()` over valid (non-NA) elements.
- `compute_time_series_news()`: Replaced element-wise loop with vectorized subtraction
  (`future_series[2:n_obs] - current_series[1:(n_obs - 1)]`). NAs propagate naturally via R's
  NA arithmetic, producing identical behavior to the explicit NA-checking loop.

All 578 tests pass after vectorization, confirming exact equivalence with the loop-based
implementations.

---

### ~~Finding: Implicit Data Coupling via Package Data Loading~~ FIXED

**Severity:** Low
**Files:** `compute_w1_residuals.R:65-68`, `validate_w2_inputs.R:56-59`,
`compute_w2_residuals.R:117-127`

Several core functions silently load the `variables` package dataset when certain parameters are
NULL, creating hidden coupling that makes the dependency graph opaque.

**Evidence:**

- `compute_w1_residuals()` (line 65-68): loads `variables` when `data = NULL`
- `compute_w2_residuals()` via `load_w2_pcs()` (`validate_w2_inputs.R:56-59`): loads `variables`
  when `pcs = NULL`
- `compute_w2_residuals()` (line 117-118): loads `variables` **again** in the `return_df = TRUE`
  path when `dates = NULL`

This means `compute_w2_residuals()` can load `variables` **twice** per call (once for PCs, once
for dates), and the caller has no visibility into this. A user who provides `yields` and
`term_premia` from a different dataset may unknowingly get PCs from the bundled ACM dataset,
creating a silent data mismatch.

**Recommendation:** Make data loading explicit by requiring PCs and dates as parameters (no
silent fallback) or centralizing the fallback into a single, well-documented factory function
that constructs the full input set. At minimum, emit a `message()` when falling back to
package data so users are aware.

**Status:** **FIXED** (2026-03-24). Implemented per plan at
`docs/superpowers/plans/2026-03-23-explicit-data-loading.md`:
- `compute_w1_residuals()` now emits a `message()` when falling back to bundled data
- `load_w2_pcs()` now emits a `warning()` (stronger than `message()` because positional alignment
  is silently incorrect) and returns bundled dates alongside PCs, eliminating the double load
- When user provides custom PCs without dates, falls back to row indices (not bundled dates)
- Commits: `5d8674c`, `7ce6f9b`

---

### ~~Finding: W2 `return_df` Date Alignment Bug with Interior NA Rows~~ FIXED

**Severity:** Medium
**Files:** `compute_w2_residuals.R:137`, `validate_w2_inputs.R:138`
**Status:** Fixed (2026-03-23) -- `process_w2_maturity()` now returns `kept_idx`, used for correct date selection via `dates[which(kept)]`
**Discovered by:** Codex (adversarial critique) and Codex (brainstorming), independently confirmed

`compute_w2_residuals(..., return_df = TRUE)` constructs its output data frame using
`date = dates[1:n_obs]` at line 137. This assumes that any dropped rows are trailing. However,
`process_w2_maturity()` (in `validate_w2_inputs.R:138`) uses `complete.cases(sdf_innov, pcs_lagged)`
to filter rows, which can drop **interior** rows if the PCs or SDF innovations have interior NAs.

When interior rows are dropped, `n_obs` is smaller than the original time dimension, but
`dates[1:n_obs]` takes the first `n_obs` consecutive dates rather than the dates corresponding
to the rows that survived `complete.cases()`. This means the returned dates are misaligned with
the actual residuals — each residual is labeled with the wrong date.

**Evidence:** Verified by injecting an interior NA into a custom PCs matrix and calling
`compute_w2_residuals(..., return_df = TRUE)`. The resulting data frame had consecutive dates
even though one interior row was dropped, confirming the misalignment.

**Impact:** Affects any user calling `compute_w2_residuals(..., return_df = TRUE)` with data
containing interior NAs. With the bundled ACM dataset (which has no NAs), this bug is dormant.
It would surface with user-provided data that has gaps.

**Recommendation:** Have `process_w2_maturity()` return the `complete_idx` boolean vector
alongside its existing return values. In the `return_df` branch of `compute_w2_residuals()`,
use `dates[which(complete_idx)]` instead of `dates[1:n_obs]` to select the correct dates.

---

### ~~Finding: Positional Splice Between Bundled PCs and ACM Yields~~ FIXED

**Severity:** Medium
**Files:** `validate_w2_inputs.R:57-77`, `compute_w2_residuals.R`
**Status:** Fixed (2026-03-24) -- upgraded `message()` to `warning()` with positional alignment explanation, strengthened `@details` documentation
**Discovered by:** Codex (adversarial critique and brainstorming)

When `compute_w2_residuals()` falls back to bundled PCs (`pcs = NULL`), it loads
`variables[, pc_cols]` and pairs them with the user-provided `yields` and `term_premia`
**by row position**, not by calendar date. The bundled `variables` dataset starts at
`1962-01-01` (243 quarterly observations), while quarterly ACM data starts at `1961-06-30`
(257 quarterly observations). This means:

- Row 1 of bundled PCs corresponds to `1962-Q1`
- Row 1 of quarterly ACM yields corresponds to `1961-Q2`
- The fallback silently pairs data from different quarters

The `process_w2_maturity()` function then aligns PCs and SDF innovations by taking the
shorter of the two, which means the first ~14 rows of ACM data (1961-Q2 through 1964-Q3)
are paired with PCs from a different time period.

**Impact:** This positional mismatch means that the default call
`compute_w2_residuals(yields, term_premia)` (without explicit PCs) produces regressions
where the regressors (PCs) are from different quarters than the dependent variable
(SDF innovations computed from yields/term_premia). The regression results are still
statistically valid in a time-series sense (both series are ordered correctly), but the
economic interpretation assumes contemporaneous alignment that doesn't hold for the
first ~14 observations.

**Recommendation:** Either:
- Align bundled PCs to yields by date (merge on year-quarter) before running regressions
- Document the positional-splice behavior explicitly in the `@details` section
- Require users to provide PCs explicitly when using non-bundled yields data (the
  `message()` plan partially addresses this by making the fallback visible)

---

### ~~Finding: Missing Validation of `ncol(pcs)` Against `n_pcs`~~ FIXED

**Severity:** Low
**Files:** `validate_w2_inputs.R:78-87`
**Status:** Open -- discovered during adversarial plan review (2026-03-23)
**Discovered by:** Codex (adversarial critique and brainstorming)

`load_w2_pcs()` validates `nrow(pcs) == n_obs` for user-provided PCs but does not validate
that `ncol(pcs) >= n_pcs`. If a user provides a PCs matrix with fewer columns than `n_pcs`
(e.g., a 2-column matrix with `n_pcs = 4`), the function proceeds without error.

The failure surfaces later in `process_w2_maturity()` when the regression formula references
`pc3` and `pc4` columns that don't exist in `reg_data`, producing an opaque error:
`object 'pc3' not found`.

**Evidence:** Calling `compute_w2_residuals(yields, tp, n_pcs = 4, pcs = matrix(1:20, ncol = 2))`
produces `Error in eval(predvars, data, env) : object 'pc3' not found` deep inside `lm()`.

**Recommendation:** Add `ncol(pcs) >= n_pcs` validation in `load_w2_pcs()` immediately after
the `nrow` check:

```r
if (ncol(pcs) < n_pcs) {
  stop(
    "User-provided pcs has ", ncol(pcs), " columns but n_pcs = ", n_pcs,
    ". Provide at least ", n_pcs, " columns."
  )
}
```

Note: The existing `validate_n_pcs()` utility in `validation_utils.R:61` could also be
called here to validate the `n_pcs` parameter itself (range 1-6), which neither
`compute_w1_residuals()` nor `compute_w2_residuals()` currently uses.

---

### Finding: Missing Validation of `dates` Length in W2

**Severity:** Low
**Files:** `compute_w2_residuals.R:137`
**Status:** Open -- discovered during adversarial plan review (2026-03-23)
**Discovered by:** Codex (adversarial critique and brainstorming)

When a user provides an explicit `dates` vector with `return_df = TRUE`, its length is never
validated against the expected number of residual observations. The code at line 137 does
`date = dates[1:n_obs]`, which:

- If `dates` is shorter than `n_obs`: silently produces `NA` dates via out-of-bounds indexing
- If `dates` is longer than `n_obs`: silently truncates without warning

Neither case produces an informative error. Compare with `prepare_return_data()` in
`computation_utils.R:79-81` which explicitly validates:
```r
if (length(dates) != nrow(yields)) {
  stop("Length of dates must match number of rows in yields")
}
```

**Recommendation:** Add a length check before the data frame construction loop:

```r
if (!is.null(dates) && length(dates) < max_expected_n_obs) {
  stop("dates has ", length(dates), " elements but at least ",
       max_expected_n_obs, " are needed")
}
```

where `max_expected_n_obs` is derived from the yields dimensions. Alternatively, adopt the
`prepare_return_data()` pattern of requiring exact length match.

---

### ~~Finding: `require_date_column` Inflexibility in W1~~ FIXED

**Severity:** Low (design consideration)
**Files:** `compute_w1_residuals.R:72`
**Status:** Open -- discovered during brainstorming review (2026-03-23)
**Discovered by:** Gemini Pro (brainstorming)

`compute_w1_residuals()` unconditionally requires a `"date"` column in the input data
(line 72: `required_cols <- c("date", ...)`), even when `return_df = FALSE`. When a user
passes a custom numeric data frame without a date column just to get the residuals list,
the function errors unnecessarily.

The `dates` field IS returned in the list output (line 134: `dates = dates_clean`), so there
is a reason for requiring it. However, when `return_df = FALSE`, the user may not care about
dates and just wants the residuals vector.

**Recommendation:** Consider making the `"date"` column optional when `return_df = FALSE`:

```r
required_cols <- if (return_df) {
  c("date", HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, paste0("pc", 1:n_pcs))
} else {
  c(HETID_CONSTANTS$CONSUMPTION_GROWTH_COL, paste0("pc", 1:n_pcs))
}
```

And set `dates_clean` to `NULL` or generic indices when the column is absent. This is a
minor usability improvement for users working with non-standard data frames.

---

### Finding: Examples and README Teach the Broken Default Path

**Severity:** Medium
**Files:** `compute_w2_residuals.R:42-58`, `hetid-package.R:59`, `README.Rmd:87`
**Status:** Open -- discovered during adversarial review of positional splice plan (2026-03-24)
**Discovered by:** Codex (adversarial critique)

The documented examples for `compute_w2_residuals()`, the package-level help page
(`hetid-package.R`), and `README.Rmd` all show calls to `extract_acm_data()` without
specifying `frequency = "quarterly"`. Since `extract_acm_data()` defaults to
`frequency = "monthly"`, the documented workflow produces monthly ACM data (769 rows)
paired with quarterly bundled PCs (243 rows), using only 242 observations after
truncation in `process_w2_maturity()`.

This means the documented default workflow:
```r
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia")
)
yields <- acm_data[, grep("^y", names(acm_data))]
term_premia <- acm_data[, grep("^tp", names(acm_data))]
res_y2 <- compute_w2_residuals(yields, term_premia)
```

silently discards ~527 of 769 monthly observations and pairs the remaining 242
with quarterly PCs from a different time grid.

**Recommendation:** Update all examples and README to either:
- Specify `frequency = "quarterly"` in `extract_acm_data()` calls
- Show the proper year-quarter merge workflow with explicit `pcs=`
- Or both: show the quick quarterly path and the proper aligned path

---

### Finding: Material R-squared Impact from Positional Splice

**Severity:** Documented (informational)
**Files:** `compute_w2_residuals.R`, `validate_w2_inputs.R`
**Status:** Documented -- measured during adversarial review (2026-03-24)
**Discovered by:** Codex (adversarial critique)

During adversarial review of the positional splice fix, Codex measured the
quantitative impact of the misalignment. For maturity 5 on current quarterly
ACM data:

- **Positional alignment** (bundled PCs by row position): R-squared ~ 0.1962
- **Date-aligned** (merged by year-quarter): R-squared ~ 0.1097

This is a ~78% relative difference in R-squared, confirming that the positional
splice produces materially different regression results, not just a cosmetic
mismatch. The warning added in commit `55439c2` makes this issue visible to
users, and the `@details` documentation explains how to fix it.

---

### ~~Finding: W2 `dates` Param Docs Use Wrong Shared Template~~ FIXED

**Severity:** Low
**Files:** `man-roxygen/param-return-df-dates.R`, `R/compute_w2_residuals.R:10`
**Status:** Open -- discovered during adversarial review of finding 18 plan (2026-03-24)
**Discovered by:** Codex (brainstorming)

The shared roxygen template `man-roxygen/param-return-df-dates.R` says: "If not provided and
return_df = TRUE, will use row indices." This is accurate for `compute_n_hat()`,
`compute_price_news()`, and `compute_sdf_innovations()`, which all fall back to row indices.
However, `compute_w2_residuals()` has function-specific fallback behavior: when `pcs = NULL`
(bundled PCs), it prefers bundled dates from the `variables` dataset
(`compute_w2_residuals.R:139-141`), only falling back to row indices when the user provides
custom PCs without dates.

Since the template is shared across four functions (`compute_n_hat.R:8`, `compute_price_news.R:8`,
`compute_sdf_innovations.R:9`, `compute_w2_residuals.R:10`), the correct fix is to add a
W2-specific `@param dates` override in `compute_w2_residuals.R` rather than modifying the shared
template.

---

### ~~Finding: Test Grep Pattern in W2 Merge Test Matches Merge Key Column~~ FIXED

**Severity:** Low
**File:** `tests/testthat/test-compute_w2_residuals.R:148`
**Status:** Open -- discovered during adversarial review of finding 18 plan (2026-03-24)
**Discovered by:** Codex (brainstorming)

The "R-squared matches manual regression" test at line 148 uses
`grep("^(y|tp)", names(acm_data), value = TRUE)` to select yield and term premia columns
from the merged data frame. This pattern also matches the `year_quarter` merge key column
(which starts with "y"). The test still works because having a duplicate column in the merge
`c()` selector is harmless -- R includes the column twice and `merge()` uses the first occurrence.
However, this is inconsistent with the tightened `grep("^(y[0-9]|tp)", ...)` pattern used in the
fixed examples and could confuse future developers reading the test as a reference implementation.

---

### ~~Finding: `README.html` Generation Not Covered by `devtools::build_readme()`~~ FIXED

**Severity:** Low
**File:** `README.html`, `README.Rmd`
**Status:** Open -- discovered during adversarial review of finding 18 plan (2026-03-24)
**Discovered by:** Codex (brainstorming)

`README.html` is tracked in git, but `devtools::build_readme()` sets `html_preview = FALSE`
internally (in `devtools:::build_rmd_impl`), so it only generates `README.md`. Developers using
`devtools::build_readme()` as their standard workflow will not regenerate `README.html`, which
will silently become stale over time. To regenerate both files, use
`rmarkdown::render("README.Rmd")` directly (which respects the default `html_preview = TRUE`
for `github_document` output format). Consider either removing `README.html` from version control
or documenting the correct render command.

---

### Finding: Pre-Existing `maturities = c(2, 5, 10)` Bug in Old Examples

**Severity:** Low
**Files:** `R/hetid-package.R:66`, `README.Rmd:94` (old versions, now fixed)
**Status:** Fixed as part of finding 18 (2026-03-24)
**Discovered by:** Codex (code review)

The pre-existing "Typical Workflow" examples in `hetid-package.R` and `README.Rmd` used
`maturities = c(2, 5, 10)` in the `compute_w2_residuals()` call. However,
`validate_w2_inputs()` caps maturities at `MAX_MATURITY - 1 = 9` (because `compute_w2_residuals`
needs maturity `i+1` for SDF innovations). Maturity 10 was silently dropped with a warning,
meaning the documented workflow never actually computed results for all listed maturities.
Fixed to `c(2, 5, 9)` as part of the finding 18 plan.

---

### Finding: Over-Abstraction in `apply_time_series_transform()` -- REJECTED

**Severity:** Low
**File:** `computation_utils.R:157-179`
**Status:** Rejected -- keeping the generic interface for anticipated future reuse.

The generic `apply_time_series_transform()` accepts an arbitrary `transform_fn` and applies it
element-by-element with NA handling via a for-loop. Its only current call site is
`compute_sdf_innovations.R:76-78`, but the abstraction is intentionally general to support
future time series transformations.

---

### ~~Finding: V_i Computation Is Mathematically Simplifiable~~ FIXED

**Severity:** Low
**File:** `compute_identified_set_components.R`
**Status:** Fixed (2026-03-23)

**What was fixed:**
- Replaced the 3-step V_i computation (outer product + quadratic form) with the algebraically
  equivalent `crossprod(gamma_i, p_i_0_vec)^2`. By associativity,
  `gamma^T (p p^T) gamma = (gamma^T p)^2`, eliminating the intermediate J x J matrix allocation.
- Numerical equivalence verified with random inputs before and after.

---

### ~~Finding: Deeply Nested Date Parsing in `load_term_premia()`~~ FIXED

**Severity:** Low
**File:** `load_term_premia.R`
**Status:** Fixed (2026-03-23)

**What was fixed:**
- Replaced 3-level nested `tryCatch` (20 lines) with a loop over a list of date formats
  (12 lines). Each format is tried with `tryCatch`; the first that produces non-NA results wins.
- The original analysis proposed `c(format1, NULL, format2)` but `c()` drops NULLs; the
  implementation uses `list()` to preserve the NULL (no-format) case, with an `if/else` to call
  `as.Date` without `format` when `fmt` is NULL.
- Also fixes a latent bug: the original code's `as.Date(x, format = ...)` returns NAs on format
  mismatch (does not error), so the nested `tryCatch` would silently assign a column of NAs.
  The new code checks `!all(is.na(parsed))` before committing the assignment.

---

### Finding: Missing Unit Tests for Utility Functions

**Severity:** Low (partially resolved)
**Files:** `tests/testthat/`

`convert_to_quarterly()` now has a dedicated test file (`test-convert_to_quarterly.R`) with 5
tests covering: complete quarters, one missing month with warning, two missing months with
warning, complete quarters without warning, and year-boundary handling. A public API regression
test was also added to `test-extract_acm_data.R` verifying the warning surfaces through
`extract_acm_data(end_date = ..., frequency = "quarterly")`. The function itself was updated
to warn (with `call. = FALSE`) when any quarter uses a non-terminal month.

The following functions still lack dedicated unit tests:

| Function                  | File                     | Risk Level |
|---------------------------|--------------------------|------------|
| `build_acm_col_mapping()` | `build_acm_col_mapping.R`| Medium     |
| `get_package_data_dir()`  | `data_paths.R`           | Low        |
| `get_data_file_path()`    | `data_paths.R`           | Low        |
| `check_data_file_exists()`| `data_paths.R`           | Low        |
| `get_acm_data_path()`     | `data_paths.R`           | Low        |

These are tested indirectly via integration tests (e.g., `test-cross-function-consistency.R`
calls `extract_acm_data()` which calls `build_acm_col_mapping()`), but lack edge-case unit
tests. `build_acm_col_mapping()` is the most important remaining function to test directly
because it handles format-sensitive column renaming where edge cases (e.g., empty maturities,
duplicate maturities) could silently produce wrong mappings.

---

### Finding: `compute_w1_residuals` Returns Full `lm` Object -- REJECTED

**Severity:** Low
**File:** `compute_w1_residuals.R:135`
**Status:** Rejected -- keeping the full `lm` object for anticipated use in further development.
Can be revisited later if the object proves unnecessary.

---

## Numerical Stability Assessment

### Overflow in `exp()` Calls -- NOT AN ISSUE

The functions `compute_c_hat()` and `compute_sdf_innovations()` use `exp()` on the n_hat series.
Verified that `exp(2*n)` overflows at n > ~355 and `exp(n)` at n > ~710, but on bundled ACM data
n_hat is in [-0.13, 0.003]. Even with decimal-unit yields (divide by 100), n_hat is smaller
still. Overflow would require yields ~35,000x larger than ACM data, which is outside the domain
of the ACM term structure model. No guard needed.

### ~~Division by `sigma_i_sq`~~ FIXED

See "Defect: Unguarded Division by Zero in Quadratic Form" in the Confirmed Defects section.
Fixed in commit `55bf368` with two-layer defense: pre-loop input validation and post-division
overflow check.

---

## CRAN Readiness Assessment

### Current R CMD Check Status

The package passes R CMD check with **0 notes, 0 warnings, 0 errors** (Status: OK) as of
2026-03-23. The previous 2 notes (non-standard file at top level, LazyData) have been resolved.

### Additional CRAN Considerations

- ~~**Examples in `\dontrun{}`**~~: Fixed in commit `7e9c080`. Five identification-layer
  functions (`compute_scalar_statistics`, `compute_vector_statistics`,
  `compute_matrix_statistics`, `compute_identified_set_components`,
  `compute_identified_set_quadratic`) now have runnable examples using synthetic data. The
  remaining 12 functions keep `\dontrun{}` because they require downloaded data, perform file
  I/O, or produce plots.
- **URL accessibility**: `DATA_URLS$ACM_TERM_PREMIA` points to the NY Fed website. CRAN checks
  that URLs in documentation are accessible; verify this URL is stable and responds to automated
  HEAD requests.
- **License file**: MIT license with `+ file LICENSE` is standard. Ensure the `LICENSE` file
  exists and contains the required year and copyright holder.
- **Package size**: With the `variables` dataset (243 x 186), the installed package size should
  be checked against CRAN's 5MB limit for source packages.

---

## Test Suite Assessment

### Strengths

- **Mathematical correctness tests**: Known-value tests for quadratic form components
  (`test-compute_identified_set_quadratic.R:172-221`) verify `d_i = tau^2 * V_i / sigma^2`,
  `A_i`, `b_i`, `c_i` against hand-computed values
- **Statistical property tests**: SDF innovations verified to have mean near zero and high
  correlation (>0.8) with price news
- **Cross-function consistency**: Dedicated test file verifies all functions handle the same data
  formats, dates, NAs, and invalid inputs
- **Reusable test infrastructure**: `helper-test-utils.R` provides `load_standard_test_data()`,
  `expect_single_finite_value()`, `expect_time_series()`, `create_synthetic_test_data()`, etc.
- **Symmetry verification**: A_i matrices tested for symmetry both via `isSymmetric()` and
  element-wise comparison
- **Edge case coverage**: NA handling, invalid inputs, large datasets, boundary maturities

### Weaknesses

- ~~**Subset test masks real bug**~~ **FIXED**: Test now passes subsetted `s_i_1` and `s_i_2`,
  and a new regression test compares batch subset results against individual maturity runs
- ~~**No boundary maturity tests**~~ **FIXED** in commit `80564d7`: all compute functions now
  have `i = 10` error tests, and `compute_k_hat(i = 10)` is verified to succeed
- ~~**No non-integer maturity tests**~~ **FIXED** in commit `80564d7`: all compute functions now
  have `i = 1.5` error tests, plus comprehensive validator tests in `test-validation_utils.R`
- ~~**No zero-variance tests for quadratic form**~~ **FIXED** in commit `55bf368`: 7 test blocks
  added covering zero, NA/NaN/Inf/negative, multiple-bad, subset, small-positive, and overflow

### Test File Coverage

| Test File                                  | Functions Covered                    |
|--------------------------------------------|--------------------------------------|
| `test-compute_c_hat.R`                     | `compute_c_hat()`                    |
| `test-compute_k_hat.R`                     | `compute_k_hat()`                    |
| `test-compute_n_hat.R`                     | `compute_n_hat()`                    |
| `test-compute_price_news.R`                | `compute_price_news()`               |
| `test-compute_sdf_innovations.R`           | `compute_sdf_innovations()`          |
| `test-compute_variance_bound.R`            | `compute_variance_bound()`           |
| `test-compute_w1_residuals.R`              | `compute_w1_residuals()`             |
| `test-compute_w2_residuals.R`              | `compute_w2_residuals()`             |
| `test-compute_scalar_statistics.R`         | `compute_scalar_statistics()`        |
| `test-compute_vector_statistics.R`         | `compute_vector_statistics()`        |
| `test-compute_matrix_statistics.R`         | `compute_matrix_statistics()`        |
| `test-compute_identified_set_quadratic.R`  | `compute_identified_set_quadratic()` (validation, structure) |
| `test-compute_identified_set_quadratic-values.R` | `compute_identified_set_quadratic()` (values, symmetry, subset) |
| `test-compute_identified_set_components.R` | `compute_identified_set_components()`|
| `test-convert_to_quarterly.R`              | `convert_to_quarterly()`             |
| `test-extract_acm_data.R`                  | `extract_acm_data()`                 |
| `test-download_functions.R`                | `download_term_premia()`             |
| `test-validation_utils.R`                  | `validate_maturity_index()`          |
| `test-cross-function-consistency.R`        | All computation functions             |

### Recommended Regression Tests to Add

| Test Case                              | Why                                           |
|----------------------------------------|-----------------------------------------------|
| ~~`compute_n_hat(yields, tp, i = 10)`~~ | ~~Verifies behavior at effective upper bound~~ (done) |
| ~~`compute_n_hat(yields, tp, i = 1.5)`~~ | ~~Verifies non-integer rejection~~ (done)    |
| ~~`compute_c_hat(yields, tp, i = 10)`~~ | ~~Tests function that needs i+1 at boundary~~ (done) |
| ~~Quadratic form with full-length inputs and `maturities = c(1, 3, 5)`~~ | ~~Exposes idx/i bug~~ (done) |
| ~~Quadratic form with `sigma_i_sq = 0`~~ | ~~Verifies zero-division guard~~ (done)       |
| ~~`compute_sdf_innovations(yields, tp, i = "a")`~~ | ~~Verifies type checking~~ (done via validator consolidation) |
| ~~`convert_to_quarterly()` with missing months~~ | ~~Done: `test-convert_to_quarterly.R`~~ |
| `build_acm_col_mapping()` with empty maturities | Edge case for column mapping           |
| ~~W2 `return_df` with interior-NA PCs~~ (done) | ~~Exposes date misalignment bug (item 13)~~ |
| W2 with `ncol(pcs) < n_pcs` | Exposes opaque `lm()` error (item 15) |
| W2 with `dates` shorter than `n_obs` | Exposes silent NA dates (item 16) |

---

## Package Statistics

| Metric                  | Value                                          |
|-------------------------|------------------------------------------------|
| R source files          | 28                                             |
| Total R source lines    | 2,767                                          |
| Largest file            | `compute_identified_set_quadratic.R` (221 lines)|
| Smallest file           | `globals.R` (6 lines)                          |
| Median file size        | 87 lines                                       |
| Test files              | 20                                             |
| Man pages               | 45                                             |
| Exported functions      | 18                                             |
| Internal functions      | ~23                                            |
| Dependencies (Imports)  | 2 (`stats`, `utils`)                           |
| Dependencies (Suggests) | 5 (`curl`, `knitr`, `readxl`, `rmarkdown`, `testthat`) |
| R CMD check result      | 0 notes, 0 warnings, 0 errors                 |

### File Size Distribution

All files are under the 200-line project constraint except
`compute_identified_set_quadratic.R` (221 lines after adding sigma_i_sq guards), which may
benefit from extracting validation into a helper:

```
  6  globals.R
 23  zzz.R
 33  build_acm_col_mapping.R
 56  convert_to_quarterly.R
 39  compute_variance_bound.R
 52  compute_c_hat.R
 67  compute_price_news.R
 82  compute_k_hat.R
 85  compute_sdf_innovations.R
 86  compute_n_hat.R
 85  constants.R
 87  load_term_premia.R
 94  download_term_premia.R
 95  compute_scalar_statistics.R
105  data_paths.R
110  compute_matrix_statistics.R
130  compute_vector_statistics.R
131  hetid-package.R
137  compute_w1_residuals.R
140  compute_identified_set_components.R
149  extract_acm_data.R
152  computation_utils.R
158  compute_w2_residuals.R
161  validation_utils.R
162  validate_w2_inputs.R
221  compute_identified_set_quadratic.R
```

---

## Recommended Actions

### Immediate: Fix Confirmed Defects

| Action | Files | Impact |
|--------|-------|--------|
| ~~Fix `idx` vs `i` indexing in quadratic form~~ (done) | `compute_identified_set_quadratic.R` | ~~Correctness bug~~ |
| ~~Add integrality check to `validate_maturity_index()`~~ (done) | `validation_utils.R` | ~~Validation gap~~ |
| ~~Add function-specific `max_maturity` for functions needing `i+1` columns~~ (done) | `validation_utils.R`, callers | ~~Validation gap~~ |
| ~~Guard `sigma_i_sq` division against zero~~ (done) | `compute_identified_set_quadratic.R` | ~~Numerical stability~~ |
| ~~Replace `min(length(...))` silent truncation with length-match validation~~ (done) | `compute_identified_set_quadratic.R` | ~~Fail-fast~~ |
| ~~Eliminate `validate_maturity_param()`, use `validate_maturity_index()` everywhere~~ (done) | `computation_utils.R`, callers | ~~Consolidation~~ |
| ~~Add regression tests for boundary maturities, non-integers, zero sigma~~ (done) | `tests/testthat/` | ~~Coverage~~ |

### Short-Term: Code Quality

| Action | Files | Impact |
|--------|-------|--------|
| ~~Vectorize `compute_vector_statistics` inner loop~~ (done) | `compute_vector_statistics.R` | ~~Code clarity, future performance~~ |
| ~~Vectorize `compute_k_hat()` and `compute_time_series_news()`~~ (done) | `compute_k_hat.R`, `computation_utils.R` | ~~Code clarity~~ |
| ~~Simplify V_i to `crossprod()^2`~~ (done) | `compute_identified_set_components.R` | ~~Math clarity~~ |
| ~~Simplify date parsing loop~~ (done) | `load_term_premia.R` | ~~Readability~~ |
| ~~Inline `apply_time_series_transform()`~~ (rejected -- keeping for future reuse) | `computation_utils.R` | -- |
| ~~Add unit tests for `convert_to_quarterly()` (done), `build_acm_col_mapping()` (done), and `data_paths` (done)~~ | `tests/testthat/` | ~~Edge case coverage~~ |

### Medium-Term: CRAN Readiness and Extensibility

| Action | Files | Impact |
|--------|-------|--------|
| ~~Resolve R CMD check notes~~ (done -- 0 notes) | `DESCRIPTION`, top-level files | ~~Required for CRAN~~ |
| ~~Convert examples from `\dontrun{}` to runnable~~ (done -- 5 functions) | `R/*.R` man pages | ~~CRAN preference~~ |
| ~~Decouple from bundled `variables` dataset~~ (done -- message/warning on fallback, W2 load deduplicated) | `compute_w1_residuals.R`, `validate_w2_inputs.R`, `compute_w2_residuals.R` | ~~Enables non-ACM data~~ |
| ~~Fix W2 `return_df` date alignment with interior NAs~~ (done) | `compute_w2_residuals.R`, `validate_w2_inputs.R` | ~~Correctness for non-clean data~~ |
| ~~Fix positional splice between bundled PCs and ACM yields~~ (done) | `validate_w2_inputs.R`, `compute_w2_residuals.R` | ~~Correctness for default workflow~~ |
| ~~Fix examples and README to use `frequency = "quarterly"` or show merge workflow~~ (done -- examples now show year-quarter merge with explicit `pcs=`) | `R/*.R`, `hetid-package.R`, `README.Rmd` | ~~Documented default path is broken~~ |
| ~~Add `ncol(pcs)` validation in `load_w2_pcs()`~~ (done) | `validate_w2_inputs.R` | ~~Fail-fast on bad input~~ |
| ~~Add `dates` length validation in W2~~ (closed -- user dates validated; bundled dates inherently consistent) | `compute_w2_residuals.R` | ~~Fail-fast on bad input~~ |
| ~~Loosen `date` column requirement in W1 when `return_df = FALSE`~~ (done) | `compute_w1_residuals.R` | ~~Usability for custom data~~ |
| Add convenience `compute_identified_set()` pipeline function | New file | User experience |
| Add a vignette from the `scripts/` analysis workflows | `vignettes/` | Adoption, documentation |
| ~~Consider whether `compute_w1_residuals` should return raw `lm` object~~ (rejected -- keeping for future dev) | `compute_w1_residuals.R` | -- |
| ~~Add W2-specific `@param dates` override (shared template is inaccurate for W2)~~ (done) | `R/compute_w2_residuals.R` | ~~Documentation accuracy~~ |
| ~~Tighten test grep `^(y|tp)` → `^(y[0-9]|tp)` in W2 merge test~~ (done) | `tests/testthat/test-compute_w2_residuals.R:148` | ~~Test code quality~~ |
| ~~Decide whether to remove `README.html` from git or document `rmarkdown::render()` usage~~ (done -- removed from git, added to .gitignore) | `README.html`, contributing docs | ~~Build process clarity~~ |
