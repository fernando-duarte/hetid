# ACM Term Structure Data Documentation

## Overview

The Adrian, Crump, and Moench (ACM) term structure dataset provides model-based estimates of U.S. Treasury yields, term premia, and risk-neutral yields. This document describes the structure and contents of the ACM data available in the `hetid` package.

## Data Description

### DATE Column

The `DATE` column contains the business-day observation date for each row in the series. The data is provided at monthly frequency, typically using end-of-month observations.

### Yield Columns (ACMY01–ACMY10)

Columns `ACMY01` through `ACMY10` contain the model-implied zero-coupon Treasury yields from the ACM model, for maturities ranging from 1 year to 10 years.

- **ACMY01**: 1-year zero-coupon yield
- **ACMY02**: 2-year zero-coupon yield
- **ACMY03**: 3-year zero-coupon yield
- **ACMY04**: 4-year zero-coupon yield
- **ACMY05**: 5-year zero-coupon yield
- **ACMY06**: 6-year zero-coupon yield
- **ACMY07**: 7-year zero-coupon yield
- **ACMY08**: 8-year zero-coupon yield
- **ACMY09**: 9-year zero-coupon yield
- **ACMY10**: 10-year zero-coupon yield

### Term Premium Columns (ACMTP01–ACMTP10)

Columns `ACMTP01` through `ACMTP10` report the ACM term premium estimates for 1- to 10-year maturities. The term premium represents the additional compensation investors require for bearing interest-rate risk.

### Risk-Neutral Yield Columns (ACMRNY01–ACMRNY10)

Columns `ACMRNY01` through `ACMRNY10` give the risk-neutral yields, which represent the expected average short-rate path over 1- to 10-year horizons, as implied by the ACM model.

## Key Features

### Maturity Suffix Convention

The numeric suffix in each column name denotes the maturity in years:
- 01 = 1 year
- 02 = 2 years
- ...
- 10 = 10 years

### Units

All yield, term-premium, and risk-neutral-yield values are expressed in **annualized percentage points**. For example, a value of 3.5 represents 3.5% per annum.

### Frequency

The dataset is provided at **monthly frequency**, with one observation per month.

### Mathematical Relationship

By construction, the following relationship holds for each maturity n:

```
ACMTP(n) = ACMY(n) - ACMRNY(n)
```

That is, the term premium equals the difference between the model-implied yield and the risk-neutral yield.

## Usage Examples

### Loading the Raw Data

```r
# Download the latest ACM data (if not already available)
download_term_premia()

# Load the raw data
acm_raw <- load_term_premia()
```

### Using the Extraction Function

```r
# Extract specific maturities and data types
acm_data <- extract_acm_data(
  data_types = c("yields", "term_premia"),
  maturities = c(2, 5, 10),
  start_date = "2010-01-01",
  end_date = "2020-12-31"
)

# Convert to quarterly frequency
acm_quarterly <- extract_acm_data(
  data_types = "term_premia",
  frequency = "quarterly"
)
```

## Data Source and Methodology

The data is sourced from the Federal Reserve Bank of New York and is based on the methodology described in:

Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure with linear regressions." Journal of Financial Economics, 110(1), 110-138.

For more information and updates, visit:
https://www.newyorkfed.org/research/data_indicators/term-premia-tabs

## Update Frequency

The Federal Reserve Bank of New York updates this data monthly. Users can download the latest data using the `download_term_premia()` function.