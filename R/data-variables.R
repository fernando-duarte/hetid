#' Quarterly Economic and Financial Variables Dataset
#'
#' A time series dataset containing quarterly U.S. economic and financial variables
#' from 1962 to present, including GDP, inflation, interest rates, financial conditions
#' indices, and various derived measures.
#'
#' @format A tibble time series (tbl_ts) with 243 observations and 186 variables:
#' \describe{
#'   \item{date}{Date of observation (quarterly)}
#'   \item{yr}{Year}
#'   \item{quarter}{Quarter (1-4)}
#'   \item{gdpc1}{Real GDP (billions of chained 2012 dollars)}
#'   \item{gdppot}{Real potential GDP}
#'   \item{pcepilfe}{PCE price index less food and energy}
#'   \item{pcecc96}{Real personal consumption expenditures}
#'   \item{fedfunds}{Federal funds rate}
#'   \item{anfci}{Adjusted National Financial Conditions Index}
#'   \item{nfci}{National Financial Conditions Index}
#'   \item{gs10}{10-year Treasury constant maturity rate}
#'   \item{tb3ms}{3-month Treasury bill rate}
#'   \item{med3}{3-month median expected inflation}
#'   \item{tb3smffm}{3-month Treasury minus federal funds rate}
#'   \item{aaa10ym}{AAA corporate bond yield minus 10-year Treasury}
#'   \item{baa10ym}{BAA corporate bond yield minus 10-year Treasury}
#'   \item{vixcls}{VIX volatility index}
#'   \item{ygr}{GDP growth rate}
#'   \item{lgdp}{Log of real GDP}
#'   \item{infl_pce}{PCE inflation rate}
#'   \item{gap}{Output gap}
#'   \item{baa_aaa}{BAA-AAA spread}
#'   \item{t10y3m}{Term spread (10-year minus 3-month)}
#'   \item{gspc}{S&P 500 index}
#'   \item{gspc_ret}{S&P 500 returns}
#'   \item{gspc_vol}{S&P 500 volatility}
#'   \item{mp_shock_*}{Various monetary policy shock measures}
#'   \item{gsfci}{Goldman Sachs Financial Conditions Index}
#'   \item{ecy}{Excess cyclical yield}
#'   \item{ebp}{Excess bond premium}
#'   \item{gz}{Gilchrist-Zakrajsek spread}
#'   \item{ciss}{Composite Indicator of Systemic Stress}
#'   \item{pc1-pc6}{Principal components}
#'   \item{mu}{Expected excess returns}
#'   \item{vfci}{Volatility Financial Conditions Index}
#'   \item{vfci_lev}{VFCI level}
#'   \item{gr*, lgr*, fgr*}{Various growth rates and transformations}
#' }
#'
#' @source Various sources including Federal Reserve Economic Data (FRED),
#' Federal Reserve Bank of Chicago, and financial market data providers.
#'
#' @examples
#' data(variables)
#' head(variables)
#'
#' # Extract specific series
#' gdp_growth <- variables$ygr
#' inflation <- variables$infl_pce
#'
#' # Time series plot
#' plot(variables$date, variables$vfci,
#'   type = "l",
#'   xlab = "Date", ylab = "VFCI",
#'   main = "Volatility Financial Conditions Index"
#' )
#'
"variables"
