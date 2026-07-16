#' Quarterly Economic and Financial Variables Dataset
#'
#' A time series dataset containing quarterly U.S. economic and financial variables
#' from 1962 to present, including GDP, inflation, interest rates, financial conditions
#' indices, and various derived measures.
#'
#' @format A data frame with 256 observations and 452 variables. Key variable
#' groups include:
#' \itemize{
#'   \item Date and period identifiers: \code{date} (calendar quarter-start as
#'     imported from the source repository, e.g. \code{1962-01-01}; see
#'     Details for aligning with the package-wide period-end convention),
#'     \code{yr}, \code{quarter}
#'   \item National accounts and prices: real GDP (\code{gdpc1}), potential
#'     GDP (\code{gdppot}), core PCE prices (\code{pcepilfe}), real
#'     consumption (\code{pcecc96}), output gap (\code{gap})
#'   \item Interest rates and spreads: \code{fedfunds}, \code{gs10},
#'     \code{tb3ms}, \code{tb3smffm}, \code{t10y3m}, \code{aaa10ym},
#'     \code{baa10ym}, \code{baa_aaa}; median expected inflation (\code{med3})
#'   \item Financial conditions and stress: \code{anfci}, \code{nfci},
#'     \code{gsfci}, \code{vixcls}, \code{ecy}, \code{ebp}, \code{gz},
#'     \code{ciss}
#'   \item Equity market: \code{gspc}, \code{gspc_ret}, \code{gspc_vol}
#'   \item Monetary policy shocks: columns prefixed \code{mp_shock_}
#'   \item Principal components: \code{pc1} through \code{pc6}
#'   \item VFCI measures: \code{vfci}, \code{vfci_lev}, and \code{vfci_}
#'     variants; expected excess returns (\code{mu})
#'   \item SDF panels: per-maturity expected SDF and SDF news
#'     (\code{expected_sdf_m3} through \code{expected_sdf_m117},
#'     \code{sdf_news_m3} through \code{sdf_news_m117}, maturity in months)
#'     and their principal components (\code{expected_sdf_pc1}--\code{pc3},
#'     \code{sdf_news_pc1}--\code{pc3})
#'   \item Growth-rate and timing transformations: columns following the
#'     naming grammar described in Details (e.g. \code{gr1.}, \code{gr4.},
#'     \code{lgr1.}, \code{fgr4.}, \code{f2gr1.}), including quarterly growth
#'     lags out to \code{l12gr1.} for GDP and consumption
#'   \item Convenience renames: each is a readability-only duplicate, exactly
#'     identical to the grammar-respecting column it renames -- \code{lgdp} is
#'     \code{log.gdpc1}, \code{lpce} is \code{log.pcepilfe}, \code{ygr} is
#'     \code{gr1.gdpc1}, and \code{infl_pce} is \code{gr1.pcepilfe}. They carry
#'     no additional data; prefer the canonical \code{log.}/\code{gr<h>.} names.
#' }
#'
#' @details
#' \strong{Date convention.} The dataset is imported verbatim from its source
#' repository and ships unmodified, so \code{date} carries quarter-start
#' labels (\code{1962-01-01}, \code{1962-04-01}, ...). The package-wide
#' convention is calendar period-end; normalize with
#' \code{\link{to_period_end}} before merging with ACM extracts (e.g.
#' \code{variables$date <- to_period_end(variables$date, "quarterly")}).
#' Package functions that fall back to the bundled dataset internally
#' apply this normalization at ingestion.
#'
#' \strong{Transformed-variable naming grammar.} Many columns are logs, growth
#' rates, leads, or lags of a base series, named compositionally as
#' \code{[lead/lag][gr<h>].<series>}. The default unit is one quarter (a
#' unit-less digit counts quarters); an explicit calendar suffix
#' (\code{m}=month, \code{y}=year, \code{d}=day) is used only for non-quarterly
#' series. \code{log.<series>} is the natural log; \code{gr<h>.<series>} is the
#' \code{h}-quarter growth rate (\code{gr1} quarterly, \code{gr4}
#' year-over-year); prefixes \code{l}, \code{l2}, \code{l3} lag backward and
#' \code{f}, \code{f2}, \code{f3} lead forward by that many quarters, with the
#' first lead/lag dropping its digit (bare \code{l}/\code{f}). Tokens stack
#' lead/lag then growth then \code{.series}, so \code{lgr1.gdpc1} is quarterly
#' GDP growth lagged one quarter and \code{fgr4.gdpc1} is year-over-year GDP
#' growth led \emph{one} quarter (the \code{4} is the growth window, not the
#' lead horizon). A pure level lag/lead is \code{l.<series>}/\code{f.<series>};
#' an off-quarter example elsewhere in the project is \code{f12m.y12} (the
#' 12-month-ahead 12-month yield on a monthly frame). The \code{*_lags} columns
#' (e.g. \code{vfci_lags}) are composite constructs, not simple shifts, and do
#' not follow this grammar.
#'
#' @source Imported unmodified from the VFCI macro_dynamics repository
#' (\code{https://github.com/VFCI/macro_dynamics} -- a private repository, so
#' the URL returns 404 to unauthenticated visitors). Underlying sources include
#' Federal Reserve Economic Data (FRED), the Federal Reserve Bank of Chicago,
#' and financial market data providers.
#'
#' @examples
#' data(variables)
#' head(variables)
#'
#' # Time series plot
#' plot(variables$date, variables$vfci,
#'   type = "l",
#'   xlab = "Date", ylab = "VFCI",
#'   main = "Volatility Financial Conditions Index"
#' )
#'
"variables"
