#' @importFrom utils download.file read.csv write.csv
"_PACKAGE"

#' hetid: Identification Through Heteroskedasticity for VFCI
#'
#' @description
#' The hetid package implements identification through heteroskedasticity methods
#' from Lewbel (2012) for triangular systems, with applications to the Volatility
#' Financial Conditions Index (VFCI) developed by Adrian, DeHaven, Duarte, and Iyer.
#'
#' The package supports data access, bond pricing, and identified-set estimation.
#'
#' @section Core Methodology:
#' The package implements the identification through heteroskedasticity approach
#' from Lewbel (2012), which exploits conditional heteroskedasticity to identify
#' structural parameters in triangular systems without requiring external instruments.
#'
#' The method is particularly useful for:
#' \itemize{
#'   \item Identifying endogenous relationships in macroeconomic models
#'   \item Estimating structural parameters when traditional instruments are unavailable
#'   \item Analyzing financial conditions and their impact on the real economy
#' }
#'
#' @section Key Features:
#' \subsection{Data Management:}{
#' \itemize{
#'   \item \strong{ACM Term Structure Data}: Access to monthly and daily
#'     (business-day) yields, term premia, and risk-neutral yields at monthly
#'     maturity steps (\code{MIN_MATURITY}-\code{MAX_MATURITY} months)
#'     based on Adrian, Crump, and Moench (2013)
#'   \item \strong{Economic Variables}: Quarterly macroeconomic and financial data
#'   \item \strong{Verified Downloads}: Functions to download the latest GitHub
#'     release with sha256 verification (NY Fed workbook as opt-in fallback)
#' }}
#'
#' \subsection{Bond Pricing Calculations:}{
#' \itemize{
#'   \item \strong{Expected Log Bond Prices}: Compute n_hat(i,t) estimators
#'   \item \strong{Price News}: Calculate unexpected bond price changes
#'   \item \strong{SDF Innovations}: Compute stochastic discount factor innovations
#'   \item \strong{Moment Estimators}: Calculate supremum (c_hat) and fourth moment
#'     (k_hat) estimators
#'   \item \strong{Variance Bounds}: Empirical bounds for forecast error variance
#' }}
#'
#' \subsection{Identification Methods:}{
#' \itemize{
#'   \item \strong{Reduced Form Residuals}: Compute \eqn{\omega_1} and
#'     \eqn{\omega_2} residuals for identification
#'   \item \strong{Multi-maturity Analysis}: Simultaneous estimation across yield curve
#' }}
#'
#' @section Monthly News Example:
#' The bundled ACM term premia use a monthly rollover convention. This example
#' uses monthly rows and \code{step = 1}; its instruments are PCs of simulated
#' nominal asset returns, rather than the bundled quarterly PCs.
#' \preformatted{
#' local({
#'   old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
#'   on.exit(if (is.null(old_seed)) {
#'     rm(".Random.seed", envir = .GlobalEnv)
#'   } else {
#'     assign(".Random.seed", old_seed, envir = .GlobalEnv)
#'   })
#'   set.seed(42)
#'   acm <- extract_acm_data(maturities = c(1, 2, 3))
#'   n_pcs <- HETID_CONSTANTS$DEFAULT_N_PCS
#'   returns <- matrix(rnorm(nrow(acm) * n_pcs), ncol = n_pcs)
#'   pc_data <- data.frame(date = acm$date, stats::prcomp(returns)$x)
#'   pc_cols <- paste0(HETID_CONSTANTS$PC_PREFIX, seq_len(n_pcs))
#'   names(pc_data)[-1] <- pc_cols
#'   merged <- merge(acm, pc_data, by = "date")
#'   w2 <- compute_w2_residuals(
#'     merged[, c("y1", "y2", "y3")], merged[, c("tp1", "tp2", "tp3")],
#'     maturities = 2, step = 1, n_pcs = n_pcs,
#'     pcs = as.matrix(merged[, pc_cols]), dates = merged$date
#'   )
#'   print(head(w2$residuals$maturity_2))
#' })
#' }
#'
#' Changing \code{step} does not convert term premia to another rollover
#' convention; see \code{\link{compute_n_hat}}. For quarterly analyses,
#' use inputs with a matching news convention. Normalize the bundled variables'
#' quarter-start dates with \code{to_period_end(..., "quarterly")} before a
#' date-keyed merge. Both residual regressions drop incomplete observations;
#' align their returned realization dates before computing joint moments.
#'
#' @section Data Sources:
#' \describe{
#'   \item{\strong{ACM Term Structure Data}}{Monthly and daily (business-day)
#'     data from Adrian, Crump, and Moench (2013) including yields, term premia,
#'     and risk-neutral yields at monthly maturity steps from
#'     \code{MIN_MATURITY} to \code{MAX_MATURITY} months.
#'     Updated from the GitHub replication release (the daily series is
#'     download-only); the NY Fed workbook is the opt-in fallback.}
#'   \item{\strong{Economic Variables}}{Quarterly macroeconomic and financial
#'     variables including GDP, inflation, financial conditions indices, and
#'     principal components of nominal financial asset returns.}
#' }
#'
#' @template section-function-categories
#'
#' @section Mathematical Background:
#' The identification strategy exploits the relationship:
#' \deqn{Y_{1,t+1} = X_t^\top \beta_1 + \theta^\top Y_{2,t+1} + \epsilon_{1,t+1}}
#' \deqn{Y_{2,t+1} = \beta_2^R X_t + \epsilon_{2,t+1}}
#'
#' Here \eqn{X_t} stacks a constant, the principal components of nominal financial asset
#' returns, and any retained lags of \eqn{Y_1}. Identification comes from
#' heteroskedasticity-based moment conditions on the residuals, as described in Lewbel (2012).
#'
#' @references
#' Adrian, T., Crump, R. K., and Moench, E. (2013). "Pricing the term structure
#' with linear regressions." Journal of Financial Economics, 110(1), 110-138.
#'
#' Adrian, T., DeHaven, M., Duarte, F., and Iyer, T. (2024). "The Volatility
#' Financial Conditions Index." Working Paper.
#'
#' Lewbel, A. (2012). "Using heteroscedasticity to identify and estimate
#' mismeasured and endogenous regressor models." Journal of Business & Economic
#' Statistics, 30(1), 67-80.
#'
#' @name hetid-package
#' @aliases hetid

## usethis namespace: start
## usethis namespace: end
NULL
