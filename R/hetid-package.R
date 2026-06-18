#' @importFrom utils download.file read.csv write.csv
#' @importFrom stats aggregate
"_PACKAGE"

#' hetid: Identification Through Heteroskedasticity for VFCI
#'
#' @description
#' The hetid package implements identification through heteroskedasticity methods
#' from Lewbel (2012) for triangular systems, with applications to the Volatility
#' Financial Conditions Index (VFCI) developed by Adrian, DeHaven, Duarte, and Iyer.
#'
#' The package provides a complete toolkit for empirical macroeconomic analysis
#' using heteroskedasticity-based identification, including data access, bond
#' pricing calculations, and structural parameter estimation.
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
#'   \item \strong{ACM Term Structure Data}: Access to monthly yields, term premia,
#'     and risk-neutral yields at monthly maturity steps (1-120 months) based on
#'     Adrian, Crump, and Moench (2013)
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
#'   \item \strong{Reduced Form Residuals}: Compute W1 and W2 residuals for identification
#'   \item \strong{Multi-maturity Analysis}: Simultaneous estimation across yield curve
#' }}
#'
#' @section Typical Workflow:
#' \preformatted{
#' # Data Setup: merge ACM yields with bundled PCs by calendar date
#' download_term_premia()
#' mats <- seq(12, 120, by = 12)
#' acm_data <- extract_acm_data(
#'   data_types = c("yields", "term_premia"),
#'   maturities = mats,
#'   frequency = "quarterly"
#' )
#' data("variables", package = "hetid")
#'
#' # ACM and the bundled PCs share the period-end date convention, so they
#' # merge directly by calendar date (covers dates available in both datasets)
#' pc_cols <- paste0("pc", 1:4)
#' merged <- merge(
#'   variables[, c("date", pc_cols)],
#'   acm_data[, c("date", paste0("y", mats), paste0("tp", mats))],
#'   by = "date"
#' )
#' pcs <- as.matrix(merged[, pc_cols])
#' yields <- merged[, paste0("y", mats)]
#' tp <- merged[, paste0("tp", mats)]
#'
#' # Compute Reduced Form Residuals
#' w1 <- compute_w1_residuals(n_pcs = 4)
#' w2 <- compute_w2_residuals(
#'   yields, tp,
#'   maturities = c(24, 60, 108),
#'   n_pcs = 4, pcs = pcs
#' )
#' }
#'
#' @section Data Sources:
#' \describe{
#'   \item{\strong{ACM Term Structure Data}}{Monthly data from Adrian, Crump, and
#'     Moench (2013) including yields, term premia, and risk-neutral yields at
#'     monthly maturity steps from 3 to 120 months. Updated from the GitHub
#'     replication release; the NY Fed workbook is the opt-in fallback.}
#'   \item{\strong{Economic Variables}}{Quarterly macroeconomic and financial
#'     variables including GDP, inflation, financial conditions indices, and
#'     principal components of financial asset returns.}
#' }
#'
#' @section Function Categories:
#' \subsection{Data Functions:}{
#' \itemize{
#'   \item \code{\link{download_term_premia}()}: Download ACM data
#'   \item \code{\link{extract_acm_data}()}: Extract and process ACM data
#'   \item \code{\link{load_term_premia}()}: Load cached ACM data
#' }}
#'
#' \subsection{Bond Pricing Functions:}{
#' \itemize{
#'   \item \code{\link{compute_n_hat}()}: Expected log bond prices
#'   \item \code{\link{compute_price_news}()}: Unexpected price changes
#'   \item \code{\link{compute_sdf_innovations}()}: SDF innovations
#'   \item \code{\link{compute_expected_sdf}()}: Expected one-period SDF
#'   \item \code{\link{compute_expected_sdf_variance_bound}()}: Expected-SDF
#'     level-error variance bound
#'   \item \code{\link{compute_c_hat}()}: Supremum estimator
#'   \item \code{\link{compute_k_hat}()}: Fourth moment estimator
#'   \item \code{\link{compute_variance_bound}()}: Variance bounds
#' }}
#'
#' \subsection{Identification Functions:}{
#' \itemize{
#'   \item \code{\link{compute_w1_residuals}()}: Primary endogenous variable residuals
#'   \item \code{\link{compute_w2_residuals}()}: Secondary endogenous variable residuals
#' }}
#'
#'
#' @section Mathematical Background:
#' The identification strategy exploits the relationship:
#' \deqn{Y_{1,t+1} = \theta Y_{2,t+1} + \epsilon_{1,t+1}}
#' \deqn{Y_{2,t+1} = \gamma' Z_t + \epsilon_{2,t+1}}
#'
#' Where identification is achieved through heteroskedasticity-based moment conditions
#' as described in Lewbel (2012).
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
