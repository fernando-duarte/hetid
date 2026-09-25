#' @section Function Categories:
#' \subsection{Data Functions:}{
#' \itemize{
#'   \item \code{\link{download_term_premia}()}: Download ACM data
#'   \item \code{\link{extract_acm_data}()}: Extract and process ACM data
#'   \item \code{\link{load_term_premia}()}: Load cached ACM data
#'   \item \code{\link{acm_column_name}()}: Build a reshaped ACM column name
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
#'   \item \code{\link{compute_k2_hat}()}: Price-news fourth moment estimator
#'   \item \code{\link{compute_variance_bound}()}: Variance bounds
#'   \item \code{\link{compute_news_q_bound}()}: First-order-cancelled
#'     news-error variance bound
#' }}
#'
#' \subsection{Identification Functions:}{
#' \itemize{
#'   \item \code{\link{compute_w1_residuals}()}: Primary endogenous variable residuals
#'   \item \code{\link{compute_w2_residuals}()}: Secondary endogenous variable residuals
#'   \item \code{\link{compute_identification_moments}()}: The seven moments
#'   \item \code{\link{compute_scalar_statistics}()}: Scalar moment statistics
#'   \item \code{\link{compute_vector_statistics}()}: Vector moment statistics
#'   \item \code{\link{compute_matrix_statistics}()}: Matrix moment statistics
#'   \item \code{\link{build_instrument_matrix}()}: Construct instruments
#'   \item \code{\link{separate_instruments_lambda}()}: Per-instrument weights
#'   \item \code{\link{compute_identified_set_components}()}: L_i, V_i, Q_i
#'   \item \code{\link{build_quadratic_system}()}: Quadratic constraints (preferred)
#'   \item \code{\link{build_general_quadratic_system}()}: Generalized constraints
#'   \item \code{\link{compute_identified_set_quadratic}()}: d_i, A_i, b_i, c_i
#'   \item \code{\link{make_constraint_checker}()}: Per-maturity constraint closure
#'   \item \code{\link{make_system_checker}()}: Full-system constraint closure
#'   \item \code{\link{align_instrument_sets}()}: Align instrument sets
#'   \item \code{\link{lambda_from_support}()}: Weights from a support pattern
#'   \item \code{\link{recover_structural_coefficients}()}: Recover beta1(theta)
#' }}
#'
#' \subsection{Tau-Zero Estimation Functions:}{
#' \itemize{
#'   \item \code{\link{compute_tau0_system}()}: Reduced forms plus tau=0 point solve
#'   \item \code{\link{compute_tau0_point}()}: Closed-form point solve of the stacked system
#'   \item \code{\link{fit_log_variance}()}: PPML or Harvey fit of the log-variance equation
#'   \item \code{\link{make_log_variance_fitter}()}: Repeated fits on a fixed design
#'   \item \code{\link{fit_log_variance_at_b}()}: Log-variance fit at a fixed structural parameter
#'   \item \code{\link{compute_log_variance_vcov}()}: Analytic covariance matrices of a fit
#'   \item \code{\link{compute_log_variance_vcov_at_coef}()}: Covariances at supplied coefficients
#'   \item \code{\link{compute_log_variance_se}()}: Standard-error frame for a log-variance fit
#'   \item \code{\link{LOG_VARIANCE_CONTROL}}: PPML estimation numerical controls
#'   \item \code{\link{LOG_VARIANCE_HARVEY_CONTROL}}: Harvey estimation numerical controls
#' }}
#'
#' \subsection{Identified-Set Search Functions:}{
#' \itemize{
#'   \item \code{\link{compute_identified_set_box}()}: Theta and structural bounds at a slack
#'   \item \code{\link{compute_linear_functional_bounds}()}: Named affine bounds and evidence
#'   \item \code{\link{profile_log_variance_set}()}: Volatility coefficients over that set
#'   \item \code{\link{sample_log_variance_set}()}: Retained joint fits for prediction
#'   \item \code{\link{predict.hetid_log_variance_sample}()}: Sampled prediction envelopes
#'   \item \code{\link{IDENTIFIED_SET_CONTROL}}: Identified-set search numerical controls
#' }}
#'
#' \subsection{Constants and Utilities:}{
#' \itemize{
#'   \item \code{\link{effective_max_maturity}()}: Largest usable maturity index
#'   \item \code{\link{to_period_end}()}: Normalize dates to the period-end convention
#'   \item \code{\link{HETID_CONSTANTS}}: Package-wide constant defaults
#' }}
#'
