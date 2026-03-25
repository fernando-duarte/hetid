#' Validate inputs for the quadratic identified set computation
#'
#' Checks types, dimensions, positivity constraints, and resolves
#' maturities for \code{\link{compute_identified_set_quadratic}}.
#'
#' @inheritParams compute_identified_set_quadratic
#'
#' @return A list with components:
#' \describe{
#'   \item{maturities}{Resolved integer vector of maturity indices}
#'   \item{n_components}{Number of columns in \code{gamma} (I)}
#'   \item{n_maturities}{Length of \code{maturities}}
#' }
#'
#' @keywords internal
validate_quadratic_inputs <- function(gamma, tau,
                                      L_i, V_i, Q_i,
                                      s_i_0, s_i_1,
                                      s_i_2, sigma_i_sq,
                                      maturities = NULL) {
  if (!is.matrix(gamma)) {
    stop("gamma must be a matrix", call. = FALSE)
  }

  if (!is.numeric(tau) || !is.vector(tau)) {
    stop("tau must be a numeric vector", call. = FALSE)
  }

  if (any(tau <= 0)) {
    stop("All elements of tau must be positive", call. = FALSE)
  }

  n_components <- ncol(gamma)

  if (length(tau) != n_components) {
    stop("tau must have length I (number of columns in gamma)", call. = FALSE)
  }

  if (!is.numeric(L_i) || !is.numeric(V_i)) {
    stop("L_i and V_i must be numeric vectors", call. = FALSE)
  }

  if (!is.list(Q_i)) {
    stop("Q_i must be a list", call. = FALSE)
  }

  if (!is.numeric(s_i_0) || !is.numeric(sigma_i_sq)) {
    stop("s_i_0 and sigma_i_sq must be numeric vectors", call. = FALSE)
  }

  if (!is.list(s_i_1) || !is.list(s_i_2)) {
    stop("s_i_1 and s_i_2 must be lists", call. = FALSE)
  }

  # Resolve maturities from names or explicit parameter
  maturities <- resolve_maturities(
    maturities,
    list(
      L_i = L_i, V_i = V_i, Q_i = Q_i,
      s_i_0 = s_i_0, s_i_1 = s_i_1,
      s_i_2 = s_i_2, sigma_i_sq = sigma_i_sq
    ),
    n_components
  )

  # Validate maturities against gamma dimensions
  if (any(maturities < 1) || any(maturities > n_components)) {
    stop(
      "maturities must be between 1 and ncol(gamma) (",
      n_components, ")",
      call. = FALSE
    )
  }

  n_maturities <- length(maturities)
  if (any(c(
    length(L_i), length(V_i), length(Q_i),
    length(s_i_0), length(s_i_1), length(s_i_2),
    length(sigma_i_sq)
  ) != n_maturities)) {
    stop(
      "All statistical inputs must have length matching ",
      "maturities (", n_maturities, ")",
      call. = FALSE
    )
  }

  # Validate sigma_i_sq: must be finite and strictly positive
  bad_sigma <- which(
    !is.finite(sigma_i_sq) | sigma_i_sq <= 0
  )
  if (length(bad_sigma) > 0) {
    stop(
      "sigma_i_sq is non-positive, non-finite, or NA ",
      "for maturity/maturities ",
      paste(maturities[bad_sigma], collapse = ", "),
      ". Cannot compute identified set -- ",
      "insufficient heteroskedasticity.",
      call. = FALSE
    )
  }

  list(
    maturities = maturities,
    n_components = n_components,
    n_maturities = n_maturities
  )
}
