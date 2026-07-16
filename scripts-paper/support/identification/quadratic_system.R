# compute_identification_moments() is exported by the installed hetid package;
# the moments object is a validated hetid_moments container
# carrying the maturity identity.
#
# Single pipeline entry point for assembling the identified-set quadratic system.
# Delegates to the exported generalized-instrument front end
# hetid::build_general_quadratic_system(), which with a J x I matrix gamma (one
# combination per component, K_i = 1) is bit-identical on $quadratic to the
# legacy hetid::build_quadratic_system() and exposes the same L_i/V_i/Q_i values
# in $components. This is the K_i = 1 (one combination per component) date-t
# specialization of docs/lewbel_multivariate_set_identification.tex
# ("Instrument choices as special cases"), so the
# whole pipeline runs on one generalized assembly path.
#
# The general builder returns $components as a plain list (plus a $labels frame).
# We re-attach the hetid_components class and its maturities/n_components integer
# attributes so the persisted RDS objects and interactive print stay identical to
# the legacy shape and remain valid inputs to the hetid quadratic-stage APIs
# (which gate on inherits(., "hetid_components")). The added $labels element is an
# accepted, documented delta.
#
# Set HETID_ASSERT_EQUIV to a non-empty value to additionally assert, on every
# call, numeric-leaf identity with the legacy builder on the real pipeline inputs
# (off by default: zero overhead in normal runs). One live pipeline run with the
# flag set self-certifies every migrated call site against the legacy path.
build_pipeline_quadratic_system <- function(gamma, tau, moments) {
  out <- hetid::build_general_quadratic_system(gamma, tau, moments)
  out$components <- structure(
    out$components,
    maturities = as.integer(attr(out, "maturities")),
    n_components = as.integer(attr(out, "n_components")),
    class = "hetid_components"
  )
  if (nzchar(Sys.getenv("HETID_ASSERT_EQUIV", ""))) {
    legacy <- hetid::build_quadratic_system(gamma, tau, moments)
    assert_pipeline_quadratic_equiv(out, legacy)
  }
  out
}

# Numeric-leaf equivalence guard: the migrated generalized assembly must equal
# the legacy build_quadratic_system() output exactly on the solver-consumable
# quadratic form and on the L_i/V_i/Q_i component values. Names and the
# re-attached class are included by comparing the whole $quadratic list and each
# component leaf with identical().
assert_pipeline_quadratic_equiv <- function(general, legacy) {
  stopifnot(
    identical(general$quadratic, legacy$quadratic),
    identical(general$components$L_i, legacy$components$L_i),
    identical(general$components$V_i, legacy$components$V_i),
    identical(general$components$Q_i, legacy$components$Q_i)
  )
  invisible(TRUE)
}

#' Get baseline gamma matrix (raw VFCI PC loadings)
#' @param method label for the method (stored as attr)
#' @param n_pcs number of principal components; must equal the length of the
#'   VFCI loading vector (4) -- the loading is defined only on pc1..pc4, so
#'   any other value errors rather than silently recycling
#' @param n_components number of components (NULL = infer)
#' @return J x I matrix with identical columns
get_baseline_gamma <- function(
  method = "vfci",
  n_pcs = HETID_CONSTANTS$DEFAULT_N_PCS,
  n_components = NULL
) {
  if (is.null(n_components)) {
    n_components <- length(DEFAULT_ID_MATURITIES)
  }
  # Raw VFCI PC loading vector (the regression coefficients themselves, NOT
  # unit-normalized). From regressing variables$vfci on the raw pc1:pc4:
  #   vfci = mean(vfci) + 0.1095399*pc1 - 0.1692329*pc2
  #                     - 0.1320361*pc3 + 0.1699299*pc4
  # With these raw weights the combined instrument PC %*% gamma equals the
  # de-meaned VFCI exactly (the PCs are kept at native scale).
  # The identified set is scale-invariant in this column, so normalizing would
  # change no result -- the raw weights are used so the instrument literally IS
  # VFCI - mean(VFCI) rather than a rescaled copy.
  vfci_loading <- c(
    0.1095399, -0.1692329,
    -0.1320361, 0.1699299
  )
  if (n_pcs != length(vfci_loading)) {
    stop(
      "get_baseline_gamma: the VFCI loading is defined only for ",
      length(vfci_loading), " PCs (pc1..pc4); got n_pcs = ", n_pcs,
      " -- recycling it would produce wrong loadings. For a custom-width ",
      "instrument set supply HETID_BASELINE_GAMMA=<path-to-R-file ",
      "defining build_gamma(moments)>"
    )
  }
  gamma <- matrix(vfci_loading, nrow = n_pcs, ncol = n_components)
  attr(gamma, "method") <- method
  gamma
}

#' @return list with tau_point and tau_set vectors
get_tau_spec <- function(
  tau_point = 0,
  tau_set = BASELINE_TAU,
  n_components = NULL
) {
  if (is.null(n_components)) {
    n_components <- length(DEFAULT_ID_MATURITIES)
  }
  list(
    tau_point = rep(tau_point, n_components),
    tau_set = rep(tau_set, n_components)
  )
}
