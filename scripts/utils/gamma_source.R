# Baseline-gamma hook: which J x I weight matrix plays the baseline
# gamma role. HETID_BASELINE_GAMMA selects among:
#   "vfci"         -- the fixed unit-norm VFCI PC loading (default; defined
#                     ONLY on pc1..pc4, so it errors instructively when the
#                     instrument count J != 4),
#   "reduced_form" -- the factors-mode Y2-on-PC slope projection (rows are
#                     first-stage PCs, so it requires J == n_pcs),
#   a path to an R script defining build_gamma(moments) returning a numeric
#   J x I matrix (J = nrow(moments$r_i_0), I = attr(moments,
#   "n_components")) -- the arbitrary-width escape hatch.
# Resolution is centralized here so stage 04 and the tau* stage cannot
# drift; the package's build-time validators remain the backstop.

baseline_gamma_method <- function() {
  Sys.getenv("HETID_BASELINE_GAMMA", "vfci")
}

resolve_baseline_gamma <- function(method, moments, gamma_rf = NULL) {
  n_inst <- nrow(moments$r_i_0)
  n_comp <- attr(moments, "n_components")

  if (identical(method, "reduced_form")) {
    if (is.null(gamma_rf)) {
      stop(
        "HETID_BASELINE_GAMMA='reduced_form' but the reduced-form gamma ",
        "is unavailable (factors mode only)"
      )
    }
    if (nrow(gamma_rf) != n_inst) {
      stop(
        "reduced-form gamma has ", nrow(gamma_rf), " rows (it is defined ",
        "on the PC first stage), but the instrument matrix has J = ",
        n_inst, " columns; for a custom-width Z supply ",
        "HETID_BASELINE_GAMMA=<path-to-R-file defining build_gamma(moments)>"
      )
    }
    attr(gamma_rf, "method") <- "reduced_form"
    return(gamma_rf)
  }

  if (identical(method, "vfci")) {
    return(get_baseline_gamma(
      method = method, n_pcs = n_inst, n_components = n_comp
    ))
  }

  if (!file.exists(method)) {
    stop(
      "HETID_BASELINE_GAMMA must be 'vfci', 'reduced_form', or a path to ",
      "an existing R file defining build_gamma(moments); got '", method, "'"
    )
  }
  env <- new.env(parent = globalenv())
  sys.source(method, envir = env)
  if (!is.function(env$build_gamma)) {
    stop("HETID_BASELINE_GAMMA script must define build_gamma(moments)")
  }
  gamma <- env$build_gamma(moments)
  if (!is.matrix(gamma) || !is.numeric(gamma)) {
    stop("build_gamma(moments) must return a numeric matrix")
  }
  if (nrow(gamma) != n_inst || ncol(gamma) != n_comp) {
    stop(
      "build_gamma(moments) must return a ", n_inst, " x ", n_comp,
      " matrix (J instrument rows x I component columns); got ",
      nrow(gamma), " x ", ncol(gamma), " (check rows = J)"
    )
  }
  if (!all(is.finite(gamma))) {
    stop("build_gamma(moments) returned non-finite values")
  }
  zero_cols <- which(colSums(gamma != 0) == 0)
  if (length(zero_cols) > 0) {
    stop(
      "build_gamma(moments) returned all-zero weight column(s) ",
      paste(zero_cols, collapse = ", "),
      "; every component needs a nonzero weight direction"
    )
  }
  if (!is.null(rownames(gamma)) &&
    !identical(rownames(gamma), rownames(moments$r_i_0))) {
    stop(
      "build_gamma(moments) returned rownames that do not match the ",
      "instrument names (rownames(moments$r_i_0)); gamma rows are ",
      "positional -- return an unnamed matrix or use the container's ",
      "instrument names in the container's order"
    )
  }
  rownames(gamma) <- rownames(moments$r_i_0)
  attr(gamma, "method") <- paste0("custom:", basename(method))
  gamma
}
