# Baseline-gamma hook: which J x I weight matrix plays the baseline
# gamma role. HETID_BASELINE_GAMMA selects among:
#   "vfci"         -- the fixed unit-norm VFCI PC loading (default; defined
#                     ONLY on pc1..pc4, so it errors instructively when the
#                     instrument count J != 4),
#   a path to an R script defining build_gamma(moments) returning a numeric
#   J x I matrix (J = nrow(moments$r_i_0), I = attr(moments,
#   "n_components")) -- the arbitrary-width escape hatch.
# Resolution is centralized here so stage 04 and the tau* stage cannot
# drift; the package's build-time validators remain the backstop.

baseline_gamma_method <- function() {
  Sys.getenv("HETID_BASELINE_GAMMA", "vfci")
}

#' Reduced-form gamma: the Y2-on-PC slope block of beta2R, transposed to the
#' J x I (instruments x components) layout the pipeline quadratic builder
#' (build_pipeline_quadratic_system) expects. Selects the principal-component
#' columns by name (anchored "^pc[0-9]+$"); intercept and any y1_lag* columns
#' are excluded because the instruments are the PCs only -- the conditioning
#' lags never enter gamma. Shared by the stage-06 consumption-equation table
#' and the tau* identification-strength benchmark.
#' @param beta2r I x (1 + n_pcs [+ n_lags]) Y2-on-PC reduced-form coefficients,
#'   with columns named (Intercept), pc1..pcJ, and optionally y1_lag1..y1_lagH
#' @return n_pcs x I matrix with attr "method" = "reduced_form"
build_reduced_form_gamma <- function(beta2r) {
  pc_cols <- grep("^pc[0-9]+$", colnames(beta2r))
  if (length(pc_cols) == 0L) {
    stop(
      "build_reduced_form_gamma: beta2r has no principal-component columns ",
      "matching '^pc[0-9]+$'; got columns ",
      paste(colnames(beta2r), collapse = ", "),
      " (instruments must be the named pc1..pcJ slopes)"
    )
  }
  pc_block <- beta2r[, pc_cols, drop = FALSE]
  if (all(pc_block == 0)) {
    stop(
      "build_reduced_form_gamma: the PC slope block of beta2r is all zeros, ",
      "so the reduced-form gamma is undefined under imposed exact news ",
      "(B = 0); callers should skip the reduced-form benchmark in this case"
    )
  }
  gamma <- t(pc_block)
  stopifnot(is.matrix(gamma), all(is.finite(gamma)))
  attr(gamma, "method") <- "reduced_form"
  gamma
}

resolve_baseline_gamma <- function(method, moments) {
  n_inst <- nrow(moments$r_i_0)
  n_comp <- attr(moments, "n_components")

  if (identical(method, "vfci")) {
    return(get_baseline_gamma(
      method = method, n_pcs = n_inst, n_components = n_comp
    ))
  }

  if (!file.exists(method)) {
    stop(
      "HETID_BASELINE_GAMMA must be 'vfci' or a path to ",
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
