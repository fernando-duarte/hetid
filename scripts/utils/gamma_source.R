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

# Single source of truth for the common-design column regexes. The spec
# conditions BOTH Y1 and Y2 on the same X_t = (1, PC, H lags of Y1): PCs are
# the only instruments (anchored "^pc[0-9]+$"), l.y1* the predetermined
# conditioning lags (anchored "^l[0-9]*\\.y1$", matching l.y1, l2.y1, ...), and
# the lone remaining column is the intercept. Returns the column indices and
# counts so callers never re-derive the regexes (and so build_reduced_form_gamma's
# pc selection matches exactly). Hardened: any column that is neither the
# intercept, a PC, nor a Y1 lag aborts rather than being silently absorbed.
# Only the standard PC + Y1-lags design is accepted; exog-path coefficient
# columns (anything outside the three patterns) abort with a named error.
classify_common_design_cols <- function(nms) {
  pc_cols <- grep("^pc[0-9]+$", nms)
  lag_cols <- grep("^l[0-9]*\\.y1$", nms) # Y1-only; matches l.y1, l2.y1, ...
  intercept_col <- which(nms == "(Intercept)")
  unknown <- setdiff(seq_along(nms), c(intercept_col, pc_cols, lag_cols))
  if (length(intercept_col) != 1L) {
    stop("classify_common_design_cols: expected exactly one '(Intercept)' column.",
      call. = FALSE
    )
  }
  if (length(unknown) > 0L) {
    stop("classify_common_design_cols: unrecognised columns: ",
      paste(nms[unknown], collapse = ", "),
      ". Expected only '(Intercept)', '^pc[0-9]+$', '^l[0-9]*\\.y1$'.",
      call. = FALSE
    )
  }
  list(
    intercept_col = intercept_col,
    pc_cols = pc_cols,
    lag_cols = lag_cols,
    n_pcs = length(pc_cols),
    n_lag = length(lag_cols)
  )
}

# Assert beta2R is column-matched to beta1R (same length, identical names in
# the same order). The exact recovery identity beta1(theta) = beta1R -
# (beta2R)'theta carries every column through directly only when both fits
# share the full common design; a legacy PC-only beta2R must FAIL LOUDLY
# rather than be silently re-padded, which would falsely point-identify psi.
assert_beta_columns_matched <- function(beta1r, beta2r) {
  if (length(beta1r) == ncol(beta2r) &&
    identical(names(beta1r), colnames(beta2r))) {
    return(invisible(TRUE))
  }
  cli_abort(c(
    "beta2R is not column-matched to beta1R.",
    "i" = "Expected the FULL common design (1, PC, l.y1*) in both fits.",
    "x" = paste0(
      "got beta1R names {.val {names(beta1r)}} vs beta2R columns ",
      "{.val {colnames(beta2r)}}"
    ),
    ">" = paste0(
      "This requires the common-conditioning change (A1-A4): W2 must be ",
      "regressed on the same X_t as W1."
    )
  ))
}

#' Reduced-form gamma: the Y2-on-PC slope block of beta2R, transposed to the
#' J x I (instruments x components) layout the pipeline quadratic builder
#' (build_pipeline_quadratic_system) expects. Selects the principal-component
#' columns by name (anchored "^pc[0-9]+$"); intercept and any l.y1* columns
#' are excluded because the instruments are the PCs only -- the conditioning
#' lags never enter gamma. Shared by the stage-06 consumption-equation table
#' and the tau* identification-strength benchmark.
#' @param beta2r I x (1 + n_pcs [+ n_lags]) Y2-on-PC reduced-form coefficients,
#'   with columns named (Intercept), pc1..pcJ, and optionally l.y1..l<H>.y1
#' @return n_pcs x I matrix with attr "method" = "reduced_form"
build_reduced_form_gamma <- function(beta2r) {
  pc_cols <- classify_common_design_cols(colnames(beta2r))$pc_cols
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

# Reduced-form gamma when it is defined, or NULL (with one standard warning)
# when it is not -- the impose-B = 0 / exact-news case where the PC slope block
# of beta2R is all zeros. Centralizes BOTH the upfront
# impose_news_projection_zero() check AND a defensive catch of
# build_reduced_form_gamma's all-zero "B = 0" error (in case the env flag and
# the actual coefficients ever disagree). Under the DEFAULT estimate-B path
# this returns EXACTLY what build_reduced_form_gamma returns (same matrix, same
# "method" attr); callers branch on NULL to skip the reduced-form benchmark.
reduced_form_gamma_or_skip <- function(beta2r) {
  skip_msg <- paste0(
    "Imposed exact-news projection (B = 0): the reduced-form gamma is ",
    "undefined (PC slopes are zero), so the reduced-form benchmark is skipped."
  )
  if (impose_news_projection_zero()) {
    cli_alert_warning(skip_msg)
    return(NULL)
  }
  tryCatch(
    build_reduced_form_gamma(beta2r),
    error = function(e) {
      if (grepl("B = 0", conditionMessage(e))) {
        cli_alert_warning(skip_msg)
        return(NULL)
      }
      stop(e)
    }
  )
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
