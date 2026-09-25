# Fixed-design delegation keeps paper labels and per-response rank semantics.
local({
  x <- cbind("(Intercept)" = 1, a = seq(-1, 1, length.out = 40), b = sin(seq_len(40)))
  labels <- list(colnames(x), NULL, c("constant", "", ""), rep("", 3))
  for (estimator in c("ppml", "harvey")) {
    fit_response <- get(paste0("logvar_", estimator, "_fit_response"))
    for (i in seq_along(labels)) {
      design <- x
      colnames(design) <- labels[[i]]
      y <- exp(drop(x %*% c(0.2, -0.3, 0.1)))
      fit <- fit_response(y, design)
      expected <- if (estimator == "harvey" && is.null(labels[[i]])) {
        paste0("V", seq_len(3))
      } else {
        labels[[i]]
      }
      check(
        paste(estimator, "preserves complete design labels", i),
        logvar_fit_ok(fit) && identical(names(fit$coef), expected) &&
          isTRUE(all.equal(unname(fit$coef), c(0.2, -0.3, 0.1), tolerance = 1e-6))
      )
      if (estimator == "harvey") {
        expected_dimnames <- if (is.null(labels[[i]])) NULL else rep(labels[i], 2)
        check(
          paste("Harvey preserves information labels", i),
          identical(dimnames(fit$diagnostics$info_matrix), expected_dimnames)
        )
      }
    }
  }
  check("named starts cannot silently reorder paper design columns", {
    error <- tryCatch(logvar_ppml_fit_response(rep(1, 40), x,
      start = setNames(c(0, 0, 0), rev(colnames(x)))
    ), error = identity)
    inherits(error, "error") && grepl("Start names must equal the paper design labels",
      conditionMessage(error),
      fixed = TRUE
    )
  })
  for (estimator in c("ppml", "harvey")) {
    fit_response <- get(paste0("logvar_", estimator, "_fit_response"))
    for (response in list(rep(1, 3), rep(0, 3))) {
      error <- tryCatch(fit_response(response, x[1:3, ]), error = identity)
      check(
        paste(estimator, "requires the package minimum sample", any(response > 0)),
        inherits(error, "hetid_error_insufficient_data")
      )
    }
    error <- tryCatch(fit_response(rep(1, 40), x, start = 0), error = identity)
    check(
      paste(estimator, "rejects a short start with a structured error"),
      inherits(error, "hetid_error_bad_argument")
    )
  }
  singular <- logvar_harvey_fit_response(rep(1, 40), cbind(x, duplicate = x[, 2]))
  check(
    "singular Harvey design returns a typed fit failure",
    !logvar_fit_ok(singular) && identical(singular$diagnostics$error_class, "singular_design")
  )
})
