# Frozen-input contract for the PPML estimator.

logvar_ppml_validate_inputs <- function(inputs, sample_contract) {
  qtr <- inputs$qtr
  if (anyNA(qtr)) stop("logvar_ppml_validate_inputs: qtr has missing values")
  if (anyDuplicated(qtr)) {
    stop("logvar_ppml_validate_inputs: qtr has duplicate keys")
  }
  if (!identical(sort(qtr), sort(sample_contract$qtr))) {
    stop("logvar_ppml_validate_inputs: qtr universe differs from the contract")
  }
  if (!identical(qtr, sample_contract$qtr)) {
    stop("logvar_ppml_validate_inputs: qtr order differs from the contract")
  }
  n <- sample_contract$n
  bad_rows <- length(inputs$w1) != n ||
    nrow(inputs$w2) != n ||
    nrow(inputs$pcr) != n
  if (bad_rows) {
    stop(
      "logvar_ppml_validate_inputs: w1/w2/pcr row counts must equal contract n"
    )
  }
  pc_names <- colnames(inputs$pcr)
  expected_pc_names <- PAPER_ANALYSIS_CONTRACT$model$return_pc_cols
  if (!identical(pc_names, expected_pc_names) ||
    !identical(pc_names, sample_contract$pc_names)) {
    stop(
      sprintf(
        "logvar_ppml_validate_inputs: pcr columns must be %s",
        paste(expected_pc_names, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (nrow(inputs$w2) != length(inputs$w1) ||
    nrow(inputs$pcr) != length(inputs$w1)) {
    stop("logvar_ppml_validate_inputs: w1/w2/pcr dimensions disagree")
  }
  recomputed <- logvar_sample_id(
    inputs$qtr, inputs$w1, inputs$w2, inputs$pcr
  )
  if (!identical(recomputed, sample_contract$sample_id)) {
    stop(
      paste(
        "logvar_ppml_validate_inputs: recomputed sample_id does not match",
        "the contract"
      )
    )
  }
  invisible(inputs)
}
