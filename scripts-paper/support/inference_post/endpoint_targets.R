# Package-owned endpoint calibration; paper callers retain tuning and reporting policy.
paper_source_once(paper_path(
  "support", "inference_post", "identified_set_inference.R"
))

# Legacy paper fixtures may omit names. Known positional axes are labeled here;
# any supplied names are preserved so the public package validator checks them.
paper_endpoint_inputs <- function(draws, full) {
  full$lower <- full$set_lower
  full$upper <- full$set_upper
  for (field in c("lower", "upper", "lower_status", "upper_status", "point", "point_status")) {
    if (!is.null(draws[[field]]) && is.null(colnames(draws[[field]]))) {
      colnames(draws[[field]]) <- full$coef
    }
  }
  list(full = full, draws = draws)
}
