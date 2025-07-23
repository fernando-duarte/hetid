#' Run Theta Identification for Multiple Maturities
#'
#' Convenience function to run optimize_theta_identification for multiple
#' maturities and/or different numbers of principal components extracted from
#' financial asset returns.
#'
#' @param maturities_to_test Vector of maturities to test
#' @param n_pcs_options Vector of PC counts to test (default 4)
#' @param ... Additional arguments passed to optimize_theta_identification
#'
#' @return Data frame with results for all combinations
#'
#' @export
optimize_theta_all_maturities <- function(maturities_to_test,
                                          n_pcs_options = 4,
                                          ...) {
  results_list <- list()

  for (mat in maturities_to_test) {
    for (J in n_pcs_options) {
      cat("\nOptimizing for maturity", mat, "with J =", J, "PCs...\n")

      result <- tryCatch(
        {
          optimize_theta_identification(
            i = mat,
            n_pcs = J,
            ...
          )
        },
        error = function(e) {
          cat("Error for maturity", mat, "J =", J, ":", e$message, "\n")
          return(NULL)
        }
      )

      if (!is.null(result)) {
        results_list[[paste0("mat_", mat, "_J_", J)]] <- result
      }
    }
  }

  # Convert to data frame
  results_df <- do.call(rbind, lapply(results_list, function(res) {
    data.frame(
      maturity = res$maturity,
      J = res$n_pcs,
      tau_opt = res$tau_opt,
      theta_lower = res$theta_lower,
      theta_upper = res$theta_upper,
      interval_width = res$interval_width,
      objective = res$objective_value,
      time = res$time_elapsed,
      stringsAsFactors = FALSE
    )
  }))

  rownames(results_df) <- NULL

  return(list(
    summary = results_df,
    full_results = results_list
  ))
}
