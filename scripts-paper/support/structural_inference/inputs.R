# Helper function: rebuild the paper's date-keyed inputs on forecast origins
# the mean frame is the identified-set join over the paper window, complete on
# every column; PC_R joins on the left so its missing first quarter stays a
# mean row. each response quarter t + 1 becomes origin t, under the settings'
# origin names
paper_structural_inference_prepare <- function(
  settings = structural_inference_settings(),
  mean_inputs = list(gr1_pcecc96, lag_expected_sdf_pc, sdf_news_pc, z_source()),
  return_pcs = lag_asset_return_pc
) {
  mean_frame <- mean_inputs |>
    purrr::reduce(dplyr::full_join, by = "qtr") |>
    filter_window() |>
    tidyr::drop_na() |>
    dplyr::arrange(qtr)
  frame <- as.data.frame(dplyr::left_join(mean_frame, return_pcs, by = "qtr"))
  missing_cols <- setdiff(settings$inputs, names(frame))
  if (length(missing_cols)) {
    stop("Paper structural inference inputs lack column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  origin <- stats::setNames(frame[settings$inputs], names(settings$inputs))
  structural_inference_prepare(
    data.frame(qtr = frame$qtr - 1L, origin, check.names = FALSE),
    settings
  )
}

# Helper function: prepare the mean sample and mark the quarters the variance equation reads
# quarters are forecast origins: the mean equation reads every origin of the
# window, the variance equation the origins that also have PC_R, which may drop
# only at the window's start
structural_inference_prepare <- function(variables, settings) {
  mean_cols <- c(settings$y, settings$x, settings$y2, settings$z)
  needed <- c("qtr", mean_cols, settings$x_var)
  stopifnot(is.data.frame(variables), !anyDuplicated(names(variables)))
  missing_cols <- setdiff(needed, names(variables))
  if (length(missing_cols)) {
    stop("Structural inference inputs lack column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  stopifnot(
    inherits(variables$qtr, "yearquarter"), !anyNA(variables$qtr),
    all(vapply(variables[needed[-1L]], is.numeric, logical(1)))
  )
  begin <- tsibble::yearquarter(settings$date_begin)
  end <- tsibble::yearquarter(settings$date_end)
  stopifnot(
    length(begin) == 1L, length(end) == 1L, !is.na(begin),
    !is.na(end), begin <= end
  )
  # plain data frame, so row subsetting never goes through tsibble's rules
  source_row <- which(variables$qtr >= begin & variables$qtr <= end)
  frame <- as.data.frame(variables)[source_row, needed]
  if (!nrow(frame) || is.unsorted(frame$qtr, strictly = TRUE)) {
    stop("The estimation window must contain sorted, unique, contiguous quarters.",
      call. = FALSE
    )
  }
  # the mean sample: the window, complete on what the mean equation and its
  # instrument read. only the edges may drop, an interior drop would join
  # quarters that are not adjacent
  complete_mean <- stats::complete.cases(frame[mean_cols])
  frame <- frame[complete_mean, ]
  source_row <- source_row[complete_mean]
  n <- nrow(frame)
  if (!n || any(frame$qtr[-1L] - frame$qtr[-n] != 1)) {
    stop("The mean-equation estimation sample must have no missing quarters.", call. = FALSE)
  }
  # the variance rows travel with their mean rows through every resample, and a
  # draw drops the ones without PC_R after the mean fit. nothing is filled in
  variance <- stats::complete.cases(frame[settings$x_var])
  first <- match(TRUE, variance)
  if (is.na(first) || !all(variance[first:n])) {
    stop("Variance regressors must be complete on the mean-equation sample ",
      "after its first quarters.",
      call. = FALSE
    )
  }
  if (!all(is.finite(as.matrix(frame[mean_cols]))) ||
    !all(is.finite(as.matrix(frame[variance, settings$x_var])))) {
    stop("The prepared observation tuples must contain finite numeric values.", call. = FALSE)
  }
  stopifnot(
    n > max(length(settings$x), length(settings$x_var)) + 2L,
    sum(variance) > length(settings$x_var) + 2L, settings$block_length <= n
  )
  list(
    y = frame[[settings$y]], x = as.matrix(frame[settings$x]),
    y2 = as.matrix(frame[settings$y2]), z = as.matrix(frame[settings$z]),
    x_var = as.matrix(frame[settings$x_var]), variance = variance, qtr = frame$qtr,
    dates = hetid::to_period_end(as.Date(frame$qtr), "quarterly"),
    n_obs = n, variance_n_obs = sum(variance), columns = needed, settings = settings,
    sample = data.frame(qtr = frame$qtr, source_row = source_row, variance = variance)
  )
}
