profile_containing_box <- function(tab) {
  lower <- tab$outer_lower
  upper <- tab$outer_upper
  assert_bad_argument_ok(!is.null(lower) && !is.null(upper) &&
    !is.null(tab$status) && !anyNA(tab$status), "Containing bounds and statuses are required")
  if (any(!is.na(lower) & !is.na(upper) & lower > upper)) {
    stop_hetid("A containing lower bound exceeds its upper bound.")
  }
  bounded <- tab$status == "bounded"
  if (any(bounded & !(is.finite(lower) & is.finite(upper)))) {
    stop_hetid("A bounded theta row lacks finite containing bounds.")
  }
  inside <- lower[bounded] <= tab$set_lower[bounded] &
    tab$set_upper[bounded] <= upper[bounded]
  if (!all(inside)) {
    stop_hetid("An attained theta endpoint lies outside its containing bound.")
  }
  list(lower = lower, upper = upper)
}
