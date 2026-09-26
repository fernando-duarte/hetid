# Validate the raw system at the public boundary.
quadratic_real_finite <- function(x) {
  is.numeric(x) && !is.complex(x) && all(is.finite(x))
}

quadratic_validate_constraint <- function(a, b, dimension) {
  assert_bad_argument_ok(
    is.matrix(a) && quadratic_real_finite(a) &&
      identical(dim(a), c(dimension, dimension)) && identical(unname(a), unname(t(a))),
    "A_i must contain finite symmetric square matrices",
    arg = "quadratic"
  )
  assert_bad_argument_ok(
    quadratic_real_finite(b) && is.null(dim(b)) && length(b) == dimension,
    "b_i must contain matching finite vectors",
    arg = "quadratic"
  )
}

quadratic_validate_system <- function(quadratic) {
  assert_bad_argument_ok(
    is.list(quadratic) && all(c("A_i", "b_i", "c_i") %in% names(quadratic)),
    "quadratic must contain A_i, b_i and c_i",
    arg = "quadratic"
  )
  count <- length(quadratic$A_i)
  assert_bad_argument_ok(is.list(quadratic$A_i) && count > 0,
    "A_i must be a nonempty list",
    arg = "quadratic"
  )
  dimension <- nrow(quadratic$A_i[[1L]])
  assert_bad_argument_ok(length(dimension) == 1L && dimension > 0,
    "A_i must contain nonempty square matrices",
    arg = "quadratic"
  )
  assert_bad_argument_ok(is.list(quadratic$b_i),
    "b_i must be a list of finite real vectors",
    arg = "quadratic"
  )
  assert_dimension_ok(
    length(quadratic$b_i) == count && length(quadratic$c_i) == count,
    "A_i, b_i and c_i must have equal lengths"
  )
  for (i in seq_len(count)) {
    quadratic_validate_constraint(quadratic$A_i[[i]], quadratic$b_i[[i]], dimension)
  }
  assert_bad_argument_ok(
    quadratic_real_finite(quadratic$c_i) && is.null(dim(quadratic$c_i)),
    "c_i must be finite numeric",
    arg = "quadratic"
  )
  dimension
}

quadratic_validate_objectives <- function(objectives, dimension) {
  assert_bad_argument_ok(
    is.matrix(objectives) && quadratic_real_finite(objectives) &&
      nrow(objectives) == dimension && ncol(objectives) > 0,
    "objectives must be a finite matrix with one row per coordinate",
    arg = "objectives"
  )
}

quadratic_validate_rows <- function(x, name, dimension) {
  if (is.null(x)) {
    return(matrix(numeric(), nrow = 0L, ncol = dimension))
  }
  assert_bad_argument_ok(
    is.matrix(x) && quadratic_real_finite(x) && ncol(x) == dimension,
    paste0(name, " must have one column per coordinate"),
    arg = name
  )
  x
}

validate_quadratic_evidence <- function(quadratic, objectives, points, directions) {
  dimension <- quadratic_validate_system(quadratic)
  quadratic_validate_objectives(objectives, dimension)
  list(
    quadratic = quadratic, objectives = objectives,
    points = quadratic_validate_rows(points, "points", dimension),
    directions = quadratic_validate_rows(directions, "directions", dimension)
  )
}
