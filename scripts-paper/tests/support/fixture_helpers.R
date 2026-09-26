# Read frozen 17-digit coefficients as a raw quadratic system.
if (!exists("profile_fixture_root", inherits = FALSE)) {
  profile_fixture_root <- paper_path("tests", "support", "fixtures")
}
profile_fixture_quadratic <- function(name) {
  path <- file.path(profile_fixture_root, paste0(name, "-quadratic.csv"))
  rows <- read.csv(path, stringsAsFactors = FALSE)
  dimension <- max(rows$row, rows$column, na.rm = TRUE)
  ids <- sort(unique(rows$constraint))
  matrices <- lapply(ids, function(id) {
    out <- matrix(0, dimension, dimension)
    part <- rows[rows$constraint == id & rows$kind == "A", ]
    for (j in seq_len(nrow(part))) {
      out[part$row[j], part$column[j]] <- part$value[j]
    }
    out
  })
  vectors <- lapply(ids, function(id) {
    out <- numeric(dimension)
    part <- rows[rows$constraint == id & rows$kind == "b", ]
    for (j in seq_len(nrow(part))) out[part$row[j]] <- part$value[j]
    out
  })
  constants <- vapply(ids, function(id) {
    rows$value[rows$constraint == id & rows$kind == "c"]
  }, numeric(1))
  list(A_i = matrices, b_i = vectors, c_i = constants)
}

profile_ray_check <- function(qs, direction, start) {
  all(vapply(c(-start, start, -2 * start, 2 * start), function(t) {
    all(quadratic_constraint_values(t * direction, qs) < 0)
  }, logical(1))) &&
    all(vapply(qs$A_i, function(A) {
      drop(crossprod(direction, A %*% direction)) < 0
    }, logical(1)))
}
