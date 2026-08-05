# Checks for the shared bounds-by-tau display cap. Run from the package root:
#   Rscript scripts-paper/tests/support/bounds_axis_checks.R
# The cap decides where five published figures stop, and it reads the grid's own
# spacing rather than a configured slack, so the cases that matter are the real
# subdivided grid, a grid with no subdivision at all, and the shapes that would
# make a naive trailing-run rule cut in the wrong place.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "graphics", "bounds_axis.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

# The real shape: a uniform backbone whose final intervals are subdivided, which
# is what paper_bounds_tau_grid builds and where the branch switch lives.
subdivided_grid <- function(tau_star = 0.415, backbone_n = 25L,
                            tail_fraction = 0.9, subdivisions = 4L) {
  backbone <- seq(0, tau_star, length.out = backbone_n)
  backbone <- backbone[backbone > 0 & backbone < tau_star]
  tail_start <- tail_fraction * tau_star
  below <- backbone[backbone < tail_start]
  ends <- c(max(below), backbone[backbone >= tail_start])
  dense <- unlist(lapply(seq_len(length(ends) - 1L), function(i) {
    seq(ends[i], ends[i + 1L], length.out = subdivisions + 1L)[-1L]
  }))
  list(grid = sort(c(0, below, dense)), boundary = max(below))
}

real <- subdivided_grid()
cap <- paper_bounds_tau_display_cap(real$grid)
check(
  "the cap lands on the sampled tau where subdivision begins",
  isTRUE(all.equal(cap, real$boundary))
)
check(
  "the cap hides exactly the subdivided tail",
  sum(real$grid > cap) == 8L
)
check(
  "the cap is itself a sampled tau, never an interpolated value",
  any(real$grid == cap)
)

# A grid that is never subdivided must not be truncated at all, so the rule can
# be applied unconditionally by every caller.
uniform <- seq(0, 0.4, length.out = 20L)
check(
  "a uniform grid keeps its largest tau",
  identical(paper_bounds_tau_display_cap(uniform), max(uniform))
)
check(
  "a uniform grid hides nothing",
  sum(uniform > paper_bounds_tau_display_cap(uniform)) == 0L
)

# Only a TRAILING subdivision is the branch switch. A finer patch in the middle
# is not, and truncating there would throw away most of the axis.
mid_fine <- sort(c(seq(0, 0.4, by = 0.05), c(0.205, 0.21, 0.215)))
check(
  "an interior fine patch does not truncate the axis",
  identical(paper_bounds_tau_display_cap(mid_fine), max(mid_fine))
)

# Order and duplication are the caller's data, not the caller's problem: the two
# renderers pass a raw tau column, and one of them plots a certified subset.
check(
  "unsorted input gives the same cap as sorted input",
  identical(
    paper_bounds_tau_display_cap(sample(real$grid)),
    paper_bounds_tau_display_cap(real$grid)
  )
)
check(
  "duplicated taus do not shift the cap",
  identical(
    paper_bounds_tau_display_cap(c(real$grid, real$grid)),
    cap
  )
)

# Fail loudly rather than return a cap that would render an empty or misleading
# panel.
check(
  "too few taus to judge spacing is an error",
  inherits(
    tryCatch(paper_bounds_tau_display_cap(c(0, 0.1)), error = function(e) e),
    "error"
  )
)
check(
  "a nonfinite tau is an error",
  inherits(
    tryCatch(
      paper_bounds_tau_display_cap(c(0, 0.1, 0.2, NA_real_)),
      error = function(e) e
    ),
    "error"
  )
)

.test$finish()
