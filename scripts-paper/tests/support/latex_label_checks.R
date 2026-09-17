# Checks for the LaTeX-typeset figure labels. Run from the package root:
#   Rscript scripts-paper/tests/support/latex_label_checks.R
# Needs TeX Live's latex and dvisvgm on the PATH, as the region figures do.
# The cases are the ones the placement depends on: a label's ink box comes
# back in points, an anchor pins the edge adj names, a rotation turns the
# corners about that anchor, and the written file carries paths, not text.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("support", "graphics", "latex_labels.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

close_to <- function(a, b) isTRUE(all.equal(unname(a), unname(b), tol = 1e-6))

glyphs <- typeset_latex_labels(c("$0.00$", "$b_{1,N}$"), 12L)
digits <- glyphs[[1]]
check(
  "a typeset label is glyph paths with a positive ink box in points",
  length(digits$paths) == 4L && all(grepl("^<path d=", digits$paths)) &&
    length(digits$box) == 4L && all(digits$box[3:4] > 0) && digits$box[3] < 30
)
check(
  "a subscript deepens the ink box",
  glyphs[[2]]$box[4] > digits$box[4]
)

# adj = c(1, 1) pins the top-right corner of the ink at the anchor, so no
# corner lies right of x or above y (y runs down the page)
pinned <- place_latex_label(digits, 100, 50, adj = c(1, 1))
check(
  "the anchored edge of the ink lands on the anchor",
  close_to(max(pinned$corners[, 1]), 100) && close_to(min(pinned$corners[, 2]), 50) &&
    close_to(pinned$corners[, 1], 100 - c(digits$box[3], 0, digits$box[3], 0))
)
check(
  "the group carries the anchor's translation and fills its paths",
  grepl("translate(100.00,50.00) rotate(-0.00)", pinned$group, fixed = TRUE) &&
    sum(gregexpr("fill: #000000", pinned$group)[[1]] > 0) == 4L
)

# a quarter turn counterclockwise, as graphics::text counts it, stands the
# label up from its left-middle anchor: it reads upward from y, its width
# becoming its vertical extent, and its height straddles x
turned <- place_latex_label(digits, 100, 50, adj = c(0, 0.5), rot = 90)
check(
  "rotation turns the corners about the anchor",
  close_to(diff(range(turned$corners[, 2])), digits$box[3]) &&
    close_to(diff(range(turned$corners[, 1])), digits$box[4]) &&
    close_to(mean(turned$corners[, 1]), 100) && close_to(max(turned$corners[, 2]), 50)
)

fixture <- tempfile(fileext = ".svg")
writeLines(
  c(
    "<svg xmlns='http://www.w3.org/2000/svg' width='200pt' height='100pt' viewBox='0 0 200 100'>",
    "<g class='svglite'>", "<g clip-path='url(#c)'>", "<line x1='1' y1='1' x2='2' y2='2' />",
    "</g>", "</g>", "</svg>"
  ),
  fixture
)
write_latex_labels(fixture, pinned$group)
written <- readLines(fixture)
check(
  "the written file keeps its drawing, adds the label after it, and has no text",
  !any(grepl("<text", written, fixed = TRUE)) &&
    which(grepl("<g transform=", written, fixed = TRUE)) >
      which(grepl("<line ", written, fixed = TRUE)) &&
    identical(utils::tail(written, 2), c("</g>", "</svg>"))
)

.test$finish()
