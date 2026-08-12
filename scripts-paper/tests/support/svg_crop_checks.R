# Checks for the ink crop applied to the published 3D region figures. Run from
# the package root:
#   Rscript scripts-paper/tests/support/svg_crop_checks.R
# The crop reads geometry back out of a finished file, so the cases that matter
# are the ones where a coordinate hides behind something that is not a number:
# the "pt" on the canvas, the "px" on a text advance, the letters inside
# translate()/rotate(), and the full-canvas background rects that would
# otherwise make every crop a no-op.

source(file.path("scripts-paper", "config", "paths.R"))
paper_source_once(paper_path("support", "graphics", "device.R"))
paper_source_once(paper_path("tests", "support", "harness.R"))
.test <- paper_test_harness()
check <- .test$check

svg_fixture <- function(...) {
  path <- tempfile(fileext = ".svg")
  writeLines(
    c(
      paste0(
        "<svg xmlns='http://www.w3.org/2000/svg' width='200.00pt' ",
        "height='100.00pt' viewBox='0 0 200.00 100.00'>"
      ),
      "<rect width='100%' height='100%' style='fill: #FFFFFF;'/>",
      "<rect x='0.00' y='0.00' width='200.00' height='100.00' />",
      ...,
      "</svg>"
    ),
    path
  )
  path
}

cropped_box <- function(..., pad = 0) {
  path <- crop_svg_to_ink(svg_fixture(...), pad = pad)
  svg <- paste(readLines(path), collapse = "\n")
  header <- regmatches(svg, regexpr("<svg [^>]*>", svg))
  svg_free_numbers(svg_attr(header, "viewBox"))
}

close_to <- function(box, expected) isTRUE(all.equal(box, expected, tol = 1e-6))

# Strokes: the crop follows the polyline and ignores both background rects, one
# of which is sized in percent and the other in device units.
strokes <- cropped_box("<polyline points='50.00,30.00 150.00,70.00 ' />")
check(
  "a polyline crops to its own extent, not the canvas",
  close_to(strokes, c(50, 30, 100, 40))
)

# Lines and circles carry their coordinates in separate attributes, and the
# circle's radius has to widen the box on both sides.
marks <- cropped_box(
  "<line x1='10.00' y1='12.00' x2='20.00' y2='22.00' />",
  "<circle cx='180.00' cy='80.00' r='5.00' />"
)
check(
  "a line and a circle bound the box, radius included",
  close_to(marks, c(10, 12, 175, 73))
)

# The rotated axis label. Its advance width is quoted in px and its placement
# hides inside translate()/rotate(), whose own letters must not be read as
# coordinates -- "translate" and "rotate" both end in an "e".
rotated <- cropped_box(paste0(
  "<text transform='translate(100.00,50.00) rotate(-90)' ",
  "text-anchor='middle' style='font-size: 10.00px; font-family: \"Arial\";' ",
  "textLength='40.00px'>"
))
check(
  "a rotated label is bounded across its advance width",
  close_to(rotated[c(2, 4)], c(30, 40))
)
check(
  "a rotated label is bounded across its ascent and descent",
  close_to(rotated[c(1, 3)], c(92, 10.5))
)

# Padding is real but never runs off the canvas the viewBox still refers to.
padded <- cropped_box("<polyline points='2.00,1.00 150.00,70.00 ' />", pad = 3)
check(
  "padding is clamped at the canvas edge",
  close_to(padded, c(0, 0, 153, 73))
)

# A file with nothing drawn in it is a rendering failure, not an empty crop.
check(
  "an inkless file is an error",
  inherits(
    tryCatch(crop_svg_to_ink(svg_fixture()), error = function(e) e),
    "error"
  )
)

.test$finish()
