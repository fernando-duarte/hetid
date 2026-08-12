# Fail-safe SVG device lifecycle shared by publication figures.

write_svg <- function(path, width, height, draw, family = NULL) {
  stopifnot(
    is.character(path),
    length(path) == 1L,
    is.finite(width),
    width > 0,
    is.finite(height),
    height > 0,
    is.function(draw)
  )
  arguments <- list(
    filename = path,
    width = width,
    height = height
  )
  if (!is.null(family)) {
    arguments$family <- family
  }
  do.call(grDevices::svg, arguments)
  device <- grDevices::dev.cur()
  on.exit(
    {
      open <- grDevices::dev.list()
      if (!is.null(open) && device %in% open) {
        grDevices::dev.off(which = device)
      }
    },
    add = TRUE
  )
  draw()
  invisible(path)
}

svg_attr <- function(tag, name) {
  hit <- regexpr(paste0(" ", name, "='[^']*'"), tag)
  if (hit < 0L) {
    return(NA_character_)
  }
  gsub("^ [^=]+='|'$", "", regmatches(tag, hit))
}

svg_attr_num <- function(tag, name) as.numeric(svg_attr(tag, name))

svg_tags <- function(svg, pattern) {
  regmatches(svg, gregexpr(paste0("<(", pattern, ")[^>]*>"), svg))[[1]]
}

# Pulls the numbers out of an attribute that mixes them with syntax or units --
# a points list, a transform, a "504.00pt" canvas dimension.
svg_free_numbers <- function(text) {
  if (is.na(text)) {
    return(numeric(0))
  }
  as.numeric(regmatches(text, gregexpr("-?[0-9.]+", text))[[1]])
}

# svglite records the advance width in textLength; the vertical extent is the
# font's ascent and descent, which the file does not carry, so it is taken as a
# generous fraction of the em to bound rather than clip the glyphs.
svg_text_corners <- function(tag) {
  size <- as.numeric(sub(".*font-size: ([0-9.]+)px.*", "\\1", tag))
  width <- svg_free_numbers(svg_attr(tag, "textLength"))
  if (length(width) != 1L) width <- 0
  anchor <- svg_attr(tag, "text-anchor")
  left <- if (identical(anchor, "middle")) {
    -width / 2
  } else if (identical(anchor, "end")) {
    -width
  } else {
    0
  }
  placement <- svg_attr(tag, "transform")
  placement <- if (is.na(placement)) {
    c(svg_attr_num(tag, "x"), svg_attr_num(tag, "y"), 0)
  } else {
    svg_free_numbers(placement)
  }
  radians <- placement[3] * pi / 180
  corner <- expand.grid(dx = left + c(0, width), dy = c(-0.8, 0.25) * size)
  cbind(
    placement[1] + corner$dx * cos(radians) - corner$dy * sin(radians),
    placement[2] + corner$dx * sin(radians) + corner$dy * cos(radians)
  )
}

# The full-canvas background rectangles svglite emits are deliberately excluded:
# they cover the device by construction and would make every crop a no-op.
svg_ink_box <- function(svg) {
  corners <- list()
  for (tag in svg_tags(svg, "polyline|polygon")) {
    xy <- svg_free_numbers(svg_attr(tag, "points"))
    corners[[length(corners) + 1L]] <- cbind(
      xy[c(TRUE, FALSE)], xy[c(FALSE, TRUE)]
    )
  }
  for (tag in svg_tags(svg, "line")) {
    corners[[length(corners) + 1L]] <- cbind(
      c(svg_attr_num(tag, "x1"), svg_attr_num(tag, "x2")),
      c(svg_attr_num(tag, "y1"), svg_attr_num(tag, "y2"))
    )
  }
  for (tag in svg_tags(svg, "circle")) {
    radius <- svg_attr_num(tag, "r")
    corners[[length(corners) + 1L]] <- cbind(
      svg_attr_num(tag, "cx") + c(-radius, radius),
      svg_attr_num(tag, "cy") + c(-radius, radius)
    )
  }
  for (tag in svg_tags(svg, "text")) {
    corners[[length(corners) + 1L]] <- svg_text_corners(tag)
  }
  ink <- do.call(rbind, corners)
  stopifnot(nrow(ink) > 0L, all(is.finite(ink)))
  c(apply(ink, 2, min), apply(ink, 2, max))
}

# svglite fixes the canvas before the first stroke, and persp keeps the aspect
# of its projected box, so the slack around a 3D figure cannot be recovered by
# margin settings -- the finished file is retrofitted to the ink it carries.
crop_svg_to_ink <- function(path, pad = 2) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  header <- regmatches(svg, regexpr("<svg [^>]*>", svg))
  canvas <- vapply(c("width", "height"), function(side) {
    svg_free_numbers(svg_attr(header, side))
  }, numeric(1))
  ink <- svg_ink_box(svg)
  lo <- pmax(0, ink[1:2] - pad)
  hi <- pmin(canvas, ink[3:4] + pad)
  span <- hi - lo
  stopifnot(all(span > 0))
  cropped <- sub(" width='[^']*'", sprintf(" width='%.2fpt'", span[1]), header)
  cropped <- sub(
    " height='[^']*'", sprintf(" height='%.2fpt'", span[2]), cropped
  )
  cropped <- sub(
    " viewBox='[^']*'",
    sprintf(" viewBox='%.2f %.2f %.2f %.2f'", lo[1], lo[2], span[1], span[2]),
    cropped
  )
  writeLines(sub(header, cropped, svg, fixed = TRUE), path)
  invisible(path)
}
