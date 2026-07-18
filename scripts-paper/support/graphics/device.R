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
