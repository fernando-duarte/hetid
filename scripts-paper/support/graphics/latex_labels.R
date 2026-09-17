# Figure labels typeset by LaTeX and embedded as glyph outlines. The strings of
# one figure are typeset together, one per page, and converted with dvisvgm,
# so each label is a few path elements with an exact ink box in the points
# svglite writes. A label is placed on the finished file by that box, and the
# file then carries no text: the paper includes it with inkscapelatex=false
# and Inkscape draws exactly what is here.

# The manuscript's class, body size and math package, so a label matches the
# text around the figure.
latex_label_document <- function(strings, pointsize) {
  c(
    sprintf("\\documentclass[%dpt]{article}", pointsize),
    "\\usepackage{amsmath}",
    "\\pagestyle{empty}",
    "\\begin{document}",
    paste0("\\noindent", strings, collapse = "\n\\newpage\n"),
    "\\end{document}"
  )
}

run_latex_label_tool <- function(command, args, work) {
  if (!nzchar(Sys.which(command))) {
    stop(command, " is not on the PATH; figure labels need TeX Live's latex and dvisvgm")
  }
  status <- system2(command, args, stdout = FALSE, stderr = FALSE)
  if (status != 0) {
    log <- file.path(work, "labels.log")
    trace <- if (file.exists(log)) utils::tail(readLines(log, warn = FALSE), 15) else character()
    stop(
      command, " failed (status ", status, ") typesetting figure labels\n",
      paste(trace, collapse = "\n")
    )
  }
}

# One entry per string: its glyph path elements and its ink box as
# c(x, y, width, height), both in the page coordinates dvisvgm wrote.
typeset_latex_labels <- function(strings, pointsize) {
  work <- tempfile("latex-labels-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  tex <- file.path(work, "labels.tex")
  writeLines(latex_label_document(strings, pointsize), tex)
  run_latex_label_tool("latex", c(
    "-interaction=batchmode", "-halt-on-error",
    paste0("-output-directory=", shQuote(work)), shQuote(tex)
  ), work)
  # the page number is padded to a fixed width, as dvisvgm would otherwise
  # pad it to the width of the page count
  run_latex_label_tool("dvisvgm", c(
    "--no-fonts=1", "--exact-bbox", "--page=1-",
    "-o", shQuote(file.path(work, "label-%3p.svg")),
    shQuote(file.path(work, "labels.dvi"))
  ), work)
  lapply(seq_along(strings), function(k) {
    svg <- paste(
      readLines(file.path(work, sprintf("label-%03d.svg", k)), warn = FALSE),
      collapse = "\n"
    )
    header <- regmatches(svg, regexpr("<svg [^>]*>", svg))
    list(
      paths = regmatches(svg, gregexpr("<path [^>]*/>", svg))[[1]],
      box = svg_free_numbers(svg_attr(header, "viewBox"))
    )
  })
}

# Places one typeset label at (x, y) in the file's coordinates, y down the
# page. adj reads as in graphics::text: adj[1] = 0 puts the ink box's left
# edge at x and 1 its right edge; adj[2] = 0 puts its bottom at y and 1 its
# top. rot is graphics::text's counterclockwise angle, applied about that
# anchor. Returns the group to write and the placed corners of the ink box.
place_latex_label <- function(glyph, x, y, adj = c(0.5, 0.5), rot = 0) {
  box <- glyph$box
  anchor <- c(box[1] + adj[1] * box[3], box[2] + (1 - adj[2]) * box[4])
  radians <- -rot * pi / 180
  turn <- matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), 2)
  corner <- unname(as.matrix(expand.grid(box[1] + c(0, box[3]), box[2] + c(0, box[4]))))
  corners <- sweep(t(turn %*% t(sweep(corner, 2, anchor))), 2, c(x, y), "+")
  # svglite's stylesheet strokes every path and fills none; a glyph is the
  # other way round
  paths <- sub(
    "<path ", "<path style='fill: #000000; stroke: none;' ", glyph$paths,
    fixed = TRUE
  )
  group <- sprintf(
    "<g transform='translate(%.2f,%.2f) rotate(%.2f) translate(%.2f,%.2f)'>\n%s\n</g>",
    x, y, -rot, -anchor[1], -anchor[2], paste(paths, collapse = "\n")
  )
  list(group = group, corners = corners)
}

# Appends the placed groups inside svglite's root group, after its clipped
# drawing group, in the order given.
write_latex_labels <- function(path, groups) {
  svg <- paste(readLines(path, warn = FALSE), collapse = "\n")
  closing <- "\n</g>\n</svg>"
  stopifnot(endsWith(svg, closing))
  body <- substr(svg, 1L, nchar(svg) - nchar(closing))
  writeLines(paste0(body, "\n", paste(groups, collapse = "\n"), closing), path)
  invisible(path)
}
