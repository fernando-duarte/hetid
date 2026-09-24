# Generic SVG coordinate parsing is not a table-acceptance definition.

svg_numbers_fixture <- function(text) {
  as.numeric(regmatches(text, gregexpr("-?[0-9.]+", text, perl = TRUE))[[1L]])
}
