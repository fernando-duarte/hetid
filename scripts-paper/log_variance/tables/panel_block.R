# Canonical marker and tablenote splicing for log-variance panels.

logvar_panel_block <- function(
  fragment,
  notes_lines,
  estimator,
  panel_marker = sprintf(
    "LOGVAR PANEL %s",
    estimator
  )
) {
  cut <- match("\\end{threeparttable}", fragment)
  if (is.na(cut)) {
    stop(
      "Panel fragment has no \\end{threeparttable}",
      call. = FALSE
    )
  }
  notes_marker <- sprintf(
    "LOGVAR NOTES %s",
    estimator
  )
  notes_block <- c(
    sprintf("%% BEGIN %s", notes_marker),
    "\\begin{tablenotes}[flushleft]",
    "\\scriptsize",
    paste0("\\item ", notes_lines),
    "\\end{tablenotes}",
    sprintf("%% END %s", notes_marker)
  )
  c(
    sprintf("%% BEGIN %s", panel_marker),
    fragment[seq_len(cut - 1L)],
    notes_block,
    fragment[cut:length(fragment)],
    sprintf("%% END %s", panel_marker)
  )
}
