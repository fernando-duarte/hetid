# Available heteroskedasticity-instrument choices for the mean-equation Z:
# pure data (column name + description), one entry per column
# build_yield_volatility.R already computes on yield_vol. Adding a choice:
# add its column/label here and the column itself in
# build_yield_volatility.R, then point PAPER_ANALYSIS_CONTRACT$input$
# instrument$active at its id -- no other file needs to change. Switching
# which choice is active also needs
# scripts-paper/tools/regen_egarch_decision.R (the committed EGARCH
# gate record is bound to the active choice's exact diagnostics; see that
# file's header for why).

PAPER_INSTRUMENT_CHOICES <- list(
  y60_vol = list(
    column = "y60_vol",
    label = "the de-meaned realized quarterly volatility of the five-year yield"
  ),
  y60_vol_log = list(
    column = "y60_vol_log",
    label = paste(
      "the de-meaned natural log of the realized quarterly volatility of",
      "the five-year yield"
    )
  )
)

# The active choice's column/label, resolved lazily so this file can be
# sourced before PAPER_ANALYSIS_CONTRACT exists (only the call site needs
# it to exist).
paper_instrument_choice <- function(
  active = PAPER_ANALYSIS_CONTRACT$input$instrument$active,
  choices = PAPER_INSTRUMENT_CHOICES
) {
  if (!active %in% names(choices)) {
    stop(sprintf(
      "Unknown instrument choice %s; must be one of: %s",
      active, paste(names(choices), collapse = ", ")
    ), call. = FALSE)
  }
  choices[[active]]
}

# LaTeX description of the active choice, unchanged in form from the prior
# single-instrument paper_instrument_description() (still \emph{label}
# (\texttt{column})).
paper_instrument_description <- function(choice = paper_instrument_choice()) {
  latex_column <- gsub("_", "\\_", choice$column, fixed = TRUE)
  sprintf("%s (\\texttt{%s})", choice$label, latex_column)
}
