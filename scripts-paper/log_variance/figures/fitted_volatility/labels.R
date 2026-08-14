# Vertical axis titles for the fitted-volatility panels. Two families of plots,
# each in the two forms their two renderers need. Split out of plot.R for the
# repository line cap.
#
# The families are the exponent PC_R,t' theta_R and its exponential, both on a
# linear axis. Neither halves the exponent: the design zeroes the intercept
# (envelope.R), so what these panels profile is a log variance net of its level
# and a variance ratio, not a standard deviation.
#
# write_svg draws through cairo, which bakes text into path glyphs, so its form
# has to be plotmath -- raw LaTeX would print its own dollar signs and
# backslashes. The combined slack panels render through svglite instead, whose
# real <text> \includesvg re-typesets, so those take the LaTeX source in the
# dollar-delimited form the slack keys already use.
LOGVAR_FITTED_VOL_Y_LABEL <- expression(
  "Identified set for " * PC[list(R, t)]^T * theta[R]
)
LOGVAR_FITTED_VOL_Y_LABEL_TEX <- "Identified set for $PC_{R,t}^{T}\\theta_{R}$"
LOGVAR_FITTED_VOL_Y_LABEL_EXP <- expression(
  "Identified set for " * exp(PC[list(R, t)]^T * theta[R])
)
LOGVAR_FITTED_VOL_Y_LABEL_EXP_TEX <-
  "Identified set for $\\exp(PC_{R,t}^{T}\\theta_{R})$"

# In-figure title of the single-slack panels, appended to the estimator's
# display name. Fixed here for the same reason the axis title is, and named in
# words because the title sits above a plotmath-free line.
LOGVAR_FITTED_VOL_TITLE_QUANTITY <-
  "fitted log variance, net of its intercept"
