# compute_identification_moments() is exported by the installed hetid package;
# the moments object is a validated hetid_moments container
# carrying the maturity identity.
#
# Single pipeline entry point for assembling the identified-set quadratic system.
# Delegates to the exported generalized-instrument front end
# hetid::build_general_quadratic_system(), which with a J x I matrix gamma (one
# combination per component, K_i = 1) is bit-identical on $quadratic to the
# legacy hetid::build_quadratic_system() and exposes the same L_i/V_i/Q_i values
# in $components. This is the K_i = 1 (one combination per component) date-t
# specialization of docs/lewbel_multivariate_set_identification.tex
# ("Instrument choices as special cases"), so the
# whole pipeline runs on one generalized assembly path.
#
# The general builder returns $components as a plain list (plus a $labels frame).
# We re-attach the hetid_components class and its maturities/n_components integer
# attributes so the components remain valid inputs to the hetid quadratic-stage APIs
# (which gate on inherits(., "hetid_components")). The added $labels element is an
# accepted, documented delta.
build_pipeline_quadratic_system <- function(gamma, tau, moments) {
  out <- hetid::build_general_quadratic_system(gamma, tau, moments)
  out$components <- structure(
    out$components,
    maturities = as.integer(attr(out, "maturities")),
    n_components = as.integer(attr(out, "n_components")),
    class = "hetid_components"
  )
  out
}
