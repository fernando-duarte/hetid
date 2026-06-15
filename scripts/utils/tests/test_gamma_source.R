# Baseline-gamma hook contract on synthetic moments (hermetic; needs no
# pipeline data). The data.rds integration fixture lives in
# test_z_width_pipeline.R. Run from the package root:
#   Rscript scripts/utils/tests/test_gamma_source.R
source(here::here("scripts/utils/common_settings.R"))

.pass <- 0L
.fail <- 0L
check <- function(label, cond) {
  if (isTRUE(cond)) {
    .pass <<- .pass + 1L
    cat(sprintf("PASS  %s\n", label))
  } else {
    .fail <<- .fail + 1L
    cat(sprintf("FAIL  %s\n", label))
  }
}

# Synthetic moments at a non-PC width (J = 5, I = 2)
set.seed(42)
t_obs <- 80L
j_dim <- 5L
w1 <- rnorm(t_obs)
w2 <- matrix(rnorm(t_obs * 2L), nrow = t_obs)
z <- matrix(
  rnorm(t_obs * j_dim),
  nrow = t_obs,
  dimnames = list(NULL, paste0("inst", seq_len(j_dim)))
)
moments <- suppressMessages(compute_identification_moments(w1, w2, z))

# vfci is defined only on pc1..pc4: at J = 5 it must error instructively
vfci_err <- tryCatch(
  {
    resolve_baseline_gamma("vfci", moments)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "vfci baseline errors instructively when J != 4",
  is.character(vfci_err) && grepl("HETID_BASELINE_GAMMA", vfci_err)
)

# Unknown non-path methods error rather than silently returning VFCI numbers
unk_err <- tryCatch(
  {
    resolve_baseline_gamma("not_a_method", moments)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "unknown gamma method errors instead of mislabeling VFCI",
  is.character(unk_err) && grepl("not_a_method", unk_err)
)

# Path form: build_gamma(moments) is honored, validated, and labeled
gamma_file <- tempfile(fileext = ".R")
writeLines(c(
  "build_gamma <- function(moments) {",
  "  j <- nrow(moments$r_i_0)",
  "  i <- attr(moments, \"n_components\")",
  "  matrix(seq_len(j * i) / (j * i), nrow = j, ncol = i)",
  "}"
), gamma_file)
g <- resolve_baseline_gamma(gamma_file, moments)
check(
  "path-form gamma has J x I shape, instrument rownames, and a custom method attr",
  is.matrix(g) && identical(dim(g), c(5L, 2L)) &&
    identical(rownames(g), paste0("inst", 1:5)) &&
    grepl("^custom:", attr(g, "method"))
)
check(
  "path-form gamma feeds build_quadratic_system at the custom width",
  is.list(build_quadratic_system(g, rep(0.2, 2L), moments)$quadratic)
)

# Invalid build_gamma outputs are rejected with context
bad_file <- tempfile(fileext = ".R")
writeLines("build_gamma <- function(moments) matrix(0, 5, 2)", bad_file)
bad_err <- tryCatch(
  {
    resolve_baseline_gamma(bad_file, moments)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "all-zero gamma column from build_gamma is rejected",
  is.character(bad_err) && grepl("zero", bad_err)
)

wrong_dim_file <- tempfile(fileext = ".R")
writeLines("build_gamma <- function(moments) matrix(1, 3, 2)", wrong_dim_file)
dim_err <- tryCatch(
  {
    resolve_baseline_gamma(wrong_dim_file, moments)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "wrong-row-count gamma from build_gamma is rejected",
  is.character(dim_err) && grepl("rows", dim_err)
)

# build_reduced_form_gamma selects PC slopes by name (drops intercept)
# Legacy PC-only beta2r: (Intercept, pc1..pc4), I = 2
beta2r <- matrix(seq_len(2L * 5L), nrow = 2L, ncol = 5L) # I=2 x (1 + 4 PCs)
colnames(beta2r) <- c("(Intercept)", paste0("pc", 1:4))
g_rf <- build_reduced_form_gamma(beta2r)
check(
  "build_reduced_form_gamma is (n_pcs x I) with the reduced_form method attr",
  is.matrix(g_rf) && identical(dim(g_rf), c(4L, 2L)) &&
    identical(attr(g_rf, "method"), "reduced_form")
)
# Backward compat: same numbers as the old t(beta2r[, -1]) on a PC-only matrix
check(
  "legacy PC-only beta2r yields the old t(beta2r[, -1]) values",
  identical(unclass(`attr<-`(g_rf, "method", NULL)), t(beta2r[, -1, drop = FALSE]))
)

# Wide beta2r: (Intercept, pc1..pc4, l.y1..l4.y1) -- lag slopes excluded
lag_sentinel <- -999
beta2r_wide <- cbind(
  beta2r,
  matrix(lag_sentinel,
    nrow = 2L, ncol = 4L,
    dimnames = list(NULL, hetid:::lag_grammar_names("y1", 4))
  )
)
g_rf_wide <- build_reduced_form_gamma(beta2r_wide)
check(
  "wide beta2r builds J x I gamma from PC slopes only (J = 4)",
  is.matrix(g_rf_wide) && identical(dim(g_rf_wide), c(4L, 2L)) &&
    isTRUE(all.equal(g_rf_wide, g_rf))
)
check(
  "lag sentinel slopes never appear in the reduced-form gamma",
  !any(g_rf_wide == lag_sentinel)
)

# All-zero beta2r (B = 0 imposed): undefined reduced-form gamma errors
zero_err <- tryCatch(
  {
    build_reduced_form_gamma(`colnames<-`(
      matrix(0, 2L, 5L),
      c("(Intercept)", paste0("pc", 1:4))
    ))
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "all-zero PC block errors instructively (mentions B = 0 / exact news)",
  is.character(zero_err) && grepl("B = 0", zero_err) &&
    grepl("reduced-form gamma", zero_err)
)

# No PC columns: error instead of building a degenerate gamma
nopc_err <- tryCatch(
  {
    build_reduced_form_gamma(`colnames<-`(
      matrix(1, 2L, 3L),
      c("(Intercept)", "l.y1", "l2.y1")
    ))
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "beta2r with no ^pc columns errors instructively",
  is.character(nopc_err) && grepl("principal-component", nopc_err)
)

# Misaligned supplied rownames signal row-order bugs, not display preferences
named_file <- tempfile(fileext = ".R")
writeLines(c(
  "build_gamma <- function(moments) {",
  "  j <- nrow(moments$r_i_0)",
  "  i <- attr(moments, \"n_components\")",
  "  g <- matrix(1, j, i)",
  "  rownames(g) <- paste0(\"wrong\", seq_len(j))",
  "  g",
  "}"
), named_file)
name_err <- tryCatch(
  {
    resolve_baseline_gamma(named_file, moments)
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "mismatched build_gamma rownames are rejected as misalignment",
  is.character(name_err) && grepl("positional", name_err)
)

# classify_common_design_cols hardening: an unrecognized column (neither the
# intercept, a PC, nor a Y1 lag) must abort rather than be silently absorbed
# into the intercept slot.
classify_err <- tryCatch(
  {
    classify_common_design_cols(c("(Intercept)", "pc1", "junk"))
    NULL
  },
  error = function(e) conditionMessage(e)
)
check(
  "classify_common_design_cols errors on an unknown column",
  is.character(classify_err)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
