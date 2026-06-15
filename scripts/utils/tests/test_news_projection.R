# News-projection switch contract: the tri-state resolver that decides whether
# the pipeline imposes the exact-news projection B = 0 or estimates B from the
# data. Mirrors the baseline-gamma hook tests. Run from the package root:
#   Rscript scripts/utils/tests/test_news_projection.R
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

env_var <- "HETID_IMPOSE_NEWS_PROJECTION_ZERO"

# Restore environment + constant to a known state after every case so the
# suite is hermetic regardless of the caller's shell.
restore_env <- function() {
  Sys.unsetenv(env_var)
}
restore_env()

# Env unset + script constant FALSE (the shipped default) ⇒ FALSE
check(
  "env unset + constant FALSE returns FALSE",
  identical(impose_news_projection_zero(), FALSE)
)

# Env unset + script constant TRUE ⇒ TRUE. The resolver reads the GLOBAL
# constant (the production lookup path), so flip it in globalenv and restore
# the prior value afterward -- even on failure -- to stay hermetic.
local({
  had_const <- exists("IMPOSE_NEWS_PROJECTION_ZERO", envir = globalenv())
  prior <- if (had_const) get("IMPOSE_NEWS_PROJECTION_ZERO", envir = globalenv())
  on.exit(
    if (had_const) {
      assign("IMPOSE_NEWS_PROJECTION_ZERO", prior, envir = globalenv())
    } else {
      rm("IMPOSE_NEWS_PROJECTION_ZERO", envir = globalenv())
    }
  )
  assign("IMPOSE_NEWS_PROJECTION_ZERO", TRUE, envir = globalenv())
  check(
    "env unset + constant TRUE returns TRUE",
    identical(impose_news_projection_zero(), TRUE)
  )
})

# Env set to truthy tokens ⇒ TRUE (overrides the FALSE script constant)
for (tok in c("TRUE", "true", "1")) {
  Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = tok)
  check(
    sprintf("env '%s' returns TRUE", tok),
    identical(impose_news_projection_zero(), TRUE)
  )
  restore_env()
}

# Env set to falsy tokens ⇒ FALSE
for (tok in c("FALSE", "false", "0")) {
  Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = tok)
  check(
    sprintf("env '%s' returns FALSE", tok),
    identical(impose_news_projection_zero(), FALSE)
  )
  restore_env()
}

# Invalid value ⇒ fail closed with an instructive error naming the variable
Sys.setenv(HETID_IMPOSE_NEWS_PROJECTION_ZERO = "maybe")
bad_err <- tryCatch(
  {
    impose_news_projection_zero()
    NULL
  },
  error = function(e) conditionMessage(e)
)
restore_env()
check(
  "invalid value fails closed naming the variable",
  is.character(bad_err) && grepl("HETID_IMPOSE_NEWS_PROJECTION_ZERO", bad_err)
)

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
