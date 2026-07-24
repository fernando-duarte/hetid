# Exact legacy wrapper behavior beyond numerical draw equality.

call_shape <- paper_run_mbb_draws(
  2L, 8L, 3L,
  function(index, draw_id) sys.call(),
  1L
)
check(
  "the compatibility callback retains its legacy call shape",
  identical(
    call_shape$draws,
    rep(list(quote(draw(indices[[draw_id]], draw_id))), 2L)
  )
)

progress_calls <- list()
paper_run_mbb_draws(
  2L, 8L, 3L,
  function(index, draw_id) sum(index),
  1L,
  progress = function(draw_id, n_draws, started_at) {
    progress_calls[[length(progress_calls) + 1L]] <<- sys.call()
  }
)
check(
  "the compatibility progress callback retains its legacy call shape",
  identical(
    progress_calls,
    rep(list(quote(progress(draw_id, n_draws, started_at))), 2L)
  )
)

local({
  old_kind <- RNGkind()
  old_seed_exists <- exists(
    ".Random.seed",
    envir = .GlobalEnv, inherits = FALSE
  )
  old_seed <- if (old_seed_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      suppressWarnings(do.call(RNGkind, as.list(old_kind)))
      if (old_seed_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(
        ".Random.seed",
        envir = .GlobalEnv, inherits = FALSE
      )) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  suppressWarnings(RNGkind(
    "Mersenne-Twister", "Inversion", "Rounding"
  ))
  set.seed(881L)
  warnings <- character()
  withCallingHandlers(
    paper_run_mbb_draws(
      2L, 8L, 3L,
      function(index, draw_id) sum(index),
      1L
    ),
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  check(
    "the compatibility wrapper retains one legacy RNG warning window",
    identical(warnings, "non-uniform 'Rounding' sampler used")
  )
})

local({
  original_index <- mbb_index
  method_name <- "as.integer.mbb_cores_fixture"
  old_method_exists <- exists(
    method_name,
    envir = .GlobalEnv, inherits = FALSE
  )
  old_method <- if (old_method_exists) {
    get(method_name, envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  coercions <- 0L
  index_calls <- 0L
  assign(method_name, function(x, ...) {
    coercions <<- coercions + 1L
    if (coercions == 2L) {
      stop(structure(
        list(
          message = "typed cores coercion fixture",
          call = sys.call(),
          token = list(owner = "fixture", code = 17L)
        ),
        class = c("typed_mbb_cores_error", "error", "condition")
      ))
    }
    1L
  }, envir = .GlobalEnv)
  assign("mbb_index", function(...) {
    index_calls <<- index_calls + 1L
    original_index(...)
  }, envir = .GlobalEnv)
  on.exit(
    {
      assign("mbb_index", original_index, envir = .GlobalEnv)
      if (old_method_exists) {
        assign(method_name, old_method, envir = .GlobalEnv)
      } else {
        rm(list = method_name, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  error <- tryCatch(
    paper_run_mbb_draws(
      2L, 8L, 3L,
      function(index, draw_id) sum(index),
      1L,
      cores = structure(1, class = "mbb_cores_fixture")
    ),
    error = identity
  )
  check(
    "legacy cores coercion fails before schedule generation",
    inherits(error, "typed_mbb_cores_error") &&
      identical(error$token, list(owner = "fixture", code = 17L)) &&
      identical(index_calls, 0L)
  )
})

rm(call_shape, progress_calls)
