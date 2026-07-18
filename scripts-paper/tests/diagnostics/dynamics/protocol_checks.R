# Supplied-protocol checks for the residual-dynamics gate.

test_protocol <- LOGVAR_DYNAMICS_GATE_PROTOCOL
test_protocol$tested_lags <- c(1L, 2L, 6L)
test_protocol$gate_lag <- 2L
test_protocol$acf_max <- 6L
test_protocol$alpha <- 0.10

set.seed(20260718L)
protocol_decision <- logvar_gate_decide(
  stats::rnorm(240L),
  protocol = test_protocol
)

check(
  "dynamics gate uses the supplied lag axis",
  identical(
    names(protocol_decision$p_values),
    sprintf("lag%d", test_protocol$tested_lags)
  )
)

check(
  "dynamics gate uses the supplied ACF horizon",
  identical(
    names(protocol_decision$acf),
    sprintf("lag%d", seq_len(test_protocol$acf_max))
  )
)

check(
  "dynamics gate records its exact protocol",
  identical(protocol_decision$protocol, test_protocol)
)

run_gate_source <- paste(
  readLines(
    paper_path("log_variance", "diagnostics", "dynamics", "run_gate.R"),
    warn = FALSE
  ),
  collapse = "\n"
)
check(
  "dynamics console prose derives its ACF horizon from the protocol",
  grepl("dyn_g$protocol$acf_max", run_gate_source, fixed = TRUE) &&
    !grepl("lag 1..8", run_gate_source, fixed = TRUE)
)

# The base-R promise is executable: no heavy-dependency reference.
gate_files <- c(
  paper_path("log_variance", "diagnostics", "dynamics", "gate_core.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "gate_record.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "gate_sensitivity.R"),
  paper_path("log_variance", "diagnostics", "dynamics", "run_gate.R"),
  paper_path("log_variance", "extensions", "egarch", "cleanup.R")
)
has_gate_dependency <- any(vapply(gate_files, function(file) {
  any(grepl("rugarch", readLines(file), fixed = TRUE))
}, logical(1)))
check(
  "no heavy-dependency reference in the gate and cleanup files",
  !has_gate_dependency
)
