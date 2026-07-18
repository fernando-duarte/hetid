# Rules exercised by check_contract_ownership.R.
forbid(
  "ignored docs route",
  "docs/(execution-ledgers|superpowers/plans)"
)
forbid(
  "direct paper source",
  "source\\(paper_path\\("
)
forbid(
  "noncanonical full-precision format",
  "%\\.17g",
  "support/runtime/core.R"
)
forbid(
  "noncanonical RDS version",
  "saveRDS\\([^\\n]*version = 3(L)?",
  "support/runtime/core.R"
)
forbid(
  "secondary year-quarter serialization",
  "%Y Q%q|\\^\\[0-9\\]\\{4\\} Q\\[1-4\\]\\$",
  "support/runtime/core.R"
)
forbid(
  "noncanonical MD5 implementation",
  "tools::md5sum",
  "support/runtime/core.R"
)
forbid(
  "literal nominal inference alpha",
  "alpha = 0\\.1(0)?",
  "config/analysis_contract.R"
)
forbid(
  "hard-coded normalized-inverse tolerance",
  "rcond\\(ms\\) < 1e-10"
)
forbid(
  "secondary normal log-square formula",
  "qchisq\\(0\\.5|digamma\\(0\\.5\\)",
  "support/statistics/normalizations.R"
)
forbid(
  "secondary condition-capture implementation",
  "withCallingHandlers\\(",
  "support/runtime/core.R"
)
forbid(
  "positional baseline",
  "(set_tables|tau_display|mean_eq_bounds_tau)\\[\\[1L?\\]\\]"
)
forbid(
  "literal baseline region",
  "region_sd_(system|box)\\(0\\.0?5\\)"
)
forbid(
  "literal figure grid",
  "length\\.out = 25(L)?"
)
forbid(
  "literal start policy",
  "starts_per_side = [35]L",
  "log_variance/estimators/controls.R"
)
forbid(
  "fixed lag-four schema field",
  "gate_[qp]_lag4"
)
forbid(
  "fixed joint-GMM dimensions",
  paste0(
    "n_moments_unprofiled = 10L|",
    "n_parameters_unprofiled = 9L|",
    "n_moments_profiled = 8L|",
    "n_parameters_profiled = 7L"
  )
)
forbid(
  "quarterly ACM acquisition outside its owner",
  "frequency = \"quarterly\"",
  c(
    "config/analysis_contract.R",
    "support/data/acm_inputs.R"
  )
)
forbid(
  "retired typed-I/O implementation",
  paste0(
    "\\.(jg|logvar_joint_null|logvar_lad_closure)_",
    "(manifest|encode|check_roundtrip|roundtrip) <- function"
  )
)
forbid(
  "retired bound constants",
  paste0(
    "BOX_EDGE_RTOL|BOUND_STABILITY_RTOL|",
    "UNBOUNDED_SCALING_FACTOR"
  )
)
forbid(
  "literal endpoint-polish blow factor",
  "blow_factor = 5",
  "support/identification/quadratic_evaluation.R"
)
forbid(
  "secondary quadratic helper",
  "constraint_(vals|jac) <- function"
)
forbid(
  "direct legacy quadratic-system builder",
  "hetid::build_quadratic_system\\(",
  "support/identification/quadratic_system.R"
)
forbid(
  "secondary panel splicer",
  "logvar_panel_block <- function",
  "log_variance/tables/panel_block.R"
)
forbid(
  "secondary exact-RDS writer",
  "paper_write_exact_rds <- function",
  "support/artifacts/typed_artifacts.R"
)
source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "contract_ownership_reporting_rules.R"
))
forbid(
  "registered artifact I/O bypass",
  "((utils|base)::)?(write[.]csv|saveRDS)[(]",
  setdiff(
    vapply(names(code), relative, character(1)),
    c(
      "mean_equation/inference/run_bootstrap.R",
      "log_variance/inference/run_set_bootstrap.R",
      "log_variance/diagnostics/dynamics/run_gate.R",
      "log_variance/extensions/egarch/run_route.R"
    )
  )
)
forbid(
  "secondary artifact-manifest constructor",
  "artifact_manifest <- data.frame",
  "config/artifact_manifest_data.R"
)
source(file.path(
  "scripts-paper",
  "tests",
  "support",
  "contract_ownership_audit_rules.R"
))
