# Publication table for the structural consumption-growth equation estimated
# in estimate_identified_set.R: one row per coefficient of
# Delta c_{t+1} = b_0 + PC_{E,t}' b_E + PC_{N,t+1}' b_N + eps_{t+1},
# with the OLS benchmark (Newey-West t statistics), the closed-form Lewbel
# point at tau = 0, and the exact identified set at each tau_display slack.
# Row/column assembly lives in structural_table_parts.R so Panel A of the
# combined table cannot drift from this standalone table. Writes the
# structural-equation inference table and its standalone PDF to the typed table
# directory after mean-set estimation.
paper_source_once(paper_path("support", "latex", "table_pipeline.R"))
paper_source_once(paper_path("support", "latex", "simple_table.R"))
paper_source_once(paper_path("mean_equation", "tables", "structural_table_parts.R"))

parts <- structural_equation_table_parts(set_id_mean_eq, set_id_boot, n_pc)
artifact_id <- artifact_variant_id("structural_equation", "inference")
# Emit only the tabular, wrapped in a \begingroup that scopes the font size and
# column separation; the paper supplies the float, caption, and notes.
structural_table <- c(
  "\\begingroup",
  PAPER_TABLE_STYLE$coefficient$fontsize,
  simple_tabular_lines(
    parts$row_labels, parts$columns,
    col_headers = parts$headers,
    rule_after = parts$rule_after
  ),
  "\\endgroup"
)
publish_latex_artifact(artifact_id, structural_table)
# how many news-coefficient sets exclude zero, for the console summary
n_excl <- sum(with(
  set_id_mean_eq$theta_table,
  status == PAPER_ENDPOINT_STATUS[["bounded"]] & (set_lower > 0 | set_upper < 0)
))
cat(
  sprintf("structural equation table: %d of %d b_N sets exclude zero", n_excl, n_pc),
  sprintf(
    "at tau = %s\n",
    paper_format_tau(set_id_mean_eq$tau_baseline)
  )
)
rm(parts, artifact_id, structural_table, n_excl)
