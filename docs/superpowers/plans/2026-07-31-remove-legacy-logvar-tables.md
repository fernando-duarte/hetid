# Step 3: remove the fifteen superseded log-variance table outputs

**Created** 2026-07-31
**Branch** `feature/unified-bootstrap-inference`
**Prerequisite** `c8e4f02` — the per-estimator document exists and is green.

Steps 1 and 2 are done. This is the removal, mapped but not executed: it touches
ten files, several through indirect defaults, and was deferred rather than begun
with too little room to finish.

## What goes

Fifteen artifacts, five renderers:

| renderer | producer code | artifacts |
|---|---|---|
| `render_ppml_table.R` | `c` | `log_var_eq.tex`, `_standalone.tex`, `_standalone.pdf` |
| `render_harvey_table.R` | `d` | `log_var_eq_harvey.tex` + 2 |
| `render_lad_table.R` | `e` | `log_var_eq_lad_panel.tex` + 2 |
| `render_panels.R` | `f` | `log_var_eq_panels.tex` + 2 |
| `render_inference_panels.R` | `g` | `log_var_eq_panels_inference.tex` + 2 |

`structural_var_inference*` **stays** — confirmed with the user.

## Touch points

- `config/artifact_manifest_data.R` — 15 artifact rows; producer codes `c d e f g`;
  consumer `D` (`render_inference_panels.R`) needs repointing or removing.
- `config/artifact_latex.R` — 5 publication specs, and label rows. **Careful:**
  `panel_ppml`, `panel_logols`, `panel_harvey`, `panel_lad` are now owned by
  `structural_var_estimators_table`; only `logvar_ppml` and `logvar_harvey`
  become unused.
- `config/artifact_manifest_variants.R` — both `logvar_panels` mappings.
- `config/logvar_estimators.R` — references the removed ids.
- `run_pipeline.R` — lines 118, 132, 133, 134, 138.
- `tests/support/publication_helper_checks.R` — publication count drops from 10.
- Delete `panels_builder.R` only if nothing else needs `build_logvar_panels`;
  `logvar_logols_table_parts` lives there and **is** needed, so most likely the
  file stays and only `build_logvar_panels` goes.

## The two that will bite

**Default arguments, not call sites.** `harvey_panel.R` and `lad_panel_builder.R`
default their `label` to `artifact_latex_label("log_variance_panels_table", ...)`.
Removing that artifact breaks the default even though no caller passes it. Grep
for the id, not for the function.

**Shared builders survive.** `logvar_harvey_build_fragment`,
`logvar_lad_build_fragment`, `logvar_estimator_panel_parts`,
`logvar_ppml_table_parts`, `logvar_logols_table_parts` and every notes builder
are all used by the new document. Only the five *renderers* go.

## Verify

- 36 suites green.
- `grep -rn "log_var_eq\.tex\|log_var_eq_panels\|log_var_eq_harvey\|log_var_eq_lad_panel"`
  returns nothing outside this plan.
- `git rm` the tracked `.tex`; the `.pdf` are gitignored and need plain `rm`.

## Still outstanding beyond this

- The per-estimator renderer has never executed against data. It is parsed and
  contract-checked only, and first runs on the next full pipeline run.
- That run is ~5 hours: the `beta1` null-loading fix moved `draw_spec_sha`.
- The manuscript may `\input` `structural_eq_inference.tex`, removed in `03a6bde`.
  Panel A of `structural_var_inference.tex` replaces it and the `\ref` label is
  unchanged.
