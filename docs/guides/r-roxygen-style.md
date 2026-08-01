# R Roxygen Style & Validation — `hetid` (subagent spec)

Apply this whenever you write, edit, audit, or review a roxygen `#'` block in `R/` (function
docs, dataset docs, the package doc). For plain `#` code comments, use
[`r-comment-style.md`](r-comment-style.md) instead — its terse / no-period / two-line rules do
**not** apply here: roxygen is real documentation prose (full sentences, capital + full stop).
This spec is authoritative; on conflict with generic roxygen2/tidyverse advice, **this spec
wins** because it encodes this package's house style and its recurring real bugs.

*Last reviewed: 2026-06-22 16:24 EDT.*

## Cardinal rule: the code is ground truth

Documentation drifts because someone trusted an earlier doc. So:

- **Compare every claim to the code and the artifacts — never to memory, a prior doc, or a
  summary.** The function body, `NAMESPACE`, `HETID_CONSTANTS`, and the data file win every
  disagreement.
- **Verify before you flag, and again before you fix.** Audit passes over-flag. Real false
  positives seen: "function X is exported" when `grep 'export(X)' NAMESPACE` → 0; a HIGH on
  `@return` wording that was terse but not wrong.
- **Resolve `@template` and `@inheritParams` before calling a formal undocumented.** Run
  `ls man-roxygen/`; a param documented by a template is documented. Skipping this produces a
  wall of false positives.
- **Never hand-edit generated files.** `man/` + `NAMESPACE` come from `devtools::document()`;
  `README.md` from `README.Rmd` via `devtools::build_readme()`. Edit the `#'` source, regenerate,
  confirm the hook is green.

| Question | Authoritative source |
|---|---|
| Maturity floor/ceiling, step, PC counts, tolerances | `HETID_CONSTANTS` in `R/constants.R` |
| Bundled ACM column count / names | `inst/extdata/ACMTermPremium_replicated_monthly_1m_120m.csv.gz` (read the header) |
| What is exported | `NAMESPACE` (`grep '^export('`) |
| A function's true formals / return | read the body; `Rscript -e 'args(fn)'` after `load_all()` |
| Params documented via templates | `man-roxygen/` + any `@inheritParams` |

## House style (match what `R/` already does)

- **Title** — one line, title case, no trailing period
  (`Compute Supremum Estimator (c_hat) for Term Structure Analysis`). The first paragraph after
  it is the `@description`; longer "how/why" goes in `@details`.
- **`@param` / `@return`** — full sentences, capital + full stop. State **type and shape**, not
  internals. Reuse shared params via `@template <name>` (see `man-roxygen/`); do **not** also
  write the templated param inline.
- **Code & cross-links** — `\code{}` for code; `\code{\link{fn}}` to link another package
  function. (`R/` uses `\code{}` ~800×, `\link{}` ~100×.)
- **Math** — `\eqn{}` inline, `\deqn{}` display, long derivations under
  `@section Mathematical Formula:`. Escape inside math: literal braces `\{ \}`, underscores in
  names `c\_hat`. Notation must match `docs/lewbel_multivariate_set_identification.tex`
  (`\theta`, `\tau`, `\epsilon`, `A_i`/`b_i`/`c_i`, `L_i`/`V_i`/`Q_i`, centered `1/T` moments).
- **Visibility** — non-exported helpers use `@keywords internal` (still gets an `.Rd`, still
  linkable) or `@noRd` (no `.Rd`). Never `@export` an internal.
- **Examples** — runnable, current API, **minimal** maturities the call needs (for `i` with
  `step`: `step`, `i`, `i + step`), not the whole grid. Guard downloads/slow paths with
  `@examplesIf interactive()`.
- **Constants** — cite the `HETID_CONSTANTS` value by name/meaning, never a bare number that can
  drift (e.g. say "the default annual step" / `MAX_MATURITY - step`, not a stale `108`).

## Drift checks (each is a real bug a pass has surfaced)

**Stale numeric / range facts — the recurring one.** The ACM grid floor is **1**
(`MIN_MATURITY = 1L`, `ALL_ACM_MATURITIES = 1:120`, file `..._monthly_1m_120m.csv.gz`, 361
columns = 120 maturities × 3 families + date). A reverted "3-month floor" change has repeatedly
left docs claiming a 3-month start. Candidate-find, then **reconcile — do not auto-edit**:
```bash
grep -rnE '3[ -](to[ ]+)?120|118 maturit|355 column|ACMY003M' R/ README.Rmd README.md scripts/README.md
Rscript -e 'con<-gzfile("inst/extdata/ACMTermPremium_replicated_monthly_1m_120m.csv.gz");h<-readLines(con,1);close(con);cat(length(strsplit(h,",")[[1]]),"cols; first:",substr(h,1,40),"\n")'
```
False positives are legitimate: `ACMY003M` as a *naming-format* example (3-month is a real
node); `seq(3,120,3)` in `scripts/` (the pipeline's news clock, independent of the data floor).
Real bugs are **range claims** ("3 to 120 months", "118 maturities", "355 columns"), and they
hide in `R/data-acm.R` `@format` and `R/hetid-package.R`'s Data Sources section — check both.

**`@return` omits the degenerate path.** A function returning `NA_real_`/`NULL` in a degenerate
case but whose `@return` only describes the happy path (seen: `compute_c_hat`, `compute_k_hat`,
`compute_k2_hat`). Find, then add the clause + its trigger:
```bash
grep -rlE 'return\(NA_real_\)|return\(NULL\)' R/
```

**Dangling `\link{}`.** A `\link{X}` is dangling only when **no `man/*.Rd` carries
`\alias{X}`** — `@keywords internal` topics still get an `.Rd` and ARE linkable; only `@noRd`
(or never-documented) topics lack one. Testing export status in `NAMESPACE` is **wrong** (false
positives for every internal-but-documented helper). Use the alias test with fixed-string `-F`
(this repo's `grep` is `ugrep`, which rejects `alias\{X\}`):
```bash
grep -rhoE '\\link\{[a-zA-Z0-9_.]+\}' R/ | sed -E 's/\\link\{(.*)\}/\1/' | sort -u \
  | while read s; do grep -rqF "alias{$s}" man/ || echo "DANGLING link target: $s"; done
```
Remaining hits may be base/other-package topics — confirm before acting. Fix a genuinely
dangling in-package target to plain `\code{X()}`.

**Dimension / convention wording.** Per CLAUDE.md: `n_components` is the theta axis
(`= ncol(w2) = ncol(gamma)`); `length(maturities)` is the constraint axis; per-maturity outputs
are named `maturity_N`. Flag `@return`/`@param` text that conflates these — "`J x I`",
"column i", "list of length n_components" where the object is actually `J x length(maturities)`
or a maturity-keyed list. State both axes and the naming explicitly. And never call `pc1..pc6`
PCs "of yields" — they are PCs of **asset returns**.

**Package-doc index completeness (`hetid-package.R`).** Every exported user-facing function
appears once in the Function Categories section; nothing non-exported is linked there.
```bash
comm -23 <(grep -oE 'export\([a-zA-Z0-9_.]+\)' NAMESPACE | sed -E 's/export\((.*)\)/\1/' | sort -u) \
         <(grep -oE '\\link\{[a-zA-Z0-9_.]+\}' R/hetid-package.R | sed -E 's/\\link\{(.*)\}/\1/' | sort -u)
```
Left-only = exported but unindexed; judge which are user-facing. Confirm each is exported before
linking (don't reintroduce a dangling link).

**Return-convention consistency.** Validators return `invisible(TRUE)` on success. Flag a peer
returning bare `NULL` (with a `@return` that says so) as an inconsistency (seen:
`assert_scalar_finite`). Align body and doc.

**Examples — run, current API, coverage, relevance.** `run_examples(run_donttest = TRUE)` exits
0; example arg names match the live signature (no removed args); examples exercise non-trivial /
late-added formals (e.g. `y1_lags`, `exog`); none copy-pasted from a sibling function.
Download examples being `@examplesIf interactive()` is intended, not a defect.

**Comments duplicating roxygen.** A `#` comment that restates `@return`/`@param`/`@note` is a
defect — but that is [`r-comment-style.md`](r-comment-style.md)'s jurisdiction; here, just don't
move documentation prose *out* of the `#'` block into a `#` comment.

**Generated-file & cross-reference sync.** `README.md` must equal a fresh render of
`README.Rmd`. Any `file:line` reference inside a doc drifts when code moves — re-check each
(`sed -n '<line>p' <file>`; seen: a README cited `:274`, actual `:275`).

## Verification gates (all must pass before claiming done)

```bash
Rscript -e 'devtools::document()'                        # no warnings; man/ regenerated
Rscript -e 'devtools::run_examples(run_donttest = TRUE)' # exit 0
Rscript -e 'cat(length(lintr::lint_package()))'          # 0
Rscript -e 'devtools::test()'                            # FAIL 0
git diff --stat man/ NAMESPACE                           # only the intended topics changed
```
New legitimate technical terms go in `inst/WORDLIST`, never reword to dodge spell-check.

## Good vs bad

| Bad | Good |
|-----|------|
| `@return Numeric c_hat_i.` *(omits degenerate path)* | `@return Numeric c_hat_i, or \code{NA_real_} when no valid paired observations remain.` |
| `\link{compute_k_hat}` flagged dangling via `NAMESPACE` | alias-test it: internal topics have an `.Rd` and resolve |
| inline `@param i Integer maturity index...` re-typed per file | `@template param-maturity-index` |
| `the grid spans 3 to 120 months` | `the grid spans \code{MIN_MATURITY}–\code{MAX_MATURITY} months (1–120)` |
| `@return A J x I matrix.` | `@return A \code{J x length(maturities)} matrix; rows index w2 columns, not bond maturities.` |

## Self-check before returning

- [ ] Title case, no period; `@param`/`@return` full sentences ending in `.`?
- [ ] `@param` set ↔ current formals (names + order, templates resolved); `@return` ↔ actual
  return including any `NA_real_`/`NULL` path?
- [ ] `\link{}` targets alias-resolve; `@references`/`@seealso` present-and-relevant?
- [ ] `\eqn{}`/`\deqn{}` valid (escaped braces/underscores) and notation matches the spec?
- [ ] Examples run clean, current API, minimal maturities, downloads guarded; each exported
  function has one?
- [ ] No hard-coded constant that can drift; domain terms correct (PCs = asset returns, maturity
  = w2 column)?
- [ ] `devtools::document()` run; `man/`/`NAMESPACE` diff is exactly the intended topics?
