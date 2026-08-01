# R Comment Style — `hetid` (subagent spec)

Apply these rules whenever you write, edit, or review a `#` comment in this package's `R/` or
`tests/` code. For roxygen `#'` documentation blocks, use
[`r-roxygen-style.md`](r-roxygen-style.md) instead. They are authoritative; on any conflict with
the tidyverse style guide, **this spec wins**. Touch only `#` comments unless the task says
otherwise. Keep `man/` and `NAMESPACE` untouched (generated).

*Last reviewed: 2026-06-22 16:24 EDT.*

## Gate: should this comment exist?

Write or keep a comment **only if all** hold. Otherwise delete it (or rewrite the code instead).

- It explains *why* / a non-obvious decision / a domain invariant / a gotcha — not *what* the
  code plainly does.
- The fact is **not** already in the roxygen block or any other doc.
- It is not a to-do, reminder, or "fix later" note.
- It is not a value judgement about the code.

If a `#` comment would only restate the code or the roxygen, the right action is **delete**.

## MUST

- Start every comment with `# ` — hash, exactly one space, text. Inline comments: one space
  before the `#` (`x <- f(y) # why`), matching the styler/lintr default the pre-commit hook enforces.
- Keep each comment to **two lines maximum**; wrap with a fresh `# ` on the second line. Need
  more? Trim, or split it so each piece sits on the line/block it explains.
- Keep total line length **under 100 chars**, comment included.
- Use the project's settled terms when relevant: *centered* `1/T` moments; *maturity N* = w2
  column index (not necessarily a bond maturity); `theta` axis vs `maturities` constraint axis;
  `hetid_error` structured conditions; nloptr `hin <= 0`; **`pc1..pc6` are PCs of asset
  returns, never yields**.
- Document functions and datasets in **roxygen2 `#'` blocks**, then run
  `Rscript -e 'devtools::document()'`. Never hand-edit `man/` or `NAMESPACE`.
- Match the length, register, and placement of neighboring comments in the same file.

## MUST NOT

- No trailing period.
- No filler. Drop prepositions and filler words; a clipped, slightly ungrammatical phrase is
  fine if it reads the same.
- No jargon or academic register. Write as a person would say it.
- No value-judgement words (`clever`, `ugly`, `hacky`, `elegant`, `temporary`, `nice`).
- No to-dos, reminders, or deferred-work notes.
- No ALL CAPS for normal words or emphasis. Caps only for acronyms (FCI, RHS, SDF, MDS) and
  exact variable/constant names and literals (`MAX_MATURITY`, `FALSE`, `VFCI_t`).
- No hard-wired numbering in comments or test sections (`Test 1:`, `Step 2:`, `(i)`). Use
  descriptive headers: `# Test: single maturity`.
- No section-divider banners (`# Load data ----`, `# ==== Fit ====`). Files stay under 200
  lines; if a file feels like it needs dividers, split the file.
- No commented-out / dead code. Version control holds history.
- No comment that repeats or points at the roxygen block (`# see @param above`).

## Good vs bad

| Bad | Good |
|-----|------|
| `# loop over maturities.` | `# maturities axis = w2 columns, not bond maturities` |
| `# Clever trick to speed this up.` | `# vapply(..., numeric(1)) forces a double on the NA branch` |
| `# TODO: handle step = 0 later` | *(remove — track in issues)* |
| `# pc1..pc6 are PCs of the yield curve` | `# pc1..pc6 are PCs of asset returns` |
| `# @param tau the slack parameter` *(restates roxygen)* | *(remove)* |
| `# This loop maximizes over the bound index set rather than over all of the dates.` | `# maxes over bound index set, not all dates` |

## Final self-check before returning

- [ ] Passes the Gate (why / not in docs / not a to-do / not a judgement)?
- [ ] `# ` + one space; inline = one space; under 100 chars?
- [ ] Two lines max, no trailing period, shortest wording that stays clear?
- [ ] No jargon, value judgements, to-dos, ALL-CAPS emphasis, numbering, or banners?
- [ ] Nothing duplicated from or pointing at roxygen; `man/`/`NAMESPACE` untouched?
- [ ] Domain terms correct (PCs = asset returns, maturity = w2 column, …)?
- [ ] Matches neighboring comments' length and style?

## Roxygen `#'` blocks → see the dedicated spec

Styling and validating roxygen documentation (`@param`/`@return` ↔ code, examples that run,
references, `\eqn{}` math, the recurring drift checks) is governed by its own spec:
[`r-roxygen-style.md`](r-roxygen-style.md). When a task touches `#'` blocks, follow that file;
this one covers only `#` code comments.

This spec is the long form of the `CLAUDE.md` **Code Comment Rules**; keep the two consistent.
