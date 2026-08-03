## Operator quickstart (do these before pasting)

Run these in order; the run is hands-off only once they're done.

1. **Open a fresh session in this repo.** Start Claude Code from the `hetid` repo root — the
   prompt calls `scripts-paper/...`, reads `docs/...`, and runs `git` here.
2. **Set the model.** `/model` → **`claude-opus-5[1m]` (Opus 5, 1M context) at `xhigh`
   effort**. (Stage 0 re-checks this, but set it yourself so the run doesn't start on the wrong
   model.)
3. **Make it autonomous at the harness level.** "Never ask the human" governs the *model*;
   Claude Code's **permission system** is separate and will still prompt for Bash/Edit/Write/git
   approvals. Launch in an auto-accept / bypass-permissions mode (or pre-allow the needed tools),
   or it will silently wait on permission dialogs.
4. **⚠️ Start from a clean checkout, or Stage 0 will halt the run.** This protects
   uncommitted source work; it does not authorize pipeline cleanup. Do not run
   `reset_pipeline_state.R` or delete anything under `scripts-paper/output`, including ignored
   caches. **Whatever branch you are on when you paste the prompt becomes the base branch** —
   the run reads it from `HEAD` and never assumes `main`. Put yourself on the branch you want
   as the base, then make the tree clean without disturbing pipeline state:
   ```
   git status          # commit or stash pending source work; preserve pipeline state
   git branch --show-current   # this is the base the run will branch from
   git pull --ff-only          # optional; only if this branch tracks a remote
   ```
   The run does **not** perform tracked work in this checkout. Stage 0 creates a separate isolated
   worktree outside the Dropbox tree. The only later write here is Stage K's locked refresh of
   authorized untracked graph state.
5. **Paste from `## ORCHESTRATOR PROMPT` down.** Everything above that heading (this quickstart +
   the "How to use" note) is for you, not the agent. Paste from the "You are the **orchestrator**…"
   line to the end of the file.
6. **Know what you're launching.** Stage C invokes the full pipeline once at the production
   bootstrap depth derived from current configuration. Its validated cache gate reuses an eligible bootstrap and
   reruns the multi-hour draw stage only when the cache is missing or stale — Stage 0 seeds the
   run worktree with this checkout's ignored pipeline state so that reuse stays possible. The
   workflow spawns workers and sub-orchestrators, commits to a new branch in a new isolated worktree, and pushes
   that branch to `origin`. **It never merges.** It ends by assessing whether the branch would
   merge cleanly back into the base and reporting the verdict — the merge itself stays yours.
   Stage K and the two final documentation sub-orchestrators are the named writer exceptions to the
   usual worker rule. Stage K owns only graph state; each Stage-O sub-orchestrator may edit only its
   assigned TeX file in the run worktree and working branch. Launch only when you want that end state.
   Ordinary workers keep the checkout read-only
   and checkpoint their findings to private Markdown files under `RUN/scratch/agents/` so partial
   work survives an agent crash.

---

> **How to use:** Paste the section below (everything under "ORCHESTRATOR PROMPT") into a
> fresh **Claude Opus 5 (1M context, `claude-opus-5[1m]`) session at `xhigh` effort**,
> running in this repository, then leave it to run. Opus acts as
> the orchestrator: it sequences the work, delegates to workers and sub-orchestrators, enforces the barriers, and
> verifies each stage before moving on. The run is **fully autonomous** — start to finish
> (Stage A through Stage O and the final mergeability assessment) with **no human involvement**:
> Opus must never pause to ask a question, request approval, or defer a decision back to the
> human. It stops short of merging: the branch is left pushed and assessed, never integrated.

---

## ORCHESTRATOR PROMPT

You are the **orchestrator** for an end-to-end pipeline regeneration, documentation
validation, and quality-remediation run on the `hetid` R package. You drive the whole
sequence to completion autonomously. You decompose work, delegate to workers where it
helps, enforce ordering barriers, and verify every stage with evidence before advancing.

### Required shared contract and authority

Read `docs/prompts/shared-workflow-contracts.md` completely before Stage 0. This prompt extends that
contract and does not restate its model, effort, autonomy, history-independence, role, worker,
concurrency, evidence, snapshot, retry, or completion rules.

This orchestrator owns Git state and canonical repository writes for Stages A-O. The workflow
authorizes the stated worktree creation, pipeline execution, source and documentation edits, commits,
and pushes. It does not authorize a merge, rebase, cherry-pick, or push of the base branch. Workers do
not inherit this authority. The Stage-K graph sub-orchestrator and two Stage-O documentation
sub-orchestrators receive only the exceptions stated in their complete dependent prompts.

### Role and operating principles

- Verify mutable behavior from current source and observed output within the stage's authority.
- Use the shared bounded-failure rule. Set exact retry caps in the run plan; do not embed an
  approximate universal retry count in this prompt.
- Honor every **WAIT** barrier. The resource ledger must prove each permitted overlap safe before
  launch.
- Keep all plans, reports, logs, and worker records under the run root defined below. Canonical source,
  documentation, and publication targets remain in their repository paths.
- Use independent reviews when they add evidence. The orchestrator verifies and synthesizes every
  result.

### Delegating to workers and sub-orchestrators

Apply the shared worker dispatch contract to every worker and nested worker. The exceptions are the
Stage-K graph sub-orchestrator and the two Stage-O documentation sub-orchestrators; each receives the
complete shared contract plus its complete dependent prompt.

#### Workflow-specific worker exceptions

- Stage K may write only the graph and graph-tooling paths authorized by its dependent prompt.
- Each Stage-O sub-orchestrator may write only its assigned TeX and PDF plus its unique workflow
  record.
- Every other worker writes only to `RUN/scratch/agents/<agent-id>/` and returns the durable path and
  status required by the shared contract.
- The orchestrator remains the sole Git owner and repository writer outside these exceptions.

### Hard constraints (project conventions — non-negotiable)

- Use the `karpathy-guidelines` skill when writing, reviewing, or refactoring code.
- No Claude attribution, signatures, or co-author tags in commits — ever.
- R package files (`R/`): under 200 lines each, lines under 100 chars, structured
  conditions (not bare `stop()`), never hand-edit `man/` or `NAMESPACE` (run
  `devtools::document()`). The paper pipeline (`scripts-paper/`) is not part of the package and
  is excluded from lintr / `deps-in-desc` / R CMD check — apply the package rules to `R/`,
  `tests/`, `man/`, `data/`, `inst/`, not to `scripts-paper/`. This exclusion is package-tooling
  only: `scripts-paper/` still enforces its own topology gate (200-line/100-column limits,
  checked by `scripts-paper/tests/support/check_topology.R` and run via
  `scripts-paper/tests/run_tests.R`) — do not treat the package-rule exclusion as license to
  skip that check on paper code.
- **Error handling:** raise structured conditions via the `R/conditions.R` helpers
  (`stop_bad_argument()`, `stop_dimension_mismatch()`, `stop_insufficient_data()`,
  `stop_hetid()`, the `assert_*_ok` helpers — all carry the `hetid_error` class). **Never use a
  blanket `tryCatch(..., error = function(e) NULL)`** — over-broad catch-and-discard masks bugs;
  re-raise as a `hetid_error` or capture `conditionMessage()` then `stop_hetid()`, or prefer a
  non-throwing API.
- `nloptr` inequality constraints use the `hin <= 0` convention (inside the set ⇒ negative).
- Spell-check is `en-US`; add legitimate terms to `inst/WORDLIST`, never reword to dodge it,
  and never run `spelling::update_wordlist()` (it prunes words the hook still needs).
- **Comment style — apply the comment rules in this prompt whenever the orchestrator writes
  or edits `#` comments** (they summarize `docs/guides/r-comment-style.md`, which
  Stage F audits): no hard-wired numbering in comments or section headers (no "Test 1:",
  "Step 2:", "a/b/c", "i/ii/iii" — use descriptive headers); no ALL CAPS for plain emphasis words
  (caps only for acronyms, variable/constant names, and code literals); and match the length,
  capitalization, `#`-spacing, and inline-vs-own-line placement of nearby comments so an edit
  doesn't read as newer or more formal than the code around it.
- Constants live in `HETID_CONSTANTS` (`R/constants.R`) — never hard-code maturities, PC counts,
  tolerances, date formats, or column prefixes.
- **README:** never add a "Development" section to `README.Rmd`/`README.md`; `README.md` is
  generated from `README.Rmd` (the `readme-rmd-rendered` hook enforces this) — edit the `.Rmd`
  and run `devtools::build_readme()`, never hand-edit `README.md`. (`scripts-paper/README.md`
  and `scripts-paper/support/README.md` are plain Markdown and not subject to this.)
- **Preserve pipeline state for every invocation.** Never prepare a `run_pipeline.R` invocation
  by running a reset entrypoint, clearing output, deleting caches, selecting a source-defined
  force-rerun mode, or using a draft configuration. This applies to Stage C and to any validation
  rerun required by Stages I or M. Derive the production depth, supported reuse request, cache
  scope, and downstream scheduling rules from the frozen source. Let each current validator decide
  whether its callbacks execute; do not encode a remembered cache topology in this prompt.
- **Protected file — never modify, ever:** `docs/heteroskedasticity_tests_general_instruments.tex`.
  Do not edit, rewrite, reformat, recompile-in-place, move, rename, or delete it, and do not
  let any worker, sub-orchestrator, or skill do so — at any stage, for any reason. You may read it for
  reference only. This overrides anything that would otherwise touch it.

### Workflow exclusions

Treat these as present scope rules. Do not remediate or report them as defects in this workflow:

- **`download_term_premia` sub-80 % coverage — do NOT add an offline-mockable test.**
  The only uncovered path is the live network download; the coverage gap is accepted as-is.
- **`R/constants.R` group-label comments are OK and must NOT be changed or deleted** — the
  labels for principal-component defaults, data constraints, news-period geometry, shared guard
  messages, maturity grids, numerical parameters, calendar values, date formats, column names,
  data identity, and column-format patterns inside `HETID_CONSTANTS` are intentional navigation
  aids, not banner dividers.
- **The graph is maintained only by Stage K's delegated pass, never ad hoc.** Outside that stage,
  treat the graph and its tooling as read-only: do not update, rebuild, re-extract, or diagnose them
  because a hook suggested a graphify command, because the graph looks out of date, or because a
  plan or report proposes it. The graph's coverage is not a defect to remediate opportunistically,
  and no stage other than K may write to it.

### Git workflow

You own the repository's version control for this run. Apply this consistently; do not
improvise around it.

- **The base branch is whatever is checked out at invocation — never assume `main`.** Read it
  once in Stage 0 with `git rev-parse --abbrev-ref HEAD`, record it in the log as `BASE`, and
  refer to `BASE` everywhere after. Do not `git switch` to another branch to "get onto the right
  base", do not hard-code `main` in any command, and do not commit to `BASE` itself. If `HEAD` is
  detached, stop and report — a detached `HEAD` is not a usable base.
- **Work in a new isolated worktree on a new branch.** Per **Stage 0**, confirm the invoking
  checkout is clean, then create one worktree and one branch for this run in a single command:
  `git worktree add <worktree-path> -b chore/pipeline-validation-<run-id> <BASE>`. Every
  stage from A onward runs with that worktree as the working directory. The invoking checkout's
  tracked state is left untouched for the whole run — do not edit, commit, stage, or run the pipeline
  in it. Stage K's locked, untracked graph port-back is the sole write exception. If
  the tree isn't clean, stop and report rather than branching on top of someone else's
  uncommitted work. Verify with `git -C <worktree-path> status` and
  `git -C <worktree-path> branch --show-current` before your first commit.
- **What gets committed.** Commit the tracked source changes produced in the implementation
  cycles (Stages I and M), any package files touched along the way, and the reviewed,
  non-ignored publication artifacts changed or created by Stage C or a later validation run.
  Derive tracked, untracked, and ignored status from the current checkout; do not assume that
  the output tree starts empty. Stage changed or new publication artifacts explicitly. Do not
  force-add ignored caches, diagnostics, or PDFs. `docs/` carries a directory-level ignore rule,
  so everything this run writes under `RUN/` — reports, plans, logs, the consolidated md files —
  is **not** committed and does not appear in `git status`; do not try to force-add it.
  **But `docs/` is not uniformly ignored.** Run `git ls-files docs/` in Stage 0 and record the
  complete current set. Treat every tracked `docs/` file other than the
  two Stage-O targets as read-only reference material for this run. The two Stage-O deliverables,
  `docs/run_pipeline_code.tex` and `docs/run_pipeline_math.tex`, are the only ones this run may
  change: stage and commit those two files explicitly after their sub-orchestrators finish and the
  orchestrator verifies them. Their PDFs and all synchronization working files remain ignored and
  uncommitted.
- **When to commit.** Commit at each natural checkpoint — once per completed implementation
  cycle, after its changes are verified and its pre-commit gate passes. Concretely: one
  commit at the end of Stage J (the Stage-G/H/I cycle), one at the end of Stage M (the Stage-L
  cycle), and one documentation commit in Stage O after both TeX files pass their independent
  synchronization and the orchestrator's verification. If a cycle produces logically distinct
  change sets, split them into focused commits. Do not commit broken or unverified states.
- **How to commit.** Stage the specific files you changed (prefer explicit `git add <paths>`
  over `git add -A` so stray artifacts don't sneak in). Write a clear, human-style message:
  a concise imperative summary line under ~72 chars, then a body explaining the *why*.
- **Pre-commit hooks are the gate.** Committing triggers the hooks. If a hook fails, fix the
  **root cause** and re-commit. Never use `--no-verify`, never disable or weaken a hook, and
  never edit linter/hook configs just to pass. A commit is only "done" when it lands with all
  hooks green. (Note hook quirks: roxygen version drift can rewrite `man/`; let
  `devtools::document()` reconcile it. Never run `spelling::update_wordlist()` — add terms to
  `inst/WORDLIST` by hand.)
- **When to push.** After each gate-passing commit, including the Stage-O documentation commit,
  push the working branch to `origin`
  (`git push -u origin <branch>` on first push). Pushing is in-scope for this run — you do
  not need to ask. No pull request is required.
- **Never merge.** Do not merge the working branch into `BASE` or any other branch, at any point,
  and do not merge `BASE` into the working branch. Do not rebase onto `BASE`. After Stage O the
  run performs a read-only **mergeability assessment** (see **Final mergeability assessment**)
  and stops. Integration is the human's decision, made later, from the report.
- **Recover, don't rewrite.** Do not `git reset --hard`, force-push, rebase, or amend already
  pushed commits to "clean things up." If something went wrong, make a new commit that fixes
  it and explain in the message.

### File output locations (everything under `docs/`)

All run artifacts live under `docs/` in a single dated run folder so the run is
self-contained and easy to clean up. Use this layout (create folders as needed). Give every
worker its exact private response path and identify canonical targets as read-only context —
workers must not write to `/tmp`, the repo root, or the source tree.

Create one collision-resistant `RUN_ID` at Stage 0 in the form
`YYYYMMDD-HHMMSS-<unique-suffix>`, using the system clock plus a task-unique suffix. Verify that no
branch, worktree, or run directory already uses it; generate a new suffix rather than reusing or
overwriting anything. Let `RUN = docs/pipeline-run-<RUN_ID>/` and reuse that exact identifier
throughout the workflow. Within it:

| What | Where |
|---|---|
| Running decision/evidence log | `RUN/orchestrator-log.md` |
| Stage-C pipeline run log | `RUN/logs/pipeline-full.log` |
| Other command/console captures | `RUN/logs/<stage>-<desc>.log` |
| Stage-D quality-suite report | `RUN/reports/quality-suite.md` |
| Stage-E Advanced-R deviations report | `RUN/reports/advanced-r-deviations.md` |
| Stage-F comment-style + roxygen-doc deviations report | `RUN/reports/comment-style-deviations.md` |
| Stage-G consolidated report (D+E+F) | `RUN/reports/consolidated-quality.md` |
| Stage-H plan + Stage-I execution notes | `RUN/plans/stage-h-plan.md`, `RUN/plans/stage-i-execution.md` |
| Worker response and private work area | `RUN/scratch/agents/<agent-id>/response.md` and sibling files |
| Stage-L duplication worker response | `RUN/scratch/agents/stage-l-dup/response.md` |
| Stage-L bug worker response | `RUN/scratch/agents/stage-l-bugs/response.md` |
| Stage-L consolidated report | `RUN/reports/consolidated-graphify.md` |
| Stage-M plan + execution notes | `RUN/plans/stage-m-plan.md`, `RUN/plans/stage-m-execution.md` |
| Stage-O documentation monitoring record | `RUN/reports/stage-o-documentation.md` |
| Any other intermediate/scratch file | `RUN/scratch/` |

**Timestamp every Markdown file.** Every `.md` file created or updated during this run
(reports, plans, audits, notes, logs, consolidated files, the orchestrator log — anything in
`RUN/`) must carry a **"Last modified" stamp with both date and time** at the very top, just
under the title — e.g.
`_Last modified: YYYY-MM-DD HH:MM ZZZ_`. Obtain the real timestamp from the system clock
(`date '+%Y-%m-%d %H:%M %Z'`) — never invent or hard-code it. Whenever you rewrite a file
(e.g. consolidating, or re-running a stage), refresh its stamp to the actual modification
time. Require every worker, sub-orchestrator, and skill that writes an `.md` to do the same.

For worker `response.md` files, create the timestamped header before substantive work and
refresh the timestamp at every checkpoint. Keep all `RUN/scratch/agents/` directories through the
final handoff. They are durable evidence; cleanup requires a separate, explicitly authorized
operation after the run.

Note: `quality-check.R` writes its own artifacts to `docs/quality-reports/` — leave those in
place (that path is fixed by the script) and summarize/link them from `RUN/reports/`. The
Stage-O TeX files keep their existing canonical paths (`docs/run_pipeline_code.tex` and
`docs/run_pipeline_math.tex`) — they are deliverables, not run artifacts. Preserve every
orchestrator-owned report and worker record through the final handoff.

### Reference paths

| Purpose | Path |
|---|---|
| Pipeline runner | `scripts-paper/run_pipeline.R` |
| Pipeline output dir | `scripts-paper/output` |
| Pipeline module docs | `scripts-paper/README.md`, `scripts-paper/support/README.md` |
| Code document (the pipeline explainer) | `docs/run_pipeline_code.tex` |
| Math document (the reproduction manual) | `docs/run_pipeline_math.tex` |
| Shared workflow contract | `docs/prompts/shared-workflow-contracts.md` |
| Code-document synchronization prompt | `docs/prompts/synchronize-run-pipeline-code.md` |
| Math-document synchronization prompt | `docs/prompts/synchronize-run-pipeline-math.md` |
| Graph maintenance prompt | `docs/prompts/maintain-graphify-graph.md` |
| Quality suite | `docs/quality-check.R` |
| Style guides | `docs/guides/Advanced R Solutions.xml`, `docs/guides/Advanced R.xml` |
| R comment style | `docs/guides/r-comment-style.md` |
| R roxygen style | `docs/guides/r-roxygen-style.md` |

### Execution plan

Maintain a task list mirroring the stages below and keep it current with the harness's available
task registry. Discover the current task-control interface rather than depending on particular tool
names. Keep a running log of decisions made and evidence captured at
`RUN/orchestrator-log.md` (see **File output locations**).

**Stage 0 — Preflight (before Stage A). [WAIT]**
Verify the run can proceed; if any check fails, fix it or **stop and report** (do not start the
pipeline on a broken footing):
- **Shared contract and fixed execution:** the shared prompt exists, is readable, and its required
  model, effort, and absolute skill files are available.
- **Skills and final-stage prompts available:** read the exact `karpathy-guidelines`,
  `multistep-plan`, `econ-write`, and clear-writing paths fixed by the shared contract, and verify
  that all dependent prompt files listed under **Reference paths** exist and are readable. Check the
  repository-local graph skill at `.claude/skills/graphify/SKILL.md`; its absence degrades Stages K-L
  to their stated source-only or partial paths but does not block independent stages.
  The optional `commit-push` skill may implement the existing commit/push contract if it is
  available; its absence is not a blocker because the Git commands are specified below.
  **Tools available:** require `git`, `Rscript`/`R`, `pre-commit`, the current package and pipeline
  validation commands, and the LaTeX/PDF tools required by the dependent Stage-O prompts. Treat
  `gh`, graphify interfaces, PAL reviewers, and context-documentation interfaces as optional unless
  a later current-source gate has no compliant substitute. Record each missing optional tool and
  use the fallback defined by the owning stage.
- **Clean, known starting point, and the base branch comes from `HEAD`:** run
  `git status --short --branch` and `git rev-parse --abbrev-ref HEAD`. **Do not assume the repo
  is on `main` or clean** — it may not be, and `main` has no special status in this run. Record
  the current branch as `BASE` in `RUN/orchestrator-log.md` and use it for every later reference;
  never substitute a hard-coded branch name. If `HEAD` is detached (`git rev-parse` returns
  `HEAD`), **stop and report**. If the working tree has uncommitted or untracked changes, **stop
  and report**; do not absorb pre-existing work into this run's branch. Do not switch branches in
  the invoking checkout — the run leaves its tracked state and branch exactly as found. Stage K's
  pass may refresh untracked generated graph state there; that is not a change to the checkout's
  tracked state and does not violate this rule.
- **Record the force-tracked `docs/` set.** Run `git ls-files docs/` and log the complete current
  result. Treat every entry other than
  `docs/run_pipeline_code.tex` and `docs/run_pipeline_math.tex` as read-only for this run. See
  **Git workflow**.
- **Discover optional-estimator gates.** Read each current decision parser, tracked decision record,
  dependency check, and conditional artifact declaration. Record the selected state and its exact
  consequences. Do not edit a tracked scientific decision merely to turn an estimator on. If an
  approved state requires an exact dependency version, satisfy that recorded contract or stop before
  Stage C. If source selects an off state, record its conditional artifact absences as expected.

Then follow the dependency order and only the explicit overlaps below:

**(1) Create the isolated run worktree and branch.** One command creates both, from `BASE`:

```
git worktree add ~/hetid-worktrees/pipeline-run-<RUN_ID> \
    -b chore/pipeline-validation-<RUN_ID> <BASE>
```

- **The worktree path must be outside the Dropbox tree.** The repository is under
  `~/Library/CloudStorage/Dropbox-Personal/`. Use `~/hetid-worktrees/<name>` or another path whose
  resolved parent is outside that synchronized tree. Verify the resolved path before creating the
  worktree.
- **Every stage from A onward runs with that worktree as the working directory.** `cd` into it
  once and stay there, or pass `-C <worktree-path>` on every `git` call. `RUN` is relative to the
  worktree, so the run folder is `<worktree-path>/docs/pipeline-run-<RUN_ID>/`. Record the
  absolute worktree path, the branch name, and `BASE` in the log.
- **The invoking checkout is read-only for the whole run, with one named exception.** Do not edit,
  stage, commit, run the pipeline, or install the package from it. Its roles after this step are as
  the donor of ignored pipeline state in step (2), and as the destination of the port-back that
  Stage K's graph maintenance pass performs. That exception covers **untracked generated state
  only** — never a tracked file, never Git state, never the working tree's branch. Nothing else in
  the run may write to the invoking checkout for any reason.

**(2) Seed the worktree with the invoking checkout's ignored pipeline state. [required]** A new
worktree contains tracked files only. Derive the current tracked and ignored output sets with Git and
the current cache/state readers. Copy the complete existing output tree so the run does not
manufacture a cache miss or discard a scientifically relevant state record:

```
cp -R <invoking-checkout>/scripts-paper/output/. \
      <worktree-path>/scripts-paper/output/
```

- **Copy every state family as a complete set, never file-by-file.** Derive each family and its
  cross-record bindings from current readers and validators. Never mix records from different source
  trees or pipeline-state snapshots.
- **Copy, do not move, symlink, or hard-link.** The invoking checkout keeps its own working state
  intact; the run must not be able to corrupt it. Verify the copy by comparing
  `md5 scripts-paper/output/state/*.rds` in both locations and record both digest sets in the log.
- This is a copy of existing state, not a repair or a reset. Do not run `reset_pipeline_state.R`,
  delete anything, or "clean up" either tree while doing it. If the invoking checkout has no
  `scripts-paper/output/` at all, record that fact and proceed — a gate-driven full rerun is then
  legitimate, and Stage A's inventory will show an empty baseline.

**(2b) Seed every other required untracked resource.** Derive the dependency closure of Stages D-F,
K, and O from their current commands, guides, protected references, skills, and complete dependent
prompts. For each required path absent from `git ls-files`, copy it from the invoking checkout before
Stage A and verify it by digest. At minimum, resolve the following explicitly named dependencies; a
new source dependency must be added by discovery rather than omitted because it is absent here:

```
cp <invoking-checkout>/docs/quality-check.R                                   <worktree-path>/docs/
cp <invoking-checkout>/docs/lewbel_multivariate_set_identification.tex        <worktree-path>/docs/
cp <invoking-checkout>/docs/heteroskedasticity_tests_general_instruments.tex  <worktree-path>/docs/
mkdir -p <worktree-path>/.claude/skills && \
  cp -R <invoking-checkout>/.claude/skills/graphify <worktree-path>/.claude/skills/graphify
```

| Resource | Needed by | What breaks if it is missing |
|---|---|---|
| `docs/quality-check.R` | Stage D | Stage D cannot run at all — the script does not exist |
| `.claude/skills/graphify` | Stage L | the project-local skill does not resolve for read-only queries |
| `docs/lewbel_multivariate_set_identification.tex` | Stage F | **silent**: the roxygen spec requires `\eqn{}`/`\deqn{}` notation to match this file, and auditors simply cannot perform that check |
| `docs/heteroskedasticity_tests_general_instruments.tex` | reference reads | the protected file this prompt says you may read for reference |

Copy, never move or symlink — the invoking checkout keeps its originals, and the run must not be
able to damage them. Confirm each landed.

**Do not seed the graph directory here — Stage K's pass owns that.** Its prompt acquires the
canonical lock, selects a coherent candidate by source provenance, seeds its worktree, and points the
root marker at that tree. Seed the skill needed for queries and leave graph state to Stage K.

**Baseline the protected file by checksum — `git` cannot police it and mtime is worthless here.**
`docs/heteroskedasticity_tests_general_instruments.tex` is **untracked**, so it never appears in
`git status` and `git log` can say nothing about it; and `cp` stamps the worktree copy with the copy
time, so that copy's mtime proves nothing either. Record a content digest for the file in both trees
at this step, and re-verify both digests before the final summary.

**(3) Create the run folder.** Create
`RUN = docs/pipeline-run-<RUN_ID>/` **inside the worktree**, with its `logs/`, `reports/`,
`plans/`, `scratch/`, and `scratch/agents/` subfolders.

**(4) Provision the R environment.** Derive runtime and quality-tool dependencies from the current
pipeline source, package metadata, decision gates, and `docs/quality-check.R`. Confirm the selected
versions are installed, and **install the package
itself** (`R CMD INSTALL .`, or `devtools::install()`) so `scripts-paper/` can load its exported
functions. If a step fails with a missing-package or "there is no package called …" error,
install the dep and retry — this is expected after a fresh/wiped R library, not a code bug.
Record what you installed in the log.
**Install from the worktree, and treat the R library as shared.** Run the install with the
worktree as the working directory so the installed `hetid` is the worktree's source, not the
invoking checkout's. The R library is shared between the two checkouts, so this install replaces
whatever the invoking checkout had installed — expected and acceptable, but note it in the log
and re-run it after every later `R/` change (per Stage I) so `scripts-paper/` never loads stale
code from the other tree.

---

**Stage A — Preserve and inventory pipeline state.**
Do **not** run `scripts-paper/reset_pipeline_state.R`, delete any manifest-owned artifact, clear
the output tree, remove caches, or otherwise emulate a from-scratch run. The existing bootstrap
cache is the input its reuse gate reads, and the rest of the existing output is the baseline you
reconcile Stage C against. Inventory `scripts-paper/output` against the current
`artifact_manifest`, recording each present or absent manifest path, its status class,
and the existing bootstrap cache and gate/status records in `RUN/orchestrator-log.md`. Record
unexpected files separately. This inventory is evidence, not permission to repair, remove, or
invalidate anything. The reset CLI remains a manual maintenance tool and is never invoked during
this run.

**Stage B — Validate the manifest and rerun gates. [WAIT]**
Do not invoke `scripts-paper/run_pipeline.R` in this stage. Read the current manifest, analysis
configuration, and bootstrap cache-validation code. Confirm that the production configuration is
internally consistent. Derive the production bootstrap depth, cache modes, defaults, and draft-run
controls from current source. Confirm the selected default requests reuse and ensure the Stage-C
environment does not inherit a draft or forced-rerun override. Verify the
gate inputs and dependencies that can be checked before execution, including every optional-estimator
decision discovered in Stage 0, but do not manually declare the bootstrap cache valid or stale. Do not warm,
downgrade, rewrite, or delete it. The cache validator called by Stage C is authoritative and must
decide whether to reuse the existing result or rerun the draw callbacks.

**Stage C — Single full pipeline run, gate-directed bootstrap.**
Make Stages A-C's only invocation of `scripts-paper/run_pipeline.R` with the production depth,
reuse request, and resource controls derived and recorded in Stage B. Use only launch inputs that
the frozen source recognizes. Prefer the source-defined serial reproducibility setting unless
parallel execution is both supported and isolated. Never request a forced rerun merely to prove
reproducibility.
**Save the full R output (stdout + stderr) to a log file** at `RUN/logs/pipeline-full.log`
(redirect both streams to that file). If the resource ledger permits background execution, record
the process identity and poll the process plus log until terminal status. A log tail alone does not
prove that the process exited successfully.
- **Freeze the current resampling and cache contract before launch.** Record every draw family,
  sharing or sensitivity relation, resource control, reuse predicate, invalidation input, status,
  and fallback consequence found in source. After the run, reconcile the saved log and resulting
  status records with that ledger. Accept a validator-required rebuild; never manufacture one by
  deleting state or selecting a force mode. Use current source names and statuses rather than any
  remembered topology or label.
- **Optional estimators and diagnostics are separate from bootstrap depth.** Build a current gate
  ledger before launch. For every decision record and route, classify whether it selects execution,
  configures an always-attempted diagnostic, writes routing or status only, refuses a request, or
  reserves an unimplemented producer. Record its dependency behavior and conditional artifact
  consequences. Do not infer a uniform toggle from similar names or a remembered source structure.
- **Prove any Stage-C overlap from current read and write sets.** Derive every pipeline output,
  cache, state, package-library, and records path before launch. Stage C owns those mutable paths
  until it exits and reconciliation finishes. Stages D-F may overlap only if their current commands
  neither read nor write any Stage-C-owned path, mutate the installed package library, nor write a
  source file they read. If proof is incomplete, run Stage C in the foreground and serialize the
  stages. Any later consumer waits for process exit and output reconciliation.
- **Do not assume the output tree starts empty or complete.** Use the Stage-A inventory as the
  pre-run record, and re-derive manifest counts and ignore status from the current code rather
  than hard-coding them. Existing valid state is intentional input. A successful Stage C may
  reuse the bootstrap cache while regenerating or validating downstream artifacts; an unchanged
  valid cache is evidence that the gate worked, not evidence that the pipeline failed to run.
- After Stage C completes, verify required-manifest coverage, reconcile conditional absences with
  every discovered optional-estimator and diagnostic status record, compare the result with the
  Stage-A inventory, and run
  `git status --short --untracked-files=all scripts-paper/output`. Review every non-ignored
  publication artifact that changed or was created, then stage it explicitly for the Stage-J
  commit or a separate focused regeneration commit. Do not force-add the ignored cache,
  diagnostics, or PDF artifacts. Treat an unexplained missing required artifact, unmanifested
  file, or unexpected numerical change as a regression and investigate it under Stage I's
  regression-gate guidance. Do not treat bootstrap-cache reuse or an unchanged bootstrap result
  as a regression.

**Stage D — Quality suite + report.**
Derive the quality suite's complete repository write set before execution. If any current output path
is a tracked `docs/` path protected by Stage 0, redirect it to `RUN` only when the suite exposes a
documented equivalent interface; otherwise stop Stage D with a blocker rather than overwrite the
tracked file. Run the discovered quality suite to completion. Then write `RUN/reports/quality-suite.md`
capturing **all** findings (pkgcheck, rcmdcheck, dupree, lintr, covr, spelling, etc.),
including severities and file/line references, and linking the script's own artifacts in
`docs/quality-reports/`. In particular, surface any **roxygen documentation** problems
`rcmdcheck` reports — `\examples` that error, and `checkDocumentation()` `@param`/usage/`@return`
mismatches — explicitly: these are the execution-side complement to Stage F's static roxygen
review, and Stage G reconciles the two. This report is consumed in Stage G.

**Stage E — Advanced R guideline deviations.**
Audit the codebase for **all** deviations from the standards and guidelines in
`docs/guides/Advanced R Solutions.xml` and `docs/guides/Advanced R.xml`. Document every
deviation with file/line citations and the specific guideline violated. Fan out across `R/`
with independently scoped workers — partition by a **non-overlapping slice** of files/directories per
worker (per **Delegating to workers and sub-orchestrators**: objective = find guideline deviations in your slice;
**output** = incrementally maintain `RUN/scratch/agents/stage-e-<slice>/response.md` and return
only that path; tools = read the
guides + the assigned `R/` files; boundary = read-only checkout, private scratchpad only, and
only the assigned slice; stopping criterion = every file in the slice checked against both
guides). To keep the standard consistent across workers, give every worker the same finite
checklist of guideline rules to apply (derive it once from the two guides up front). Partition every
`R/` file, but limit simultaneous workers to the current global slot allocation and serialize the
remaining slices. When all finish, the orchestrator reads and verifies every response file and
**consolidates** them into `RUN/reports/advanced-r-deviations.md`. Preserve the worker directories.
This report is consumed in Stage G.

**Stage F — Comment-style and roxygen-doc validation. [parallel with D/E]**
Enforce two authoritative specs across the package code — `docs/guides/r-comment-style.md` for
`#` comments (the **Comment style** hard constraint above is its short form) and
`docs/guides/r-roxygen-style.md` for roxygen `#'` blocks — in two scopes:
- **`#` comments (ordinary and inline)** in `.R` files under **both `R/` and `tests/`** — against
  the guide's **Gate / MUST / MUST NOT** rules.
- **roxygen `#'` blocks** in `R/` — against **`r-roxygen-style.md`** (its House style, Drift
  checks, and self-check): `@param` set ↔ current formals (names + order), `@return` ↔ what the
  function actually returns (including any `NA_real_`/`NULL` path), `\link{}` targets
  alias-resolve, `@references` / `@seealso` cross-links present-and-relevant (none missing,
  irrelevant, or dangling), `\eqn{}` / `\deqn{}` math valid and matching the spec notation,
  examples neither missing (each exported function has one) nor dead/redundant, and no
  hard-coded constant that can drift. Resolve `@template` / `@inheritParams` before flagging a
  formal undocumented.

This is a **sibling** audit to Stage E, not part of it: Stage E stays `R/`-only and driven by the
two Advanced-R XML guides. Audit only `.R` files under `R/` and `tests/`; never `scripts-paper/`,
`man/`, `NAMESPACE`, generated files, data, or non-code docs.

**Execution of examples is Stage D's job, not F's.** F is **read-only** and judges roxygen
*statically* — do `@param` / `@return` / refs / math / examples *look* correct against the code
the worker reads. Whether `@examples` actually **run**, and whether `checkDocumentation()` flags a
usage/`@param` mismatch, comes from Stage D's `rcmdcheck` / quality-suite run and reaches Stage G
via the Stage-D report; Stage G reconciles the two. Stage-F workers must not run examples or edit
source. Run Stage F concurrently with Stages D or E only when the current resource ledger proves
their read and write sets disjoint. It must not consume Stage C output.

Mirror Stage E's mechanics: derive one finite checklist up front from `r-comment-style.md`
(**Gate / MUST / MUST NOT**) and `r-roxygen-style.md` (**House style / Drift checks /
self-check**), then fan out parallel
workers over a **non-overlapping slice** each (slice by file, so a file's `#` comments and `#'`
blocks go to the same worker). Per **Delegating to workers and sub-orchestrators**: objective = list every
comment-style and roxygen-doc deviation in your slice; **output** = incrementally maintain
`RUN/scratch/agents/stage-f-<slice>/response.md` and return only that path; tools = read both
guides + your assigned files, including each function body to check its `#'` against the code;
boundary = read-only checkout, private scratchpad only, `#` comments and `#'` blocks only, and
your assigned slice only; stopping criterion = every `#` comment and every `#'` block in the
slice checked against the checklist.
Each finding records `file:line`, the rule violated, the proposed action (**delete | rewrite |
fix**), and a confidence (**high/med/low**) — so Stage H can defer low-confidence changes (a
borderline comment deletion, a debatable example removal) instead of auto-applying them. When all
finish, the orchestrator reads and verifies every response file and **consolidates** them into
`RUN/reports/comment-style-deviations.md`. Preserve the worker directories through the final
handoff. This report is consumed in Stage G.

**Stage G — Consolidate D + E + F. [WAIT for D, E, and F]**
Do not start until all three source reports exist (`RUN/reports/quality-suite.md`,
`RUN/reports/advanced-r-deviations.md`, `RUN/reports/comment-style-deviations.md`). Merge them
into a **single** consolidated file at `RUN/reports/consolidated-quality.md`, **deduplicated**
into unique action items keyed by normalized `file:line`/span, issue family, and proposed fix —
the same defect can surface in more than one report (e.g. hard-wired numbering is both an
Advanced-R/hard-constraint item and a comment-style item; over-long comment lines also trip
lintr). Tag each merged item with its provenance (`D`/`E`/`F`); on overlap keep the most
specific authoritative wording (Stage D for tool/check failures, Stage E for Advanced-R
findings, Stage F for comment-style and roxygen-doc findings) and do not leave duplicate fixes for
Stage H to re-plan. Preserve the three source reports and their worker records as evidence. After
the consolidated report is verified, release every terminal Stage-E and Stage-F worker while
preserving its scratch directory. The consolidated file is the input to Stage H.

**Stage H — Plan of action (worker, `multistep-plan`).**
Spawn a worker that invokes the `multistep-plan` skill to produce a plan of action
responding to the items in `RUN/reports/consolidated-quality.md`. It incrementally writes its
draft and evidence to `RUN/scratch/agents/stage-h-plan/response.md`, leaves the checkout
read-only, and returns only that path and status. The worker has full analytical autonomy, may spawn compliant read-only
nested workers, and asks no human questions. Do not defer an item merely for convenience: implement
high-certainty, low-execution-risk fixes and record a concrete reason for every lower-certainty or
higher-risk deferral. The orchestrator reads and verifies the response, then
writes the canonical plan to `RUN/plans/stage-h-plan.md` itself and releases the terminal planning
worker while preserving its record.

**Stage I — Implement the plan from Stage H. [WAIT]**
The orchestrator executes the Stage-H plan (`RUN/plans/stage-h-plan.md`) to completion and
records execution notes in `RUN/plans/stage-i-execution.md`. Subagents may supply read-only
analysis through their checkpoint files, but the orchestrator applies every repository change.
Use TDD/`karpathy-guidelines` where code changes are involved, and run the package test suite
(`devtools::test()`) to confirm nothing breaks.
If a change touches `scripts-paper/`, also run its own topology checks and isolated suites
(`Rscript scripts-paper/tests/run_tests.R`) — the package suite does not cover paper code.
For any change intended to be **numerically neutral** (a refactor that must not move published
results), capture the completed pre-change output tree under `RUN/scratch/`, regenerate the
candidate output, and discover the current direct acceptance command and comparison universe from
the frozen repository source and guidance. Record exactly which artifact classes, numeric tokens,
labels, stars, paths, and statuses the comparator does and does not test. Supply the reference and
candidate roots in the form its current interface requires; do not reshape them merely to obtain a
pass. If no independent current comparator covers the claimed neutrality, record that validation
gap as a blocker for the claim. If a planned change is meant to alter published numbers or stars,
validate the intended differences directly instead of asking a numerical-neutrality gate to pass.
**Comment-only edits are numerically inert.** A change that only deletes or rewrites `#`
comments (ordinary or inline) and touches no executable code, data, runnable example, or
roxygen tag is numerically inert — skip the before/after snapshot capture and any pipeline re-run
for it, but still run `devtools::test()` and let the Stage-J pre-commit hooks (lintr included)
be the gate. If a fix also edits a roxygen `#'` block, run `devtools::document()` and inspect
the `man/`/`NAMESPACE` diff; if it changes runnable examples, roxygen tags, or exports/imports,
treat it as a normal package change with the full gates.
**Reinstall before any pipeline invocation that follows a package change.** Verify from the current
runner how it selects the installed `hetid` namespace and from the current freshness code how it
combines checkout and installed code identity. Never run a pipeline with a known mismatch between
the edited checkout and selected installed namespace. Run `R CMD INSTALL .` or the current documented
equivalent first. Then **re-run the validations that the change could have invalidated**: the test suite
always; and, if the change can affect pipeline behavior or numbers, the pipeline. Preserve the
existing output and invoke `scripts-paper/run_pipeline.R` with the source-derived production
configuration and validated reuse mode; never reset or force a rerun. Let the bootstrap cache
gate decide for itself whether the draw callbacks must execute. Stage O performs the documentation
synchronization only after all source work is final, so do not run either synchronization prompt
here. Be honest in the log about any Stage-C output that remains stale. **Do not start Stage J
until implementation is complete and verified.**

**Stage J — Commit the Stage-I work (hooks must pass).**
Stage the source files changed in Stage I and the reviewed, non-ignored
publication artifacts reconciled after Stage C. Commit them on the working branch with a clear,
human-style message. If output regeneration is logically separate from the Stage-I code changes,
split it into its own focused commit. The commit triggers the pre-commit
hooks; if any fail, run `pre-commit run --all-files` to reproduce, fix the **root cause**, and
re-commit — iterate until the commit lands with **all** hooks green. Then push the working branch
to `origin`. See **Git workflow**.

**Stage K — Graphify graph maintenance (one sub-orchestrator, delegated to its own prompt).**
Graph refresh is an ancillary best-effort stage because it writes only generated, machine-local
state. Its required parent-workflow deliverable is a verified terminal record and safe canonical
state, not a successful mutation. A safe `Partial` degrades Stage L as specified below but does not
block independent source validation.

Dispatch one sub-orchestrator with the complete current contents of
`docs/prompts/shared-workflow-contracts.md`, followed by the complete current contents of
`docs/prompts/maintain-graphify-graph.md`. Do not summarize, paraphrase, reorder, or replace either
file. The dependent prompt
owns the entire method: seeding, updating, repair, the health gate, and porting the result back. It
is maintained independently of this prompt, so anything said here about *how* to maintain the graph
would drift out of date; this stage owns only the dispatch and the verification.

**Make no assumption about the graph's state.** Do not tell the sub-orchestrator the graph is current, stale,
behind, or freshly built, and do not decide from repository history whether a pass is warranted.
Run the stage unconditionally and let the pass discover what needs doing — a pass that finds
nothing to change is a valid and cheap outcome, not a wasted one.

**Supply the context required by the shared contract:** the absolute run-worktree root, branch,
`RUN` as the enclosing records root, the recorded source snapshot for Stage K, owned graph
paths, invoking-checkout destination, and current global slot allocation. The sub-orchestrator creates
its own unique workflow record below `RUN`; do not assign it an ordinary worker scratch path.

**This sub-orchestrator is a writer exception to the ordinary worker protocol.**
Unlike every other worker in this run it is expected to write outside a private scratchpad, because
maintaining the graph is inherently a write operation. State its boundaries explicitly when you
dispatch it:

- It **may** write the graph directory and its tooling directory — the locations its own prompt
  names — in the run worktree, and it may perform the port-back that prompt specifies. Those paths
  are git-ignored, so none of it touches tracked state.
- It **may not** create, edit, move, or delete any tracked file, run the scientific pipeline, or
  change Git state in any way: no add, commit, push, checkout, switch, branch, stash, reset,
  restore, rebase, or merge. It works on whatever branch the run worktree already has checked out
  and must not switch away from it.
- **Its own nested workers write to their private scratch directories and nowhere else** — never the
  graph, never a tracked file, never Git state. Do not dispatch them as strictly read-only: its
  prompt has them return fragments *through disk*, and a worker that cannot write returns nothing
  while reporting success. Scratch-only is the boundary, not no-writes-at-all.
- It owes the unique durable workflow record required by the shared contract and dependent prompt.

**Its prompt directs it to copy results back to the checkout that owns the canonical graph, which is
normally the invoking checkout.** That is a deliberate and sanctioned exception to this run's
isolation rule, and the only one: it concerns untracked, generated, machine-local state that lives
outside version control precisely so it can be regenerated. Do not extend the exception to anything
else, and do not let it become licence to touch tracked files or Git state in either tree.

**That port-back is the run's only write outside its worktree.** The dependent prompt's canonical
lock is mandatory. If the sub-orchestrator cannot acquire or safely reclaim the lock, it must not
update or port the graph. Run one logical Stage-K workflow and never overlap it with another writing
stage. Any bounded recovery continues from its durable record rather than launching a competing
maintenance pass.

**Verify after it returns** — read its report and check the claims rather than accepting them:

- for `Complete`, the health and port-back gates passed at the reported counts; for `Partial`, the
  exact open gate is recorded and no unverified graph was ported;
- `git status` in the run worktree shows **no tracked change** attributable to this stage;
- the pipeline was not run, and no scientific output, cache, or manifest instance was touched;
- the branch checked out in the run worktree is unchanged.

Record the outcome, the before-and-after graph size it reports, and what it states the graph now
describes. **If the pass fails or cannot finish, record that and continue.** Stage L then works from
whatever graph is present, or from source alone if there is none. A failed maintenance pass degrades
the next stage; it does not fail the run.

After verifying and incorporating the Stage-K record, release the terminal sub-orchestrator from the
task roster. Preserve its workflow record. A terminal sub-orchestrator must not occupy capacity
needed by later stages.

**Stage L — Graphify audits (two scopes). [WAIT for both]**
Assign the following two disjoint scopes to two workers. Run them concurrently only when the global
resource ledger shows two free worker slots; otherwise run them sequentially. Both follow the shared
task envelope. Before dispatch, copy the required graph inputs into each worker's private scratch
directory and verify each copy against one recorded digest set. Recheck the live inputs after both
copies; if they changed during capture, discard only those private copies and retry from one stable
snapshot within the plan's finite cap. If no stable snapshot can be captured, run both audits from
source alone and record the graph limitation. Workers query only their immutable private copies.
They never read a graph another session may be replacing, and any write-capable graphify operation
remains confined to private scratch.

**Brief both on the graph's evidence boundary**, whatever Stage K reported. Use
it to *locate* candidates; never let it settle a question about what the code currently does, and
require every graph-sourced lead to be confirmed against current source before it is reported. Pass
along whatever Stage K established about the graph's coverage, without inferring more than it says.
Name both structural limitations: a node may outlive the code it described, and a file outside graph
coverage has no node. Graph absence therefore never proves source absence.

**Absence of a finding is not evidence of absence**, and both workers must state the exact audited
universe and method. A textual-duplication sweep certifies only the patterns and source scope it
actually inspected; it does not certify every possible semantic duplication.

- Worker 1 → `RUN/scratch/agents/stage-l-dup/response.md`: objective = find potential
  **duplications**, single-source-of-truth violations, DRY violations, and **magic variables**;
  tools = read-only `graphify` query/explain/path operations over the live graph or a private
  scratch copy; boundary = structural/quality smells only, not runtime bugs (that's Agent 2).
- Worker 2 → `RUN/scratch/agents/stage-l-bugs/response.md`: objective = find potential **bugs
  and errors**; tools = read-only `graphify` operations plus inspection of implicated source;
  boundary = correctness defects only, not style/duplication (that's Agent 1).

Wait for both to finish or recover their partial checkpoint files. The orchestrator verifies
and consolidates their findings into `RUN/reports/consolidated-graphify.md`. Retain both worker
directories and their intermediate artifacts. After incorporation, release the terminal workers from
the task roster while preserving their records.

**Stage M — Plan / implement / hooks on the Stage-L report.**
Apply the Stage-H → Stage-I → Stage-J cycle to `RUN/reports/consolidated-graphify.md` instead
of the Stage-G report:

1. Spawn a `multistep-plan` worker using the same high-certainty, low-execution-risk action rule and
   explicit-deferral requirement as Stage H. It checkpoints incrementally to
   `RUN/scratch/agents/stage-m-plan/response.md`, keeps the checkout read-only, and returns only
   that path and status. The orchestrator verifies the response, writes the canonical plan to
   `RUN/plans/stage-m-plan.md`, and releases the terminal planning worker while preserving its record.
2. The orchestrator implements that plan to completion, recording notes in
   `RUN/plans/stage-m-execution.md`. Subagents may analyze through private checkpoint files but
   never apply repository changes.
   **[WAIT]**
3. Commit the Stage-M.2 work on the working branch (clear human-style message); the commit
   runs the pre-commit hooks — fix root causes and re-commit until all hooks pass, then push
   to `origin`. See **Git workflow**.

**Stage N — Freeze the completed working branch for final documentation. [WAIT]**
Do not begin Stage N until Stages A–M are verified complete, the Stage-J and Stage-M commit gates
landed with all hooks green, every task-related source and publication change is committed, and
the working branch is fully pushed. Confirm that the tracked worktree is clean, record the branch
name and HEAD SHA, and freeze that source snapshot as the authority for both Stage-O
sub-orchestrators. From this point through the Stage-O documentation commit, do not change source,
configuration, tests, pipeline outputs, or any tracked file except the two Stage-O TeX targets.
If a source change becomes necessary, return to the affected earlier stage, repeat its validation
and commit/push gate, then re-enter Stage N with a new recorded snapshot.

**Stage O — Synchronize the two pipeline TeX documents. [final substantive stage]**
Stage O runs in the **same run worktree and working branch** used by Stages A–N. Do not create
a documentation branch or a second worktree, do not switch branches, do not touch the invoking
checkout, and do not let either sub-orchestrator change Git state. The two canonical and
task-related repository targets are:

- `docs/run_pipeline_code.tex`
- `docs/run_pipeline_math.tex`

No README or other source/documentation file belongs to this stage. Prompt-authorized ignored
plans, reports, LaTeX sidecars, renders, and PDFs may support validation, but do not stage or
commit them. Preserve unrelated files and changes.

Launch exactly two sub-orchestrators, one for each target. Run them concurrently when the global
resource ledger proves that two slots are available; otherwise run them sequentially. Scheduling may
change, but the roles, scopes, and review coverage may not be combined or reduced. Give each one the
complete current contents of `docs/prompts/shared-workflow-contracts.md`, followed by the complete
current contents of its dependent prompt. Do not summarize, combine, reorder, or replace either file:

- **Code-document sub-orchestrator:** use
  `docs/prompts/synchronize-run-pipeline-code.md` for
  `docs/run_pipeline_code.tex`.
- **Math-document sub-orchestrator:** use
  `docs/prompts/synchronize-run-pipeline-math.md` for
  `docs/run_pipeline_math.tex`.

**Supply each with the context required by the shared contract:**

- the absolute run-worktree root, and that every path resolves relative to it;
- the branch, and that it must not be switched;
- `RUN` as the enclosing records root; each sub-orchestrator must create its own unique workflow
  record under the path defined by its dependent prompt;
- the Stage-N snapshot SHA as the source it must describe;
- its exact owned TeX and PDF targets, the sibling's disjoint targets, and whether the sibling is
  not yet launched, active, or terminal; and
- the caller's current global slot allocation.

Arbitrate every remaining worker slot centrally. A sub-orchestrator may dispatch a nested worker only
from its current allocation. When both siblings run, allocate a lone remaining slot to one at a time
and require the other to continue direct work or wait at its next worker barrier. During sequential
scheduling, release the first terminal sub-orchestrator before launching the second, while preserving
its workflow record. Capacity may serialize nested reviews but may not combine roles, reduce
coverage, or relax independence.

**Each is a sub-orchestrator, not a worker.** It executes its governing prompt itself, start to
finish, and is responsible for the whole method that prompt defines. Concretely:

- **It edits its own assigned TeX file directly.** Do not ask it to return patches, proposals, or
  line-specific prescriptions for you to apply. Its prompt already makes it the sole editor of that
  file within its own workflow, and it is the only sub-orchestrator in this run permitted to write
  that file while it is running.
- **It may spawn and manage its own workers** as its prompt directs, and it owns their dispatch,
  their scope, and their verification. Those nested workers stay read-only under its governing
  prompt: they return findings to it, and it applies them.
- **It owns its file for the duration.** While a sub-orchestrator is running, neither the main
  orchestrator nor any other worker or sub-orchestrator edits that file. A concurrent edit
  invalidates the version its reviewers are certifying.

**The main orchestrator retains ultimate and final editing authority over both files**, exercised
only *after* a sub-orchestrator is terminal. You may correct, adjust, or reverse anything either one
produced, and you own every cross-document decision. What you must not do is duplicate their work by
re-deriving edits they already made and verified.

Each may edit only its assigned canonical TeX file and may create only the ignored working and
validation artifacts allowed by its prompt. Each must preserve the other TeX file, every source
file, and all Git state — no add, commit, push, checkout, switch, branch, stash, reset, restore,
rebase, or merge, in either tree. Both must run on the Stage-N source snapshot and must complete
every barrier in their prompt, including the distinct `econ-write` and
`writing-clearly-and-concisely` passes. The main orchestrator must verify those passes rather than
infer compliance from a successful TeX build.

**Concurrent execution must not collide.** Their targets are disjoint and each is forbidden the
other's file, which is what makes overlap safe; state that boundary explicitly in both dispatches. A
sub-orchestrator skips any optional read of a sibling target while that sibling is active. Under
sequential scheduling it may read the stable sibling only as the nonauthoritative lead allowed by
its dependent prompt.

**Report the Stage-O schedule immediately after the first task starts.** Identify both targets and
state whether their sub-orchestrators are concurrent or sequential. This notice is mandatory even
though the workflow is otherwise hands-off; do not wait until the sub-orchestrators finish.

**Monitor both sub-orchestrators until they reach a terminal status.** Use the environment's task
status tools and inspect their prompt-required durable reports/checkpoints while they run. Do not
fire and forget, treat task creation as completion, or rely only on a final chat message. Record
launch time, task identity, current status, last durable checkpoint, failures/retries, and terminal
status in `RUN/reports/stage-o-documentation.md`. If either task crashes, stalls, or returns a
partial result, inspect and preserve its durable work, recover or relaunch only the uncovered
scope under the same target boundary, and continue monitoring. Stage O cannot pass while either
sub-orchestrator is running, missing, partial, or unverified.

After both finish, the main orchestrator must:

1. Read both complete audit trails and final reports, verify their source snapshot against the
   Stage-N SHA, and inspect every change to the two TeX files.
2. Confirm that no tracked file other than the two TeX targets changed during Stage O. Investigate
   and remove only Stage-O-created out-of-scope changes; never discard pre-existing user work.
3. Verify that each prompt's source-fidelity, terminology, `econ-write`,
   `writing-clearly-and-concisely`, LaTeX, and final-integrity gates passed. Re-run decisive
   validations when needed; do not rerun or source the scientific pipeline. Every certification must
   cover the exact bytes delivered by that sub-orchestrator.
4. **The edits are already in the files** — the sub-orchestrators applied them. Do not re-apply or
   re-derive them. Your job here is to accept, correct, or reverse what is there, using the final
   editing authority you hold once they are terminal. Resolve every cross-document inconsistency
   yourself, against current source: you are the only party that may read both files, so anything
   spanning them is yours alone and neither sub-orchestrator could have settled it. Preserve the
   different purposes and content contracts of the two documents rather than forcing a match — a
   shared symbol carrying different meanings in a source trace and a mathematical manual may be
   correct in both, and is only a defect if one contradicts source. Any main-orchestrator edit after
   sub-orchestrator completion invalidates affected certifications. Reopen the earliest affected gate
   under that dependent prompt and obtain clean certifications on the new final snapshot.
5. Run `git diff --check` and review the complete prospective diff for both TeX paths.
6. After all final certifications and cross-document checks pass, release both terminal
   sub-orchestrators while preserving their complete workflow records. If a later edit reopens a
   dependent gate, reallocate a slot and obtain the required fresh certification before proceeding.
7. Commit the two TeX files on the current working branch with all ordinary hooks enabled, then
   push that same branch to `origin` under **Git workflow**. Stage the two paths explicitly and no
   others. The optional `commit-push` skill may perform this exact scoped operation if available;
   otherwise use the specified `git add`, `git commit`, and `git push` flow. Fix hook failures at
   their root cause and retry within the bounded-retry policy. Never commit to `BASE`.
8. Report both sub-orchestrators' completion, the verification evidence, the documentation commit
   SHA/message, the working branch, and the branch push result.

Stage O is complete only after both sub-orchestrators are terminal and verified, both canonical
TeX files are incorporated, and the documentation commit is present on and pushed from the same
working branch used by the rest of the run.

### Final mergeability assessment (after Stage O) — assess, report, do not merge

**Do not merge anything.** The run ends with the working branch committed, pushed, and
*assessed*. Integration into `BASE` is the human's decision, taken later from your report. Do not
run `git merge`, `git rebase`, `git cherry-pick`, or `git pull` on either branch, and do not push
`BASE`. Only after every stage is verified complete and all three commit gates landed green:

1. **Confirm the branch is committed and actually pushed.** The run worktree must be clean
   (`git status --short`). Prove the push by comparing refs, never by reading command output:
   `git rev-list --count @{upstream}..HEAD` must be `0`, or `git rev-parse HEAD` must equal
   `git rev-parse origin/<branch>`. A pre-push hook writes to the same stream as the push, so a
   trailing hook line can look exactly like success on a push that was aborted — the ref
   comparison is the only proof.
2. **Refresh the base tip without changing any branch.** `git fetch origin` only. Resolve the
   comparison tip in this order and record which you used: `origin/<BASE>` if it exists,
   otherwise local `<BASE>`. Report how far the branch and the base have diverged
   (`git rev-list --left-right --count <base-tip>...<branch>`).
3. **Simulate the merge without performing it.** Use the read-only merge simulator, which writes
   nothing to any worktree, index, or ref:
   ```
   git merge-tree --write-tree --name-only <base-tip> <branch>
   ```
   Exit status `0` means the merge is clean and the only output is the merged tree's OID. Exit
   status `1` means conflicts: the first output line is still the tree OID, the lines after it are
   the conflicted paths, and an informational block below them names each conflict type
   (`CONFLICT (content): Merge conflict in <path>`). Read the exit status, not the presence of
   output — a clean merge also prints. If a detailed inspection is needed, do it in a **throwaway** worktree
   (`git worktree add --detach <scratch-path> <base-tip>`, merge there, inspect, then
   `git worktree remove --force <scratch-path>`) — never in the run worktree, never in the
   invoking checkout, and never leave the trial merge committed or pushed. `<scratch-path>` must
   also be outside the Dropbox tree.
4. **Classify the result into exactly one verdict** and justify it per conflicted path:
   - **CLEAN** — `merge-tree` exits `0`. No conflicts.
   - **EASY** — conflicts exist, but every one is mechanically resolvable with no judgement call
     about intent or about which result is correct. Qualifying cases: both sides made byte-identical
     changes; the conflict is whitespace or line-endings only; the conflicted file is generated and
     is reproduced deterministically by re-running its generator (`man/`, `NAMESPACE` via
     `devtools::document()`; `README.md` via `devtools::build_readme()`); or both sides appended
     disjoint entries to an append-only list (`inst/WORDLIST`, manifest rows) where keeping both
     sides is unambiguously right. State the resolving action for each path.
   - **HARD** — anything else. Specifically: both sides edited the same function body or logic;
     the conflict is in a published numeric artifact under `scripts-paper/output/` (resolving it
     means deciding which run's numbers are authoritative — a scientific judgement, not a merge
     mechanic); both sides edited the same prose in `docs/run_pipeline_code.tex` or
     `docs/run_pipeline_math.tex`; or the correct resolution depends on intent you cannot verify
     from the diff. When in doubt between EASY and HARD, report **HARD** — over-calling a conflict
     costs the human a look, under-calling it invites a bad merge.
5. **Write the assessment to `RUN/reports/mergeability.md`** (timestamped like every other run
   Markdown file) and reproduce its verdict in the final summary. Record: `BASE`, the branch name,
   both tip SHAs, the divergence counts, the exact `merge-tree` command and its exit status, the
   full list of conflicted paths with a per-path classification and proposed resolution, and the
   overall verdict. Attach the two dynamically derived caveats below.
6. **Record merge-time hazards from current configuration.**
   - Inspect installed Git hooks and `.pre-commit-config.yaml`. State exactly which hooks a clean
     merge and later push would run, which file scopes they cover, and which full validation command
     the integrator should run before pushing. Do not embed a remembered hook count.
   - Derive ignored pipeline-state families and their bindings to tracked decisions from current
     `.gitignore`, readers, and validators. State which authoritative state remains only in the run
     worktree and which files must travel as one coherent family after integration. Do not embed a
     remembered extension list or state-file count.
7. **Leave the branch and the run worktree in place.** They are the deliverable. Do not delete the
   working branch locally or on `origin`, and do not remove the run worktree — it holds the
   gitignored `RUN/` evidence and the authoritative `output/state/` set, neither of which exists on
   the branch. Report its absolute path so the human can pick up from it.
8. **Finish resource teardown.** Terminal workers should already have been released after their
   durable results were verified and incorporated. Build a final roster from the current harness
   task registry, then reconcile it with live processes and `git worktree list`. Never stop an active
   agent before its last checkpoint. Release remaining terminal agents, stop obsolete watchers, and
   remove verified read-only worker worktrees and throwaway merge-assessment worktrees or branches.
   Preserve every durable workflow and worker record through the final summary.
   **Do not remove the run worktree and do not delete the working branch, local or remote** —
   they are this run's deliverable and are explicitly out of scope for teardown. `git worktree
   list` is clean when it contains the invoking checkout and the run worktree and nothing else.
   Do not delete `RUN/scratch/agents/` or dependent workflow records; they are the durable evidence
   for this run and remain available for audit or recovery.

---

### Completion criteria

You are done only when all of these conditions hold:

- Stages A–J and L–O are verified complete in the dependency order above, including every permitted
  D–F overlap and the Stage-N source freeze. Stage K reached a verified terminal state: either
  `Complete`, or a safe `Partial` in which
  no unverified graph was ported and Stage L completed from a stable graph snapshot or source alone.
- `BASE` was read from `HEAD` at invocation and recorded; no command hard-coded a branch name.
- A new isolated worktree outside the Dropbox tree and a new working branch were created up front
  from `BASE`, the worktree was seeded with the invoking checkout's ignored pipeline state, and
  every stage used it as its repository root. The invoking checkout stayed on its original branch
  and commit; only a successful Stage K locked port-back could change its authorized untracked graph
  state.
- Every task commit — Stage J, Stage M.3, and the Stage-O documentation commit — landed and was
  pushed on the working branch. No task work or task commit landed on `BASE`.
- No reset, output cleanup, draft bootstrap, or forced bootstrap rerun prepared any pipeline
  invocation. The Stage-C log records each current cache gate's actual decision and any
  gate-justified rebuild, its outputs were reconciled against the Stage-A inventory, and every later
  pipeline validation preserved that contract.
- Stage G was built from the Stage-D, Stage-E, and Stage-F reports, and Stage L was built from
  its two worker responses. Their source reports were handled as specified, and durable worker
  checkpoints remained available through successful run completion.
- Both implementation cycles, Stages I and M, are complete. The Stage-J and Stage-M.3 commit
  gates landed with all hooks green, and all reviewed non-ignored publication artifacts were
  committed.
- The main orchestrator reported the Stage-O schedule when its first sub-orchestrator started,
  monitored each sub-orchestrator to a terminal status, inspected its durable evidence, and verified every
  source-fidelity, terminology, `econ-write`, `writing-clearly-and-concisely`, LaTeX, and
  final-integrity gate required by its complete prompt.
- Stage O changed and incorporated no task-related tracked file other than
  `docs/run_pipeline_code.tex` and `docs/run_pipeline_math.tex`. Both TeX files
  reflect the frozen Stage-N source snapshot, and the documentation commit was created and
  pushed from the same working branch and worktree used for the rest of the run.
- **Nothing was merged.** No `git merge`, `git rebase`, `git cherry-pick`, or `git pull` ran on
  the working branch or on `BASE`, and `BASE` was never pushed. The push proof for the working
  branch is a ref comparison, not command output.
- The mergeability assessment ran against the fetched base tip, returned exactly one verdict
  (CLEAN / EASY / HARD) with a per-path classification, and is recorded in
  `RUN/reports/mergeability.md`. The working branch and the run worktree are still in place.

Provide a final summary listing, per stage, what was done and the evidence confirming it. Include
`BASE`, the run worktree's absolute path, the working branch, commit SHAs/messages, branch-push
results (with the ref-comparison proof), both Stage-O sub-orchestrator
statuses, the documentation commit, and the mergeability verdict with its conflicted paths and
proposed resolutions. State plainly that no merge was performed. Confirm that every ordinary worker
kept the checkout read-only and left a durable response, Stage K respected its locked graph-only
exception, and the Stage-O sub-orchestrators touched only their assigned targets and workflow
records. Write this summary durably, finish the resource teardown in assessment step 8, then report
the verified teardown state: no agent left running, no agent worktree left behind, and no throwaway
branch left on `origin`. Preserve every worker record, the run worktree, and the working branch.
