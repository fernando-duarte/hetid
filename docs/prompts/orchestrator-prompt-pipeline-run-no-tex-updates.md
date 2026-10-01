# Pipeline validation without TeX document updates

_Last modified: 2026-09-30 20:55 EDT_

Run the production pipeline, audit package quality, implement justified fixes, and publish an
isolated working branch. TeX documents under `docs/` remain read-only. Generated publication
artifacts under `scripts-paper/output/`, including TeX tables, remain in scope.

## Operator quickstart

1. Open a fresh Claude Code session at the `hetid` repository root.
2. Select **Opus 5.5 | effort: `xhigh`**, or use the model and effort explicitly requested by the user.
3. Configure harness permissions for autonomous execution of the authorized commands.
   Model instructions cannot resolve a pending tool-permission dialog.
4. Start on the intended base branch with a clean checkout. Preserve ignored pipeline state:
   do not reset the pipeline, delete output, or clear caches.
5. Paste everything below `## ORCHESTRATOR PROMPT`.

The workflow runs in a new worktree outside Dropbox, copies existing pipeline state for validated
reuse, commits verified changes, pushes the new branch to `origin`, and reports mergeability.
The invoking checkout remains unchanged. Integration remains the operator's decision.

## ORCHESTRATOR PROMPT

You are the orchestrator for a production pipeline run and quality-remediation workflow on
`hetid`. Complete Stages 0-K and the final assessment under the boundaries below.

### Contract and authority

Read `docs/prompts/shared-workflow-contracts.md` completely before acting and record its digest.
It governs autonomy, source discovery, worker dispatch, resource arbitration,
durable evidence, review snapshots, bounded retries, and terminal status. Follow current repository
instructions and path-scoped rules. Do not execute a referenced workflow merely because it exists.

This prompt authorizes worktree creation, pipeline execution, in-scope source and
package-documentation changes, commits, and pushes of the new working branch. The orchestrator is
the sole canonical writer and Git owner. Workers inspect assigned slices and write only to
`RUN/scratch/agents/<agent-id>/`; apply the shared dispatch contract to every nested worker.
No delegated agent may edit repository targets or change Git state.

### Agent configuration

Default to **Opus 5.5 | effort: `xhigh`**. Explicit user instructions override the corresponding
setting; retain the default for any unspecified field. Apply the effective settings to the
orchestrator and ordinary workers, including nested workers, unless the user specifies a different
configuration for a particular role.

Independent review roles required by `multistep-plan` may use other available models to meet its
context-independence and model-diversity requirements. Verify underlying model identities,
reviewer counts, and each actual configuration; separate contexts using one model do not establish
model diversity. Explicit user constraints still prevail. If they conflict with a required review
gate, report the conflict rather than silently relaxing either requirement.

This section supplies the workflow-specific configuration permitted by the shared contract.
Resolve the chosen model's selector/API identifier and supported effort settings from the current
harness rather than guessing. Use its supported context window and honor any explicit user
context setting. Pass model and effort explicitly where supported; otherwise verify inheritance.
Record the effective configuration before dispatch. If a requested model or effort is unavailable,
report a blocker rather than silently substituting another setting.

### Scope and preservation

- **TeX references:** preserve every `.tex` document under `docs/`, tracked or ignored,
  including `docs/run_pipeline_code.tex`, `docs/run_pipeline_math.tex`, and
  `docs/heteroskedasticity_tests_general_instruments.tex`. Do not edit, reformat,
  recompile-in-place, move, rename, or delete them. This applies to skills, plans, hooks, and
  validation commands. Record documentation drift, including derived-count changes, without
  updating these documents. Stage 0 may copy required references into the new worktree.
- **Excluded workflows:** do not read, invoke, dispatch, or otherwise use
  `docs/prompts/synchronize-run-pipeline-code.md` or
  `docs/prompts/synchronize-run-pipeline-math.md`. Neither is a dependency.
- **Tracked references:** discover `git ls-files docs/`; every listed path stays read-only.
  Run records remain ignored and uncommitted. Never force-add working documents.
- **Allowed generated TeX:** pipeline publication artifacts under `scripts-paper/output/`
  may be regenerated and validated. Immutable artifact copies may be created under the exact
  new `RUN/scratch/` tree for numerical comparisons.
- **Scientific state:** preserve the donor and immutable baseline evidence. Never prepare an
  invocation by running `scripts-paper/reset_pipeline_state.R`, manually clearing output or
  active caches, requesting a force-rerun mode, or using a draft configuration. Derive production
  depth, reuse inputs, resources, and scheduling from current source; validators decide which
  callbacks execute. Within the isolated worktree, allow the runner's normal output overwrites,
  manifest-defined conditional cleanup and LaTeX sidecar cleanup.
  Record those operations against the preserved baseline; do not invoke cleanup helpers separately.
- **Scientific decisions:** discover optional-estimator decisions, dependency requirements,
  diagnostics, status routes, and conditional artifacts. Do not change an approved scientific
  decision to enable an estimator or evade a dependency.

Baseline each tree's protected TeX path inventory and content digests in Stage 0. Exclude only the
exact new `RUN/` evidence tree, so comparison snapshots do not count as reference changes.
Record absent pipeline TeX documents without creating replacements. The invoking checkout and
run worktree may have different inventories because only required untracked resources are copied.
Compare each with its own baseline at Stage K and final handoff; Git status alone is insufficient.

### Package and pipeline rules

Apply current repository conventions, with these workflow requirements:

- Use `/Users/fduarte/.agents/skills/karpathy-guidelines/SKILL.md` for code work and
  `/Users/fduarte/.agents/skills/multistep-plan/SKILL.md` at Stage H. Read both completely,
  including their required references. Retain these exact skill paths.
- All `.R` files stay below 200 lines and 100 columns, including paper code (199/99 budgets).
  The paper pipeline is outside package lintr, dependency, and R CMD check scopes, but retains its
  own topology and test gates:
  `Rscript scripts-paper/tests/run_tests.R`.
- In package code, raise structured `hetid_error` conditions through `R/conditions.R` helpers.
  Preserve the paper pipeline's self-contained source graph and paper-owned structured-condition
  helpers such as `paper_stop_condition()`. Never discard errors with a blanket
  `tryCatch(..., error = function(e) NULL)`.
- Use `HETID_CONSTANTS` for maturities, PC counts, tolerances, dates, and column prefixes.
  Preserve `nloptr`'s `hin <= 0` constraint convention.
- Generate `man/` and `NAMESPACE` with `devtools::document()`. Edit the root `README.Rmd`
  and rebuild with `devtools::build_readme()`; never hand-edit its generated `README.md` or
  add a Development section. Pipeline READMEs are plain Markdown.
- Follow `docs/guides/r-comment-style.md`: descriptive headings without sequence labels,
  capitals only for acronyms/constants/code literals, and surrounding comment style.
  Follow `docs/guides/r-roxygen-style.md` for roxygen blocks.
- Spell-check in `en-US`. Add legitimate terms to `inst/WORDLIST` by hand; never run
  `spelling::update_wordlist()` or reword valid terminology to evade a check.
- Fix failures at their source. Keep hooks enabled and unchanged merely for passing; no
  `--no-verify`. Commits contain no assistant attribution, attribution signatures, or co-author tags.

Two accepted conditions are outside remediation and defect reporting: the live-download coverage
gap in `download_term_premia` (do not add an offline-mockable test), and the intentional group-label
comments inside `HETID_CONSTANTS`.

### Records and scheduling

At Stage 0 select one collision-resistant `RUN_ID = YYYYMMDD-HHMMSS-<unique-suffix>`.
Confirm that its branch, worktree, and run directory do not exist; never reuse an earlier run.

| Identifier | Meaning |
|---|---|
| `DONOR` | Absolute invoking-checkout root; read-only throughout |
| `BASE` / `BASE_SHA` | Branch and commit read once from invoking `HEAD` |
| `BRANCH` | `chore/pipeline-validation-<RUN_ID>` |
| `WORKTREE` | New absolute worktree path outside Dropbox |
| `RUN` | `docs/pipeline-run-<RUN_ID>/`, relative to `WORKTREE` |

Create `RUN/` immediately after creating the worktree and save buffered preflight evidence there.
Record repository/source identities, the initial Git status, worktree/agent/process roster, resource
ledger, and finite retry caps. Use the harness's current task interface; do not assume tool names.

| Record | Path |
|---|---|
| Decisions, source/gate ledgers, stage status | `RUN/orchestrator-log.md` |
| Full pipeline stdout/stderr | `RUN/logs/pipeline-full.log` |
| Other command captures | `RUN/logs/<stage>-<description>.log` |
| Quality-suite report | `RUN/reports/quality-suite.md` |
| Advanced R audit | `RUN/reports/advanced-r-deviations.md` |
| Comment/roxygen audit | `RUN/reports/comment-style-deviations.md` |
| Consolidated findings | `RUN/reports/consolidated-quality.md` |
| Remediation plan / execution notes | `RUN/plans/stage-h-plan.md`, `RUN/plans/stage-i-execution.md` |
| Worker checkpoints | `RUN/scratch/agents/<agent-id>/response.md` |
| Mergeability / final handoff | `RUN/reports/mergeability.md`, `RUN/reports/final-summary.md` |
| Immutable reviews and comparisons | `RUN/snapshots/`, `RUN/scratch/` |

Every Markdown record has a title and an updated
`_Last modified: YYYY-MM-DD HH:MM ZZZ_` stamp obtained with
`date "+%Y-%m-%d %H:%M %Z"`. Apply the shared contract's source/status and checkpoint requirements.
Preserve all records through handoff. The quality suite's fixed `docs/quality-reports/` output is
the exception to the records root; retain it and link it from the Stage-D report.

The dependency order is:

| Work | May begin when |
|---|---|
| A -> B -> C | Previous stage is verified complete |
| D, E, F | Preflight complete; any overlap satisfies the resource ledger |
| G | C, D, E, F are terminal and their evidence verified |
| H -> I -> J -> K | Previous stage is verified complete |
| Final assessment | K has frozen the delivered candidate |

Stage C owns its output, cache, state, records, and relevant library paths until exit and
reconciliation. D-F may overlap C, or each other, only when their current read/write sets prove
independence. Otherwise serialize. Run all independent audit slices, regardless of slot limits.
Use one finite checklist per audit, non-overlapping worker file assignments, private checkpoints,
and the shared snapshot rules. Never edit source while a pipeline, audit, or review still reads it.

### Git policy

Read `BASE` and `BASE_SHA` from the invoking checkout. Reject detached HEAD and any uncommitted
or untracked non-ignored work. Do not switch its branch or absorb user changes.
Create `WORKTREE` and `BRANCH` from that recorded commit; verify the new branch and root.
All later repository commands run there. Do not modify `DONOR`, commit to `BASE`, or push it.

Stage only reviewed, task-related source/package changes and non-ignored publication artifacts.
Discover each path's Git status. Exclude protected references, ignored caches, diagnostics, PDFs,
and run records. Use explicit paths rather than `git add -A`.
Commit verified change sets at Stage J, splitting independent regeneration and remediation changes
when useful. Use a concise imperative subject and explain the reason in the body.

Keep ordinary commit and push hooks enabled. If a hook changes files or a repair changes source,
inspect the full delta and reopen affected Stage-I checks before recommitting. Reproduce the
failing check; run the full hook suite when required by the current gate.
Do not reset, rebase, amend pushed commits, force-push, cherry-pick, pull, or merge in this workflow.

Push the working branch after each verified commit; set its upstream on the first push.
If no reviewed changes exist, record a no-change Stage J and run
`pre-commit run --all-files`. Route any hook edits through I; publish and verify the branch
without manufacturing a commit.
Prove each delivery against the actual remote: the SHA returned by
`git ls-remote --exit-code origin refs/heads/<BRANCH>` must equal local `HEAD`.
Record the exact ref and both SHAs. Command text, local tracking refs, and an ahead count alone
do not prove delivery.

### Execution

#### Stage 0 — Preflight and isolated environment [WAIT]

1. Read current applicable instructions and the shared contract; verify the effective agent
   configuration, both required skill files, Git, R, pre-commit, and tools/dependencies required
   by current pipeline and quality commands. Verify Stage H's two independent adversarial
   reviewers with known-distinct underlying models, one independent code-review route, and the
   brainstorming review capabilities required by the installed skill. Honor its permitted reduction
   only for brainstorming. Optional interfaces are not blockers when a compliant substitute exists.
   Do not require synchronization prompts or documentation-only tools.
2. Capture the clean invoking checkout, `BASE`, `BASE_SHA`, tracked `docs/` set, scientific
   decisions, and initial resources. Select unique identifiers and verify the resolved worktree
   parent is outside `~/Library/CloudStorage/Dropbox-Personal/`.
3. Create the isolated branch/worktree with
   `git worktree add <WORKTREE> -b <BRANCH> <BASE_SHA>`; create `RUN/`, record its absolute
   location, and use the new worktree for every later stage.
4. Seed the complete existing `DONOR/scripts-paper/output/` tree, including ignored state.
   Discover all state families and their cross-record bindings from current readers/validators.
   Establish a stable donor snapshot without stopping unrelated writers. Record complete source
   inventories/digests before copying; verify destination equality and unchanged donor
   inventories/digests afterwards. Copy rather than move or share symlinks/hard links.
   Resolve any linked resources into independent copies only when their bindings remain valid;
   otherwise report a blocker. If the output tree is absent, record it and proceed.
   An unstable donor requires bounded waiting or a new consistent copy, never a reset.
5. Discover the remaining dependency closure from current instructions, commands, guides,
   references, and required skills. Copy required untracked repository resources from `DONOR`
   and verify digests, including applicable instruction files under `.claude/rules/`.
   From harness configuration directories, copy only applicable instruction files;
   leave unrelated settings and credentials untouched.
   At minimum resolve:

   - `docs/quality-check.R` for Stage D;
   - `docs/lewbel_multivariate_set_identification.tex` for Stage F notation;
   - `docs/heteroskedasticity_tests_general_instruments.tex` as a protected reference.
   Discover their actual tracking status; never overwrite a tracked worktree copy.
   Stop an affected stage if a required resource is missing.
6. Record both protected TeX baselines under **Scope and preservation**. Derive runtime and
   quality-tool dependencies from source, decisions, package metadata, and `docs/quality-check.R`.
   Install missing required dependencies, then install `hetid` from `WORKTREE`
   (`R CMD INSTALL .` or its documented equivalent). Record selected versions and library paths.
   The R library is shared: this replaces its installed package and must not overlap another
   library consumer or writer. Reinstall after later package changes before a pipeline invocation.

#### Stage A — Inventory preserved output

Inventory `scripts-paper/output/` against the current `artifact_manifest`: present/absent paths,
status classes, cache and gate/status records, and unexpected files. Record tracked, untracked,
and ignored distinctions. Before C, retain an immutable, digest-verified copy of the complete
pre-run output tree under `RUN/scratch/stage-a-output/`, including coherent state/cache families.
This preserves evidence for the runner-owned lifecycle operations allowed above; it does not
authorize manual resets, cache invalidation, or unrelated cleanup.

#### Stage B — Freeze the launch contract [WAIT]

Read current runner, manifest, configuration, decision parsers, dependency checks, and cache
validators. Record a source snapshot and one prelaunch ledger containing:

- production depth, supported reuse request, resource controls, draft/force overrides to exclude;
- draw families, sharing/sensitivity relations, reuse predicates, invalidation inputs, statuses,
  scheduling, and fallback consequences;
- each optional-estimator/diagnostic route, its approved state and exact dependency requirements,
  whether it executes, configures diagnostics, writes status, refuses requests, or reserves an
  unimplemented producer, and its conditional artifacts;
- execution read/write sets, including shared package libraries and any user-data/cache roots
  used for pinned external inputs, selected installed namespace identity, and permitted overlaps.

Verify production consistency, approved dependency versions, and that the selected default requests
reuse. Prevent inherited draft or force overrides. Verify frozen input snapshots, pin contracts,
and approved dependency versions without changing scientific input policy. Do not invoke the
pipeline, warm or rewrite caches, or declare them valid/stale; execution-time validators own that
decision.

#### Stage C — One production invocation and reconciliation

Make Stages A-C's only invocation of `Rscript scripts-paper/run_pipeline.R`, using only launch
inputs recognized by the frozen source and recorded in Stage B. Prefer the source-defined serial
reproducibility setting unless supported parallel execution is isolated.

Capture full stdout/stderr at `RUN/logs/pipeline-full.log`. Record command, environment, source
and installed-package identities, process identity, exit status, and output/state evidence.
For a background launch, monitor the exact process and log through termination; a quiet log or
log tail is insufficient. Preserve a failed attempt and its evidence.

Reconcile the terminal result with both the prelaunch ledger and Stage-A inventory. Verify required
manifest coverage, explain every conditional absence, and inspect all status routes and unexpected
files. Derive counts and ignore status from source. Validator-required rebuilds are allowed;
unchanged valid caches and unchanged numerical output are legitimate reuse results.

Review every changed/new non-ignored publication artifact using
`git status --short --untracked-files=all scripts-paper/output`. Carry reviewed artifacts to J.
Unexplained missing required artifacts, unexpected files, numerical changes, or execution failures
become explicit findings for G/H/I when they can be remediated in scope. Preserve the attempt;
do not treat incomplete output as a valid numerical reference. A hard execution blocker stops
dependent work while safe independent audits finish.

#### Stage D — Execute the quality suite

Discover the quality suite's complete write set, including temporary build artifacts.
Honor protected paths; redirect only through a documented equivalent interface, otherwise report
the affected stage blocked. Run `Rscript docs/quality-check.R` to completion and capture its log.
Inspect individual tool outcomes and reports; process exit alone cannot certify a suite that catches
tool errors. Also run the roxygen guide's full-example gate:
`Rscript -e 'devtools::run_examples(run_donttest = TRUE)'`, capturing its result separately.
The quality suite's R CMD check does not itself request this complete example coverage.

Write `RUN/reports/quality-suite.md` with all pkgcheck, rcmdcheck, dupree, lintr, covr, spelling,
and other findings, severity, file/line references, and links to native reports.
Explicitly include failing roxygen examples and documentation parameter/usage/return mismatches.
Example execution belongs to D; static documentation review belongs to F.

#### Stage E — Audit Advanced R guidance

Derive one finite checklist from `docs/guides/Advanced R Solutions.xml` and
`docs/guides/Advanced R.xml`. Partition every file under `R/` into non-overlapping worker slices.
Workers read both guides and their slice; record every deviation with file/line, violated rule,
proposed action, and confidence in `RUN/scratch/agents/stage-e-<slice>/response.md`.

Wait for all assignments to become terminal, verify their coverage/evidence, and consolidate into
`RUN/reports/advanced-r-deviations.md`. Preserve all checkpoints.

#### Stage F — Audit comments and roxygen

Keep this audit separate from E. Ordinary/inline `#` comments cover `.R` files in `R/` and
`tests/`; roxygen `#'` blocks cover `R/`. Exclude paper code, generated files, data, and
non-code documents. Workers are read-only and must not execute examples.

Derive checklists from the comment guide's Gate/MUST/MUST NOT rules and the roxygen guide's house
style, drift checks, and self-check. Partition by file so its comments and roxygen have one owner.
Roxygen review resolves templates/inheritance and checks:

- parameter names/order against formals and return claims against all function paths;
- alias resolution and complete, relevant reference/seealso links;
- `\eqn{}`/`\deqn{}` validity and notation against the protected mathematical spec;
- examples for every export, redundancy/dead examples, and constants that could drift.

Each worker records file/line, rule, action (`delete | rewrite | fix`), and confidence
(`high | medium | low`) at `RUN/scratch/agents/stage-f-<slice>/response.md`.
Wait for terminal assignments, verify complete coverage, and consolidate into
`RUN/reports/comment-style-deviations.md`. Preserve the worker evidence.

#### Stage G — Consolidate verified findings [WAIT for C, D, E, F]

Require terminal, reconciled C evidence and terminal, verified D/E/F results; an existing report
is not a terminal status. Include C's unresolved findings alongside the D/E/F reports, preserving
their distinct execution provenance. Deduplicate by normalized file/span, issue family, and fix.
Retain provenance: C owns pipeline failures, D quality-tool failures, E Advanced R guidance,
F comment/roxygen rules.
Reconcile D's dynamic documentation findings with F's static findings.
Write `RUN/reports/consolidated-quality.md`, preserving source reports and explicit audit coverage.
Release verified terminal audit workers without deleting their records.

#### Stage H — Plan remediation

Dispatch a read-only worker using `multistep-plan` on the consolidated report. Its private output
is `RUN/scratch/agents/stage-h-plan/response.md`; any nested review needs a disjoint scope and slot.
Complete the installed skill's two-model adversarial critique, independent code review, and
brainstorming rounds, using the review-role exception above. Record reviewer evidence in the
planner's private output. The orchestrator also saves full stateless-review responses under
`RUN/reports/`, with the reviewed snapshot IDs, as required by the shared contract. Verify identity
and coverage rather than treating a tool label as proof. Run no approval pause for the already
authorized implementation stage.
Plan all justified fixes, exact write sets, affected checks, numerical implications, and bounded
recovery. Prioritize high-certainty, low-risk fixes for Stage I; give a concrete reason for every
other deferral.
Protected TeX documents stay outside the write set; record any documentation drift.

Verify the worker's complete result, write `RUN/plans/stage-h-plan.md`, and release the terminal
worker while retaining its evidence.

#### Stage I — Implement and verify [WAIT]

Apply the verified plan as sole canonical writer and record actions in
`RUN/plans/stage-i-execution.md`. Use `karpathy-guidelines` and TDD where code changes require it.
Complete each edit batch before its verification pass; rerun a passed check only after a change,
failure, or unresolved concern invalidates it, unless a required gate explicitly runs it again.

Run `devtools::test()` for the completed package candidate. Paper-code changes also require
`Rscript scripts-paper/tests/run_tests.R` with topology checks.
Rerun the original failing quality checks and affected static audit rules on the final candidate.
Regenerate and inspect `man/`/`NAMESPACE` after roxygen changes; rerun
`devtools::run_examples(run_donttest = TRUE)` when runnable examples or their dependencies change.
Verify rebuilt README output when its source changes. Reopen every gate invalidated by later
edits or hook repairs.

For changes requiring numerical acceptance:

- Freeze verified, completed pre-change output under `RUN/scratch/` before editing executable
  code. Do not use incomplete Stage-C output as a reference.
- For intended table-result neutrality, verify the current comparator's documented interface and
  comparison rule in `scripts-paper/validation/README.md`. Its current command is
  `Rscript --vanilla scripts-paper/validation/compare_output_tables.R <reference-output-root> <candidate-output-root>`.
  It compares final table numeric tokens at displayed precision and attached stars; it does not
  certify labels, notes, nonnumeric statuses, figures, or intermediate state.
  Verify manifest/table coverage and the observed numeric comparison universe so empty projections
  cannot yield a vacuous acceptance. Do not reshape inputs to obtain a pass.
- Regenerate through the normal production/reuse route. Reinstall changed package code first and
  verify checkout/installed-namespace compatibility from current runner/freshness contracts.
- Limit a passing comparison claim to its actual coverage. Inspect other changed artifacts and
  status/input identities separately; obtain additional evidence before claiming broader numerical
  neutrality. For intended number/star changes, validate the specific intended differences directly.

Ordinary comment-only edits that change no executable code, data, runnable example, or roxygen
tag need no numerical-neutrality comparison, but still require package tests and hooks.
Classify every edited file against current draw/runtime/presentation hash manifests: raw-byte
hashes can change on comments. If final cache provenance or freshness is invalidated, run the
normal production/reuse validation and reconcile its gate decisions; do not claim fresh output
from stale provenance. Presentation-only hashes need not invalidate draw reuse when source says so.
Roxygen edits require documentation regeneration; changes to runnable examples, tags, or
exports/imports take the normal package gates.

Rerun the pipeline when changes can affect its behavior or numbers, or when resolving a failed C
attempt, including dependency/environment repairs without source edits. Before each invocation,
rederive affected Stage-B launch fields from the amended source and freeze a new per-attempt
source/installed-package ledger. Retain earlier ledgers; do not carry forward obsolete source
identities, gate predicates, manifests, or write sets. Preserve the production/reuse requirements
and donor/baseline state while allowing only source-owned lifecycle changes in the worktree.
Record any output that remains stale. Reconcile regenerated
publication artifacts as in C. Do not start J with unfinished implementation, failed required checks, or
unexplained scientific differences.

#### Stage J — Deliver reviewed changes

Apply **Git policy** to reviewed source changes and publication artifacts.
Return hook-induced edits and repairs through affected Stage-I checks before committing.
Push each verified commit and compare its SHA with the exact remote branch ref.
If there is no diff, apply the no-change hook and publication gate in **Git policy**. No documentation commit or synchronization stage follows.

#### Stage K — Freeze the delivered candidate [WAIT]

Require verified A-J, all task changes committed, successful required hooks, exact remote
delivery proof, and a clean tracked worktree. Record branch, HEAD, installed/source identities,
validation evidence, and output freshness. Verify both protected TeX baselines and unchanged donor
branch/commit/status. Freeze this candidate for final assessment.

Any necessary source change returns to its affected earlier gates and J, then requires a new K
snapshot. No tracked file or pipeline output changes during final assessment.

### Final assessment and handoff

1. **Refresh and prove delivery.** Run `git fetch origin` without changing branches; reconfirm
   local frozen HEAD equals `git ls-remote --exit-code origin refs/heads/<BRANCH>`.
   Select `origin/<BASE>` if present, otherwise local `<BASE>`; record this choice, both tips,
   and `git rev-list --left-right --count <base-tip>...<BRANCH>`.
2. **Assess without merging.** Run
   `git merge-tree --write-tree --name-only <base-tip> <BRANCH>`.
   This may write Git objects but changes no worktree, index, or ref. Record complete output and
   exit status using the installed command's format. Exit 0 is clean; exit 1 indicates conflicts;
   another exit status is an assessment failure. Inspect objects/diffs if needed; do not perform
   a trial merge or create a merge-assessment worktree.
3. **Classify exactly one verdict.** CLEAN means no conflicts. EASY requires every conflict to be
   mechanically resolvable with verified intent: identical edits, whitespace/line endings,
   deterministic non-scientific generated documentation, or unambiguously disjoint append-only
   additions. State each resolution. HARD covers anything else, including shared logic edits,
   published numeric conflicts, or uncertain scientific/authorial intent. Uncertainty means HARD.
4. **Save `RUN/reports/mergeability.md`.** Include command/exit, base and branch tips, divergence,
   all conflicted paths, individual classifications/resolutions, and verdict. Derive integration
   caveats from installed hooks, `.pre-commit-config.yaml`, and current ignored-state readers/validators:
   which merge/push checks run, required validation before integration, and which state families
   and decision bindings must move together. No merge, base push, or integration occurs here.
5. **Reconcile preservation and resources.** Recheck both protected TeX inventories/digests,
   donor state, task-attributable changes, and final validation snapshot. Build the run-owned
   agent/process/worktree roster against the initial roster. Wait for worker checkpoints and
   terminal status before release. Stop only verified obsolete run-owned watchers; remove only
   verified disposable resources created by this run. Preserve pre-existing resources and worktrees.
   Keep the deliverable worktree, local/remote working branch, pipeline state, and every record.
6. **Save `RUN/reports/final-summary.md` and report the result.** Give per-stage outcome and
   decisive evidence, `BASE`, source/final SHAs, absolute worktree path, branch, commits or
   no-change result, exact remote-ref proof, protected TeX checks, drift/deferrals, mergeability
   and proposed resolutions, and any open limitations. Confirm no merge occurred, excluded
   synchronization workflows were unused, workers stayed read-only, and all run-owned workers
   and disposable resources reached their required final state.

### Completion gate

Report **Complete** only when every required stage/check certifies the delivered candidate,
scientific outputs are reconciled, no required failure remains unresolved, preservation checks pass,
the branch exists on `origin` at the frozen SHA, and mergeability plus scoped teardown are recorded.
Intentional deferrals do not erase findings; explain them. Otherwise use the shared contract's
**Partial** or **Blocked** status, with preserved evidence and exact remaining scope.
