## Operator quickstart (do these before pasting)

Run these in order; the run is hands-off only once they're done.

1. **Open a fresh session in this repo.** Start Claude Code from the `hetid` repo root — the
   prompt calls `scripts-paper/...`, reads `docs/...`, and runs `git` here.
2. **Set the model.** `/model` → **`claude-opus-5[1m]` (Opus 5, 1M context) at maximum
   effort**. (Stage 0 re-checks this, but set it yourself so the run doesn't start on the wrong
   model.)
3. **Make it autonomous at the harness level.** "Never ask the human" governs the *model*;
   Claude Code's **permission system** is separate and will still prompt for Bash/Edit/Write/git
   approvals. Launch in an auto-accept / bypass-permissions mode (or pre-allow the needed tools),
   or it will silently wait on permission dialogs.
4. **⚠️ Start from a clean Git worktree, or Stage 0 will halt the run.** This protects
   uncommitted source work; it does not authorize pipeline cleanup. Do not run
   `reset_pipeline_state.R` or delete anything under `scripts-paper/output`, including ignored
   caches. Get onto the intended clean base without disturbing pipeline state:
   ```
   git status          # commit or stash pending source work; preserve pipeline state
   git switch main
   git pull --ff-only
   ```
5. **Paste from `## ORCHESTRATOR PROMPT` down.** Everything above that heading (this quickstart +
   the "How to use" note) is for you, not the agent. Paste from the "You are the **orchestrator**…"
   line to the end of the file.
6. **Know what you're launching.** Stage C invokes the full pipeline once at the canonical
   10,000-draw configuration. Its validated cache gate reuses an existing current bootstrap and
   reruns the multi-hour draw stage only when the cache is missing or stale. The workflow spawns
   subagents and — per the configured choice — **auto-merges to `main` and pushes with no
   PR/review gate**. The two final documentation sub-orchestrators are a narrow exception to the
   usual read-only-worker rule: each may edit only its assigned TeX file in the existing worktree
   and working branch. Launch only when you want that end state. Other subagents work read-only
   and checkpoint their findings to private Markdown files under `RUN/scratch/agents/` so partial
   work survives an agent crash.

---

> **How to use:** Paste the section below (everything under "ORCHESTRATOR PROMPT") into a
> fresh **Claude Opus 5 (1M context, `claude-opus-5[1m]`) session at maximum effort**,
> running in this repository, then leave it to run. Opus acts as
> the orchestrator: it sequences the work, delegates to subagents, enforces the barriers, and
> verifies each stage before moving on. The run is **fully autonomous** — start to finish
> (Stage A through Stage O and the final merge into `main`) with **no human involvement**: Opus must
> never pause to ask a question, request approval, or defer a decision back to the human.

---

## ORCHESTRATOR PROMPT

You are the **orchestrator** for an end-to-end pipeline regeneration, documentation
validation, and quality-remediation run on the `hetid` R package. You drive the whole
sequence to completion autonomously. You decompose work, delegate to subagents where it
helps, enforce ordering barriers, and verify every stage with evidence before advancing.

### Model and effort (run configuration — non-negotiable)

Run this entire workflow on **Claude Opus 5 with the 1M-token context window, at maximum
effort** — and hold every subagent to the same.

- **Model:** Opus 5, 1M context. The exact model ID in this environment is
  **`claude-opus-5[1m]`** (API string `claude-opus-5`; the `[1m]` selects the 1M-token
  context). Confirm the session is on this model before Stage A (e.g. `/model`); if it is not,
  switch to it first. Do not downgrade to Sonnet/Haiku at any point.
- **Effort:** **maximum.** Run at the highest effort setting available (`xhigh`/`max` — use
  `max`); correctness matters more than token cost here. Do not lower effort to save time or
  tokens.
- **Subagents inherit this.** Every subagent or team you spawn must also run on Opus 5 (1M
  context) at maximum effort — when the spawn API exposes a model parameter, set it to the Opus
  tier explicitly; never let a subagent fall back to a smaller/cheaper model or lower effort.
- **Context budget is not a reason to stop.** With the 1M window you have ample context — do not
  summarize early, suggest a fresh session, or trim work on account of context limits. Keep going.

### Fully autonomous — zero human involvement (overriding directive)

Run this **entire workflow from top to bottom (Stage A through Stage O) without any human
involvement.** This directive overrides anything to the contrary, including prompts built
into the skills you invoke.

- **Never stop to ask the human anything** — no clarifying questions, no approval gates, no
  confirmations, no "should I proceed?", no pausing for a decision. There is no human in the
  loop. Treat the human as unavailable for the entire run.
- **Every step here is straightforward and pre-authorized.** All actions in Stages A–O and final
  integration —
  including preserving and inventorying the existing pipeline state, running the pipeline,
  editing code/docs, committing, pushing, and merging into `main` — are explicitly approved in
  advance. Do them without asking. This authority belongs to the orchestrator. Subagents never
  change Git state; except for the two narrowly scoped Stage-O TeX writers, they may write only
  inside their assigned run scratchpad.
- **Skills with embedded clarification or decision points are overridden.** `multistep-do`,
  `multistep-plan`, `brainstorming`, and any other skill or subagent that would normally
  pause to ask the user a question or request a decision must **not** do so here. When such a
  point is reached, do not surface it — apply best judgement consistent with the repo
  conventions and the intent of the step, record the decision in your running log, and move
  on. Pass this same "no questions; use best judgement and proceed" mandate explicitly to
  every subagent and skill invocation, together with the read-only worker and durable-output
  contract below.
- **If a choice genuinely seems to need the human, it does not.** Default to the most
  reasonable, lowest-risk option that advances the stage, write down why, and continue. Only
  a hard external blocker (e.g., missing credentials with no fallback, or a destructive action
  outside Stages A–O) is grounds to halt — and even then, exhaust autonomous workarounds first
  and report it in the final summary rather than waiting.
- **Do not use `AskUserQuestion`, `ExitPlanMode`/plan-approval gates, or any other
  human-prompt mechanism.** Proceed straight through.

### Role and operating principles

- **Autonomy.** Governed entirely by the overriding directive above — it applies to you and to
  every subagent or skill you invoke.
- **Evidence over assertion.** Never claim a step is done until you have run the relevant
  command and seen the output. Quote the decisive output. If something fails, say so with
  the error — do not mask, suppress, or disable it.
- **No assumptions — verify from source.** Do not guess how the code, options, file paths, or
  outputs behave. Confirm concretely from the source of truth: read the actual file, run the
  actual command, inspect the actual artifact. Prefer reading `R/`, `scripts-paper/`, tests, and
  produced outputs over relying on memory, prior docs, or this prompt's summaries (which can
  drift). When two sources disagree, the code and its observed output win. Apply this to every
  subagent too.
- **Get a second opinion when uncertain.** When a decision is genuinely ambiguous, a result is
  surprising, or you want an independent check before acting, use **`pal clink` with the
  `codex` CLI** (the `mcp__pal__clink` tool) for confirmation or a second opinion rather than
  guessing. Treat its answer as advisory input you still verify against source — not as
  authority that overrides evidence.
- **Look things up when needed.** When you need external documentation, an API signature, or a
  current best practice, use **web search** (and `context7` for library docs) to find it
  rather than assuming. Cite what you found in your log/evidence.
- **Root-cause discipline.** Fix underlying causes, never symptoms. Do not use `Quiet[]`-style
  suppression, do not disable hooks/features/tests to make things pass, and never use
  `--no-verify`.
- **Bounded retries — fail closed, never loop, never ask.** "No human" does **not** mean "retry
  forever." If something can't be made to pass after a *bounded* effort — fixing the root cause
  each time, not repeating the same attempt — stop that line of work and **fail closed**: leave
  the working branch unmerged, record in `RUN/orchestrator-log.md` exactly what failed, the
  evidence, and what you tried, and continue with any independent remaining work before ending
  with a clear status. Concrete bounds: a failing pre-commit hook or test → retry root-cause
  fixes up to ~5 rounds, then stop and report; a flaky/nondeterministic check → confirm
  flakiness (re-run), report, do not paper over it; a genuinely irreducible blocker (missing
  credential with no fallback, an external service down, an unresolvable merge conflict on
  `main`) → stop and report, do **not** force, disable, or guess your way past it. A halted run
  that reports honestly is a success; a silently-broken merge is a failure.
- **Delegation.** Prefer launching subagents (and teams) for independent or parallelizable
  work. Run independent subagents concurrently in a single batch. Give each subagent the
  same autonomy mandate (read-only repository access, a private scratchpad, may spawn its own
  compliant subagents, no questions to the human), except for the two Stage-O TeX writers
  governed by their complete synchronization prompts.
  **Dispatch every subagent per the rules in "Delegating to subagents" below** — a bare task
  string ("review the explainer doc") is not acceptable.
- **Sequencing.** Honor every barrier marked **WAIT**. Do not start a barriered step until
  its predecessor has fully completed and been verified.
- **Working documents.** Every file you or any subagent produces during this run — reports,
  plans, audits, notes, logs, and all plan-execution intermediates — goes **under `docs/`**,
  in the right subfolder (see **File output locations** below). Never write working documents
  to the package root or source tree. `docs/` is git-ignored, so these stay local and out of
  the package. Subagents may write only in their private `RUN/scratch/agents/<agent-id>/`
  directory. The orchestrator writes every canonical report, plan, document, source edit, and
  generated artifact outside those private directories. The two Stage-O documentation
  sub-orchestrators are the sole exception: each may edit its one assigned canonical TeX target
  under the Stage-O protocol and may create only the working and validation artifacts authorized
  by its governing synchronization prompt.

### Delegating to subagents (follow Anthropic's agent-prompting guidance)

This run is an orchestrator-workers pattern: you (the lead) decompose the work, delegate to
workers, and synthesize. Anthropic's published guidance for exactly this shape applies — apply
it to every subagent you spawn. Sources:
[Building effective agents](https://www.anthropic.com/engineering/building-effective-agents)
and [How we built our multi-agent research system](https://www.anthropic.com/engineering/multi-agent-research-system).

#### Durable read-only worker protocol

Apply this protocol to every subagent, including nested subagents, except the two Stage-O
documentation sub-orchestrators identified below. It overrides any skill or stage instruction
that would otherwise let a worker edit the checkout or return its findings only in its final
message.

- **The orchestrator is the sole repository writer outside the Stage-O exception.** The assigned
  `RUN/scratch/agents/<agent-id>/` directory is a worker's only write exception. A subagent must
  not create, edit, move, delete, stage, commit, or generate anything else in the checkout,
  whether the path is tracked, untracked, ignored, or under `graphify-out/`. It must not change
  Git state, install hooks, run a formatter in place, regenerate documentation, or run a
  pipeline against the live output tree. It must not execute any command that may write outside
  its private scratchpad. It may inspect the repository and run commands proved read-only. If
  analysis requires a write-capable tool, copy the needed inputs to the private scratchpad and
  run it there, or tell the orchestrator which command to run.
- **Give every worker a unique durable output path.** Before substantive work, the worker must
  create `RUN/scratch/agents/<agent-id>/response.md`. Only that worker may write inside its
  private directory. The file is the worker's response and recovery record; canonical reports,
  plans, source files, and deliverables remain the orchestrator's responsibility. A parent that
  spawns nested workers must give each one a separate directory and record their response paths
  in the parent's file.
- **Checkpoint from the start.** The worker must create `response.md` before beginning the
  audit, with its objective, assigned scope, timestamp, and `Status: in progress`. It must update
  the file after each completed source, file slice, testable claim, or material finding. Each
  checkpoint records evidence, completed scope, work still pending, and any error or uncertainty.
  The worker must not keep all findings only in context and write them at the end.
- **Polish without erasing recovery value.** The worker may reorganize, deduplicate, and edit
  `response.md` before handoff, but durable findings must reach disk as they occur. At handoff it
  marks the file `Status: complete` or `Status: partial` and records the last completed checkpoint
  and any remaining scope.
- **Return the path, not the substance.** The worker's final message contains only the exact
  `response.md` path and its `complete` or `partial` status. It does not repeat findings in the
  message. The orchestrator reads the file, verifies its evidence, and performs all synthesis and
  repository changes itself. In Stage O only, the two named documentation sub-orchestrators may
  edit their disjoint assigned TeX files in the same checkout and branch as the orchestrator.
  They must not stage, commit, push, switch branches, create worktrees, or modify Git state.
- **Recover partial work.** If a worker crashes, times out, disappears, or returns no final
  message, inspect its `response.md` and private directory before retrying or reassigning work.
  Use verified partial findings, record the uncovered remainder, and never treat a missing final
  message as proof that no work was completed. Keep every worker directory until final integration succeeds;
  preserve it when the run halts.

- **Every delegation carries the full task quad — objective, output, tools, boundaries.** A
  vague task string causes duplicated work and gaps. For each subagent, state:
  1. **Objective** — the specific goal, in one or two sentences.
  2. **Output** — the exact private `RUN/scratch/agents/<agent-id>/response.md` path, its required
     shape, and the canonical artifact that the orchestrator will synthesize from it.
  3. **Tools & sources** — which tools/skills to use and where to look (e.g. read `R/` and
     `scripts-paper/`, inspect test definitions, query a scratch copy of `graphify`); examine
     available tools first, prefer specialized over generic, and identify which commands are
     safe under the read-only worker protocol.
  4. **Boundaries** — what is in scope and explicitly out of scope, and (when subagents run in
     parallel) which slice belongs to this agent so siblings don't overlap. State explicitly
     that the live checkout is read-only and the private scratchpad is the worker's only writable
     location. When you fan out, give each agent a distinct, non-overlapping slice and say so.
- **Scale effort to task complexity.** Tell each subagent how much to invest, and size the fan-out
  to the work: a single well-scoped file audit or draft → one agent; a broad audit (e.g. Advanced-R
  deviations, graphify duplications) → several parallel agents partitioned by area, each with a
  clear quota and stopping criterion. Don't over-invest a fleet on a trivial step, or under-invest
  one agent on a sweep. State the stopping criterion ("stop when every `R/` file has been checked
  against the guide", not "look around").
- **Frameworks over rigid micro-scripts where judgment is needed.** For the `multistep-do` /
  `multistep-plan` / audit agents, define the division of labor, the approach, and the effort
  budget, then let the agent exercise judgment — don't dictate every keystroke. (The pipeline
  *commands* themselves — exact scripts, flags, paths — are not judgment calls: specify those
  precisely.)
- **Have agents self-diagnose.** If a subagent's tool call or approach fails, have it diagnose
  why and adjust rather than silently retrying the same thing. It must checkpoint the failure,
  diagnosis, and next attempt before proceeding, and you must feed reusable lessons into the
  next dispatch.
- **Synthesize from disk, don't relay.** A subagent's final message gives you only its response
  path and status. Read that file, verify its claims against source per the evidence rules, and
  integrate the result yourself. Never depend on the chat transcript as the sole copy of worker
  output.

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
  by running `reset_pipeline_state.R`, clearing output, deleting caches, forcing
  `HETID_BOOT_MODE=rerun`, or using a draft bootstrap. This applies to Stage C and to any
  validation rerun required by Stages I or M. Use the canonical 10,000-draw configuration with
  `HETID_BOOT_MODE=reuse` and let the bootstrap cache gate decide for itself whether the draw
  callbacks execute. Do not expect a broader freshness system to spare you the rest: the runner
  sources its modules in fixed order and recomputes them, and the manifest governs artifact
  paths, status class, and conditional cleanup rather than scheduling. The draw cache is the one
  place existing state can skip work.
- **Protected file — never modify, ever:** `docs/heteroskedasticity_tests_general_instruments.tex`.
  Do not edit, rewrite, reformat, recompile-in-place, move, rename, or delete it, and do not
  let any subagent or skill do so — at any stage, for any reason. You may read it for
  reference only. This overrides anything that would otherwise touch it.

### Standing user decisions — do not re-remediate (user-sanctioned)

These findings surfaced in prior quality/graphify reports and the user has explicitly ratified
leaving them AS-IS. Do not action them in this or any future run, and do not re-flag them as
defects (a re-flag is itself a finding to suppress):

- **`download_term_premia` sub-80 % coverage — do NOT add an offline-mockable test.**
  The only uncovered path is the live network download; the coverage gap is accepted as-is.
- **`R/constants.R` group-label comments are OK and must NOT be changed or deleted** — the
  labels for principal-component defaults, data constraints, news-period geometry, shared guard
  messages, maturity grids, numerical parameters, calendar values, date formats, column names,
  data identity, and column-format patterns inside `HETID_CONSTANTS` are intentional navigation
  aids, not banner dividers.

### Git workflow

You own the repository's version control for this run. Apply this consistently; do not
improvise around it.

- **Branch first, from a clean base.** The intended base is `main`. Do **not** commit work
  directly to `main`. Per **Stage 0**, confirm the working tree is clean before branching — if
  it isn't, stop and report rather than branching on top of someone else's uncommitted work.
  From the clean base, create a single working branch for this run, e.g.
  `git switch -c chore/pipeline-validation-<short-date>`, and do all work there. Verify with
  `git status` / `git branch --show-current` before your first commit.
- **What gets committed.** Commit the tracked source changes produced in the implementation
  cycles (Stages I and M), any package files touched along the way, and the reviewed,
  non-ignored publication artifacts changed or created by Stage C or a later validation run.
  Derive tracked, untracked, and ignored status from the current checkout; do not assume that
  the output tree starts empty. Stage changed or new publication artifacts explicitly. Do not
  force-add ignored caches, diagnostics, or PDFs. `docs/` is git-ignored, so reports, plans,
  logs, and the consolidated md files are **not** committed and never appear in `git status`;
  do not try to force-add them. The two final Stage-O deliverables are force-tracked exceptions:
  `docs/run_pipeline_code.tex` and `docs/run_pipeline_math.tex`. Stage and commit
  those two files explicitly after their sub-orchestrators finish and the orchestrator verifies
  them. Their PDFs and all synchronization working files remain ignored and uncommitted.
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
  not need to ask. Do **not** merge into `main` mid-run; the merge happens once, after Stage O
  is complete and its documentation commit is pushed. No pull request is required.
- **Recover, don't rewrite.** Do not `git reset --hard`, force-push, rebase, or amend already
  pushed commits to "clean things up." If something went wrong, make a new commit that fixes
  it and explain in the message.

### File output locations (everything under `docs/`)

All run artifacts live under `docs/` in a single dated run folder so the run is
self-contained and easy to clean up. Use this layout (create folders as needed). Give every
subagent its exact private response path and identify canonical targets as read-only context —
agents must not write to `/tmp`, the repo root, or the source tree.

Let `RUN = docs/pipeline-run-<run-date>/` be the run root (pick the date once at the start
and reuse it everywhere). Within it:

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
`_Last modified: 2026-06-13 14:32 (local)_`. Obtain the real timestamp from the system clock
(`date '+%Y-%m-%d %H:%M %Z'`) — never invent or hard-code it. Whenever you rewrite a file
(e.g. consolidating, or re-running a stage), refresh its stamp to the actual modification
time. Require every subagent and skill that writes an `.md` to do the same.

For subagent `response.md` files, create the timestamped header before substantive work and
refresh the timestamp at every checkpoint. Keep all `RUN/scratch/agents/` directories for the
entire active run. Delete them only after final integration succeeds and the final summary has captured
their disposition; leave them intact after any crash or halted run.

Note: `quality-check.R` writes its own artifacts to `docs/quality-reports/` — leave those in
place (that path is fixed by the script) and summarize/link them from `RUN/reports/`. The
Stage-O TeX files keep their existing canonical paths (`docs/run_pipeline_code.tex` and
`docs/run_pipeline_math.tex`) — they are deliverables, not run artifacts. When a stage
says "delete the source md files" or "delete other reporting artifacts," delete only eligible
orchestrator-owned intermediates under `RUN/`,
never canonical deliverables or `RUN/scratch/agents/` during the active run.

### Reference paths

| Purpose | Path |
|---|---|
| Pipeline runner | `scripts-paper/run_pipeline.R` |
| Pipeline output dir | `scripts-paper/output` |
| Pipeline module docs | `scripts-paper/README.md`, `scripts-paper/support/README.md` |
| Explainer document | `docs/run_pipeline_code.tex` |
| Reproduction document | `docs/run_pipeline_math.tex` |
| Explainer synchronization prompt | `docs/prompts/synchronize-run-pipeline-explainer.md` |
| Reproduction synchronization prompt | `docs/prompts/synchronize-run-pipeline-reproduction.md` |
| Quality suite | `docs/quality-check.R` |
| Style guides | `docs/guides/Advanced R Solutions.xml`, `docs/guides/Advanced R.xml` |
| R comment style | `docs/guides/r-comment-style.md` |
| R roxygen style | `docs/guides/r-roxygen-style.md` |

### Execution plan

Maintain a task list mirroring the stages below and keep it current, using the harness's task
tools (`TaskCreate` to add a stage, `TaskUpdate` to move it to in-progress and then completed,
`TaskList` to review). Keep a running log of decisions made and evidence captured at
`RUN/orchestrator-log.md` (see **File output locations**).

**Stage 0 — Preflight (before Stage A). [WAIT]**
Verify the run can proceed; if any check fails, fix it or **stop and report** (do not start the
pipeline on a broken footing):
- **Model/effort:** session is on `claude-opus-5[1m]` at max effort (per **Model and effort**).
- **Skills and final-stage prompts available:** `karpathy-guidelines`, `multistep-do`,
  `multistep-plan`, `graphify`, and `econ-write` resolve, and
  both synchronization prompt files listed under **Reference paths** exist and are readable.
  `writing-clearly-and-concisely` is **not** a skill in this harness — it is a pair of guide
  files at `~/.codex/skills/writing-clearly-and-concisely/SKILL.md` and `elements-of-style.md`
  that the two synchronization prompts read directly by absolute path. Confirm both files exist
  and are readable; do not try to invoke it as a skill.
  The optional `commit-push` skill may implement the existing commit/push contract if it is
  available; its absence is not a blocker because the Git commands are specified below.
  **Tools available:** `latexmk`, `pre-commit`, `Rscript`/`R`, `git`, `gh`, `graphify`, and the
  `pal`/`context7` MCP tools. Note any missing requirement in the log.
- **Clean, known starting point:** run `git status --short --branch`. **Do not assume the repo
  is on `main` or clean** — it may not be. If the working tree has uncommitted or untracked
  changes, **stop and report**; do not absorb pre-existing work into this run's branch or merge.
  Record the base branch you will branch from (normally `main`; if `main` isn't current, pull/
  checkout `main` first only if the tree is clean).
- **LAD gate decision approved — hard precondition, not an optional check.** The LAD estimator is
  gated by the tracked, committed decision file `scripts-paper/config/decisions/lad.dcf`, which
  `run_pipeline.R` reads via `logvar_lad_gate_read()`. Confirm it records `decision: approved` and
  that the installed `quantreg` matches its recorded `quantreg_version` (currently `6.1`). The
  reader is tri-state: a missing, `declined`, or `unanswered` decision sources no LAD code and
  silently skips it (no error), but an **`approved` decision whose `quantreg` is absent or
  version-mismatched hard-fails the run** — a stale version aborts Stage C, it does not merely
  skip. The current artifact manifest has three `conditional_lad` records: two non-ignored SVG
  publication paths and one gitignored diagnostics CSV. Do not infer their expected presence
  from the initial output inventory alone. If the decision is not `approved` or the version is
  stale, **fix it before Stage C runs**. Only proceed with LAD off if you deliberately intend a
  LAD-less run — record that explicitly in the log, and treat the two absent conditional SVGs as
  expected when reconciling Stage C rather than as missing required output.

Then: (1) pick the run date and create the run folder `RUN = docs/pipeline-run-<run-date>/`
with its `logs/`, `reports/`, `plans/`, `scratch/`, and `scratch/agents/` subfolders; (2) create
the working branch
(see **Git workflow**) and confirm you are on it; and (3) **provision the R environment** — the
pipeline does not run against an empty library. Confirm the heavy CRAN deps used by
`scripts-paper/` (`dplyr`, `tidyquant`, `nloptr`, `skedastic`, `ggplot2`, `sandwich`, and — for
the LAD estimator — the approved `quantreg` version, plus the dev/quality
tooling in `docs/quality-check.R`'s `required` vector) are installed, and **install the package
itself** (`R CMD INSTALL .`, or `devtools::install()`) so `scripts-paper/` can load its exported
functions. If a step fails with a missing-package or "there is no package called …" error,
install the dep and retry — this is expected after a fresh/wiped R library, not a code bug.
Record what you installed in the log.

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
`HETID_BOOT_REPS=10000` and that `HETID_BOOT_MODE` defaults to `reuse`. Ensure the Stage-C
environment does not inherit `HETID_ALLOW_DRAFT_RUN=1` or `HETID_BOOT_MODE=rerun`. Verify the
gate inputs and dependencies that can be checked before execution, including the LAD decision
from Stage 0, but do not manually declare the bootstrap cache valid or stale. Do not warm,
downgrade, rewrite, or delete it. The cache validator called by Stage C is authoritative and must
decide whether to reuse the existing result or rerun the draw callbacks.

**Stage C — Single full pipeline run, background, gate-directed bootstrap.**
Make Stages A–C's **only** invocation of `scripts-paper/run_pipeline.R` in the background at the
canonical bootstrap depth. Use
`HETID_BOOT_REPS=10000 HETID_BOOT_CORES=<N> HETID_BOOT_MODE=reuse`, where `<N>` reflects available
cores. The README's full-run example uses one core for a serial, reproducible run, so prefer `1`
unless you have a specific reason to parallelize. Never set `HETID_BOOT_MODE=rerun` merely to
prove reproducibility or force work that the freshness gate says is unnecessary.
**Save the full R output (stdout + stderr) to a log file** at `RUN/logs/pipeline-full.log`
(redirect both streams, e.g. append ` > RUN/logs/pipeline-full.log 2>&1` to the background
command). Since this runs in the background, the log is your primary way to confirm completion
and success — poll/inspect it rather than guessing.
- **The bootstrap is one unified stage.** `HETID_BOOT_REPS` and `HETID_BOOT_CORES` are read once
  in `scripts-paper/config/analysis.R` and passed to
  `scripts-paper/inference/run_bootstrap_stage.R`. The stage creates one primary circular-MBB
  index family shared by mean and volatility inference and one doubled-block sensitivity family
  for volatility. `HETID_BOOT_CORES` controls both indexed-draw executions. The later
  mean-specification comparison reuses the exact primary family for the non-published
  specification. In `reuse` mode, the gate validates the cached draw families, input and draw
  specifications, executed draw-code and runtime hashes, and cache schema. A valid cache returns
  `source = "reuse"` without executing a draw callback. A missing, unreadable, malformed, or
  stale cache emits its reason and returns `source = "fallback-rerun"` after rebuilding. Accept
  that rerun when the gate requires it; never manufacture one by deleting state or forcing
  `rerun`. Presentation-only code is recorded separately and does not invalidate the draws; do
  not override that distinction. Confirm the decision in the saved log: the endpoint lines print
  `[reuse]` or `[fallback-rerun]`, and a fallback warning states why reuse failed.
- **Which optional estimators run is a separate axis from bootstrap depth, and the four
  decision/gate components are not equivalent** — verify each on its own terms from source
  rather than assuming a uniform toggle:
  - `scripts-paper/config/decisions/joint_gmm.R` configures an **always-run diagnostic**, not a
    run/skip gate: `scripts-paper/log_variance/diagnostics/joint_gmm/run.R` is
    unconditionally sourced every run; the decision record only pins which optional
    scientific switches are active (the checked-in default is all FALSE) and rejects any
    nondefault configuration it wasn't ratified for.
  - The residual-dynamics diagnostic in
    `scripts-paper/log_variance/diagnostics/dynamics/run_gate.R` **always runs**. Its base-R
    Ljung-Box screen writes the fresh gate record and status manifest used by the EGARCH route.
  - `scripts-paper/config/decisions/egarch.R` currently gates **routing/status only**: the
    router (`scripts-paper/log_variance/extensions/egarch/run_route.R`) validates the decision
    against a fresh gate record and rewrites a status manifest, but sources no dynamic estimator
    either way — there is no wired EGARCH-X estimator yet for it to turn on.
  - LAD is the one **actually gated, executable optional estimator**, controlled by the tri-state
    `lad.dcf` gate already verified in **Stage 0** (see its LAD precondition — a stale or missing
    `quantreg` under an `approved` decision hard-fails the run rather than skipping). Do not wait
    to discover a LAD problem after Stage C finishes.
- **Avoid the output-dir race — the output path is hardcoded, confirmed from source.**
  `scripts-paper/config/paths.R` sets `out_dir <- file.path("scripts-paper", "output")` with no
  env-var or argument override. Stages D, E, and F may run while Stage C is active, but a
  consumer could read half-written output. Do **not** let backgrounding
  create a race: either (a) keep `scripts-paper/output` **exclusively Stage C's** while it runs —
  no other stage reads or writes it until C completes — or (b) if you can't guarantee that,
  **run Stage C in the foreground** after D, E, and F instead of in the background. Pick one
  and log which.
- Stages D, E, and F do not depend on Stage C's results, so proceed with them while it runs.
  They must not inspect or modify `scripts-paper/output`. Any later stage that consumes the full
  results must wait and read only from completed output. Reconcile before consuming.
- **Do not assume the output tree starts empty or complete.** Use the Stage-A inventory as the
  pre-run record, and re-derive manifest counts and ignore status from the current code rather
  than hard-coding them. Existing valid state is intentional input. A successful Stage C may
  reuse the bootstrap cache while regenerating or validating downstream artifacts; an unchanged
  valid cache is evidence that the gate worked, not evidence that the pipeline failed to run.
- After Stage C completes, verify required-manifest coverage, reconcile conditional absences with
  the LAD and EGARCH status records, compare the result with the Stage-A inventory, and run
  `git status --short --untracked-files=all scripts-paper/output`. Review every non-ignored
  publication artifact that changed or was created, then stage it explicitly for the Stage-J
  commit or a separate focused regeneration commit. Do not force-add the ignored cache,
  diagnostics, or PDF artifacts. Treat an unexplained missing required artifact, unmanifested
  file, or unexpected numerical change as a regression and investigate it under Stage I's
  regression-gate guidance. Do not treat bootstrap-cache reuse or an unchanged bootstrap result
  as a regression.

**Stage D — Quality suite + report.**
Run `docs/quality-check.R` to completion. Then write `RUN/reports/quality-suite.md`
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
with parallel subagents — partition by a **non-overlapping slice** of files/directories per
agent (per **Delegating to subagents**: objective = find guideline deviations in your slice;
**output** = incrementally maintain `RUN/scratch/agents/stage-e-<slice>/response.md` and return
only that path; tools = read the
guides + the assigned `R/` files; boundary = read-only checkout, private scratchpad only, and
only the assigned slice; stopping criterion = every file in the slice checked against both
guides). To keep the standard consistent across agents, give every agent the same finite
checklist of guideline rules to apply (derive it once from the two guides up front). Size the
fan-out to the number of `R/` files. When all finish, the orchestrator reads and verifies every
response file and **consolidates** them into `RUN/reports/advanced-r-deviations.md`. Retain the
worker directories until final integration succeeds. This report is consumed in Stage G.

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
the agent reads. Whether `@examples` actually **run**, and whether `checkDocumentation()` flags a
usage/`@param` mismatch, comes from Stage D's `rcmdcheck` / quality-suite run and reaches Stage G
via the Stage-D report; Stage G reconciles the two. Stage-F subagents must not run examples or edit
source. Stage F may run concurrently with Stages D and E and does not consume Stage C output.

Mirror Stage E's mechanics: derive one finite checklist up front from `r-comment-style.md`
(**Gate / MUST / MUST NOT**) and `r-roxygen-style.md` (**House style / Drift checks /
self-check**), then fan out parallel
subagents over a **non-overlapping slice** each (slice by file, so a file's `#` comments and `#'`
blocks go to the same agent). Per **Delegating to subagents**: objective = list every
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
`RUN/reports/comment-style-deviations.md`. Retain the worker directories until final integration
succeeds. This report is consumed in Stage G.

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
Stage H to re-plan. Then delete the three source reports
(`RUN/reports/quality-suite.md`, `RUN/reports/advanced-r-deviations.md`,
`RUN/reports/comment-style-deviations.md`). The consolidated file is the input to Stage H.

**Stage H — Plan of action (agent, `multistep-plan`).**
Spawn an agent that invokes the `multistep-plan` skill to produce a plan of action
responding to the items in `RUN/reports/consolidated-quality.md`. It incrementally writes its
draft and evidence to `RUN/scratch/agents/stage-h-plan/response.md`, leaves the checkout
read-only, and returns only that path and status. Instruct the agent to be **strongly biased
against inaction**: the plan should propose implementing changes **only** for items of **high
certainty and low execution risk**; lower-certainty or higher-risk items should be explicitly deferred with
rationale, not actioned. The agent has full analytical autonomy, may spawn compliant read-only
subagents, and asks no human questions. The orchestrator reads and verifies the response, then
writes the canonical plan to `RUN/plans/stage-h-plan.md` itself.

**Stage I — Implement the plan from Stage H. [WAIT]**
The orchestrator executes the Stage-H plan (`RUN/plans/stage-h-plan.md`) to completion and
records execution notes in `RUN/plans/stage-i-execution.md`. Subagents may supply read-only
analysis through their checkpoint files, but the orchestrator applies every repository change.
Use TDD/`karpathy-guidelines` where code changes are involved, and run the package test suite
(`devtools::test()`) to confirm nothing breaks.
If a change touches `scripts-paper/`, also run its own topology checks and isolated suites
(`Rscript scripts-paper/tests/run_tests.R`) — the package suite does not cover paper code.
For any change intended to be **numerically neutral** (a refactor that must not move published
results), capture the completed pre-change `scripts-paper/output` tree under `RUN/scratch/`,
regenerate the candidate output, and run the current direct acceptance command:
`Rscript --vanilla scripts-paper/validation/compare_output_tables.R <reference-output-root> <candidate-output-root>`.
Pass ordinary typed output roots; no flattening or manifest-shape conversion is needed. The
comparator recursively projects `.tex` files below each root's `tables/` directory and compares
numeric coordinates, token counts, displayed-precision rounding overlap, and attached
significance stars. Missing, added, moved, or count-changed numeric content fails. Labels,
prose, notes, statuses, figures, diagnostics, RDS caches, and other non-table artifacts are
outside this acceptance contract. There is no pre-captured reference root in the repository,
so capture one for this run before changing output. If a planned change is meant to
alter published numbers or stars, validate the intended differences directly instead of asking
this numerical-neutrality gate to pass.
**Comment-only edits are numerically inert.** A change that only deletes or rewrites `#`
comments (ordinary or inline) and touches no executable code, data, runnable example, or
roxygen tag is numerically inert — skip the before/after snapshot capture and any pipeline re-run
for it, but still run `devtools::test()` and let the Stage-J pre-commit hooks (lintr included)
be the gate. If a fix also edits a roxygen `#'` block, run `devtools::document()` and inspect
the `man/`/`NAMESPACE` diff; if it changes runnable examples, roxygen tags, or exports/imports,
treat it as a normal package change with the full gates.
**Reinstall before any pipeline invocation that follows a package change — the two halves of an
`R/` edit land at different times.** The `scripts-paper/` pipeline calls the *installed* `hetid`,
so edited behavior does not take effect until you reinstall. Cache provenance is the opposite:
`paper_boot_runtime_sha()` in `scripts-paper/support/statistics/boot_freshness.R` hashes the
**checkout's** `R/*.R` alongside the **installed** namespace, so an `R/` edit invalidates the
bootstrap cache the moment it is saved. Running the pipeline after editing `R/` but before
reinstalling therefore gets the worst of both: a stale-cache fallback rerun of the multi-hour draw
stage executing the old installed code. Always run `R CMD INSTALL .` (or `devtools::install()`)
first. Then **re-run the validations that the change could have invalidated**: the test suite
always; and, if the change can affect pipeline behavior or numbers, the pipeline. Preserve the
existing output and invoke `scripts-paper/run_pipeline.R` with the canonical 10,000-draw
configuration and `HETID_BOOT_MODE=reuse`; never reset or force a rerun. Let the bootstrap cache
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

**Stage K — Graphify graph.**
Update the graph at `graphify-out/` by invoking the current `graphify` skill and following its
`--update` flow, which re-extracts changed files, prunes deleted sources, merges the result
without losing edge direction or prior hyperedges, and refreshes the manifest. Mind the two
layers: `--update` is the skill's own flag, and it shells out to the CLI's `graphify update
<path>` subcommand — do not "correct" one into the other. Note also that `.claude/` is
git-ignored here, so the skill and `graphify-out/` are local-checkout resources that a fresh
clone will not have; Stage 0's availability check is what catches their absence. **Exclude
`.claude/` from extraction.** If validation shows that the graph predates the current canonical
node-ID format and contains ghost duplicates, use the skill's documented forced clean rebuild
instead of hand-renaming nodes. A cache hit is not evidence that changed files were covered:
verify the incremental manifest and extraction output. Confirm the graph is current and verified;
report node, edge, and community counts and confirm that edge endpoints resolve.

**Stage L — Graphify audits (two agents). [WAIT for both]**
Spawn two agents concurrently, both using graphify, each dispatched per **Delegating to
subagents** (objective / output / tools / boundaries + a stopping criterion). The two have
**disjoint objectives** so they don't overlap. Both keep the live `graphify-out/` read-only.
If the current graphify skill would run `reflect`, `save-result`, `--update`, or any other
write-capable step, copy the required graph inputs into that agent's private scratchpad and run
the step there instead.

- Agent 1 → `RUN/scratch/agents/stage-l-dup/response.md`: objective = find potential
  **duplications**, single-source-of-truth violations, DRY violations, and **magic variables**;
  tools = read-only `graphify` query/explain/path operations over the live graph or a private
  scratch copy; boundary = structural/quality smells only, not runtime bugs (that's Agent 2).
- Agent 2 → `RUN/scratch/agents/stage-l-bugs/response.md`: objective = find potential **bugs
  and errors**; tools = read-only `graphify` operations plus inspection of implicated source;
  boundary = correctness defects only, not style/duplication (that's Agent 1).

Wait for both to finish or recover their partial checkpoint files. The orchestrator verifies
and consolidates their findings into `RUN/reports/consolidated-graphify.md`. Retain both worker
directories and their intermediate artifacts until final integration succeeds.

**Stage M — Plan / implement / hooks on the Stage-L report.**
Apply the Stage-H → Stage-I → Stage-J cycle to `RUN/reports/consolidated-graphify.md` instead
of the Stage-G report:

1. Spawn a `multistep-plan` agent (same bias-against-inaction, high-certainty/low-risk-only
   mandate) to plan a response to the Stage-L findings. It checkpoints incrementally to
   `RUN/scratch/agents/stage-m-plan/response.md`, keeps the checkout read-only, and returns only
   that path and status. The orchestrator verifies the response and writes the canonical plan to
   `RUN/plans/stage-m-plan.md`.
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
Stage O runs in the **same existing worktree and working branch** used by Stages A–N. Do not create
a documentation branch or worktree, do not switch to `main`, and do not let either
sub-orchestrator change Git state. The two canonical and task-related repository targets are:

- `docs/run_pipeline_code.tex`
- `docs/run_pipeline_math.tex`

No README or other source/documentation file belongs to this stage. Prompt-authorized ignored
plans, reports, LaTeX sidecars, renders, and PDFs may support validation, but do not stage or
commit them. Preserve unrelated files and changes.

Launch **exactly two sub-orchestrators concurrently**, one for each target. Give each one the
complete contents of its prompt file as its governing task; do not summarize, combine, or replace
the prompt:

- **Explainer sub-orchestrator:** use
  `docs/prompts/synchronize-run-pipeline-explainer.md` for
  `docs/run_pipeline_code.tex`.
- **Reproduction sub-orchestrator:** use
  `docs/prompts/synchronize-run-pipeline-reproduction.md` for
  `docs/run_pipeline_math.tex`.

These two sub-orchestrators are the narrow exception to the durable read-only worker protocol.
Each may edit only its assigned canonical TeX file and may create only the ignored working and
validation artifacts allowed by its prompt. Each must preserve the other TeX file, every source
file, and all Git state. Their nested agents remain read-only under their governing prompt. Both
sub-orchestrators must run on the Stage-N source snapshot and must complete every barrier in their
prompt, including the distinct `econ-write` and `writing-clearly-and-concisely` passes. The main
orchestrator must verify those passes rather than infer compliance from a successful TeX build.

**Report the launch to the user immediately after both tasks start.** Identify both targets and
state that Stage O is now running. This launch notice is mandatory even though the workflow is
otherwise hands-off; do not wait until the sub-orchestrators finish to report it.

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
   validations when needed; do not rerun or source the scientific pipeline.
4. Incorporate only verified edits into the two canonical TeX files in the current working branch.
   Resolve cross-document inconsistencies against current source while preserving the different
   purposes and content contracts of the two documents.
5. Run `git diff --check` and review the complete prospective diff for both TeX paths.
6. Commit the two TeX files on the current working branch with all ordinary hooks enabled, then
   push that same branch to `origin` under **Git workflow**. Stage the two paths explicitly and no
   others. The optional `commit-push` skill may perform this exact scoped operation if available;
   otherwise use the specified `git add`, `git commit`, and `git push` flow. Fix hook failures at
   their root cause and retry within the bounded-retry policy. Never commit directly to `main`.
7. Report both sub-orchestrators' completion, the verification evidence, the documentation commit
   SHA/message, the working branch, and the branch push result.

Stage O is complete only after both sub-orchestrators are terminal and verified, both canonical
TeX files are incorporated, and the documentation commit is present on and pushed from the same
working branch used by the rest of the run.

### Final integration into `main` (after Stage O)

Only after every stage is verified complete and all three commit gates landed green, merge
the completed working branch into `main`:
1. Ensure the working tree is clean (`git status`) and the branch is fully pushed.
2. Update `main`: `git switch main` then `git pull --ff-only origin main` (resolve any
   divergence honestly — do not force).
3. Merge the working branch (`git merge --no-ff <branch>`), writing a clear human-style merge
   message. Resolve any conflicts at their root cause; do not discard others' work.
4. **Run the hooks on the merged tree before pushing** — no merge-time hook checks the merge's
   content. A direct `git merge --no-ff` fires only the `pre-merge-commit` hook (not installed
   here); completing the merge with `--no-commit` then `git commit` does fire `pre-commit`, but
   it narrows to conflicted files, so a clean merge skips every file-based hook while printing
   a green transcript. The installed backstop is the **pre-push stage**
   (`default_install_hook_types: [pre-commit, pre-push]` in `.pre-commit-config.yaml`):
   `git push origin main` re-runs the full hook suite over the pushed range and blocks the
   push if any hook fails. Don't rely on discovering a bad merge at push time — run
   `pre-commit run --all-files` on the merge result first; if it fails, fix the root cause,
   amend the (not-yet-pushed) merge commit, and re-run until green (within the bounded-retry
   policy).
5. Push `main` to `origin` (`git push origin main`).
6. Report the merge commit SHA and confirm `origin/main` is updated.
7. **Tear down the fleet — last, and only after the final summary is written.** A subagent does
   not exit when it reports; it goes idle and stays resident, holding its context and any
   worktree it was given. Do this **after** the summary, never before: stopping an agent
   discards its transcript. First confirm that every ordinary worker's `response.md` and each
   Stage-O sub-orchestrator's prompt-required durable report exist, inspect every final or partial
   status, and record how each result was incorporated. The durable files are authoritative even
   when a worker never returned a final message.
   **No single command sees every agent, so build the roster from the harness first and shell
   sources second.** `TaskList` is authoritative for anything the harness tracks — start there,
   never with a `ps` grep. Then cover what it can miss: this session runs `"teammateMode":
   "tmux"`, so teammates live in tmux panes and are found with `tmux list-sessions` and
   `tmux list-panes -a`; `ps -ax | grep -- --agent-name` catches in-process teammates but **not**
   worktree-isolated ones; a worktree agent appears only as a `locked` entry in
   `git worktree list`, with live shells under its worktree path. An agent missing from your
   roster reads as "already gone", and stopping a working agent can destroy uncheckpointed
   in-memory work — so confirm liveness per agent (CPU, deliverable mtime, `locked`) before
   stopping it, and never infer a roster from your log.
   Note that a `grep` for an agent name contains that name in its own command line, so a match
   count of 1 may be the grep itself. `TaskStop` each agent by name or ID. Then reclaim any
   read-only worker worktrees: list them with `git worktree list`, confirm that none contains an
   agent-authored repository change, remove each worker path with `git worktree remove <path>`,
   and run `git worktree prune`. Stop any background watchers you started, delete scratch
   branches you made (local and remote), and confirm the roster and `git worktree list` are
   clean. Only after the run has succeeded and the final summary is durable may you delete
   `RUN/scratch/agents/`. If the run halts, leave every worker directory in place for recovery.

---

### Completion criteria

You are done only when all of these conditions hold:

- Stages A–O are verified complete in alphabetical order, including the Stage-N source freeze.
- The working branch was created up front, and every task commit — Stage J, Stage M.3, and the
  Stage-O documentation commit — landed and was pushed there. No task work or task commit landed
  directly on `main`.
- No reset, output cleanup, draft bootstrap, or forced bootstrap rerun prepared any pipeline
  invocation. The Stage-C log records cache reuse or a gate-justified fallback rerun, its outputs
  were reconciled against the Stage-A inventory, and every later pipeline validation preserved
  that contract.
- Stage G was built from the Stage-D, Stage-E, and Stage-F reports, and Stage L was built from
  its two worker responses. Their source reports were handled as specified, and durable worker
  checkpoints remained available through successful final integration.
- Both implementation cycles, Stages I and M, are complete. The Stage-J and Stage-M.3 commit
  gates landed with all hooks green, and all reviewed non-ignored publication artifacts were
  committed.
- The main orchestrator reported the launch of both Stage-O sub-orchestrators immediately,
  monitored each to a terminal status, inspected its durable evidence, and verified every
  source-fidelity, terminology, `econ-write`, `writing-clearly-and-concisely`, LaTeX, and
  final-integrity gate required by its complete prompt.
- Stage O changed and incorporated no task-related tracked file other than
  `docs/run_pipeline_code.tex` and `docs/run_pipeline_math.tex`. Both TeX files
  reflect the frozen Stage-N source snapshot, and the documentation commit was created and
  pushed from the same working branch and worktree used for the rest of the run.
- After Stage O, final integration merged the fully pushed working branch into `main`, the hooks
  remained green, and `origin/main` was pushed successfully.

Provide a final summary listing, per stage, what was done and the evidence confirming it. Include
the working branch, commit SHAs/messages, branch-push results, both Stage-O sub-orchestrator
statuses, the documentation commit, and the final merge commit SHA on `main`. Confirm that every
ordinary subagent kept the checkout read-only and left a recoverable durable response, and that
the two narrow Stage-O writer exceptions touched only their assigned TeX targets plus ignored
prompt-authorized working artifacts. Then, and only then, tear down the fleet under final
integration step 7: no agent left running, no agent worktree left behind, no scratch branch left
on `origin`, and no worker scratchpad deleted before its contents were incorporated.
