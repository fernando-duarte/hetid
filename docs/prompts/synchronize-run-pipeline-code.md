# Orchestrator prompt: synchronize the pipeline explainer

You are the primary orchestrator for a complete source-to-document synchronization.

Select the repository root before any inspection or edit:

1. If an enclosing workflow supplies an isolated run-worktree root, use that exact root.
2. Otherwise, if the current working directory belongs to a Git worktree containing
   `scripts-paper/run_pipeline.R`, use that worktree's top level.
3. Only as a standalone fallback, use
   `/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid`.

Record the selected root and why it was selected. Resolve every repository path in this prompt
relative to it. The principal paths are:

- production entrypoint: `scripts-paper/run_pipeline.R`;
- canonical TeX document: `docs/run_pipeline_code.tex`; and
- canonical compiled PDF: `docs/run_pipeline_code.pdf`.

Verify that the selected root contains the entrypoint and target TeX before proceeding. Do not
inspect or modify another checkout, including the standalone fallback when an enclosing workflow has
selected a different worktree.

Your objective is to make the TeX document fully faithful to the current checkout source. Assume that both the source and document have changed since any prior audit. Trust neither the document nor old reports. Derive every claim from a freshly traced source snapshot.

The checkout source specifies intended package behavior, but a plain pipeline invocation calls the
installed `hetid` namespace. Treat equality between the checkout package source and the installed
namespace as a separate precondition. If read-only evidence from the enclosing workflow proves that
they match, record that evidence. Otherwise, certify checkout-source parity, label installed-package
parity unverified, and explain that the runtime may differ. Do not load or execute project logic merely
to remove this limitation.

The finished document must tell a reader:

1. The exact path prescribed by checkout source for a plain invocation of
   `Rscript scripts-paper/run_pipeline.R`, with any installed-package parity caveat stated beside it.
2. Every input, option, prerequisite, decision, constant, and runtime condition that can affect that path.
3. Every alternative path or capability implemented in the code but not selected by a plain invocation.
4. Every reserved or described path that lacks a substantive implementation.
5. Every separate repository entrypoint that directly produces, resets, validates, tests, reproduces,
   or quality-checks this paper pipeline, or implements one of its scientific alternatives, but that
   the runner does not call.
6. The exact downstream effects of each choice on stages, estimators, diagnostics, caches, artifacts, publication, and failure behavior.
7. Which statements follow from static source and which outcomes require runtime evidence.

A reader must never have to infer whether the runner executes something. Mark current execution, alternative execution, unavailable functionality, uncalled functionality, and separate entrypoints explicitly.

## Governing rules

Run this workflow from beginning to end without human involvement.

- Never ask the user a question.
- Never request approval, confirmation, or a plan decision.
- Do not use a human-prompt or plan-approval mechanism.
- Treat this prompt as written authorization to pass a discretionary skill approval gate. Pause only
  when a higher-priority instruction requires it.
- Use your best judgment when several defensible choices exist.
- When uncertain, ask another agent or use the available PAL clink interface with Claude or Fable.
  In Codex, the expected interface is `mcp__pal__clink`. Treat external reviews as advice and verify
  every suggestion against source.
- Stop only for a hard external blocker after exhausting safe alternatives. Record the blocker and
  completed work in the current run directory; do not wait for a human.
- Do not commit, push, open a pull request, or modify remote state.

Evidence controls every conclusion:

- Do not claim that a stage is covered until you have inspected its source.
- Do not claim that a check passed until you have run it and read its output.
- Record decisive command output in the final audit and quote only the short portion needed to prove
  the result.
- Do not hide, suppress, disable, or weaken a failed check.
- Fix the cause of a failure when the cause lies in the target TeX or its build. Record an external or
  out-of-scope cause as a limitation; do not modify another file to force a pass.
- Use bounded retries. Change the approach or fix the cause before retrying.
- Set a finite retry cap for each fallible external step in the execution plan.
- If a check remains impossible, record the command, error, attempts, and remaining consequence in a
  Markdown report in the current run directory. Continue independent work and report the incomplete
  gate at the end.

The current checkout code is authoritative for intended behavior:

- Read the executable statements, predicates, call graph, configuration, decision records, and manifest-building source.
- Treat comments, tests, README files, the installed-package description, and the old TeX as
  supporting evidence. When they disagree with checkout executable source, describe the checkout
  behavior and record any unresolved runtime-parity limitation.
- Do not preserve a TeX statement merely because it compiled or appeared in an earlier audit.
- Do not import old counts, hashes, defaults, budgets, object names, or route descriptions. Recompute them from the current source.
- Treat every hash and count as evidence for one frozen snapshot, never as a permanent contract.

## Strict no-execution boundary

Do not run or source the R pipeline.

Specifically, do not:

- run `scripts-paper/run_pipeline.R`;
- source or evaluate any repository R file;
- call an R function from the repository;
- run a test or validation command that sources production R;
- initialize, reset, populate, clean, migrate, or inspect the contents of pipeline outputs;
- read cache contents or use cache state as behavioral evidence;
- instantiate the artifact manifest;
- inspect generated pipeline artifacts as evidence;
- run `reset_pipeline_state.R`;
- force a bootstrap rerun or reduced draft run;
- change an environment variable to probe production behavior.

The output tree, caches, generated manifests, and route-state files may be empty by design. Preserve that state.

Static inspection is allowed. Shell text tools, parsers, hashes, and metadata-only filesystem checks
are allowed. Restrict content searches to the audited source universe and exclude every generated
state root. You may run a small standalone snippet in a fresh system temporary directory outside the
repository to test syntax or a self-contained hypothesis. Such a snippet must not import, source, or
evaluate project code, invoke R, or touch pipeline outputs, caches, manifests, or state.

Compiling the standalone TeX document is allowed and required. Build in a fresh system temporary
directory outside the repository. Copy only logs and page renders needed for the audit into the
current run directory, and copy only the accepted PDF to its canonical path.

## Repository and editing boundaries

Before doing task work:

1. Read every applicable `AGENTS.md`.
2. Read `CLAUDE.md` and any project guidance it names.
3. Inspect the current Git status without modifying it.
4. Preserve all unrelated user changes.
5. Record the current commit and branch; the complete initial Git status; TeX and PDF hashes and
   sizes when present; and a sorted hash-and-size manifest for every file in the audited source
   universe.
6. Create a metadata-only manifest for every protected output, cache, manifest-instance, and
   route-state root derived from source. Record paths, types, sizes, modification times, and symlink
   targets without reading file contents.
7. Obtain report timestamps with:
   `date "+%Y-%m-%d %H:%M %Z"`

Outside the current run directory, the only canonical paths this workflow may create or modify are:

  docs/run_pipeline_code.tex
  docs/run_pipeline_code.pdf

Only the primary orchestrator may modify those files. Subagents and external reviewers must never
modify them.

Create plans, audits, matrices, reports, proposed patches, retained logs, and renders as new uniquely
named files in the run directory. Never overwrite an existing working record. LaTeX sidecars and
intermediate build files remain in the external temporary build directory.

If the enclosing workflow supplies a records directory, create this prompt's unique run directory at:

  <enclosing-records-directory>/stage-o/code/YYYYMMDD-HHMMSS/

Otherwise, use the standalone location:

  docs/RUN/run_pipeline_code/YYYYMMDD-HHMMSS/

Subagents may write only inside the selected current run directory:

  <current-run-directory>/scratch/agents/<agent-id>/

Subagents have read-only access to the rest of the repository. They may create proposed patches or revised copies inside their private scratch directories. The orchestrator must inspect those proposals, verify them against source, and apply accepted changes to the canonical TeX.

Make text edits in place with the harness's file-editing tools. The final accepted binary PDF may be
copied to its canonical path and must then be verified byte-for-byte. Preserve unrelated content and
the user's existing work. Do not use destructive Git commands, broad deletion commands, or shell
redirection to rewrite a source file.

## Planning and delegation

Before editing, write a concrete execution plan. This prompt already defines the complete workflow;
do not wrap it in `multistep-plan`, `multistep-do`, or another general multistep workflow. Do not
pause for plan approval.

Write the plan in the current run directory. Include:

- the source boundary;
- the no-execution boundary;
- agent assignments;
- ordering barriers;
- evidence required at each barrier;
- files each agent may inspect;
- files each agent may write;
- acceptance tests;
- retry and blocker rules.

Use separate agents when they add independent evidence. Respect the environment's global capacity,
including agents owned by a parent workflow. Run assignments sequentially when capacity prevents
parallel work; capacity limits must not reduce coverage. Keep tightly coupled synthesis in the
orchestrator. If a transient capacity rejection occurs, continue independent local work and retry
after a slot is released; do not treat one rejection as a blocker. Follow any slot arbitration
provided by the enclosing workflow.

Cover these roles with separate read-only reviews when capacity permits, or with fresh sequential
assignments when it does not:

1. Trace the exact current runner path and every stop condition.
2. Inventory inputs, options, prerequisites, decisions, and source-fixed controls.
3. Trace alternative, dormant, unimplemented, maintenance, validation, test, cache, and artifact functionality.

Agents may subdivide their assignments only within the available global capacity. Require exact
source locators and evidence in every report.

After the main TeX synchronization, run three distinct review assignments, in order:

1. A same-concept/same-word compliance agent.
2. An `econ-write` compliance agent.
3. A `writing-clearly-and-concisely` compliance agent.

Use different agents for these passes when thread capacity permits. If a global thread limit requires
reuse, give the idle agent a new bounded assignment, the exact current snapshot, and instructions to
audit it independently without relying on its earlier conclusions. Never combine two passes. Each
pass begins only after the preceding pass has been incorporated and verified.

**"After the preceding pass is incorporated" is a precondition, not a preference.** Overlapping the
passes to save wall-clock defeats them: a pass that reviewed pre-edit text has not certified the text
you ship, and no amount of careful application recovers that. If you do overlap them, the only honest
remedy is to re-run the affected passes on the final text, which costs more than sequencing would
have.

### Review discipline that decides whether a pass verdict means anything

Each of these needs a mechanism; intending to be careful has already failed at several of them.

- **A certification covers bytes, not a document.** An edit applied after an agent certifies voids
  that verdict. Name the version each agent reviews and re-state which version each verdict covers.
- **Freeze the target for the whole of a round.** Editing while an agent reads forces it to re-anchor
  its findings and silently invalidates its verdict. The rule that holds is **apply nothing until
  every agent dispatched in that round is terminal** — not until the first one reports.
- **When only part of the document changed, prove the rest did not** by recording a digest per stable
  region and having the next agent recompute it rather than accept your claim. Ask for the
  measurement; never supply the expected value. Choose the regions after establishing they contain no
  edit — choosing first and checking later produces a bound that does not hold.
- **A brief that paraphrases the source of truth manufactures defects.** An agent barred from the TeX
  or from source can only judge what you put in front of it. Quote verbatim; a "contradiction"
  arising from your summary is yours, not the document's.
- **Write corrections from source, not from an agent's summary of it**, and re-read each passage
  immediately after editing. That habit catches more damage than any downstream audit.
- **Tell agents an overshoot is as serious as an omission**, and aim verification at the
  neighbourhood of each edit — collateral damage near a fix is about half the findings in a mature
  round.
- **Fix defect classes, not the instances named**, or the same finding returns all run.
- **Generated outputs are inadmissible as evidence** in a no-execution audit: an artifact shows what
  one run did, not what the code does.
- **An empty result is not a negative finding.** Escaping, phrases wrapped across lines, and case
  mismatch all return zero silently. Confirm the search fires against a control that must match
  before concluding a term is absent.

### Running agents under real interruptions

- **A killed agent stopped mid-task, not at a natural end.** Budget limits end agents without
  warning; partial output is untrusted and an unfinished audit is not a clean one. Re-dispatch rather
  than salvage a verdict.
- **Dispatch long rounds in waves**, and tell each agent to run its cheapest decisive checks first
  and state exactly where it stopped. A partial that declares its boundary is usable evidence.
- **Record each dispatch's expected duration**, since agents that cannot write to disk leave a long
  scope indistinguishable from a dead one. **Silence is not death** — probe before concluding.
- **Dispatch first, then write the log entry**, or the record reads as in-flight while nothing runs.

## Required reachability vocabulary

Derive a single explicit vocabulary for execution and selection. Use these terms when they fit the current code:

- `Always attempted`: the runner reaches the statement whenever every preceding statement succeeds.
- `Selected path`: checked-in source and the default launch environment choose this branch for a plain invocation.
- `Alternative path`: the code implements this branch, but a plain invocation does not select it.
- `Source-selected`: a checked-in plan, configuration value, or decision record selects the branch.
- `Launch-selected`: an environment input or command-line input selects the branch.
- `Runtime-selected outcome`: cache validity, data availability, numerical status, or another runtime fact determines the outcome.
- `Available but not called`: the implementation exists, but the current runner has no production path that calls it.
- `Not implemented`: a schema value, route result, placeholder, or artifact record exists without the substantive solver or producer.
- `Separate entrypoint`: another command or package API owns the functionality.

Change or extend this vocabulary only if the current code requires another distinct status. Use one term for each status throughout the document.

Keep these axes separate:

- execution reachability;
- selection basis;
- runtime outcome;
- artifact lifecycle;
- route decision;
- permission or request;
- producer execution;
- path presence;
- validation status;
- cache eligibility.

Never use one axis as evidence for another. A required artifact is not necessarily produced. A route may be requested although no producer runs. A function may be sourced although its body is never called.

## Stage A: Freeze the source snapshot

Create a dated audited-source-universe report.

Record:

- repository commit and branch;
- dirty-worktree state;
- hash and size of the runner;
- hash and size of the TeX;
- hash and size of the existing PDF;
- a sorted hash-and-size manifest for every file in the audited source universe;
- the rule used to derive that boundary;
- excluded generated-state directories;
- the initial metadata-only manifest of protected generated-state roots;
- the exact no-execution rule.

Define an audited source universe that includes the active production graph; package source;
configuration, decision, schema, manifest, cache, lifecycle, and publication authorities; and every
separate repository entrypoint that directly produces, resets, validates, tests, reproduces, or
quality-checks this paper pipeline or implements one of its scientific alternatives. Treat the active
production graph as a labeled subset. Explicitly exclude unrelated package APIs, tests, and general
repository utilities. Hash every file in that finite universe. Trace behavior only through files that
affect a documented route, alternative, boundary, or in-scope separate entrypoint, and record why any
inventoried family is excluded. Record external packages, executables, and data contracts separately;
do not attempt to hash or inventory their implementation graphs.

Record the installed `hetid` version, library path, package-description metadata, and any digest or
build evidence discoverable through metadata-only filesystem inspection or supplied by the enclosing
workflow. Do not run R to obtain it. Version, path, or build metadata alone does not prove equality.
Treat equality as proved only by comparable code-content or semantic digests for the exact installed
namespace selected by the plain invocation and the selected checkout snapshot. Otherwise, parity is
unverified; this is a runtime-parity limitation, not evidence of inequality.

At the end of the workflow, recompute the complete audited-source-universe manifest. If any file in
that universe changed during the audit, invalidate the affected work and restart from this stage.
After two such invalidations caused by external edits, stop with a blocker report rather than
retrying indefinitely.

## Stage B: Discover the production graph

Start with `scripts-paper/run_pipeline.R`. Follow every executable call and every repository-owned
file loaded directly or transitively. Close the graph over behaviorally relevant helpers and source
inputs. Record external package calls at the contract boundary; do not recursively inventory the
implementation of third-party dependencies.

Build an ordered ledger containing:

- runner line or statement;
- source module or function;
- guard or predicate;
- selection basis;
- current status;
- required upstream objects;
- object mutations and global bindings;
- files or artifact groups the stage may write;
- cache effects;
- failure conditions;
- later consumers.

Count and reconcile all direct source calls, source-once calls, conditional source calls, guarded driver bodies, direct function calls, final writes, and cleanup calls. Do not rely on counts from the old TeX.

Trace fail-fast order. Distinguish:

- a stage that is reached;
- a stage that completes;
- a stage whose result passes a numerical gate;
- a later stage that consumes the result;
- a final acceptance or completeness check.

Document hidden top-level work such as directory creation, cleanup, state-record writes, exact readback, sidecar removal, or manifest reconciliation.

## Stage C: Establish the exact plain-invocation route

Define a plain invocation explicitly:

  Rscript scripts-paper/run_pipeline.R

run from the directory required by the current code, with no command-line arguments and every pipeline-specific environment input unset.

From source alone, determine:

- working-directory requirements;
- default environment values;
- checked-in plans and decisions;
- source-selected specifications;
- unconditional stages;
- conditional stages selected by the checked-in state;
- conditional stages not selected;
- default cache request;
- runtime outcomes that source alone cannot determine;
- external-data resolution rules;
- package and executable gates;
- numerical and diagnostic gates;
- publication stages;
- final state writes and cleanup.

Do not say that the pipeline “runs” or “produces” a result when static source establishes only that it attempts the stage. State runtime uncertainty directly.

## Stage D: Inventory every input and option

Search the full production graph for every behavior input. Do not search only the runner.

Account for:

- environment variables;
- command-line arguments;
- tracked plans;
- tracked decision records;
- configuration files;
- external data;
- bundled package data;
- package namespaces and exact-version gates;
- external executables;
- current directory and path assumptions;
- platform and worker detection;
- runtime provenance;
- writable-filesystem requirements;
- preexisting cache artifacts that production code may read;
- source-fixed constants and control objects;
- behavior-changing formal parameters that expose a reachable production path, an implemented
  alternative, or separate public-entrypoint functionality.

For each input or option, record:

- canonical document term;
- literal code name;
- source locator;
- owner;
- allowed values;
- default or checked-in value;
- validation rule;
- selection basis;
- selected plain-invocation value;
- alternative values;
- immediate effect;
- downstream stages affected;
- cache identity or invalidation effect;
- artifact effect;
- failure behavior.

Distinguish these categories:

1. Supported launch input.
2. Tracked behavior input.
3. External prerequisite.
4. Runtime-derived input.
5. Source-fixed control.
6. Function-local implementation parameter.
7. Separate-entrypoint parameter.
8. Test-fixture parameter.
9. Dormant or future-only field.

Do not present a source edit as a supported runtime option. Document source-fixed controls because they affect behavior, but call them source-fixed controls.

Account individually for every behavior-changing formal parameter in production-relevant functions.
Include it in the TeX if it changes a reachable production path, an implemented alternative, or a
separate public entrypoint. The audit may group parameters that are provably presentation-only,
test-only, or function-local plumbing; name each group, state its exclusion rule, and give source
locators.

## Stage E: Inventory every alternative and boundary

Trace behaviorally meaningful functionality that exists outside the selected route:

- nondefault branches;
- alternate data sources and contracts;
- optional estimators;
- diagnostic modes;
- routing outcomes;
- alternate search, solver, moment, or profile code;
- cache modes and fallback paths;
- artifact variants;
- publication variants;
- in-scope package APIs;
- maintenance commands;
- validation commands;
- test orchestrators;
- quality commands;
- inactive production families;
- test-support families;
- source-loaded but uncalled helpers;
- mixed-use files containing both active and inactive functions;
- schema values without implementations;
- planned producers without estimators;
- artifacts reserved for future producers.

For every behaviorally distinct item or family, say how it could be selected or called. If no current
runner input can reach it, say so. Group repetitive helpers only when they share the same status,
selection rule, and downstream consequence; preserve source locators for every grouped member.

Distinguish:

- implemented and selectable by the runner;
- implemented but blocked by the runner;
- implemented only through a separate entrypoint;
- sourced but uncalled;
- used only by tests;
- reserved but unimplemented.

Do not describe an unimplemented producer as optional functionality.

## Stage F: Trace cache, artifact, and validation contracts

Inspect cache behavior statically.

Document:

- requested cache mode;
- allowed modes;
- validation schema;
- all-or-nothing versus partial reuse;
- provenance fields;
- code and presentation manifests;
- semantic identity;
- runtime reuse, fallback-recompute, and explicit recompute outcomes;
- transactional replacement;
- what changes invalidate reuse;
- what changes do not invalidate draws;
- estimator coverage of the cached payload;
- resample-index families and their consumers.

Do not infer whether a cache hit occurs.

Inspect artifact-manifest source without instantiating it.

Reconstruct:

- literal records;
- generated record families;
- IDs, paths, groups, families, and variants;
- producers and consumers;
- lifecycle status;
- conditional ownership;
- publication triples;
- cleanup rules;
- reconciliation scope;
- final completeness checks;
- gaps between manifest requirements and producer guarantees.

Keep artifact lifecycle separate from runner reachability.

Inspect validation and test source statically. State what each check proves and what it does not prove. Do not promote a separate validation command into the production runner.

## Stage G: Trace scientific and numerical logic

For every estimator, diagnostic, bound, bootstrap statistic, and published equation:

- identify the checkout-source-selected production implementation;
- identify the runner or driver that calls it;
- identify source-loaded convenience functions that are not called;
- map the TeX equation to the checkout-source-selected code;
- define every symbol and normalization;
- record the relevant search, grid, fit-evaluation, resampling, and inference controls;
- distinguish local optimization from certification;
- distinguish diagnostic evidence from estimator selection;
- distinguish numerical status from execution reachability.

When two estimators or mechanisms receive identical treatment, describe that treatment with the same term and parallel wording. When their treatment differs, identify the precise code difference and explain its consequence.

Do not assume any historical estimator, grid size, budget, or asymmetry remains current.

## Stage H: Build the coverage matrix and revise the TeX

Wait for all tracing agents. Reconcile their reports against source.

Create a canonical coverage matrix with one row for every behaviorally distinct stage, input, option,
alternative, control family, artifact family, and separate entrypoint. Group repeated family members
only when their status, selection rule, and effects are identical, and list every member and source
locator in the grouped row. Include:

- canonical concept;
- code aliases;
- source authority;
- current reachability;
- selection basis;
- plain-invocation value or path;
- alternatives;
- predicate;
- downstream effect;
- cache effect;
- artifact effect;
- failure behavior;
- TeX location;
- review status.

Revise the TeX so that the document itself contains, at minimum:

- scope and evidence boundary;
- status vocabulary;
- exact plain-invocation route;
- complete ordered runner ledger;
- complete input and prerequisite inventory;
- complete supported-input table;
- complete tracked-decision table;
- complete source-fixed-control map;
- complete alternative-functionality table;
- estimator and diagnostic logic;
- bootstrap and cache behavior;
- publication and artifact lifecycle;
- validation boundaries;
- terminology and code-name crosswalk;
- false-friend distinctions;
- code-to-document index.

Optimize the organization for comparison with code. Preserve precise file and function locators. Use tables when readers must compare repeated fields.

Do not merely append missing facts. Rewrite stale organization when necessary to make current and alternative paths unmistakable.

The orchestrator is the sole canonical TeX editor.

## Stage I: Independent fidelity review

Freeze the revised TeX and record its exact hash, line count, and byte count. Copy it to a path named
with that digest, such as `<current-run-directory>/snapshots/run_pipeline_code-<sha256>.tex`, and verify the copy
byte-for-byte, and give reviewers that snapshot rather than the mutable canonical path. Verify the
snapshot before and after every review.

Give that exact snapshot to independent read-only reviewers. Assign the first three scopes; assign the
fourth separately when capacity permits, otherwise add it to a fresh sequential review:

1. Active-path reviewer.
2. Input-and-options reviewer.
3. Alternative-path and unimplemented-functionality reviewer.
4. Artifact, cache, and validation-boundary reviewer.

Require each reviewer to inspect current source independently. They may not rely only on the coverage matrix.

Preflight the PAL interface and available CLI names. If available, request two narrow,
context-bounded external reviews through `mcp__pal__clink` using the expected current names:

- `cli_name="claude"`, `role="codereviewer"`: active route, inputs, prerequisites, and installed-package boundary.
- `cli_name="claude-fable"`, `role="codereviewer"`: alternatives, cache and artifact claims,
  implementation gaps, and estimator symmetry.

If the interface or one CLI is unavailable, record that fact and assign the same scope to another
independent read-only reviewer. PAL availability alone is not a completion blocker.

Tell both reviewers:

- review only code-to-TeX matching;
- do not review general style except when wording changes factual meaning;
- do not run or source R;
- do not inspect generated outputs, caches, or instantiated manifests;
- do not write any file;
- check only the assigned scope against the frozen source and TeX snapshot;
- return PASS or concrete findings with source and TeX locators.

Save their reviews in the current run directory.

Verify every finding yourself. Accept or reject it with source evidence. If you edit the TeX, freeze a new hash and repeat the necessary exact-snapshot reviews.

Do not call the fidelity stage complete until the accepted hash has independent PASS coverage for
every scope. If a reviewer times out, record the attempt and obtain equivalent exact-snapshot coverage
from another independent reviewer.

## Stage J: Same-concept/same-word compliance pass

After the fidelity snapshot passes, start the terminology assignment. Give its reviewer the accepted
TeX and the current source.

The terminology agent must perform a complete terminology audit, not a quick copyedit.

It must inventory every document concept and every source alias needed to interpret the document,
including each:

- scientific concept;
- estimator;
- diagnostic;
- behaviorally meaningful object;
- route;
- stage;
- cache;
- manifest;
- artifact;
- lifecycle status;
- status word;
- gate;
- check;
- audit;
- budget;
- grid;
- search;
- start pool;
- resample-index family;
- coefficient;
- normalization;
- data series;
- input;
- decision;
- protocol;
- technical phrase used in the TeX;
- acronym.

For each concept, it must produce a ledger containing:

- one canonical document term;
- every code name, alias, object name, and filename needed to map that documented concept to source;
- source locators;
- data-flow or call-graph evidence for equivalence;
- the reason for equivalence when it is not obvious;
- every TeX term currently used;
- required edits.

Apply these rules:

1. The same concept always uses the same document term.
2. Different concepts never share one ambiguous document term.
3. Code aliases do not force prose aliases.
4. When code uses several names for one concept, choose one canonical document term and map every code name to it explicitly.
5. When code uses one word for several concepts, give the concepts distinct document terms and explain the false-friend distinction.
6. Symmetric treatment uses symmetric wording, table labels, and sentence structure.
7. Describe an asymmetry only when source establishes a substantive difference.
8. Name the same property the same way across estimators, stages, tables, captions, equations, and the index.
9. Do not replace precise terms with stylistic synonyms.
10. Verify equivalence from definitions, callers, returned objects, data flow, predicates, and downstream use. Similar spelling is not evidence.

The terminology agent may write only a report and proposed patch or revised copy in its private scratch directory. The orchestrator must verify every proposed equivalence against source and apply accepted changes.

After applying the pass:

- rerun terminology searches over the full TeX;
- check the canonical-term ledger against every occurrence;
- recheck all code-name mappings;
- confirm that no factual claim changed accidentally;
- freeze a new TeX hash.

If the terminology pass changes substantive wording, rerun the relevant source-fidelity checks.

## Stage K: `econ-write` compliance pass

After the terminology pass is complete, start the economics-writing assignment.

Resolve the current `econ-write` skill from the available skill catalog. Require this agent to read
the resolved `SKILL.md` completely. The expected current location is:

  /Users/fduarte/.codex/skills/econ-write/SKILL.md

It must also read every reference that the skill requires for this task, including the McCloskey
word-choice guidance and revision checklist. If the skill is absent or unreadable, record a blocker;
do not silently substitute an invented checklist.

The agent must apply every relevant `econ-write` principle to this technical economics explainer while preserving:

- source fidelity;
- the canonical terminology ledger;
- exact status words;
- equations and definitions;
- code locators;
- current-versus-alternative distinctions;
- documented limitations.

The agent must check:

- reader-first organization;
- important information first;
- concrete statements;
- active voice where the actor matters;
- short and direct sentences;
- one idea per paragraph;
- topic sentences;
- self-contained tables and captions;
- equations introduced in words;
- symbols defined near their first use;
- plain economic intuition before technical detail;
- no throat-clearing;
- no vague claims;
- no ornamental jargon;
- no needless words;
- no elegant variation;
- no unsupported empirical or runtime claim;
- no change to scientific meaning.

Paper-section rules that do not fit a pipeline explainer should not force an artificial research-paper structure. Apply their underlying reader-first principle only where relevant.

The agent must not modify the canonical TeX. It writes a compliance report and proposed edits in its private scratch directory. The orchestrator must verify and apply each accepted edit.

After applying the pass, freeze a new hash and rerun any fidelity or terminology check affected by the edits.

**Close this pass on rule violations, not on the agent running out of suggestions.** A pass enforcing
a stated standard converges; a pass improving taste does not, because a fresh reader can always
tighten another sentence and every fix creates new prose to assess. Requiring "the agent returns
nothing" is unbounded by construction. Classify every finding as exactly one of:

- a **rule violation** — it breaches a stated rule of the named skill, or damages accuracy: a factual
  error, a lost branch predicate or caveat, a lost source locator, an undefined or duplicated term, a
  fragment, or a claim about the code unsupported by source. These block; fix them all.
- a **discretionary improvement** — compliant and accurate, merely tighter or smoother. These do not
  block; record each as declined with a one-line reason.

Report both counts so the classification is auditable, and resolve anything ambiguous as a rule
violation. Ask each agent to return, separately, the passages it deliberately left alone because
tightening them would cost a distinction — that list is how you check the agent understood the brief,
and it prevents a later pass from "fixing" a deliberate choice.

## Stage L: `writing-clearly-and-concisely` compliance pass

After the `econ-write` pass is complete, start the clear-writing assignment.

Resolve the current `writing-clearly-and-concisely` skill from the available skill catalog. Require
this agent to read its `SKILL.md` and every task-required reference completely. The expected current
locations are:

  /Users/fduarte/.codex/skills/writing-clearly-and-concisely/SKILL.md
  /Users/fduarte/.codex/skills/writing-clearly-and-concisely/elements-of-style.md

If either expected file moved, use the catalog-resolved location. If the skill or a required reference
is absent or unreadable, record a blocker. The agent must inspect every sentence, caption, note,
heading, and table entry.

It must check:

- grammar and punctuation;
- active voice;
- positive form;
- definite, specific, concrete language;
- needless words;
- paragraph unity;
- topic sentences;
- related words kept together;
- consistent tense;
- parallel form for parallel concepts;
- emphasis placement;
- sentence fragments;
- comma splices;
- dangling participles;
- ambiguous antecedents;
- excessive parenthetical material;
- vague modifiers;
- repeated loose-sentence patterns;
- overloaded noun strings.

It must preserve the terminology ledger and every factual distinction. Concision may not erase branch predicates, caveats, source locators, failure behavior, or the difference between static and runtime evidence.

The agent must not modify the canonical TeX. It writes a compliance report and proposed edits in its private scratch directory. The orchestrator verifies and applies accepted edits.

After applying the pass, freeze a new hash. Apply the same rule-violation versus discretionary split
defined for the previous pass, and report both counts.

**Both prose passes must be clean on one and the same version.** Passing one on an earlier version
and the other on a later one certifies nothing: each round's fixes invalidate the other's verdict,
and two passes that have each been clean once but never together will alternate indefinitely. Keep
the file frozen until both have reported on it.

**Let each pass own only its own defect class.** Naming, one term serving two concepts, and synonym
drift belong to the terminology pass; missing, invented, or source-contradicted content belongs to
the fidelity review. Clearing another pass's findings inside yours makes every round look as
productive as the last while nothing converges. Carry deferred items in a visible ledger and hand it
to the pass that owns them.

## Stage M: Orchestrator integrity review

The orchestrator must now reread the entire final TeX. Do not rubber-stamp the sequential passes.

Check the final document against the current code and all review reports.

Confirm:

- every subagent finding was resolved or rejected with evidence;
- later prose edits did not undo earlier source fidelity;
- later concision edits did not remove required qualifications;
- terminology remains canonical;
- symmetric concepts still use symmetric language;
- real asymmetries remain visible;
- no section contradicts another section;
- tables agree with prose;
- captions agree with tables;
- equations agree with checkout-source-selected code;
- source locators exist;
- the runner ledger agrees with the route summary;
- the option table agrees with the input inventory;
- artifact counts and formulas reconcile internally;
- lifecycle labels are not used as reachability labels;
- requested cache mode is not confused with a realized cache outcome;
- “attempted,” “completed,” “passed,” and “produced” remain distinct;
- separate entrypoints are not described as runner options;
- uncalled helpers are not described as part of the checkout-source-selected active path;
- unimplemented functionality is not described as optional;
- no historical value survived without current source evidence.

Run a final independent exact-snapshot fidelity regression after all prose passes. At minimum, require
fresh active-path, input/options, and alternative-path reviews of the final hash. Repeat an external
PAL review only for a scope whose previously reviewed text changed materially.

## Stage N: Compile, inspect, and synchronize

Preflight the available LaTeX engine, `latexmk` version and configuration, Poppler tools, `qpdf`, and
fonts. Derive and hash the complete repository- or document-owned static dependency closure of the
accepted TeX, including every local TeX fragment, image, bibliography, style, class, and explicitly
bundled font file. Record explicitly when it is standalone. Record external TeX packages, engines,
and system fonts by resolved path and version; let the installed TeX runtime supply them rather than
copying them. Create a fresh system temporary directory with `mktemp -d`, copy the exact accepted TeX
snapshot and owned dependency closure while preserving their relative layout, and verify every
copied hash. Build there with `latexmk -pdf`; keep `-pdf` explicit even if local configuration also
selects PDF. Do not build inside the repository. Retain only the audit log and necessary renders in
the current run directory.

**Build to a fixed point before judging the log.** The directory is fresh by construction, so the
first pass has no auxiliary file and *will* report unresolved references and "rerun to get
cross-references right" — that is the absence of a prior pass, not a defect in the document. Run the
build again until the log stops asking, and adjudicate only the final pass. Judging the first pass
turns a clean document into dozens of phantom warnings and invites a "fix" for a problem that does
not exist.

Do not invoke R during compilation.

Require:

- successful LaTeX exit status;
- no undefined control sequence;
- no unresolved reference or citation;
- no multiply defined label;
- no fatal package error;
- no unadjudicated LaTeX or package warning;
- no visible or materially risky overfull box;
- every remaining underfull-box warning classified by page and location and confirmed harmless by
  visual inspection;
- balanced document environments;
- every `\ref` target defined;
- no unresolved-reference marker in extracted text or rendered pages;
- stable page count across a final no-edit rebuild;
- `qpdf --check` success;
- all used fonts embedded, with expected families and encodings present and no unexplained
  substitution, as confirmed with `pdffonts`;
- every link annotation enumerated by page and subtype, every internal destination resolved, and
  every external URI nonempty and consistent with the TeX source.

Render every page with Poppler or an equivalent PDF renderer.

Inspect:

- every page through contact sheets;
- every dense or changed page at full resolution;
- landscape transitions;
- long tables across page breaks;
- headers and footers;
- code paths and monospaced text;
- equations;
- hyperlinks;
- table rules;
- captions;
- page numbering;
- blank-page behavior;
- margins;
- clipping;
- overlap;
- font substitution;
- illegible text.

Fix every material visual defect caused by the TeX and rebuild. Record harmless engine warnings with
their page, location, and visual evidence. A successful compiler exit does not prove visual
correctness.

When the build passes:

1. Copy the accepted PDF to `docs/run_pipeline_code.pdf`.
2. Verify that the canonical PDF is byte-identical to the accepted build.
3. Record final TeX and PDF hashes, sizes, and page count.
4. Recompute the audited-source-universe manifest and confirm that it did not change.
5. Recompute the protected generated-state metadata manifest. Explain every difference and confirm
   that no task command caused it; do not inspect file contents.
6. Attest from the commands launched by this workflow that it did not run R or a pipeline command,
   reset state, read cache or output contents, mutate pipeline state, or instantiate a manifest.
7. Compare final Git status with the complete initial status. Attribute every new or changed path and
   confirm that this workflow changed only the two canonical targets and created only run-directory
   records. If an enclosing Stage O runs the sibling math-document task concurrently, treat only its
   assigned TeX and prompt-authorized records as permitted concurrent external changes; do not inspect
   or modify that sibling's target.
8. After retaining the accepted evidence and synchronizing the PDF, remove only the exact system
   temporary directory created by this workflow. First verify that it is outside the repository and
   matches the recorded `mktemp` path.
9. Finalize all reports, logs, and retained renders in the current run directory.
10. As the last run-directory write, create a sorted path, size, and hash manifest for every other
    file there. List the manifest's own path as the sole deliberate self-reference exclusion, then
    verify that no later run-directory write occurs.

## Final acceptance criteria

Do not declare completion unless all conditions hold:

- The checkout-source plain-invocation route is explicit, and installed-package parity is either
  proved or clearly marked unverified.
- Every top-level runner action and guarded driver body is accounted for.
- Every user-controlled input is documented with its default, alternatives, validation, and downstream effects.
- Every tracked decision and source-fixed control family is documented.
- Every prerequisite is distinguished from an option.
- Every runtime-selected outcome is distinguished from a source-selected path.
- Every implemented alternative is documented and marked as unselected.
- Every source-loaded but uncalled capability is identified.
- Every substantive implementation gap is marked `Not implemented`.
- Every separate entrypoint is clearly separated from the runner.
- Cache and artifact claims match current source.
- The document never uses generated state as static evidence.
- Same concepts use the same terms everywhere.
- Code aliases map explicitly to canonical document terms.
- Different concepts with similar names remain distinct.
- The `econ-write` pass is complete.
- The `writing-clearly-and-concisely` pass is complete.
- The orchestrator has reconciled every pass critically.
- The final exact snapshot passes source-fidelity review.
- The TeX passes the adjudicated compilation gate.
- Every PDF page has been inspected.
- The canonical PDF matches the accepted build.
- The complete audited-source-universe manifest is unchanged.
- The protected generated-state metadata comparison has no unexplained task-attributable difference.
- The current-run-directory manifest accounts for every working record.
- The task command audit and baseline-to-final tracked-status comparison attribute no production-code
  or pipeline-state change to this workflow.
- No commit or push occurred.

**An honest partial outranks a false pass, and will be treated that way.** If a pass will not close,
stop and hand over: name what is outstanding, by section and class, in a form the next editor can act
on without re-deriving it, and say which passes are certified and against which version. Do not
declare completion because the remaining findings feel small, and do not begin an apply pass you
cannot finish.

**State coverage precisely rather than letting "all passes clean" imply more than it does.** Separate
what was read line by line on the delivered version, what was verified mechanically across the whole
file, and what is inherited from a read of an earlier version. Re-test every mechanical zero against
a control pattern that must match and report the control's count beside the zero, since an unfired
search and a clean document look identical. If no single agent verified every source claim in one
pass, say so — that is a normal outcome at this size and a limitation to record, not conceal.

## Final response

Lead with the outcome.

Report:

- the canonical TeX and PDF paths;
- final hashes, sizes, and page count;
- the frozen source commit and runner hash;
- the exact review passes completed;
- the decisive compilation and `qpdf` output;
- the number of rendered pages inspected;
- the installed-package parity evidence or explicit unverified-runtime caveat;
- a procedural attestation that this workflow launched no R or pipeline command;
- a procedural attestation that no task command read output or cache contents, wrote pipeline state,
  or instantiated a manifest;
- the baseline-to-final Git-status comparison and every task-attributable changed or new path;
- the protected-state metadata comparison and current-run-directory manifest;
- confirmation that no commit or push occurred;
- any unresolved blocker or validation limitation.

Link the main audit and review reports. Keep the final response concise, but do not hide caveats.
