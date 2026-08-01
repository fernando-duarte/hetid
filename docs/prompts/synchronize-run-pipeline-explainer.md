# Orchestrator prompt: synchronize the pipeline explainer

You are the primary orchestrator for a complete source-to-document synchronization.

Repository:
  /Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid

Production entrypoint:
  /Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/scripts-paper/run_pipeline.R

Canonical TeX document:
  /Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/docs/run_pipeline_code.tex

Canonical compiled PDF:
  /Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/docs/run_pipeline_code.pdf

Your objective is to make the TeX document fully faithful to the current code. Assume that both the code and document have changed since any prior audit. Assume that the latest document revision may have used weak standards. Trust neither the document nor old reports. Derive every claim from the current source.

The finished document must tell a reader:

1. The exact path that a plain invocation of `Rscript scripts-paper/run_pipeline.R` takes.
2. Every input, option, prerequisite, decision, constant, and runtime condition that can affect that path.
3. Every alternative path or capability implemented in the code but not selected by a plain invocation.
4. Every reserved or described path that lacks a substantive implementation.
5. Every separate maintenance, validation, test, or package entrypoint that the runner does not call.
6. The exact downstream effects of each choice on stages, estimators, diagnostics, caches, artifacts, publication, and failure behavior.
7. Which statements follow from static source and which outcomes require runtime evidence.

A reader must never have to infer whether the runner executes something. Mark current execution, alternative execution, unavailable functionality, uncalled functionality, and separate entrypoints explicitly.

## Governing rules

Run this workflow from beginning to end without human involvement.

- Never ask the user a question.
- Never request approval, confirmation, or a plan decision.
- Do not use a human-prompt or plan-approval mechanism.
- If a skill contains an approval gate, continue under this task’s authorization.
- Use your best judgment when several defensible choices exist.
- When uncertain, ask another agent or use PAL `clink` with Claude or Fable. Treat external reviews as advice and verify every suggestion against source.
- Stop only for a hard external blocker after exhausting safe alternatives. Record the blocker and completed work under `docs/`; do not wait for a human.
- Do not commit, push, open a pull request, or modify remote state.

Evidence controls every conclusion:

- Do not claim that a stage is covered until you have inspected its source.
- Do not claim that a check passed until you have run it and read its output.
- Quote decisive command output in the final audit.
- Do not hide, suppress, disable, or weaken a failed check.
- Fix the cause of a failure when the fix lies within scope.
- Use bounded retries. Change the approach or fix the cause before retrying.
- If a check remains impossible, record the command, error, attempts, and remaining consequence in a Markdown report under `docs/`. Continue independent work and report the incomplete gate at the end.

The current code is authoritative:

- Read the executable statements, predicates, call graph, configuration, decision records, and manifest-building source.
- Treat comments, tests, README files, and the old TeX as supporting evidence. When they disagree with executable code, describe the executable behavior.
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
- initialize, reset, populate, clean, migrate, or inspect pipeline outputs;
- read cache contents or use cache state as behavioral evidence;
- instantiate the artifact manifest;
- inspect generated pipeline artifacts as evidence;
- run `reset_pipeline_state.R`;
- force a bootstrap rerun or reduced draft run;
- change an environment variable to probe production behavior.

The output tree, caches, generated manifests, and route-state files may be empty by design. Preserve that state.

Static inspection is allowed. Shell text tools, parsers, hashes, and source searches are allowed. You may run a small standalone snippet in a temporary directory outside the repository to test syntax or a self-contained hypothesis. Such a snippet must not import, source, or evaluate repository code and must not touch repository outputs, caches, manifests, or state.

Compiling the standalone TeX document is allowed and required. LaTeX build files and page renders must remain under `docs/`.

## Repository and editing boundaries

Before doing task work:

1. Read every applicable `AGENTS.md`.
2. Read `CLAUDE.md` and any project guidance it names.
3. Inspect the current Git status without modifying it.
4. Preserve all unrelated user changes.
5. Record the current commit, branch, TeX hash and size, PDF hash and size if present, runner hash, and hashes of the main configuration and decision authorities.
6. Obtain report timestamps with:
   `date "+%Y-%m-%d %H:%M %Z"`

Only the primary orchestrator may modify:

  docs/run_pipeline_code.tex
  docs/run_pipeline_code.pdf

Subagents and external reviewers must never modify those files.

All plans, audits, matrices, reports, proposed patches, logs, builds, renders, and other working documents must remain under `docs/`.

Use a run directory such as:

  docs/RUN/run-pipeline-sync-YYYYMMDD-HHMMSS/

Subagents may write only inside:

  docs/RUN/scratch/agents/<agent-id>/

Subagents have read-only access to the rest of the repository. They may create proposed patches or revised copies inside their private scratch directories. The orchestrator must inspect those proposals, verify them against source, and apply accepted changes to the canonical TeX.

Make text edits in place with the harness’s own file-editing tools. Preserve unrelated content and the user’s existing work. Do not use destructive Git commands or broad deletion commands, and do not rewrite a file wholesale through shell redirection when an in-place edit will do.

## Planning and delegation

Before editing, assess whether a written plan and multi-agent execution are required. For a full pipeline synchronization, presume that both are required unless the repository has become trivial.

If `multistep-plan` and `multistep-do` exist, use them. Otherwise, use the closest available planning and subagent-execution skills. Do not pause for plan approval.

Write the plan under `docs/`. Include:

- the source boundary;
- the no-execution boundary;
- agent assignments;
- ordering barriers;
- evidence required at each barrier;
- files each agent may inspect;
- files each agent may write;
- acceptance tests;
- retry and blocker rules.

Use as many agents as the environment safely supports. Fill available concurrency slots with independent work. Keep tightly coupled synthesis in the orchestrator.

At minimum, assign separate read-only agents to:

1. Trace the exact current runner path and every stop condition.
2. Inventory inputs, options, prerequisites, decisions, and source-fixed controls.
3. Trace alternative, dormant, unimplemented, maintenance, validation, test, cache, and artifact functionality.

Agents may subdivide their assignments. Require exact source locators and evidence in every report.

After the main TeX synchronization, use three distinct new agents, in order:

1. A same-concept/same-word compliance agent.
2. An `econ-write` compliance agent.
3. A `writing-clearly-and-concisely` compliance agent.

Do not reuse one agent for these three passes. Each pass begins only after the preceding pass has been incorporated and verified.

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

Create a dated source-boundary report.

Record:

- repository commit and branch;
- dirty-worktree state;
- hash and size of the runner;
- hash and size of the TeX;
- hash and size of the existing PDF;
- hashes of every principal configuration, decision, manifest, lifecycle, cache, and reporting authority;
- the production-source boundary;
- excluded generated-state directories;
- the exact no-execution rule.

At the end of the workflow, recompute the source hashes. If production source changed during the audit, invalidate the audit and restart from this stage.

## Stage B: Discover the production graph

Start with `scripts-paper/run_pipeline.R`. Follow every executable call and every file loaded directly or transitively.

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
- formal parameters that expose alternative package or separate-entrypoint functionality.

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

Account for every formal parameter in production-relevant functions. Include it in the TeX if it changes a reachable production path, an implemented alternative, or a separate public entrypoint. Otherwise, account for its exclusion in the audit.

## Stage E: Inventory every alternative and boundary

Trace functionality that exists outside the selected route:

- nondefault branches;
- alternate data sources and contracts;
- optional estimators;
- diagnostic modes;
- routing outcomes;
- alternate search, solver, moment, or profile code;
- cache modes and fallback paths;
- artifact variants;
- publication variants;
- package APIs;
- maintenance commands;
- validation commands;
- test orchestrators;
- quality commands;
- inactive files;
- test-support files;
- source-loaded but uncalled helpers;
- mixed-use files containing both active and inactive functions;
- schema values without implementations;
- planned producers without estimators;
- artifacts reserved for future producers.

For every item, say how it could be selected or called. If no current runner input can reach it, say so.

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

- identify the active production implementation;
- identify the runner or driver that calls it;
- identify source-loaded convenience functions that are not called;
- map the TeX equation to the active code;
- define every symbol and normalization;
- record the relevant search, grid, fit-evaluation, resampling, and inference controls;
- distinguish local optimization from certification;
- distinguish diagnostic evidence from estimator selection;
- distinguish numerical status from execution reachability.

When two estimators or mechanisms receive identical treatment, describe that treatment with the same term and parallel wording. When their treatment differs, identify the precise code difference and explain its consequence.

Do not assume any historical estimator, grid size, budget, or asymmetry remains current.

## Stage H: Build the coverage matrix and revise the TeX

Wait for all tracing agents. Reconcile their reports against source.

Create a canonical coverage matrix with one row for every stage, input, option, alternative, control family, artifact family, and separate entrypoint. Include:

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

Freeze the revised TeX and record its exact hash, line count, and byte count.

Give that exact snapshot to independent read-only reviewers:

1. Active-path reviewer.
2. Input-and-options reviewer.
3. Alternative-path and unimplemented-functionality reviewer.
4. Artifact, cache, and validation-boundary reviewer, if agent capacity allows.

Require each reviewer to inspect current source independently. They may not rely only on the coverage matrix.

Also request two narrow external reviews through PAL `mcp__pal__clink`:

- `cli_name="claude"`, `role="codereviewer"`
- `cli_name="claude-fable"`, `role="codereviewer"`

Tell both reviewers:

- review only code-to-TeX matching;
- do not review general style except when wording changes factual meaning;
- do not run or source R;
- do not inspect generated outputs, caches, or instantiated manifests;
- do not write any file;
- check the exact selected path, all inputs and options, all alternatives, reachability labels, cache claims, artifact claims, and estimator symmetry;
- return PASS or concrete findings with source and TeX locators.

Save their reviews under `docs/`.

Verify every finding yourself. Accept or reject it with source evidence. If you edit the TeX, freeze a new hash and repeat the necessary exact-snapshot reviews.

Do not call the fidelity stage complete until the accepted hash has independent PASS coverage. If a reviewer times out, record the attempt and obtain equivalent exact-hash coverage from another independent reviewer.

## Stage J: Same-concept/same-word compliance pass

After the fidelity snapshot passes, start a distinct new terminology agent. Give it the accepted TeX and the current source.

The terminology agent must perform a complete terminology audit, not a quick copyedit.

It must inventory every:

- scientific concept;
- estimator;
- diagnostic;
- object;
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
- technical phrase;
- acronym.

For each concept, it must produce a ledger containing:

- one canonical document term;
- every code name, alias, comment name, object name, and filename for that concept;
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

After the terminology pass is complete, start another distinct new agent.

Require this agent to read completely:

  /Users/fduarte/.codex/skills/econ-write/SKILL.md

It must also read every reference that the skill requires for this task, including the McCloskey word-choice guidance and revision checklist.

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

## Stage L: `writing-clearly-and-concisely` compliance pass

After the `econ-write` pass is complete, start a third distinct new agent.

Require this agent to read completely:

  /Users/fduarte/.codex/skills/writing-clearly-and-concisely/SKILL.md
  /Users/fduarte/.codex/skills/writing-clearly-and-concisely/elements-of-style.md

The agent must inspect every sentence, caption, note, heading, and table entry.

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

After applying the pass, freeze a new hash.

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
- equations agree with active code;
- source locators exist;
- the runner ledger agrees with the route summary;
- the option table agrees with the input inventory;
- artifact counts and formulas reconcile internally;
- lifecycle labels are not used as reachability labels;
- requested cache mode is not confused with a realized cache outcome;
- “attempted,” “completed,” “passed,” and “produced” remain distinct;
- separate entrypoints are not described as runner options;
- uncalled helpers are not described as active code;
- unimplemented functionality is not described as optional;
- no historical value survived without current source evidence.

Run a final independent exact-hash fidelity regression after all prose passes. At minimum, require fresh active-path, input/options, and alternative-path reviews of the final hash. Use PAL Claude and Fable again if the final TeX differs materially from the snapshot they reviewed.

## Stage N: Compile, inspect, and synchronize

Build with `latexmk`. The document is plain `article` with `inputenc`, and the repository ships no `latexmkrc`, so the default pdfLaTeX route applies; confirm that from the current preamble rather than assuming it. Build in a fresh directory under the run’s `docs/RUN/` directory.

Do not invoke R during compilation.

Require:

- successful LaTeX exit status;
- no undefined control sequence;
- no unresolved reference or citation;
- no multiply defined label;
- no fatal package error;
- no overfull or underfull box left unexplained;
- balanced document environments;
- every `\ref` target defined;
- no visible `??` marker;
- stable page count after the final build;
- `qpdf --check` success.

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

Fix every visual defect and rebuild. A successful compiler exit does not prove visual correctness.

When the build passes:

1. Copy the accepted PDF to `docs/run_pipeline_code.pdf`.
2. Verify that the canonical PDF is byte-identical to the accepted build.
3. Record final TeX and PDF hashes, sizes, and page count.
4. Recompute the frozen production-source hashes.
5. Confirm that production source did not change.
6. Confirm that no R process, pipeline command, reset, cache mutation, output mutation, or manifest instantiation occurred.
7. Confirm that only permitted files under `docs/` changed.
8. Leave all reports and renders under `docs/`.

## Final acceptance criteria

Do not declare completion unless all conditions hold:

- The exact plain-invocation route is explicit.
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
- The final exact hash passes source-fidelity review.
- The TeX compiles cleanly.
- Every PDF page has been inspected.
- The canonical PDF matches the accepted build.
- No production code or pipeline state was modified.
- No commit or push occurred.

## Final response

Lead with the outcome.

Report:

- the canonical TeX and PDF paths;
- final hashes, sizes, and page count;
- the frozen source commit and runner hash;
- the exact review passes completed;
- the decisive compilation and `qpdf` output;
- the number of rendered pages inspected;
- confirmation that no R or pipeline code ran;
- confirmation that outputs, caches, and manifests remained untouched;
- confirmation that no commit or push occurred;
- any unresolved blocker or validation limitation.

Link the main audit and review reports. Keep the final response concise, but do not hide caveats.
