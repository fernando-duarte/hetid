# Orchestrator prompt: synchronize the mathematical pipeline manual

## Required shared contract

Read `docs/prompts/shared-workflow-contracts.md` completely before acting. This prompt extends that
contract. It does not restate the shared role, autonomy, history-independence, ownership, worker,
concurrency, evidence, snapshot, retry, or completion rules.

You are the orchestrator of this workflow and the sole writer of its canonical TeX and PDF targets.
An enclosing workflow is the caller defined by the shared contract.

Starting with no prior knowledge of this repository, bring the following TeX document into complete
scientific and mathematical parity with the current checkout source.

Select the repository root under the shared contract. The standalone fallback is
`/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid`. The required repository
marker and principal paths are:

- pipeline entrypoint: `scripts-paper/run_pipeline.R`;
- target TeX document: `docs/run_pipeline_math.tex`; and
- target PDF: `docs/run_pipeline_math.pdf`.

Verify that the selected root contains the entrypoint and target TeX before proceeding. Do not
inspect or modify another checkout, including the standalone fallback when an enclosing workflow has
selected a different worktree.

Apply the shared history-independence rule. Discover every numerical value, branch, input identity,
equation, capability, and conclusion from the selected source snapshot.

The final result must be a self-contained mathematical reproduction manual. A first-year economics
PhD student who has never seen the repository must be able to reproduce the scientific calculations
in another programming language from the TeX document and the explicitly identified immutable input
artifacts. The document need not reproduce repository-specific byte-integrity, cache-storage,
file-replacement, or publication machinery. If a required input lacks either a stable external
identity or an access route, record that as a blocker to repository-independent numerical
reproduction; do not disguise a repo-local snapshot as a self-contained input.

## Authority and evidence

The current checkout production source is the authority for intended scientific behavior. A plain
pipeline invocation calls the installed `hetid` namespace, so equality between that namespace and
the checkout package source is a separate precondition for exact runtime parity.

1. Read every applicable repository instruction before acting, including `AGENTS.md`, `CLAUDE.md`,
   and any more specific instruction file.
2. Begin at `scripts-paper/run_pipeline.R`. Define a finite audited source universe containing its
   transitive repository-owned graph, package-owned scientific procedures, behavior authorities, and
   separate scientific entrypoints relevant to Part II. Trace behavior through every file that can
   affect a documented calculation, alternative, refusal, or limitation. Record external packages,
   executables, and data at their contract boundaries rather than recursively inventorying
   third-party implementations.
3. Trace the effective configuration, defaults, input-selection rules, recorded decisions, branch
   gates, mathematical procedures, numerical controls, lifecycle rules, publication stages, and
   completion checks.
4. Treat `docs/run_pipeline_math.tex` as an untrusted draft. Revalidate every substantive
   sentence, formula, number, status, and capability.
   **Recompute every derived count from source rather than adjusting the one already written.** The
   document states totals that depend on the registries — inventory sizes, per-group counts,
   how many items are required versus conditional, how many of a kind are published. These move
   together, so a hand-adjusted number can contradict the decomposition printed beside it. Derive
   them by statically parsing and reconstructing the authoritative declarations without sourcing,
   evaluating, or instantiating project R code. Check that the parts sum to the whole. Read every
   numeric match in context before editing it; a number may be a source locator, maturity, parameter,
   or count.
5. You may inspect a stable `docs/run_pipeline_code.tex` only as a nonauthoritative source of
   questions. During concurrent Stage O work, skip that optional read rather than inspect the
   sibling target while it is being edited. Do not borrow its prose, structure, or conclusions. Do
   not edit it.
6. Treat comments, tests, other reports, memories, and worker conclusions as leads rather than
   authority. Generated artifacts remain uninspected and cannot supply leads. Checkout executable
   source wins whenever supporting materials disagree. Tests may clarify edge behavior but cannot
   override current production logic.
7. Record a sorted hash-and-size manifest for every file in the audited source universe at the
   beginning. Recheck the snapshot before final acceptance. If relevant source changes during the
   work, retrace the affected behavior and repeat every downstream audit. After two invalidations
   caused by external edits, stop with a blocker report rather than retrying indefinitely.
8. Never assert completion without command output or source evidence. Record failed checks and their
   corrected reruns.

Record the installed `hetid` version, library path, package-description metadata, and any digest or
build evidence discoverable through metadata-only filesystem inspection or supplied by the enclosing
workflow. Do not run R to obtain it. Version, path, or package-description metadata alone does not
prove equality. Treat equality as proved only by comparable content-level build provenance or
semantic digests covering the executable installed namespace selected by the plain invocation and
the selected checkout snapshot. Otherwise, certify checkout-source parity, label installed-package
parity unverified, and explain that a runtime invocation may differ. This disclosed limitation does
not block checkout-source mathematical certification, but it forbids a claim of exact runtime
parity for the installed namespace. Do not load project logic merely to remove the limitation.

When a genuine ambiguity remains after direct inspection, obtain an independent second opinion from
an available worker or PAL clink reviewer. Apply the shared external-review and evidence rules.

## Absolute execution boundary

Trace the scientific pipeline statically. Do not run it.

You must not:

- execute or source the pipeline entry point;
- execute project analysis modules to discover their behavior;
- initialize, reset, populate, delete, or alter scientific outputs;
- initialize, invalidate, replace, or alter caches;
- initialize or alter manifest instances;
- fetch or refresh scientific data;
- inspect generated-output, cache, route-state, or manifest-instance contents or use them as
  behavioral evidence;
- infer behavior from prior numerical results; or
- stop or interfere with an independently running process.

You may inspect source declarations, schemas, configuration, manifest definitions, tests, input
contracts, and filesystem metadata without mutation. Do not load an installed or checkout project
namespace.

Small isolated calculations are allowed only when needed to verify algebra, dimensions, numerical
syntax, or a source-reading hypothesis. Run them in a fresh system temporary directory outside the
repository. They must not import, source, or execute project logic, fetch data, or create scientific
outputs, and they must not invoke R.

If another process happens to be running the pipeline, do not inspect or disturb its products. The
final report must make no claim about what that independent process did; attest only to commands
launched by this workflow.

## File boundary

Outside the current run directory, the only canonical paths you may create or modify are the target
document and its companion render:

`docs/run_pipeline_math.tex`
`docs/run_pipeline_math.pdf`

Before editing, discover whether each target exists, is tracked, or is ignored; do not rely on this
prompt for those mutable facts. Replace or create the PDF only with the accepted build of the
certified TeX version, at the end of the workflow.

If the caller supplies an enclosing records root, create this prompt's unique workflow record
at:

`<enclosing-records-root>/stage-o-math/YYYYMMDD-HHMMSS-<unique-suffix>/`

Otherwise, use the standalone location:

`docs/RUN/run_pipeline_math/YYYYMMDD-HHMMSS-<unique-suffix>/`

In this prompt, "current run directory" means this unique workflow record.

You may create new plans, source maps, terminology ledgers, audit reports, and validation logs only
inside the selected workflow record.

Do not overwrite a record from another workflow. Within the current workflow record, update only
the records this workflow owns; give replacement or retry reports unique names.

Additional rules:

- Only you, the orchestrator of this prompt, may edit the target TeX file — never a worker, and
  never an enclosing workflow while you are still running.
- Every worker is read-only with respect to the target TeX and repository, except for its authorized
  private scratch directory.
- A worker that needs a private working file may write only at
  `<current-run-directory>/scratch/agents/<agent-id>/`.
- The orchestrator alone writes canonical plans, reports, ledgers, and logs.
- Use patch-based tools for text edits. The final accepted binary PDF may be copied to its canonical
  path and must then be verified byte-for-byte. Do not overwrite text files through shell
  redirection or bulk-writing commands.
- Preserve unrelated worktree changes.
- Do not edit scientific source, configuration, tests, inputs, repository metadata, or any other
  documentation. The canonical PDF and files inside the current run directory are the only artifact
  exceptions.
- Do not commit, push, publish, revert, reset, or rewrite history. Do not delete repository files or
  directories. Ordinary text removal within an authorized TeX edit is allowed. Cleanup may remove
  only the exact external temporary directory created by this workflow after its retained evidence
  and accepted PDF have been copied out.
- Keep LaTeX sidecar and intermediate build files outside the repository. Only the final accepted
  PDF returns, to its canonical path `docs/run_pipeline_math.pdf`.
- Obtain report timestamps with `date "+%Y-%m-%d %H:%M %Z"`. Never guess the time.

Before editing, write a concrete execution plan in the current workflow record. This
prompt already defines the execution workflow; do not invoke an additional general planning wrapper.
Proceed without requesting approval.

## Non-negotiable TeX content contract

The TeX document must contain only clear prose, mathematical notation, equations, compact
mathematical tables when necessary, and carefully structured lists.

It must contain no computational code or code-like pseudocode. Ordinary-language ordered algorithm
steps are allowed when they are the clearest way to specify a mathematical procedure.

It must not quote, reproduce, cite, link to, name, or allude to:

- internal source files or directories;
- functions, objects, arguments, configuration keys, or environment-variable names;
- programming commands or programming-language syntax;
- implementation packages or numerical libraries;
- internal architecture or source-loading mechanisms;
- cache filenames or internal state filenames;
- source comments or source line numbers; or
- any other representation of the underlying implementation.

Do not write phrases such as “the code does,” “the script calls,” or “the implementation
uses.”

Names of external scientific data, established scholarly procedures, and required input or output
artifacts may appear only when an outside reader needs them to reproduce the scientific input or
artifact contract. Never use such names as disguised implementation references.

The TeX document must contain no literal hash, fingerprint, checksum, cryptographic-digest
algorithm, hexadecimal verification value, internal integrity field, or construction of such a
value. It may state that preserved scientific inputs or reusable numerical results are accepted
only when their required identities and provenance agree with the current request, when
that condition changes a scientifically relevant route or result. Keep byte-level comparison,
storage, transactional replacement, and internal integrity representation outside the TeX. Source
audit reports may record exact snapshot evidence.

Identify every scientific input by a stable external name, release or vintage, units, frequency,
sample coverage, transformations, an immutable artifact identity, and an access route that resolves
to those exact observations. Both identity and access are required for exact numerical
reproducibility. If current source supplies only a mutable or repo-local file, state the required
supplied-input contract in the TeX and mark exact repository-independent numerical certification
incomplete.

When the pipeline selects among multiple input releases, derive a scientifically meaningful name
for each release from current evidence and state the complete precedence and selection conditions.
Do not use vague labels such as “a separately stored release” or “the preserved
alternative.”

The document must explain the pipeline through mathematics and ordinary language:

- Prefer equations when they describe a procedure more clearly than prose.
- Introduce every equation in words.
- Explain every object in an equation immediately before or after it appears.
- Number only equations cited later.
- If a genuine multistep algorithm cannot be expressed clearly with equations and prose, use an
  ordered list.
- Make every list item and subitem atomic.
- Never substitute vague prose for a difficult formula, inequality, search rule, stopping condition,
  or branch condition.

Write for a first-year economics PhD student who knows basic regression, probability, and matrix
algebra but none of the project-specific methods.

Define every technical term or abbreviation at first use. Prefer plain words. If a technical term
adds precision, define it in the sentence that introduces it.

Define every symbol at first use, either immediately before or immediately after it appears. State
all properties needed for reproduction, including, when relevant:

- whether it is a scalar, vector, matrix, set, function, statistic, parameter, random variable, or
  index;
- its dimension;
- its domain and range;
- its units;
- its indexing set;
- its sign, finiteness, rank, or definiteness restrictions; and
- the sample or time period to which it belongs.

Do not assume that a symbol’s dimension or role is obvious from context.

## Return, frequency, and annualization conventions

The document must define every return convention explicitly from current source evidence.

At the first use of each return concept, state whether it is:

- a simple net return, such as \(r_t=(P_t+D_t-P_{t-1})/P_{t-1}\);
- a gross return, such as \(R_t=1+r_t\);
- a log return, such as \(g_t=\log R_t\);
- a price return or total return;
- a nominal or real return; and
- an excess return or an unadjusted return.

Use only distinctions that apply to the current pipeline. Do not infer a convention from a series
name, label, common practice, or prior documentation. Verify it from the current source and the
underlying input definition.

For every return, rate, growth measure, volatility measure, variance, yield, or other variable whose
reported frequency differs from its observation frequency, state:

1. the original observation frequency;
2. the reported frequency;
3. whether the operation annualizes or converts an annual quantity to a shorter period;
4. the exact transformation; and
5. the resulting units.

Write the transformation mathematically. Distinguish, when applicable, among:

\[
r_t^{(a)}=m r_t^{(p)},
\]

for arithmetic annualization of a periodic net return;

\[
r_t^{(a)}=(1+r_t^{(p)})^m-1,
\]

for geometric compounding;

\[
g_t^{(a)}=m g_t^{(p)},
\]

for annualization of a periodic log return;

\[
r_t^{(p)}=\frac{r_t^{(a)}}{m},
\]

for arithmetic conversion of an annual rate to a periodic rate; and

\[
r_t^{(p)}=(1+r_t^{(a)})^{1/m}-1,
\]

for geometric conversion of an annual return to a periodic return. Here \(m\) is the positive
integer number of observation periods per year, \(r_t^{(p)}\) is the periodic net return,
\(r_t^{(a)}\) is the annual net return, and \(g_t^{(p)}\) and \(g_t^{(a)}\) are the corresponding
periodic and annual log returns.

These formulas illustrate conventions that may occur; they are not assumptions about the pipeline.
Include only the formula that the current source applies. Derive and define any different
transformation exactly.

Apply the same rule to other frequency conversions. If the pipeline annualizes a standard deviation
by multiplying it by \(\sqrt m\), annualizes a variance by multiplying it by \(m\), or aggregates
growth through geometric compounding, state the exact formula and define every object.

If one convention applies globally to an object throughout the document, define it once at the
object’s first use and state that the convention applies throughout. If conventions differ across
variables, samples, transformations, or document sections, define each convention when it first
applies. Never let one definition appear to govern an object calculated under a different
convention.

## Required document partition

The document must have two conceptually distinct parts.

### Part I: exact source-selected reproduction procedure

Part I must describe only the procedure selected by the current checkout configuration, default
launch inputs, and recorded choices. Define that launch as the pipeline entrypoint invoked from its
source-required working directory, with no command-line arguments and every pipeline-specific
environment input unset. Do not claim that a runtime-selected outcome was realized.

It must read as one ordered scientific procedure from inputs through transformations, estimation,
diagnostics, inference, and the scientific output and acceptance contract. Conditional runtime arms
may branch within that ordered procedure.

Part I must include every current item needed for independent reproduction:

- input identities and units;
- sample construction and calendar alignment;
- transformations;
- mathematical models;
- estimators and objectives;
- constraints and identified sets;
- diagnostics;
- numerical safeguards;
- search order and starting values;
- stopping and acceptance rules;
- random-number and resampling procedures;
- inference procedures;
- every conditional arm inherent to the selected procedure, together with the runtime fact and exact
  rule that choose among its outcomes;
- any preservation or reuse rule that can change the selected scientific inputs, calculations, or
  interpretation, together with its eligibility conditions and scientific fallback;
- scientific output consequences; and
- final scientific validation and acceptance conditions.

Part I must not offer or compare a noncurrent source-selected alternative, such as:

- differently configured inputs, but not runtime-conditional input arms inherent to the selected
  procedure;
- alternative estimators;
- optional diagnostics;
- different numerical settings;
- dormant branches;
- rejected routes;
- hypothetical decisions;
- unavailable extensions; or
- any other route that requires a different launch input, tracked choice, or source-fixed control.

Conditional solver rungs, input-availability rules, numerical statuses, package-availability gates,
and failure arms that belong to the selected scientific procedure remain in Part I. Include a reuse
eligibility outcome only when it can change a scientific input, calculation, or interpretation; keep
outcome-equivalent cache validation and storage mechanics in the audit. Describe included arms
conditionally and do not infer which occurred. Dormant or differently configured alternatives do not
belong in Part I. Adjudicate meaning rather than banning modal words by spelling.

### Part II: complete modification and capability catalogue

After Part I is complete, add Part II at the end of the document.

Part II must exhaust every recognized way the current source permits, requests, refuses, or reserves
the scientific procedure or results to differ from Part I. Search the audited source universe for:

- alternative inputs and acquisition paths;
- calendar and sample choices;
- transformations;
- estimators and specifications;
- moment conditions and restrictions;
- covariance and inference procedures;
- diagnostics;
- decision gates;
- numerical controls;
- search controls;
- resampling rules;
- random-number controls;
- preservation and reuse policies that can change selected scientific inputs or calculations;
- lifecycle behavior that can change scientific reproducibility;
- presentation controls that change a reported scientific quantity or its interpretation;
- scientific output consequences;
- component calculations not available to the full route; and
- declared extensions that lack a complete executable route.

Do not repeat a runtime-conditional outcome already intrinsic to the Part I procedure. Part II
covers only deviations that require another launch input, tracked choice, source-fixed control,
component or separate entrypoint, or declared but unavailable capability.

For every behaviorally distinct item or family, state:

1. the condition or choice that activates it;
2. its admissible domain;
3. the mathematical or procedural change;
4. the downstream quantities it changes;
5. the scientific identities, acceptance rules, or outputs it invalidates; and
6. its reachability and request-handling classifications.

When an item cannot be activated, cannot change a downstream quantity, or invalidates nothing,
write `None` or `Not applicable` for that field and cite the source evidence in the audit. Do not
invent an effect to fill the catalogue.

Classify every item once on each of two independent axes.

Reachability classification:

- `Executable full-route modification`: a supported selection reaches a complete pipeline producer;
- `Executable component modification`: a substantive calculation is callable only through a
  component or separate entrypoint, not the full route;
- `Routing-only choice`: the request changes a route or status but reaches no scientific producer;
- `No substantive implementation`: the source declares or reserves the item without implementing
  its calculation.

Request-handling classification:

- `Supported and unselected`: the choice is valid and would have a downstream effect, but the plain
  invocation does not select it;
- `Accepted then refused`: an upstream contract accepts the request, but a downstream gate refuses
  it explicitly;
- `Accepted but ignored`: the request or field is parsed or recorded but has no downstream effect;
- `Rejected during validation`: validation rejects the value before routing; or
- `Not exposed`: the item is not selectable through a current input or recorded choice.

The categories are mutually exclusive within each axis, not across axes. Define any additional
source-required category before using it, and apply it consistently.

Within the reachability axis, use `Routing-only choice` whenever executable routing or status
behavior exists, even if no scientific producer runs. Reserve `No substantive implementation` for
a declaration or parsed field with no executable routing, status, or scientific-production behavior.

A recognized or parsed setting is not necessarily executable. Trace every downstream gate before
assigning its classification. Do not describe a component calculation, reserved output, recorded
request, or accepted field as a working full-route capability unless the complete route can actually
perform it.

Part II must describe capabilities through mathematics and prose, not through source identifiers.
Keep pure cache storage, byte-integrity, file-replacement, publication, and repository-maintenance
machinery in the source audit rather than the TeX unless it changes a scientific input, calculation,
or interpretation.

## Terminology contract

The entire document must obey “same concept, same word.”

For every mathematical object, sample, residual, estimator, mechanism, state, threshold, range,
decision, output, and lifecycle action, choose one canonical document term and use it everywhere.

Do not inherit inconsistent or asymmetric names from internal objects, comments, modules, or
branches. Determine semantic identity from the mathematics and role of each concept.

Do not use:

- elegant variation;
- shortened aliases;
- near-synonyms;
- inconsistent abbreviations;
- arbitrary changes in hyphenation;
- a generic term in one section and a more specific term elsewhere; or
- singular and plural labels that obscure whether the referent changed.

Apply the reverse rule as well: do not collapse distinct concepts under one term. Keep separate
names when objects differ by estimator, sample, timing, transformation, dimension, denominator,
state, or inferential role.

Audit terminology in headings, prose, equations, subscripts, tables, captions, lists, and
cross-references.

## Multi-worker workflow

Apply the shared worker, concurrency, interruption, and review-certification contracts. Workers may
return evidence, derivations, line-specific findings, and proposed revisions only through their
private scratch records. They never edit the target TeX. At least the final scientific-fidelity audit
must be performed by a worker distinct from the orchestrator. Capacity may serialize assignments but
must not merge their scopes or reduce coverage.

Complete the following stages in order. Do not cross a barrier with an open finding.

### Stage A: establish the source snapshot and plan

The orchestrator must:

1. read all governing instructions;
2. record the complete initial Git status and protected paths;
3. record initial existence, size, and hash for the target TeX and PDF when present; a sorted
   manifest of hashes and sizes for the audited source universe; and a metadata-only manifest for
   every protected output, cache, manifest-instance, and route-state root;
4. obtain the timestamp;
5. write the execution plan;
6. inventory the entry point, configuration sources, input contracts, decision records, manifest
   declarations, lifecycle rules, and publication stages; and
7. close the audited source universe, label its source-selected production graph as a subset, and
   record external data, package, and executable contracts separately; and
8. record the installed-package parity evidence or explicit unverified-runtime limitation.

Barrier A passes only when every relevant dependency has been traced or explicitly proved
irrelevant.

### Stage B: independent source tracing

Assign these review scopes, using distinct workers when capacity permits:

- the exact source-selected procedure and effective configuration;
- input transformations, samples, dimensions, and mathematical models;
- estimators, diagnostics, numerical safeguards, resampling, and inference;
- scientifically relevant preservation, reuse, reporting, and output consequences, plus an audit of
  operational machinery excluded from the TeX; and
- the exhaustive noncurrent capability inventory and two-axis classification.

Agents must cite precise source locations in their private reports. Those locations must never enter
the TeX document. Agents submit proposed rows and findings; the orchestrator owns the canonical
matrices and ledgers and reconciles duplicates and disagreements.

The source-tracing workers must also submit evidence for a return-and-frequency ledger. For every
return, rate, yield, growth measure, volatility measure, variance, and frequency-converted variable,
the ledger must record its source frequency, reported frequency, gross-or-net status, log-or-simple
status, nominal-or-real status when relevant, price-or-total-return status when relevant,
excess-return status when relevant, exact conversion formula, units, and proposed TeX location.

Construct two bidirectional coverage matrices in a canonical report:

1. Source-selected-procedure matrix:
   - every operation and runtime-conditional outcome rule in the selected procedure has a Part I
     counterpart; and
   - every Part I claim has source-selected-procedure support.
2. Capability matrix:
   - every source-recognized deviation, request, refusal, and declared limit has one Part II
     counterpart; and
   - every Part II item has source support and the correct two-axis classification.

Resolve worker disagreements against the source. If static evidence cannot determine the procedure
selected by source, treat the ambiguity as a parity blocker. A runtime fact that selects among
conditional outcomes is not a static-parity blocker when the source states the condition and all
outcome rules; document those rules without inferring the realized outcome from outputs or caches.

Barrier B passes only when both matrices and the return-and-frequency ledger are complete and have
no unresolved item.

### Stage C: orchestrator synthesis and scientific audit

The orchestrator alone revises `docs/run_pipeline_math.tex`.

You may preserve existing text only after independently validating it. Rewrite or reorganize as much
as needed to satisfy the full contract. Do not create a differently named primary TeX file.

After the substantive draft is complete, assign fresh read-only workers to audit:

- source-selected-procedure completeness, including every runtime-conditional outcome rule;
- exclusion of all alternatives from Part I;
- capability completeness and two-axis classification in Part II;
- every equation, sign, inequality, denominator, dimension, boundary condition, indexing rule,
  missing-value rule, search rule, runtime fallback, stopping condition, status mapping, and output
  consequence;
- every return, unit, frequency, compounding, annualization, and deannualization convention;
- first-use symbol definitions;
- first-use technical-term definitions;
- the prohibition on literal integrity values, algorithms, fields, and byte-level implementation
  details; and
- the prohibition on implementation references.

The orchestrator verifies and applies every supported correction. Repeat these audits.

Barrier C passes only when the orchestrator and at least one distinct read-only scientific auditor
report no substantive omission, invention, misclassification, mathematical error, undefined object,
ambiguous return convention, or noncurrent Part I alternative.

Every audit certification must identify the exact TeX snapshot it reviewed by hash, byte count, and
line count. Copy that version to a digest-named path such as
`<current-run-directory>/snapshots/run_pipeline_math-<sha256>.tex`, verify it byte-for-byte, and
give reviewers the snapshot rather than the mutable canonical path. Verify it before and after
every review. A certification tied to an earlier snapshot does not count.

### Stage D: fresh same-concept–same-word pass

Only after Barrier C passes, assign a fresh worker whose sole task is a rigorous terminology audit.

This worker must:

1. build a canonical terminology ledger from the mathematical referents in the actual
   post-Barrier-C document;
2. compare concepts by mathematical role, estimator, sample, timing, transformation, dimension,
   state space, and inferential meaning;
3. find every synonym, shortened label, asymmetric inherited name, inconsistent abbreviation,
   inconsistent hyphenation, and generic-versus-specific label;
4. find any single term incorrectly used for distinct concepts;
5. audit every heading, paragraph, equation, subscript, table, caption, list, and cross-reference;
6. audit first-use definitions of all jargon and technical phrases;
7. verify that each return and frequency convention is stated at first use;
8. verify that a global convention is not needlessly repeated;
9. verify that distinct conventions are not hidden under the same term; and
10. return exact line-specific corrections without editing the TeX file.

The orchestrator must verify every finding, apply the valid corrections, and return the exact
revised file to the same worker for another complete audit.

Barrier D passes only when that worker reports no remaining same-concept–same-word violation on the
current TeX version and the scientific coverage matrices remain clean.

### Stage E: fresh `econ-write` pass

Only after Barrier D passes, assign a fresh economics-writing review.

Both the orchestrator and this worker must read the exact `econ-write` skill file fixed by the shared
contract:

`/Users/fduarte/.codex/skills/econ-write/SKILL.md`

They must also read every referenced resource required for a full revision pass, including the
McCloskey word-choice reference and the revision checklist. If the skill or a required reference is
absent or unreadable, record a blocker.

The economics-writing worker must inspect the complete post-terminology document, not a prior draft
or a sample.

Apply every relevant principle, including:

- reader-first organization;
- important information first;
- concrete rather than abstract prose;
- active voice;
- present tense where appropriate;
- short, direct sentences;
- simple words;
- one idea per paragraph;
- topic sentences;
- economical wording;
- equations introduced in words;
- consistent notation;
- one concept represented by one word; and
- a concise abstract, normally 100–150 words, that explains the manual’s purpose and two-part
  organization without inventing empirical findings.

Mark paper-only rules as inapplicable rather than forcing them onto a reproduction manual. In
particular, do not invent a contribution, literature review, policy implication, empirical finding,
or result. Do not remove mathematics needed for exact independent reproduction merely because a
general writing rule prefers less mathematics.

The worker must return a complete line-specific edit prescription. The orchestrator must verify that
each proposed edit preserves mathematical meaning, source parity, the Part I–Part II partition,
two-axis classifications, symbol definitions, return conventions, and the terminology
ledger before applying it.

Return the actual revision to the economics-writing worker for another full pass.

Barrier E passes only when the worker reports compliance with every applicable rule on the current
TeX version and all earlier scientific and terminology checks remain clean.

Use the shared rule-violation versus discretionary-improvement classification.

### Stage F: fresh `writing-clearly-and-concisely` pass

Only after Barrier E passes, assign a fresh clear-writing review.

Both the orchestrator and this worker must read the exact clear-writing files fixed by the shared
contract:

`/Users/fduarte/.codex/skills/writing-clearly-and-concisely/SKILL.md`

and:

`/Users/fduarte/.codex/skills/writing-clearly-and-concisely/elements-of-style.md`

If either file is absent or unreadable, record a blocker. The worker must inspect the complete
post-`econ-write` document.

Audit every applicable rule of grammar, punctuation, composition, usage, paragraph structure,
sentence structure, active voice, positive formulation, specificity, concision, modifier placement,
parallel construction, tense, and emphasis.

The worker must not:

- remove information needed for reproduction;
- simplify a mathematically necessary distinction;
- change a formula, condition, dimension, status, or classification;
- introduce a synonym for a canonical term;
- rename a defined symbol without updating and revalidating every occurrence;
- make a return or frequency convention ambiguous; or
- restore an alternative to Part I.

The worker returns exact line-specific corrections. The orchestrator verifies and applies valid
changes, then returns the actual revised file for another complete clear-writing audit.

Barrier F passes only when the worker reports no remaining applicable rule violation on the reviewed
snapshot. Apply the shared gate-ownership and certification protocol. Completion requires scientific
fidelity, terminology, `econ-write`, and clear-writing passes to certify one immutable snapshot with
no intervening edit; any edit reopens the earliest affected barrier and every downstream barrier.

### Stage G: final orchestrator integrity review

After every worker barrier passes, the orchestrator must reread the entire TeX document critically
from beginning to end.

Verify independently that:

- Part I contains exactly the source-selected procedure, including all runtime-conditional outcome
  rules, and no noncurrent alternative;
- Part II exhausts and correctly classifies every recognized deviation, request, refusal, and
  declared limit;
- every formula, condition, dimension, threshold, ordering rule, and status still matches source;
- every return and frequency conversion has an explicit, source-supported convention;
- every necessary concept appears without exposing its implementation representation;
- no prohibited literal integrity value, algorithm, internal field, or byte-level integrity
  implementation remains;
- every symbol and technical term is defined at first use;
- each concept uses one canonical term;
- distinct concepts remain distinct;
- input-selection priorities use explicit, unambiguous names;
- the abstract, prose, equations, lists, tables, and output contract do not contradict one another;
- no duplicate or conflicting definition remains;
- no worker finding was lost, copied blindly, or incorporated incorrectly;
- no unsupported statement survived from the existing untrusted draft; and
- the document remains linear, modular, concise, and sufficient for implementation in another
  language.

Subagent conclusions are advisory. The orchestrator owns the final judgment and must verify every
incorporated change.

## Static integrity checks

Before compilation, run targeted scans of the final TeX for:

- computational-code or code-like-pseudocode environments;
- internal source paths and extensions;
- entry-point names;
- internal function, object, argument, configuration, or environment-variable names;
- programming commands or syntax;
- implementation vocabulary;
- literal hashes, fingerprints, checksums, cryptographic-digest algorithms, hexadecimal verification
  values, internal integrity fields, and byte-level integrity implementation;
- placeholders such as `TODO`, `TBD`, or unresolved questions;
- obsolete terminology and competing synonyms from the terminology ledger;
- undefined or duplicate labels;
- malformed characters;
- copied or lightly transformed prose from the protected explanatory document; and
- alternative or optional language leaking into Part I.

Adjudicate every match. A raw search count is not proof. Refine searches that produce false
positives and rerun them. The final count of confirmed prohibited uses must be zero. List permitted
mathematical or ordinary-language uses as adjudicated matches rather than suppressing them.

Recheck every equation, index, unit, sign, inequality, denominator, rank condition, boundary
condition, missing-value rule, sample transition, return definition, and frequency conversion.

## Compilation and visual inspection

Preflight the available LaTeX engine, `latexmk` version and configuration, Poppler tools, `qpdf`,
and fonts. Derive and hash the complete repository- or document-owned static dependency closure of
the certified TeX, including every local TeX fragment, image, bibliography, style, class, and
explicitly bundled font file. Record explicitly when it is standalone. Record external TeX
packages, engines, and system fonts by resolved path and version; let the installed TeX runtime
supply them rather than copying them. Create a fresh system temporary directory outside the
repository with `mktemp -d`, copy the exact certified snapshot and owned dependency closure while
preserving their relative layout, and verify every copied hash. Build there with `latexmk -pdf`;
keep `-pdf` explicit even if local configuration also selects PDF. Never leave sidecars or
intermediate build files beside the source document. Of the compilation products, retain only the
audit log and necessary renders in the current run directory.

**Build to a fixed point before judging the log.** The directory is fresh by construction, so the
first pass has no auxiliary file and *will* report unresolved references and "rerun to get
cross-references right" — that is the absence of a prior pass, not a defect in the document. Run
the build again until the log stops asking, and adjudicate only the final pass. Adjudicating the
first pass turns a clean document into dozens of phantom warnings and invites a "fix" for a
problem that does not exist.

Require:

- compiler exit status zero;
- a nonempty PDF;
- no LaTeX error;
- no undefined control sequence;
- no unresolved reference;
- no duplicate label;
- no missing glyph;
- no visible or materially risky overfull box;
- every remaining underfull-box warning classified by page and location and confirmed harmless by
  visual inspection;
- no unadjudicated LaTeX or package warning;
- `qpdf --check` success;
- all used fonts embedded, with expected families and encodings present and no unexplained
  substitution, as confirmed with `pdffonts`; and
- every link annotation enumerated by page and subtype, every internal destination resolved, and
  every external URI nonempty and consistent with the TeX source.

Check the PDF’s page count, page size, text extraction, and structural integrity. Use appropriate
PDF inspection tools.

Render every page. Inspect the entire document visually. Contact sheets may help locate problems,
but inspect dense equations, tables, contents pages, part boundaries, and the final page at readable
resolution.

Check for:

- clipping;
- overlap;
- broken equations;
- missing glyphs;
- unreadable text;
- bad margins;
- table overflow;
- malformed headings;
- broken contents entries;
- poor page breaks;
- orphaned headings;
- unintended blank pages; and
- an unclear Part I–Part II boundary.

Correct every material defect caused by the TeX. Record harmless engine warnings with their page,
location, and visual evidence. Recompile, rerun structural checks, and repeat the complete visual
inspection.
If a correction changes prose or mathematics, repeat every affected compliance barrier.

The final PDF must come from the exact TeX version certified by the final reports. Once the build is
accepted, copy it over `docs/run_pipeline_math.pdf` and verify that the canonical PDF is
byte-identical to the accepted build.

## Final scope and evidence gate

Before declaring success:

1. Recompute the audited-source-universe manifest and confirm that it did not change.
2. Record final existence, size, and hash for the target TeX and PDF.
3. Recompute the protected generated-state metadata manifest. Explain every difference and confirm
   that no task command caused it; do not inspect file contents.
4. Compare final Git status with the complete initial status. Attribute every new or changed tracked
   path. If an enclosing Stage O runs the sibling code-document task concurrently, treat only its
   assigned TeX and prompt-authorized records as permitted concurrent external changes; do not
   inspect or modify that sibling's target.
5. From the task command audit, direct target hashes, protected-state
   metadata comparison, and tracked-status comparison, confirm that this workflow did not
   modify scientific source, configuration, tests, inputs, outputs, caches, manifest instances,
   protected documents, existing reports, or repository metadata. Do not claim that unrelated
   external processes made no change.
6. Record every failed command and its corrected rerun.
7. Record the final TeX version, byte size, line count, word count, abstract word count, compiled
   page count, compiled PDF size, compiler result, log-scan result, structural-PDF result, font
   result, visual-review coverage, static-scan results, and every worker audit outcome.
8. Attest from the commands launched by this workflow that it did not run R or the pipeline, read
   generated-state contents, mutate scientific state, or instantiate a manifest.
9. Record the installed-package parity evidence or explicit unverified-runtime caveat.
10. After retaining the accepted evidence and synchronizing the PDF, remove only the exact system
    temporary directory created by this workflow. First verify that it is outside the repository and
    matches the recorded `mktemp` path.
11. Finalize all reports listed below, logs, and retained renders in the current run directory.
12. As the last run-directory write, create a sorted path, size, and hash manifest for every other
    file there. List the manifest's own path as the sole deliberate self-reference exclusion, then
    verify that no later run-directory write occurs.

The reports finalized before the last-write manifest must include:

- the execution plan;
- the audited source universe and source snapshot;
- the source-selected-procedure coverage matrix;
- the capability coverage matrix;
- the return-and-frequency ledger;
- the terminology ledger and audit;
- `econ-write` compliance;
- `writing-clearly-and-concisely` compliance; and
- final compilation, visual, scope, and integrity validation.

Reports may contain source locators and command evidence. The TeX document may not.

## Bounded failure rule

Never weaken a check, suppress a warning, omit a difficult capability, or replace uncertain
mathematics with vague prose to obtain a passing result.

Fix the underlying cause when it lies in the target TeX or its build. Report a source, toolchain, or
external-data cause as a blocker or limitation; do not modify an out-of-scope file to force a pass.
Use bounded retries that change the diagnosis or remedy rather than repeating the same attempt. The
execution plan must set a finite cap for each fallible external step.

If a genuine external blocker remains after bounded attempts:

1. create a new Markdown report in the current run directory;
2. record the unresolved issue;
3. record the exact evidence;
4. list the sources inspected;
5. list the remedies attempted;
6. complete every independent remaining task;
7. mark parity certification as incomplete; and
8. explain the blocker in the final response.

Do not ask the user what to do.

## Completion standard

Declare success only if all of the following are true:

- the TeX document matches a stable snapshot of the current source;
- Part I contains the exact source-selected procedure, including every runtime-conditional outcome
  rule, and no noncurrent alternative;
- Part II exhausts and correctly classifies every source-recognized modification, request, refusal,
  and declared limit;
- an external reader can reproduce the scientific analysis without the repository because every
  input has both an immutable artifact identity and an access route resolving to the exact required
  observations;
- the TeX contains no computational code, code-like pseudocode, or implementation reference;
- the TeX contains no prohibited literal integrity value, algorithm, internal field, or byte-level
  integrity implementation;
- every return and frequency-converted variable is unambiguous about its mathematical definition,
  units, observation frequency, reporting frequency, and annualization or deannualization formula;
- every input-selection priority is stated with explicit names and ordering;
- every symbol and technical term is defined at first use;
- every concept uses one canonical term throughout;
- all sequential reviewer barriers certify the final artifact or remain valid after no later edit;
- no contradiction, omission, stale claim, unsupported statement, placeholder, or open audit finding
  about checkout source remains; an explicitly disclosed unverified installed-package caveat is
  permitted but bars installed-runtime certification;
- the TeX passes the adjudicated compilation gate;
- every rendered page is visually defect-free;
- the complete audited-source-universe manifest is unchanged;
- the direct target hashes, protected-state metadata comparison, and current-run-directory manifest
  account for every task-attributable change and contain no unexplained difference;
- the task command audit and baseline-to-final tracked-status comparison attribute no out-of-scope
  file or scientific-state change to this workflow;
- every execution boundary was preserved; and
- the final reports demonstrate these facts with evidence.

A successful compile alone does not establish scientific parity. A polished document alone does not
establish completeness. Completion requires source fidelity, mathematical completeness, explicit
measurement conventions, terminology consistency, writing compliance, passage of the adjudicated
compilation gate, visual integrity, and preserved scope.

**An honest partial outranks a false pass, and will be treated that way.** If a barrier will not
close, stop and hand over: name what is outstanding, by section and class, in a form the next editor
can act on without re-deriving it, and say plainly which barriers are certified and against which
version. Do not declare completion because the remaining findings feel small, and do not begin an
apply pass without enough capacity to finish it and re-run every affected barrier.

**State coverage precisely rather than letting "all barriers pass" imply more than it does.** In the
final report, separate what was read line by line on the delivered version from what was verified
mechanically across the whole file. A read of a superseded version does not certify the delivered
version; any final scope that no assigned reviewer or the orchestrator rechecked blocks `Complete`
status. Re-test every mechanical zero against a control pattern that must match, and report the
control's count alongside the zero because an unfired search and a clean document are
indistinguishable. If no single reviewer verified every claim in one pass, state how the
orchestrator's final full-file review and the disjoint review scopes provide complete coverage.
