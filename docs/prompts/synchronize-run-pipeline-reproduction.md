# Orchestrator prompt: synchronize the mathematical pipeline manual

You are the primary orchestrator, operating as a frontier reasoning agent. Starting with no prior
knowledge of this repository, bring the following TeX document into complete parity with the current
scientific pipeline.

Repository root:

`/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid`

Pipeline entry point:

`/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/scripts-paper/run_pipeline.R`

Target document:

`/Users/fduarte/Library/CloudStorage/Dropbox-Personal/MyPackages/hetid/docs/run_pipeline_reproduction.tex`

The repository, pipeline, and document may all have changed since this prompt was written. Do not
carry forward any numerical value, active branch, input identity, equation, capability, or conclusion
from prior knowledge. Discover the current state directly.

The final result must be a self-contained mathematical reproduction manual. A first-year economics
PhD student who has never seen the repository and will never see it must be able to reproduce the
entire pipeline in another programming language by reading the TeX document alone.

Run this workflow autonomously from beginning to end. Do not ask the user questions, request
approval, invoke a human-review gate, or pause for confirmation. If a skill or local instruction
normally requires such a gate, this no-human directive overrides that gate. Use your best judgment
and proceed.

## Authority and evidence

The current production source is the sole authority.

1. Read every applicable repository instruction before acting, including `AGENTS.md`, `CLAUDE.md`,
   and any more specific instruction file.
2. Begin at `scripts-paper/run_pipeline.R` and discover its complete current source graph
   recursively.
3. Trace the effective configuration, defaults, input-selection rules, recorded decisions, branch
   gates, mathematical procedures, numerical controls, lifecycle rules, publication stages, and
   completion checks.
4. Treat `docs/run_pipeline_reproduction.tex` as an untrusted draft. Revalidate every substantive
   sentence, formula, number, status, and capability.
5. You may inspect `docs/run_pipeline_explained.tex` only as a nonauthoritative source of questions.
   Do not borrow its prose, structure, or conclusions. Do not edit it.
6. Treat comments, tests, old reports, generated artifacts, memories, and subagent conclusions as
   leads rather than authority. Production-reachable behavior wins whenever sources disagree. Tests
   may clarify edge behavior but cannot override current production logic.
7. Record exact snapshot evidence for every relevant source file at the beginning. Recheck the
   snapshot before final acceptance. If relevant source changes during the work, retrace the affected
   behavior and repeat every downstream audit.
8. Never assert completion without command output or source evidence. Record failed checks and their
   corrected reruns.

When a genuine ambiguity remains after direct inspection, obtain an independent second opinion
through `pal clink` with the Claude CLI, if available. Treat that opinion as advice and verify it
against source. If that facility is unavailable, use a fresh independent reasoning agent. Use
official primary documentation when an external interface or mathematical convention requires
verification.

## Absolute execution boundary

Trace the scientific pipeline statically. Do not run it.

You must not:

- execute or source the pipeline entry point;
- execute project analysis modules to discover their behavior;
- initialize, reset, populate, delete, or alter scientific outputs;
- initialize, invalidate, replace, or alter caches;
- initialize or alter manifest instances;
- fetch or refresh scientific data;
- use generated outputs, cache contents, or manifest instances as behavioral evidence;
- infer behavior from prior numerical results; or
- stop or interfere with an independently running process.

You may inspect source declarations, schemas, configuration, manifest definitions, tests, and input
contracts without mutation.

Small isolated calculations are allowed only when needed to verify algebra, dimensions, numerical
syntax, or a source-reading hypothesis. Run them in a fresh system temporary directory outside the
repository. They must not import, source, or execute project logic, fetch data, or create scientific
outputs.

If another process happens to be running the pipeline, do not inspect or disturb its products. In
the final report, state only that this workflow did not launch the scientific pipeline.

## File boundary

The only existing file you may modify is:

`docs/run_pipeline_reproduction.tex`

You may create new timestamped plans, source maps, terminology ledgers, audit reports, and validation
logs under:

`docs/RUN/run_pipeline_reproduction/`

Do not modify or overwrite an existing report or log. Create a new uniquely named file.

Additional rules:

- Only the primary orchestrator may edit the target TeX file.
- Every subagent is read-only with respect to the target TeX and the repository.
- A subagent that needs a private working file may write only under
  `docs/RUN/scratch/agents/<agent-id>/`.
- The orchestrator alone writes canonical plans, reports, ledgers, and logs.
- Use patch-based repository edits. Do not overwrite files through shell redirection or bulk writing
  commands.
- Preserve unrelated worktree changes.
- Do not edit scientific source, configuration, tests, inputs, artifacts, repository metadata, or
  any other documentation.
- Do not commit, push, publish, delete, revert, reset, or rewrite history.
- Keep compiled PDFs and LaTeX sidecar files outside the repository.
- Obtain report timestamps with `date "+%Y-%m-%d %H:%M %Z"`. Never guess the time.

Before editing, write a concrete execution plan under `docs/RUN/run_pipeline_reproduction/`. Do not
use `multistep-plan`, `multistep-do`, or another multistep skill. Proceed without requesting
approval.

## Non-negotiable TeX content contract

The TeX document must contain only clear prose, mathematical notation, equations, compact
mathematical tables when necessary, and carefully structured lists.

It must contain no computational code or pseudocode.

It must not quote, reproduce, cite, link to, name, or allude to:

- internal source files or directories;
- functions, objects, arguments, configuration keys, or environment-variable names;
- programming commands or programming-language syntax;
- implementation packages or numerical libraries;
- internal architecture or source-loading mechanisms;
- cache filenames or internal state filenames;
- source comments or source line numbers; or
- any other representation of the underlying implementation.

Do not write phrases such as “the code does,” “the script calls,” or “the implementation uses.”

Names of external scientific data, established scholarly procedures, and required input or output
artifacts may appear only when an outside reader needs them to reproduce the scientific input or
artifact contract. Never use such names as disguised implementation references.

The TeX document must contain no reference to a hash, fingerprint, checksum, cryptographic digest,
hexadecimal verification value, or literal verification string. Remove such references even when
the source uses them internally. Do not describe their construction, storage, comparison, or role.
Source-audit reports outside the TeX document may record snapshot evidence when needed to prove the
scope of the audit.

When the pipeline selects among multiple input releases, name each scientifically relevant release
in plain language and state the selection order directly. For example, say that a preserved user
copy is selected when it exists and that the bundled copy is selected otherwise. Do not use vague
contrasts such as “a separately stored release” and “the preserved alternative.”

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

### Part I: exact current configured reproduction

Part I must describe only the route selected by the current configuration, current default inputs,
and current recorded choices.

It must read as one linear procedure from inputs through transformations, estimation, diagnostics,
inference, reporting, lifecycle actions, and completion checks.

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
- branch decisions actually reached;
- preservation or reuse behavior actually selected;
- output consequences; and
- final validation and completion conditions.

Part I must not mention, offer, compare, hint at, or explain:

- alternative inputs;
- alternative estimators;
- optional diagnostics;
- different numerical settings;
- dormant branches;
- rejected routes;
- hypothetical decisions;
- unavailable extensions; or
- any other “could,” “may,” or “instead” behavior.

Numerical safeguards that belong to the selected procedure remain in Part I. Dormant alternatives do
not.

### Part II: complete modification and capability catalogue

After Part I is complete, add Part II at the end of the document.

Part II must exhaust every way the current source permits the mathematical procedure or process to
differ from Part I. Search the complete source graph for:

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
- preservation and reuse policies;
- lifecycle behavior;
- presentation controls;
- output consequences;
- component calculations not available to the full route; and
- declared extensions that lack a complete executable route.

For every item, state:

1. the condition or choice that activates it;
2. its admissible domain;
3. the mathematical or procedural change;
4. the downstream quantities it changes;
5. the identities, acceptance rules, or outputs it invalidates; and
6. its actual execution status.

Classify every noncurrent capability as exactly one of:

- an executable full-route modification;
- an executable component modification;
- a routing-only choice;
- an accepted-but-refused choice;
- an invalid choice; or
- a declared unavailable extension.

A recognized or parsed setting is not necessarily executable. Trace every downstream gate before
assigning its classification. Do not describe a component calculation, reserved output, recorded
request, or accepted field as a working full-route capability unless the complete route can actually
perform it.

Part II must describe capabilities through mathematics and prose, not through source identifiers.

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

Apply the reverse rule as well: do not collapse distinct concepts under one term. Keep separate names
when objects differ by estimator, sample, timing, transformation, dimension, denominator, state, or
inferential role.

Audit terminology in headings, prose, equations, subscripts, tables, captions, lists, and
cross-references.

## Multi-agent workflow

Use the maximum useful number of subagents. Give each one a bounded, nonoverlapping task, read-only
repository access, a private scratch directory, permission to spawn compliant subagents, and the
same no-human and no-pipeline constraints.

No subagent may edit the target TeX. Subagents return evidence, mathematical derivations,
line-specific findings, or proposed revisions. The orchestrator verifies every finding and applies
accepted changes.

Complete the following stages in order. Do not cross a barrier with an open finding.

### Stage A: establish the source snapshot and plan

The orchestrator must:

1. read all governing instructions;
2. record initial repository status;
3. record the protected files and relevant source snapshot;
4. obtain the timestamp;
5. write the execution plan;
6. inventory the entry point, configuration sources, input contracts, decision records, manifest
   declarations, lifecycle rules, and publication stages; and
7. recursively close the production-reachable source graph.

Barrier A passes only when every relevant dependency has been traced or explicitly proved
irrelevant.

### Stage B: parallel source tracing

Assign independent agents to cover, at minimum:

- the exact current route and effective configuration;
- input transformations, samples, dimensions, and mathematical models;
- estimators, diagnostics, numerical safeguards, resampling, and inference;
- lifecycle, preservation, reporting, and output consequences; and
- the exhaustive noncurrent capability inventory and execution-status classification.

Agents must cite precise source locations in their private reports. Those locations must never enter
the TeX document.

The source-tracing agents must also create a return-and-frequency ledger. For every return, rate,
yield, growth measure, volatility measure, variance, and frequency-converted variable, the ledger
must record its source frequency, reported frequency, gross-or-net status, log-or-simple status,
nominal-or-real status when relevant, price-or-total-return status when relevant, excess-return
status when relevant, exact conversion formula, units, and proposed TeX location.

Construct two bidirectional coverage matrices in a canonical report:

1. Current-route matrix:
   - every operation executed on the selected route has a Part I counterpart; and
   - every Part I claim has current-route support.
2. Capability matrix:
   - every source-supported deviation has one Part II counterpart; and
   - every Part II item has source support and the correct execution-status classification.

Resolve agent disagreements against the source. If static evidence cannot determine the selected
route, treat the uncertainty as a parity blocker. Do not infer the answer from outputs or caches.

Barrier B passes only when both matrices and the return-and-frequency ledger are complete and have
no unresolved item.

### Stage C: orchestrator synthesis and scientific audit

The orchestrator alone revises `docs/run_pipeline_reproduction.tex`.

You may preserve existing text only after independently validating it. Rewrite or reorganize as much
as needed to satisfy the full contract. Do not create a differently named primary TeX file.

After the substantive draft is complete, assign fresh read-only agents to audit:

- current-route completeness;
- exclusion of all alternatives from Part I;
- capability completeness and classification in Part II;
- every equation, sign, inequality, denominator, dimension, boundary condition, indexing rule,
  missing-value rule, search rule, fallback, stopping condition, status mapping, and output
  consequence;
- every return, unit, frequency, compounding, annualization, and deannualization convention;
- first-use symbol definitions;
- first-use technical-term definitions;
- the prohibition on hashes, fingerprints, checksums, digests, and literal verification values; and
- the prohibition on implementation references.

The orchestrator verifies and applies every supported correction. Repeat these audits.

Barrier C passes only when the scientific auditors independently report no substantive omission,
invention, misclassification, mathematical error, undefined object, ambiguous return convention, or
Part I alternative.

Every audit certification must identify the exact current version of the TeX file it reviewed. A
certification tied to an earlier version does not count.

### Stage D: fresh same-concept–same-word pass

Only after Barrier C passes, assign a fresh agent whose sole task is a rigorous terminology audit.

This agent must:

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

The orchestrator must verify every finding, apply the valid corrections, and return the exact revised
file to the same agent for another complete audit.

Barrier D passes only when that agent reports no remaining same-concept–same-word violation on the
current TeX version and the scientific coverage matrices remain clean.

### Stage E: fresh `econ-write` pass

Only after Barrier D passes, assign a different fresh agent to perform the economics-writing
compliance pass.

Both the orchestrator and this agent must read the complete current file:

`/Users/fduarte/.codex/skills/econ-write/SKILL.md`

They must also read every referenced resource required for a full revision pass, including the
McCloskey word-choice reference and the revision checklist.

The economics-writing agent must inspect the complete post-terminology document, not a prior draft or
a sample.

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

The agent must return a complete line-specific edit prescription. The orchestrator must verify that
each proposed edit preserves mathematical meaning, source parity, the Part I–Part II partition,
execution-status classifications, symbol definitions, return conventions, and the terminology
ledger before applying it.

Return the actual revision to the economics-writing agent for another full pass.

Barrier E passes only when the agent reports compliance with every applicable rule on the current
TeX version and all earlier scientific and terminology checks remain clean.

### Stage F: fresh `writing-clearly-and-concisely` pass

Only after Barrier E passes, assign another fresh agent to perform the final prose pass.

Both the orchestrator and this agent must read:

`/Users/fduarte/.codex/skills/writing-clearly-and-concisely/SKILL.md`

and the complete reference:

`/Users/fduarte/.codex/skills/writing-clearly-and-concisely/elements-of-style.md`

The agent must inspect the complete post-`econ-write` document.

Audit every applicable rule of grammar, punctuation, composition, usage, paragraph structure,
sentence structure, active voice, positive formulation, specificity, concision, modifier placement,
parallel construction, tense, and emphasis.

The agent must not:

- remove information needed for reproduction;
- simplify a mathematically necessary distinction;
- change a formula, condition, dimension, status, or classification;
- introduce a synonym for a canonical term;
- rename a defined symbol without updating and revalidating every occurrence;
- make a return or frequency convention ambiguous; or
- restore an alternative to Part I.

The agent returns exact line-specific corrections. The orchestrator verifies and applies valid
changes, then returns the actual revised file for another complete clear-writing audit.

Barrier F passes only when the agent reports no remaining applicable violation on the current TeX
version and every earlier contract remains satisfied.

If a later edit may invalidate an earlier pass, return to the earliest affected barrier and repeat
the sequence. Do not assume that a later stylistic pass preserves scientific or terminology
compliance.

### Stage G: final orchestrator integrity review

After every agent barrier passes, the orchestrator must reread the entire TeX document critically
from beginning to end.

Verify independently that:

- Part I contains exactly the current selected route and no alternative;
- Part II exhausts and correctly classifies every permitted deviation and declared limit;
- every formula, condition, dimension, threshold, ordering rule, and status still matches source;
- every return and frequency conversion has an explicit, source-supported convention;
- every necessary concept appears without exposing its implementation representation;
- no hash, fingerprint, checksum, digest, or literal verification value remains;
- every symbol and technical term is defined at first use;
- each concept uses one canonical term;
- distinct concepts remain distinct;
- input-selection priorities use explicit, unambiguous names;
- the abstract, prose, equations, lists, tables, and output contract do not contradict one another;
- no duplicate or conflicting definition remains;
- no agent finding was lost, copied blindly, or incorporated incorrectly;
- no stale statement survived from the prior document; and
- the document remains linear, modular, concise, and sufficient for implementation in another
  language.

Subagent conclusions are advisory. The orchestrator owns the final judgment and must verify every
incorporated change.

## Static integrity checks

Before compilation, run targeted scans of the final TeX for:

- code or pseudocode environments;
- internal source paths and extensions;
- entry-point names;
- internal function, object, argument, configuration, or environment-variable names;
- programming commands or syntax;
- implementation vocabulary;
- hashes, fingerprints, checksums, cryptographic digests, hexadecimal verification language, and
  literal verification values;
- placeholders such as `TODO`, `TBD`, or unresolved questions;
- obsolete terminology and competing synonyms from the terminology ledger;
- undefined or duplicate labels;
- malformed characters;
- copied or lightly transformed prose from the protected explanatory document; and
- alternative or optional language leaking into Part I.

Adjudicate every match. A raw search count is not proof. Correct false-positive scans and rerun them.
The final prohibited count must be zero.

Recheck every equation, index, unit, sign, inequality, denominator, rank condition, boundary
condition, missing-value rule, sample transition, return definition, and frequency conversion.

## Compilation and visual inspection

Compile only the final target TeX in a fresh system temporary directory outside the repository. Use
the project-appropriate documented LaTeX workflow. Never write the PDF or sidecar files beside the
source document.

Require:

- compiler exit status zero;
- a nonempty PDF;
- no LaTeX error;
- no undefined control sequence;
- no unresolved reference;
- no duplicate label;
- no missing glyph;
- no overfull box;
- no underfull box;
- no unadjudicated LaTeX or package warning;
- a structurally valid PDF; and
- embedded fonts.

Check the PDF’s page count, page size, text extraction, and structural integrity. Use appropriate PDF
inspection tools.

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

Correct every defect. Recompile, rerun structural checks, and repeat the complete visual inspection.
If a correction changes prose or mathematics, repeat every affected compliance barrier.

The final PDF must come from the exact TeX version certified by the final reports.

## Final scope and evidence gate

Before declaring success:

1. Recheck the relevant source snapshot.
2. Confirm that the source remained stable.
3. Recheck repository status.
4. Confirm that only the target TeX and newly created `docs/` working records changed through this
   workflow.
5. Verify that no scientific source, configuration, test, input, output, cache, manifest instance,
   protected document, existing report, or repository metadata was modified.
6. Record every failed command and its corrected rerun.
7. Record the final TeX version, byte size, line count, word count, abstract word count, compiled page
   count, compiled PDF size, compiler result, log-scan result, structural-PDF result, font result,
   visual-review coverage, static-scan results, and every agent audit outcome.
8. State explicitly that this workflow did not launch the scientific pipeline.

Create new timestamped canonical reports under `docs/RUN/run_pipeline_reproduction/` for:

- the execution plan;
- the source graph and source snapshot;
- the current-route coverage matrix;
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

Fix the underlying cause of each failure. Use bounded retries that change the diagnosis or remedy
rather than repeating the same attempt.

If a genuine external blocker remains after bounded attempts:

1. create a new timestamped Markdown report under `docs/RUN/run_pipeline_reproduction/`;
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
- Part I contains the exact selected route and no alternatives;
- Part II exhausts and correctly classifies every source-supported modification and declared limit;
- an external reader can reproduce the full analysis without the repository;
- the TeX contains no code, pseudocode, or implementation reference;
- the TeX contains no hash, fingerprint, checksum, digest, or literal verification value;
- every return and frequency-converted variable is unambiguous about its mathematical definition,
  units, observation frequency, reporting frequency, and annualization or deannualization formula;
- every input-selection priority is stated with explicit names and ordering;
- every symbol and technical term is defined at first use;
- every concept uses one canonical term throughout;
- all sequential reviewer barriers certify the final artifact or remain valid after no later edit;
- no contradiction, omission, stale claim, unsupported statement, placeholder, or open audit finding
  remains;
- compilation is clean;
- every rendered page is visually defect-free;
- every file and execution boundary was preserved; and
- the final reports demonstrate these facts with evidence.

A successful compile alone does not establish scientific parity. A polished document alone does not
establish completeness. Completion requires source fidelity, mathematical completeness, explicit
measurement conventions, terminology consistency, writing compliance, clean compilation, visual
integrity, and preserved scope.
