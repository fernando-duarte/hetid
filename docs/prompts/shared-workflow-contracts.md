# Shared contract for long-running repository workflows

This file is a mandatory part of every workflow prompt in `docs/prompts/`. It defines the rules that
must remain identical across those workflows. A workflow-specific prompt defines its purpose,
targets, stages, and narrower boundaries.

## Loading and precedence

Read this file completely before acting on a dependent prompt.

- A standalone executor reads this file from the selected repository root, then reads the
  workflow-specific prompt.
- A caller dispatching a dependent workflow supplies the complete current contents of this file and
  the complete current contents of the dependent prompt, in that order. A path or summary is not a
  substitute for either file.
- Higher-priority system, user, and repository instructions still govern. Within this prompt family,
  this file governs shared behavior. A dependent prompt may narrow permissions or add gates for its
  own scope, but it may not weaken this contract.
- If the files conflict and the stricter interpretation does not resolve the conflict, stop the
  affected workflow, preserve completed evidence, and report the conflict as a blocker. Do not choose
  whichever instruction is easier.

Confirm the shared file's path and content digest in the workflow record. If it is missing or
unreadable, do not run the dependent workflow.

## Fixed execution configuration

Run the orchestrator, every sub-orchestrator, and every worker on **Claude Opus 5 with the 1M-token
context window at `xhigh` effort**. The model selector is `claude-opus-5[1m]`; the API model string is
`claude-opus-5`. When the harness exposes model or effort arguments, set both explicitly. If the
required model or effort is unavailable, record a blocker rather than silently substituting another
configuration.

Use these exact skill files when the dependent prompt requires the named skill:

- `/Users/fduarte/.claude/skills/karpathy-guidelines/SKILL.md`
- `/Users/fduarte/.codex/skills/multistep-plan/SKILL.md`
- `/Users/fduarte/.codex/skills/econ-write/SKILL.md`
- `/Users/fduarte/.codex/skills/writing-clearly-and-concisely/SKILL.md`
- `/Users/fduarte/.codex/skills/writing-clearly-and-concisely/elements-of-style.md`

Read each required file completely. Follow every task-relevant reference it identifies. If a
required file is absent or unreadable, record a blocker. Do not invent a replacement checklist or
use a similarly named file from another location.

## Roles and authority

Use these role names consistently:

- **Caller:** an enclosing workflow that dispatches a complete dependent prompt. The caller owns
  cross-workflow scheduling and verifies the result after the dispatched workflow becomes terminal.
- **Orchestrator:** the agent executing the current workflow prompt. It owns the method, decisions,
  synthesis, and every canonical file that the prompt assigns to it.
- **Sub-orchestrator:** an agent given this shared contract plus a complete dependent prompt. It is
  the orchestrator inside that dependent workflow, not an ordinary worker.
- **Worker:** a bounded subagent that inspects an assigned slice and writes only to its private
  scratch directory. It never edits a canonical target or changes Git state.
- **Stateless reviewer:** an external review interface that cannot write a durable file. It returns a
  verdict to the orchestrator, which records the response and evidence in the workflow record.

The orchestrator is the sole canonical writer unless the workflow-specific prompt names a
sub-orchestrator and its exact target. A caller does not edit a sub-orchestrator's target while that
sub-orchestrator is running. Authority returns to the caller only after the sub-orchestrator becomes
terminal.

The authority to run a workflow does not grant a worker the orchestrator's permissions. Every
delegation states the worker's narrower read and write sets explicitly.

## Autonomous operation

Run each workflow from start to terminal status without human involvement.

- Do not ask the user a question, request approval, or pause for a discretionary decision.
- Treat the workflow prompt as authorization for its stated in-scope actions. It does not authorize
  a destructive or external action outside that scope.
- When several compliant choices remain, choose the lowest-risk option that advances the objective,
  record the decision and reason, and continue.
- Stop only for a higher-priority prohibition or a hard blocker that remains after bounded,
  materially different attempts. Finish independent work before reporting a partial result.

Human-prompt and plan-approval mechanisms do not belong inside these workflows. A skill's optional
approval gate does not override this autonomous contract. A higher-priority mandatory gate still
applies.

## Repository and records roots

Select one repository root before inspection:

1. Use the exact root supplied by a caller.
2. Otherwise, use the top level of the current Git worktree if it contains the workflow's required
   repository markers.
3. Use a workflow-specific absolute fallback only when its prompt provides one and neither earlier
   rule applies.

Record the selected root and the rule that selected it. Resolve every repository path relative to
that root unless the prompt gives an absolute path. Never inspect or modify another checkout merely
because it contains a file missing from the selected root. A prompt that intentionally transfers an
untracked machine-local resource between worktrees must define that exception and its synchronization
rule explicitly.

For records, a caller supplies an **enclosing records root**, never a shared current-run directory.
The dispatched workflow creates a unique child directory below it:

```text
<enclosing-records-root>/<workflow-id>/<YYYYMMDD-HHMMSS>-<unique-suffix>/
```

Without a caller, use the standalone records root named by the dependent prompt. Never reuse or
overwrite an earlier run directory. Within the current workflow record, use:

```text
logs/
reports/
snapshots/
scratch/agents/<agent-id>/
```

Create only the directories the workflow needs. Obtain every report timestamp with
`date "+%Y-%m-%d %H:%M %Z"`. Each standalone report, plan, log, audit, and handoff note carries that
timestamp and identifies its workflow, source snapshot, and status.

## Current-state and history-independence rule

Begin from an unknown future repository state. Discover the current checkout, tracked-file set,
source graph, defaults, counts, versions, paths, artifacts, and tool availability from current
authorities.

- Treat this prompt, old documents, old reports, memories, generated outputs, and earlier agent
  conclusions as leads unless the workflow-specific prompt makes one of them an explicit authority.
- Do not carry forward a count, hash, branch, version, file inventory, status, or scientific
  conclusion from an earlier run.
- Do not infer that a named path still exists. Verify it before relying on it.
- Do not embed a newly observed mutable value into a prompt as a permanent reference value. State the
  discovery rule and the invariant relationship instead.
- State fixed policy as a present rule without narrating when, why, or in which earlier run it was
  adopted.
- Use source and observed behavior only within the execution boundary of the current workflow. A
  static workflow cannot promote generated state into behavioral evidence.

Every final claim identifies the frozen source or artifact version it covers. The word "current"
means the selected and recorded snapshot, not remembered repository state.

## File ownership and change boundaries

Before writing, record a resource ledger with these fields for the orchestrator and every active
assignment:

- canonical targets;
- private records and scratch paths;
- repository read set;
- repository write set;
- Git-state permissions;
- scientific-state permissions;
- shared external or machine-local resources.

Preserve unrelated user changes. Inspect the complete initial Git status and compare it with the
final status. Attribute every task-related difference. Do not use destructive Git commands, broad
deletion commands, or shell redirection to replace source text. Use patch-based editing for text.

An ignored path is not automatically writable. A worker writes only within its assigned private
scratch directory. The orchestrator writes only the canonical targets and records authorized by its
workflow prompt. No worker or sub-orchestrator stages, commits, pushes, switches branches, creates
worktrees, rewrites history, or changes remote state unless a complete dependent prompt explicitly
makes that sub-orchestrator the Git-owning orchestrator.

The final report separates tracked changes, ignored records, generated artifacts, and machine-local
state. A clean `git status` cannot prove that ignored or untracked state was preserved; use direct
path inventories or digests when the workflow requires that proof.

## Worker dispatch contract

Delegate only when an independent assignment provides useful evidence or isolated context. Give
each worker one bounded, nonoverlapping scope. Every dispatch includes:

1. **Objective:** the exact question or audit slice.
2. **Output:** the unique private `response.md` path and the canonical artifact the orchestrator will
   synthesize from it.
3. **Tools and sources:** required skills, allowed commands, source roots, and evidence standard.
4. **Boundaries:** explicit read set, write set, Git permissions, scientific-state permissions, and
   exclusions.
5. **Stopping rule:** the finite condition that makes the assignment complete.
6. **Expected duration:** a planning estimate used to distinguish ordinary progress from a stall.
7. **Delegation rule:** whether nested workers are allowed and, if so, their disjoint scope and global
   capacity limit.

Before substantive work, a scratch-capable worker creates its assigned `response.md` with the
objective, scope, timestamp, source snapshot, and `Status: in progress`. It checkpoints completed
scope, evidence, remaining scope, and errors as it works. At handoff it records `Status: complete` or
`Status: partial` and returns only the path and status. The orchestrator reads the file and verifies
its claims against source.

A stateless reviewer returns its full evidence in the tool response. The orchestrator saves that
response under `reports/` with the reviewed snapshot identifier. Do not impose a disk-write
requirement on a reviewer whose interface cannot satisfy it.

If a worker disappears, inspect its durable record before reassigning uncovered work. A partial
report is evidence only for the scope it explicitly completed; it is never a clean verdict for the
whole assignment.

## Concurrency and resource arbitration

Concurrency is allowed only after the orchestrator proves it safe from the resource ledger.

Two assignments may overlap only when:

- their repository and records write sets are disjoint;
- neither reads a mutable path that the other may write;
- they do not mutate the same Git index, branch, package library, output tree, cache, manifest
  instance, temporary build directory, graph, or remote resource;
- each has a unique scratch path; and
- the workflow names any required barrier after both.

Use immutable copies for review inputs that another assignment might otherwise edit. A file's
canonical path is not a stable review input while its writer is active.

Respect the environment's global agent capacity, including the caller and sibling workflows. A
capacity limit changes scheduling, not coverage. Run assignments sequentially when no safe slot is
available. Do not reduce the audit, merge distinct review roles, or treat a transient capacity
rejection as a substantive blocker. Only a prompt that states an exact fan-out may require that exact
number of simultaneous top-level assignments.

The caller owns slot arbitration across sibling sub-orchestrators. A sub-orchestrator owns only the
slots allocated to it and may serialize its workers. Nested delegation is forbidden unless the task
envelope assigns a disjoint slice and a slot.

Dispatch work before recording it as launched. Wait for every assignment in a review round to become
terminal before applying any finding from that round. Probe a quiet worker and inspect its durable
record before declaring it stalled.

## Evidence and search discipline

Evidence precedes every completion claim.

- Read executable source, predicates, configuration, schemas, and recorded decisions that govern the
  claimed behavior.
- State what a command or static check proves and what it cannot prove.
- Quote only the decisive output needed for the record.
- Verify a worker's or sub-orchestrator's claim before incorporating it.
- Treat an external model review as advice, not authority.
- Use official primary documentation for an external interface or mathematical convention when the
  workflow requires outside verification.
- Never suppress a diagnostic, weaken a test, disable a hook, or omit a failed check to obtain a
  passing result.

An empty search result is not evidence of absence until the same command and quoting pattern matches
a positive control in the searched universe. Record the control and both counts. Read every match in
context before classifying it.

## Review snapshots and certification

A review certifies bytes, not a filename or evolving document.

1. Freeze the candidate and record its digest, byte count, and line count.
2. Copy it to a digest-named immutable snapshot and verify the copy byte-for-byte.
3. Give every reviewer in the round that snapshot and the same frozen source authority.
4. Record the fixed number of reviewers and scopes at dispatch.
5. Apply no edit until every reviewer in the round is terminal.
6. Verify each finding against the authoritative source and the full passage, then accept or reject it
   with evidence.
7. If a blocking finding requires an edit, apply it only after the round closes. Freeze a new
   snapshot and reopen the earliest affected gate plus every downstream gate.

Classify each review finding as:

- **Rule violation:** a breach of an explicit requirement or any change that damages accuracy,
  coverage, definitions, predicates, caveats, locators, or valid syntax. It blocks the gate.
- **Discretionary improvement:** accurate and compliant text that one reviewer merely prefers to
  phrase differently. It does not block; record the reason for declining it.

Each gate owns its defect class. Defer an out-of-class finding to the earliest gate that owns it.
Close a gate on zero rule violations, not on the absence of optional suggestions.

When a workflow requires fidelity, terminology, economics-writing, and clear-writing gates, apply
them in that order. Edits during a gate reopen that gate. An edit that may affect an earlier gate
reopens the earlier gate and all later gates. Completion requires one final immutable snapshot to
receive clean certifications from every required gate with no intervening edit. If any final
certification finds a blocking defect, stop the certification sequence, edit after the active round
closes, and restart at the earliest affected gate.

## Bounded failures and terminal status

Set a finite retry cap for each fallible external step. A retry must follow a new diagnosis or a
materially different remedy. Repeating the same failed command is not progress.

Use these terminal labels:

- **Complete:** every required deliverable and gate passed on the delivered version.
- **Partial:** safe independent work is complete, but a named gate or deliverable remains open.
- **Blocked:** a higher-priority prohibition or hard external condition prevents further in-scope
  progress after bounded attempts.

Never describe a partial or blocked workflow as successful merely because its commands finished.
Preserve the evidence needed to resume it. Report the last certified snapshot, open findings,
attempts, and exact remaining scope.

## Final reconciliation

Before reporting completion:

- reread every changed canonical text;
- verify all local paths and cross-references;
- recompute the frozen source and target evidence required by the dependent prompt;
- compare initial and final resource ledgers and Git status;
- confirm that no later write invalidated a certification;
- account for every worker and stateless review;
- run the dependent prompt's exact mechanical and visual gates; and
- state limitations without implying broader coverage.

Lead the final response with the outcome. Identify the delivered paths and versions, decisive gates,
task-attributable changes, preserved boundaries, and unresolved limitations. Do not rely on earlier
commentary as part of the handoff.
