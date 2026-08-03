# Prompt: run a graph update / repair / optimize pass

## Required shared contract

Read `docs/prompts/shared-workflow-contracts.md` completely before acting. This prompt extends that
contract. It does not restate the shared role, autonomy, history-independence, ownership, worker,
concurrency, evidence, retry, or completion rules.

You are maintaining the graphify knowledge graph at `graphify-out/graph.json` in the `hetid` repo.

`CLAUDE.md` carries a standing instruction not to touch the graph unless explicitly asked. **Running
this pass counts as being asked; nothing else does.** Do not rebuild, update, or diagnose the graph
because a `PreToolUse` hook suggested `graphify query` — that hint fires on almost every tool call
and is not a request.

**Work in whatever worktree and branch is currently checked out.** Do not switch branches, do not move
to the primary checkout, and do not ask which one to use. Confirm where you are first:

```bash
cd "$(git rev-parse --show-toplevel)"
git rev-parse --abbrev-ref HEAD      # whatever branch this is, work on it
```

The graph is intended to be machine-local and may be absent from a linked worktree. Section 0
verifies its current Git status, acquires the canonical graph lock, and seeds the selected worktree.
Section 7 verifies and ports the result while the same lock remains held.

## Your role and what you may write

You are the orchestrator of this pass. The caller, orchestrator, and worker roles follow the shared
contract.

**What you may write**, and nothing beyond it:

- the graph directory `graphify-out/` and the tooling directory `tools/graph/`, in the tree you are
  working in and — per §7 — in the primary checkout, but only after Section 0 verifies that they are
  untracked and ignored;
- your own workflow record, and each worker's assigned private scratch directory within it.

**What you must never touch:**

- **Any tracked file.** This pass changes no source, no test, no document, no configuration.
- **Git state, in either tree.** No `add`, `commit`, `push`, `checkout`, `switch`, `branch`, `stash`,
  `reset`, `restore`, `rebase`, or `merge`. You work on whatever branch is already checked out and
  never switch away from it. If a caller wants the result committed, the caller commits it.
- **The scientific pipeline.** Never run or source it, and never read generated scientific outputs,
  caches, or manifest instances as evidence.

`git status` must show the same tracked state before and after this pass. Check it at both ends and
report both; that check is what proves the pass stayed inside its boundary.

Workers write only to their private scratch directories, never to `graph.json`, the graph directory,
a tracked file, or Git state. You merge and verify their fragments.

Keep the durable record required by the shared contract. Record the selected graph candidate, lock
ownership, state before, each step, health-gate result, port-back, and lock release.

If a caller supplies an enclosing records root, create this prompt's unique workflow record under:

`<enclosing-records-root>/stage-k-graph/YYYYMMDD-HHMMSS-<unique-suffix>/`

Otherwise use:

`docs/RUN/maintain_graph/YYYYMMDD-HHMMSS-<unique-suffix>/`

In this prompt, "current workflow record" means that unique directory. It is not part of
`graphify-out/` or `tools/graph/` and must never be ported with the graph.

---

## 0. Lock and seed the workspace

The primary checkout is the canonical machine-local home for `graphify-out/` and `tools/graph/` when
both paths are untracked and ignored. Verify that status in both trees before writing. If either path
is tracked, stop because this prompt does not authorize tracked changes. Locate the primary from
anywhere — this works from the primary checkout and from any linked worktree:

```bash
HERE="$(git rev-parse --show-toplevel)"
PRIMARY="$(cd "$(git rev-parse --git-common-dir)/.." && pwd)"
echo "working in $HERE  (primary: $PRIMARY)"
```

`--git-common-dir` is the primary's `.git` even when called from a worktree, so its parent is the
primary root. **The primary checkout does not have to be on `main`** — it is just wherever the real
`.git` lives, and its branch is irrelevant to this step.

Before reading or copying canonical graph state, acquire an exclusive lock by atomically creating:

```bash
LOCK="$PRIMARY/graphify-out/.maintenance.lock"
mkdir -p "$PRIMARY/graphify-out"
mkdir "$LOCK"
```

On success, write the working tree, branch, process or task identity, and fetched timestamp inside
the lock directory. Hold this lock through Section 7. If it already exists, inspect its ownership
record and probe the named task or process. Wait only for the finite interval set in the execution
plan. If the owner remains active or ownership cannot be disproved safely, do not update or port the
canonical graph; finish read-only diagnostics and report a partial result. Never delete an active or
ambiguous lock. Reclaim a stale lock only when direct liveness checks prove that its recorded owner is
gone, and record that evidence.

If `HERE` equals `PRIMARY`, no seed copy is needed. Before Section 1, derive the complete planned
write set from the current tool contract, excluding the lock itself. Back up each existing path and
record every absent path inside the owned lock directory; verify the backup by digest. If the pass
cannot complete and verify, restore this exact pre-write state before releasing the lock. Do not
start a direct primary update without a recoverable backup.

Otherwise inventory the worktree and primary candidates. Required paths are:

| Path | Needed for |
|---|---|
| `graphify-out/graph.json` | the graph itself — without it, stop and report |
| `graphify-out/.graphify_labels.json` + `.sig` | community names, and the guard that stops graphify hub-renaming them |
| `graphify-out/.graphify_python` | interpreter marker `maintain.py` re-execs through |
| `graphify-out/manifest.json`, `cost.json` | incremental bookkeeping |
| `graphify-out/GRAPH_REPORT.md`, `graph.html` | regenerable, but seed them so the pass can diff |
| `graphify-out/cache/` | AST cache; skipping it may make the pass slower |
| `tools/graph/` | the tooling — also untracked, so a fresh worktree has none of it |

Discover the current graph field that records its source snapshot. The primary candidate is
canonical machine-local input. A worktree candidate may be preferable only when all of these
conditions hold:

- both candidates pass `maintain.py verify` or an equivalent structural check;
- its recorded source commit exists and is an ancestor of the worktree `HEAD`;
- the primary candidate's recorded source commit is not a closer ancestor of `HEAD`; and
- its semantic tier omits no primary-candidate node without an explicit current-source reason.

Compare commit ancestry and graph structure, never mtimes. Choose the eligible candidate whose
recorded source commit has the shortest committed distance to `HEAD`; break an exact tie in favor of the
primary candidate. If neither commit is an ancestor of `HEAD`, seed from the primary candidate and
use a full rebuild instead of an incremental update. Record the evidence and choice.

Copy the selected candidate's required items with `cp -R`, then point `.graphify_root` at the current
tree:

```bash
echo "$HERE" > "$HERE/graphify-out/.graphify_root"
```

Never combine graph metadata, cache, labels, or semantic content from different candidates
file-by-file. Treat the selected candidate as one coherent state.

---

## 1. What the graph is

Discover node and edge counts from the selected graph. Inspect the current graph schema and tier
classifier before editing. Record the actual marker that distinguishes generated material from
curated semantic material. The required ownership relationship is:

| Tier | Marked by | Who owns it | Survives an update? |
|---|---|---|---|
| **Generated source tier** | current extractor marker | graphify, regenerated from source | Replaced only at the scope declared by the current updater |
| **Curated semantic tier** | current semantic marker | model-reviewed | Preserved unless this pass explicitly revises it |

Do not assume a classifier path, attribute name, or sentinel from this prompt. Verify the marker in
the selected tool receipt and graph schema. If the current tooling cannot distinguish the two tiers
unambiguously, do not run a destructive update.

An incremental update may replace AST-tier material only for re-extracted files. Verify from current
tooling and before-and-after inventories that unchanged Markdown headings and semantic-tier entries
remain unchanged.

## 2. The routine pass

Inspect the selected tooling's help and source to discover its current verify, incremental-update,
full-rebuild, labeling, export, and query interfaces. Record the exact commands selected. Run the
baseline verifier, the least-destructive update that covers the current source delta, a read-only
query smoke test, and the tracked-status check. Invalidate only the exact cache whose current key
fails to cover an extractor change; do not delete a cache from a remembered rule.

Use the graph's verified source-snapshot field to compute its delta to this worktree's `HEAD`. An
incremental update is eligible only when that commit is reachable and the current updater proves it
covers every changed language, document, generated layer, and repair pass. Otherwise use the current
full-rebuild interface. Do not substitute a generic graphify command whose changed-path behavior has
not been verified.

After extraction, run the current clustering, labeling, report, and export steps in the order the
selected tooling requires. Verify that every eligible node participates, identical stably ordered
input is deterministic, the label sidecar matches the final partition, and exported views describe
the final graph rather than an intermediate state. Derive the changed-document semantic review set
from current tool output or a direct before-and-after inventory, then complete Section 5.

Verify idempotence with two consecutive forced updates against identical source. Node, edge, and
community inventories and the reported delta must remain unchanged. Investigate any movement before
trusting the result.

Determine from the current tooling whether an update refreshes
`tools/graph/artifact_manifest.csv`, which source declarations govern it, and whether the refresh
stays inside this prompt's no-pipeline boundary. Do not source repository R merely to recreate that
CSV. If the current graph tooling cannot refresh it without executing project R, leave it unchanged,
record the limitation, and do not treat it as scientific evidence.

## 3. The health gate

Derive the verifier's complete current failure classes from its source before running it. At minimum,
the gate must reject dangling or missing endpoints, duplicate node identifiers, relation collisions
that the current graph representation cannot preserve, missing required artifacts, an unexpected
graph directionality, and any round-trip loss. Do not assume that a reported diagnostic is enforced;
record which conditions affect exit status and independently adjudicate every other diagnostic.

Record current totals for nodes, edges, semantic-tier entries, artifacts, Markdown headings,
repairs, relation collisions, collapsed edges, self-loops, and communities. Derive acceptable
self-loops and relation collisions from current source and edge semantics. Never compare these values
with a prompt-embedded reference snapshot. The structural conditions named above remain failures
even if the current verifier reports rather than enforces one of them.

## 4. Manual pass, by inspection

Use a manual splice only when the current tool API, graph type, ID remapping, tier marker, and edge
collision behavior have been verified directly. Compute the changed-source set from the selected
graph's recorded source commit and the current `HEAD`. Use repository-relative source paths and the
current extractor input type. Replace only the generated tier for the affected source, preserve the
curated tier, validate identifiers and endpoints before building, and prove that serialization
round-trips without node or relation loss. If any of those contracts is unclear, use a verified full
rebuild or stop; do not apply a remembered manual recipe.

## Tooling recovery gate

`tools/graph/` is untracked executable code. A prose prompt is not a safe implementation source for
it. If the selected coherent candidate does not contain the tooling needed by the current pass:

1. search only the selected repository root and the canonical primary candidate for a complete copy;
2. establish its provenance and compatibility with the installed graphify receipt and current graph
   schema;
3. copy the complete tool directory as one unit and verify it before execution; and
4. if no authoritative compatible copy exists, stop the writing portion of this pass and report a
   blocker.

Do not reconstruct missing executable tooling from remembered code, an old report, or an earlier
run. Do not install or upgrade graphify to manufacture compatibility. Read-only JSON diagnostics may
continue, but no result from an unverified tool copy may be ported to the primary checkout.

## 5. The semantic doc-concept layer — the only LLM-authored part

This is the only model-authored graph layer. Other steps are tool-driven or evidence-based. Current tooling
may carry `concept` and `rationale` nodes forward, but it cannot establish whether their meaning still
matches the current document corpus.

Derive the corpus from the current tooling's declared Markdown/configuration inputs and verify that
each path exists. Inventory concept and rationale nodes by source file from the selected graph. A
missing graph cannot supply an authoritative semantic inventory; reconstruct the layer from the full
current corpus under the schema below.

### Writing one

- **`concept`** — a named thing the docs define: "Identified Set for the Structural Parameter",
  "Unified Mean/Volatility Bootstrap Stage", "Moving-Block Bootstrap Determinism".
- **`rationale`** — a decision, constraint or trade-off: "Pre-Push Hook Installation",
  "Executed-Code Manifest Cache Invalidation", "Skipped Maturities Widen the Identified Set".
- Only create a node for something that is itself a named entity or concept. A reason is not a node;
  put it in a `rationale` string attribute on the node it explains.
- Use the curated semantic-tier marker verified in Section 1. If the current schema uses
  `source_location: null`, preserve that exact value; do not attach a line location that would move
  the node into the generated tier.
- Derive the semantic-node ID construction from the current extraction schema. Use one
  repository-relative, deterministic form consistently and validate every resulting ID before merge.
- Attach edges to **real AST node ids looked up from `graph.json`**, never ids you compute. graphify's
  remap drops extensions and disambiguates repeated basenames; a computed id silently matches nothing
  and the edge is dropped as dangling at build time.
- Use only relation values permitted by the current extraction schema. Preserve the distinction among
  references, rationale, conceptual relation, implementation, citation, and data-sharing edges.
  `implements` links a documented concept to the function that realizes it and therefore requires
  direct source verification.

Read `.claude/skills/graphify/references/extraction-spec.md` completely and derive its current
confidence rubric, hyperedge rules, node fields, and legal values. Follow it; this section records
only the repository-specific semantic boundary.

### Running a semantic pass

This is model work, not tooling. **You are the extractor** — no API key, provider or backend is
required or assumed. If the host happens to have one configured, graphify's own pipeline can drive
it, but never depend on that and never stop to ask for one.

Partition the discovered corpus into bounded, nonoverlapping chunks under the shared dispatch and
global-capacity rules. Keep documents in one chunk when current cross-references show that they define
one system. Capacity may serialize chunks but must not reduce corpus coverage.

Three things that decide whether the results survive:

- **Each worker writes its fragment to its assigned private scratch path.**
- **Give every worker the repo-specific rules from this section**, not just the graphify schema:
  the verified semantic-tier marker, the current deterministic ID rule, and edge endpoints copied
  from `graph.json` rather than guessed. A worker may not invent an endpoint.
- **Merge, then verify before writing.** Dedupe by node id, splice into the semantic layer, and check
  0 dangling edges *before* the build — `build_from_json` drops them silently afterwards, so a
  finished graph always looks clean.

Give a worker the exact file list and schema, and have it return the fragment; do not have it edit
`graph.json` directly.

### When to revise

Derive the changed-document review set from the current updater or a direct inventory. Source
structure may refresh without updating meaning. Re-read each changed document and check its concepts
still hold — a renamed gate, a
dropped module, a reversed default all leave the old node standing and plausible.

Nothing detects a concept that has become wrong. That is the layer's weakness and the reason it is
worth writing down rather than trusting to memory.

---

## 6. Landmines

- **Dropbox.** This checkout is inside Dropbox. **Never delete-and-recreate a directory here** —
  Dropbox reads the delete as spurious, restores the old files, and renames yours to
  `<name> (Fernando Duarte's conflicted copy <date>).<ext>`. Overwrite in place instead, delete
  leftovers as a separate step, and **verify deletions twice**. Restored files come back with
  `-rw-------` permission bits; that is the tell.
- **Verify cache identity before reuse.** Determine whether the current cache key covers extractor,
  schema, and tool identity as well as source content. Invalidate only the exact cache when a missing
  dependency makes reuse unsafe.
- **graphify writes its own backups.** A rebuild creates `graphify-out/<YYYY-MM-DD>/` holding the
  previous `graph.json`, `GRAPH_REPORT.md`, `manifest.json`, `cost.json`. That is a safety net, not
  cruft. Apply the current retention rule; do not prune backups ad hoc during this pass.
- **Path and relation normalization can change IDs or edges.** Verify the current extractor's path
  normalization and relation schema. Use repository-relative paths, and do not attach a call-target
  field to a non-call record when the current cross-file pass interprets that field as a call.
- **Verify the installed graphify build before use.** Inspect the current tool receipt and prove that
  the selected build supports the repository's languages and required APIs. Do not upgrade, install,
  or replace it during this pass. A package name or remembered source checkout does not prove which
  build the executable uses.
- **Another session may share this checkout.** The canonical lock governs graph writes. Check tracked
  status before and after as a separate boundary.

## 7. Port the result back to the primary checkout

When the pass is finished and the health gate passes, synchronize the graph while still holding the
Section-0 lock. The lock prevents another compliant maintenance writer from changing the canonical
destination concurrently. If lock ownership was lost or cannot be verified, do not port; report a
partial result.

Preflight the complete source set and its digests before changing the destination. The port set
contains every graph companion required by the current tool contract and, only when the authoritative
selected candidate supplies a compatible tool directory missing from the primary, that complete
verified tool directory. Never port a partial tool set.

If `HERE` differs from `PRIMARY`, record the destination's existing path inventory and digests and
place a recoverable pre-port backup inside the owned lock directory. Stage every replacement under
unique temporary names in the destination filesystem, including a complete cache directory when the
current tool contract uses one. Verify staged digests against the source. Do not promote any staged
item if this preflight is incomplete. If `HERE` equals `PRIMARY`, use the verified pre-write backup
from Section 0 and skip staging.

If `HERE` differs from `PRIMARY`, promote the verified support files and cache first and
`graph.json` last. Rewrite `.graphify_root` to `PRIMARY`. If `HERE` equals `PRIMARY`, skip the copy
and promotion steps; all verification, rollback, and lock-release rules still apply.

Run the current verified health command from the primary checkout, then compare every canonical
destination path with the accepted worktree source by digest and schema-relevant inventory. If a
promotion or verification fails, restore the exact pre-port inventory from the backup and verify the
rollback. For a direct primary pass, restore the Section-0 pre-write inventory instead. A verified
rollback permits lock release with `Partial` status. Keep the lock for manual
recovery only if rollback itself cannot restore and verify coherent canonical state; record the exact
remaining differences and ownership data.

After a successful port or verified rollback, verify that the lock owner is this workflow, remove
only its exact lock directory, and confirm its absence. Never report success merely because copy
commands returned zero.

Three things this must respect:

- **Copy file-by-file with `cp -f`. Never `rm -rf` the destination directory first.** The primary
  checkout is inside Dropbox, which reads a delete-then-recreate as a spurious deletion, restores the
  old files, and renames yours to `<name> (Fernando Duarte's conflicted copy <date>).<ext>`. Recheck
  the destination once after copying; if conflicted copies appeared, the conflicted file is yours —
  `mv` it back over the canonical name.
- **Rewrite `.graphify_root`** to the primary path, or later runs there resolve against the worktree.
- **Say what the primary's graph now describes.** After the port it reflects *this worktree's
  branch*, and its recorded source commit may not be on the primary's branch at all.
  That is intended, but state it plainly in the report rather than leaving it to be discovered.

---

## 8. Reporting

State which worktree and branch you used, lock acquisition and release, the selected seed and its
provenance, nodes and edges before and after, re-extracted files, applied repairs, semantic-review
coverage, health-gate result, and port-back digest verification. If any gate fails, report its counts
and terminal status; command completion alone is not success.
