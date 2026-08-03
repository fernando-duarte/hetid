# Prompt: run a graph update / repair / optimize pass

You are maintaining the graphify knowledge graph at `graphify-out/graph.json` in the `hetid` repo.

`CLAUDE.md` carries a standing instruction not to touch the graph unless explicitly asked. **Running
this pass counts as being asked; nothing else does.** Do not rebuild, update, or diagnose the graph
because a `PreToolUse` hook suggested `graphify query` — that hint fires on almost every tool call
and is not a request.

Work from the repo root of the **primary checkout**. Confirm with `git rev-parse --show-toplevel` and
`git rev-parse --abbrev-ref HEAD` before anything else.

---

## 1. What the graph is

~2,400 nodes / ~6,400 directed edges. Two layers, and the difference governs everything:

| Tier | Marked by | Who owns it | Survives an update? |
|---|---|---|---|
| **AST** | `source_location: "L<n>"` | graphify, regenerated from source | No — replaced for any re-extracted file |
| **Semantic** | `source_location: null` | hand-built, graphify cannot regenerate it | Yes — preserved unconditionally |

The rule is `graphify/build.py::_is_ast_tier`. The semantic tier (~129 nodes, ~238 edges) is the
generated-artifact layer, the doc-concept layer and the paper node. Setting `source_location: null`
on those nodes is what protects them; do not "fix" it to a line number.

What an update actually costs is narrow: the `recovered` call edges and `also_calls` markers on
**re-extracted files only**. Markdown heading nodes and the whole semantic tier survive — verified by
running a real update and observing edges on unchanged files move by exactly 0.

## 2. The routine pass

```bash
cd "$(git rev-parse --show-toplevel)"
python3 tools/graph/maintain.py verify          # baseline — note the numbers
rm -rf graphify-out/cache                       # ONLY if the graphify extractor itself changed
python3 tools/graph/maintain.py update
graphify query "which module writes the variance share table?"   # smoke test
git status -s                                   # must show no graph-related tracked changes
```

`update` reads `built_at_commit` from `graph.json`, diffs it against `HEAD` for changed `.R` files,
re-extracts only those (via `graphify.watch._rebuild_code(changed_paths=...)` — the `graphify update`
CLI does **not** do this; it passes `changed_paths=None` and re-extracts everything), re-injects the
artifact layer, re-applies the repair on the changed files, and runs the health gate. About 10s for
~58 changed files.

Add `--optimize` to also recluster and relabel. Off by default: it churns community IDs and the
report every run. Use it when `graphify` warns that the community set has drifted from the saved
labels.

`update` regenerates `tools/graph/artifact_manifest.csv` automatically when
`scripts-paper/config/artifact_manifest*.R` is among the changed files.

## 3. The health gate

`maintain.py verify` fails on `dangling`, `missing_endpoint`, `duplicate_pairs`,
`isolated_artifacts`, or a non-directed graph. Everything else is reported, not enforced.

Reference values (2026-08-02, built at `b3c06e0`; they drift with the repo — the shape matters, not
the digits):

| | |
|---|---|
| nodes / edges | 2,384 / 6,359 |
| semantic-tier nodes / edges | 125 / 232 |
| artifact nodes | 83, none isolated |
| markdown heading nodes | 77 |
| `recovered` / `also_calls` | 69 / 217 |
| self-loops | 4 |

**Do not treat these as corruption:** self-loops are real R recursion (`walk()`, `walk_direct()`,
`literal_path()`); ~218 relation-variant collisions and ~1,470 collapsed edges are inherent to
graphify's simple-DiGraph model. Only `dangling` and `missing_endpoint` mean the graph is broken.

## 4. Manual pass, by inspection

When the tooling is missing or you are fixing one thing:

1. `git diff --name-only $(python3 -c "import json;print(json.load(open('graphify-out/graph.json'))['built_at_commit'])")..HEAD -- '*.R'`
   — every file listed has stale node bodies; files *added* in that range have no node at all.
2. Re-extract one file: `extract_r(Path("rel/path.R"))`. **Pass a `Path`, not a `str`** (a `str`
   fails with `'str' object has no attribute 'read_bytes'` and returns an empty result that looks
   like "no code here"), and **use a relative path** — an absolute path bakes the machine path into
   node IDs, because the ID remap runs inside `extract()`, not inside the extractors.
3. Splice into `graph.json`: drop AST-tier nodes/edges whose `source_file` is that file, append the
   new ones. **Never touch semantic-tier items** (`source_location: null`).
4. **Keep every `(source, target)` pair unique.** The graph is a simple DiGraph; a duplicate pair
   collapses on reload and one relation is silently lost.
5. To ask "who calls X": count `calls` edges into its node id. If zero, also check for an
   `also_calls` marker on its incoming `defines` edge before concluding it is uncalled.
6. Count dangling edges *before* building. `build_from_json` silently drops them, so a finished graph
   always reads "100% verified".

## 5. Rebuilding the tooling if `tools/graph/` is gone

It is untracked (`.gitignore`: `tools/`) and has been lost once already. This section is the spec.
Four files; `.Rbuildignore` already excludes `tools/` from the package build.

### `manifest_csv.R`

Emits `tools/graph/artifact_manifest.csv` (~87 rows): `id, basename, group, old_path, new_path,
producer, consumer, status, family, variant`.

`scripts-paper/config/paths.R` refuses to run outside a package root, so build a throwaway root in
`tempdir()`: copy `scripts-paper/` **except `output/`** plus `DESCRIPTION`, `setwd()` there, then
`source(config/paths.R)` → `paper_source_once(paper_path("config","analysis_contract.R"))` →
`paper_source_once(paper_path("config","artifacts.R"))` → `write.csv(artifact_manifest, ...)`.
`analysis_contract.R` is required: it carries the tau contract the sweep/region manifest expanders
read, and without it the swept figure records never materialise.

### `layers.py`

Builds the artifact layer from that CSV. One node per record keyed `graphify.ids.make_id(new_path)`,
`source_location: null`, carrying `artifact_id`, `artifact_status`, `artifact_group`, `on_disk`.
Skip `.md` records — the markdown pass already owns those nodes. Describe figures from the manifest's
`id`/`family`/`variant`, **never from the file**: R's `svg()` device outlines every label into glyph
paths, so the figures contain no readable text at all.

Edges: producer `.R` → artifact (`defines`, context `artifact`); artifact → consumer (`references`,
context `artifact_consumer`), resolving `paper` to the `paper_manuscript` node and paths to corpus
files, emitting nothing for directories or prose; gate → conditional artifact (`references`, context
`conditional`).

**Look file nodes up from `graph.json`, never with `make_id`.** graphify's remap drops the extension
(`scripts_paper_config_paths`, not `..._paths_r`) and disambiguates repeated basenames — six files
here are named `run.R` and are labelled `fitted_volatility/run.R` and so on. Match `source_file`
where `label == basename or label.endswith("/" + basename)`.

Two modes: `--emit` (writes `.graphify_semantic.json` for a full rebuild, carrying `concept`/
`rationale` nodes forward from the current graph) and `--inject` (replaces the layer inside a built
`graph.json`). **Must be idempotent** — regenerate everything it owns, carry nothing of its own
forward, or a rebuild accumulates its prior output (this once turned 232 edges into 303).

### `repair.py`

Recovers the `calls` edges the simple-DiGraph build discards when a pair carries two relations. Every
collapse here is `defines` + `calls`, and `calls` always loses.

- **Parent is a function** (label ends `()`): re-attribute `defines` to the file node, freeing the
  pair, and keep/restore the `calls` edge. Stamp `recovered: true`, `reattached_from`. Skip if the
  file→child pair already exists — a duplicate pair would collapse and lose a relation anyway.
- **Parent is a file**: no re-attribution exists. Keep `defines`, stamp `also_calls: true` and
  `call_location`.
- **Delete nothing.** The redundant call edges still collapse at build time and the diagnostic still
  counts them; removing them only quiets the warning without repairing anything.

`--extraction` runs pre-build on `.graphify_extract.json`. `--graph` runs post-build and must
re-extract each file, because the collapsed edge is already gone from the built graph. In `--graph`
mode match endpoints **by label, not id** (extraction ids are pre-remap; graph ids are post-remap),
and normalise file labels to their basename first, or every disambiguated file silently misses.

### `maintain.py`

`verify` / `update` / `rebuild`, plus `--optimize`. Re-execs under
`graphify-out/.graphify_python` so a plain `python3` invocation works.

## 6. Landmines

- **Dropbox.** This checkout is inside Dropbox. **Never delete-and-recreate a directory here** —
  Dropbox reads the delete as spurious, restores the old files, and renames yours to
  `<name> (Fernando Duarte's conflicted copy <date>).<ext>`. Overwrite in place instead, delete
  leftovers as a separate step, and **verify deletions twice**. Restored files come back with
  `-rw-------` permission bits; that is the tell.
- **The AST cache ignores extractor changes.** It keys on file *content*, so after upgrading or
  editing the graphify extractor a rebuild silently replays stale per-file results. `rm -rf
  graphify-out/cache` first, or new edges go missing with no error.
- **graphify writes its own backups.** A rebuild creates `graphify-out/<YYYY-MM-DD>/` holding the
  previous `graph.json`, `GRAPH_REPORT.md`, `manifest.json`, `cost.json`. That is a safety net, not
  cruft — but old ones accumulate and can be pruned.
- **Absolute paths** given to `extract_r`/`extract_markdown` bake the machine path into node IDs.
- **A non-call `raw_call` must not carry a `callee` key.** graphify's shared cross-file pass turns
  any `callee` into a `calls` edge by name, so a doc/S3 record ships as a phantom call *and* blocks
  the real relation as a duplicate (upstream #1668 hit this with Ruby mixins).
- **The local `graphify` is a fork build** from `~/src/graphify-r2` (R support: upstream PRs
  Graphify-Labs/graphify#2393 and #2395). `uv tool upgrade graphifyy` is **safe** — the receipt pins
  a directory, so it rebuilds from that worktree. An explicit `uv tool install graphifyy` reverts to
  PyPI and R support disappears.
- **A parallel session may share this checkout.** Check `git status` before and after.

## 7. Reporting

State what changed and what it cost: nodes/edges before and after, which files were re-extracted,
how many repairs were applied, and the health-gate result. If the gate fails, say so with the failing
counts — do not describe a pass as successful because it completed.
