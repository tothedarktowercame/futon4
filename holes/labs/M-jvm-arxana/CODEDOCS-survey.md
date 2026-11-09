# Code ↔ docs correspondence: discovery survey

Date: 2026-11-09.  Discovery only: no service/store writes and no changes to
the legacy browser or futon4a.  The exercised slice is the one specified by
`holes/excursions/E-lean-code-docs-alignment.md`: two futon2 Clojure files,
two local `def-doc` stubs, and `SelfHealRoute.lean`.

## 1. What the legacy browser does

### Entry and display

The code section of the main browser is supplied by
`arxana-browser-code-items`, formatted by `arxana-browser-code-format` and
opened by `arxana-browser-code-open` (`dev/arxana-browser-code.el:403-448,
1720-1723`).  The direct configuration commands are
`arxana-browser-code-set-docbook`, `arxana-browser-code-strategy-status`,
`arxana-browser-code-reset-strategy`, and
`arxana-browser-code-normalize-frames`.  The docs pane has `RET` activate,
`e` open DocBook, and `C-c C-n` cycle paragraph; a transcluded Lean line has
`RET`/mouse-1 open declaration (`arxana-browser-code.el:235-249,1024-1056`).

`arxana-browser-code--open-path` opens the source file, installs
`arxana-browser-code-sync-mode`, and calls
`arxana-browser-code--display-docs`.  The latter renders and displays
`*Arxana Code Docs*` beside it (`:1198`, `:1657-1671`).  Point motion runs
`arxana-browser-code--sync-docs`: the current top-level definition is found,
the matching docs chunk is overlaid with
`arxana-browser-code-docs-highlight-face`, and the reverse direction uses
`arxana-browser-code--sync-code-highlight` and its separate target face
(`:1550-1655`).  The panes are therefore a projection over source and parsed
docs, not stored presentation coordinates.

### Sources and matching

Code roots are `arxana-browser-code-roots`; nil means the repository's `dev`
and `test`, while the self-heal slice sets the absolute futon2 AIF root
(`arxana-browser-code--resolve-roots`, `:80-83,384-424`).  One shared
`arxana-browser-code--definition-regexp` recognizes the Elisp/Clojure
definition forms and metadata introduced by excursion packet A (`:161-188`).

DocBook entries are primarily filesystem Org stubs beneath
`docs/docbook/<book>/`.  `arxana-docbook-entries` parses their property
drawers; `README-docbook.md` specifies `DOC_ID`, `ENTRY_ID`, `VERSION`, and
`SOURCE_PATH`.  The two slice stubs are:

- `docs/docbook/futon2/futon2-14b33d6f0473.org` →
  `src/futon2/aif/full_loop_runtime.clj`;
- `docs/docbook/futon2/futon2-cd090c67a861.org` →
  `src/futon2/aif/meta_self_heal_production.clj`.

Both have `VERSION: def-doc`.  For these, bullet names are retained only when
they are definitions in the resolved source (`--entry-def-doc-symbols`,
`:560-584`).  `--docbook-indices` indexes normalized source path, basename,
explicit `FUNCTION_NAME`, and these bullet symbols; `--docbook-matches`
prefers those indices and only performs loose text mention matching when
explicitly enabled (`:636-807`).  Thus the normal code→doc identity is
source-path plus definition name, not a line number.

Lean input is also file-backed.  `arxana-browser-code-lean-sources` maps a
code-path regexp to Lean files.  `--lean-declarations` parses top-level Lean
declarations and their doc/comments, excluding strings and nested comments;
`--lean-citations` considers a declaration to cite a Clojure definition when
its documentation/comment text contains the definition as an exact backticked
token (`:874-1022`).  `--insert-lean-lines` transcludes declaration name and
first doc sentence using `arxana-browser-code-docs-lean-face` (`:1043-1075`).
This is syntactic citation evidence, not kernel evidence.

### Link tiers

`arxana-browser-code-ensure-strategy` asks `arxana-links-find-strategy` for a
Tier-1 `arxana/link-strategy`, otherwise constructs finder declarations
`:symbol-as-term`, `:filename-mention`, and `:surface-form-hybrid`
(`arxana-browser-code.el:194-235`).  Tier 1 stores finder rules, not every
computed link (`arxana-links.el:262-360`).  The browser's local path/symbol
indices still work when that persistence is unavailable.

Tier 2 is `arxana-links-make-anchor`: SHA-256 of selected content, content
length, before/after context, hint offset and line (`arxana-links.el:636-665`).
`arxana-links-find-anchor` tries the hint with hash verification, exact
surrounding context, then a bounded fuzzy-context recovery (`:697-771`).
Tier 0 is the ephemeral file/path/symbol and citation computation described
above.  Promoted `arxana/voiced-link` records name source and target endpoints
plus the finding strategy (`arxana-links-make-voiced-link`, `:371` onward).

## 2. Clean-Emacs result

I ran the following read-only shape in `emacs -Q --batch`, with `dev` on
`load-path`, loading `arxana-docbook-core.el` and
`arxana-browser-code.el`, setting DocBook `futon2`, root
`/home/joe/code/futon2/src/futon2/aif`, and Lean source
`/home/joe/code/mathlib4/DarkTower/WarMachine/SelfHealRoute.lean`, then calling
`arxana-browser-code--open-path`, `--docbook-matches`, and `--lean-citations`
for each source file.

Observed output:

```
OK full_loop_runtime.clj matches=1 ... docs=*Arxana Code Docs* chars=911
OK meta_self_heal_production.clj matches=1 ... docs=*Arxana Code Docs* chars=4936
```

The first file produced four cited Lean declarations and the second produced
eleven citation-bearing symbol groups.  Representative correct links were
`prepare-diagnosis → RouteState`, `production-readiness → ReadinessTerm` and
`route! → RouteTerm`.  Therefore packets A–C still run from clean, local
files.  Batch mode cannot visually certify window placement or face colours;
that remains **unchecked** until the eventual ERT/interactive acceptance.

The store-backed Tier-1 lookup does not run.  A second clean batch loaded
`arxana-store.el` and `arxana-links.el`, enabled sync, and called only the GET
`(arxana-links-find-strategy "futon4")` against 7071.  Exact result:

```
Contacting host: 127.0.0.1:7071
RESULT=nil LAST-ERROR=(:reason protocol :detail "Malformed HTTP response"
 :context "http://127.0.0.1:7071/api/entities/latest?type=arxana/link-strategy&limit=200")
```

No persistence call was made.  This explains why the file-backed demo works
while promoted strategy reuse does not.

The noted mojibake is real.  For example
`docs/docbook/futon4/futon4-00a4b5656afb.org:12,16` contains `Emacsâ` and
`scriptâs`.  `rg -l 'â|Ã|ð' docs/docbook` found **33 of 269** Org stubs.  This
is a source-data defect; decoding it opportunistically in the projector would
hide revision identity and is not part of the correspondence port.

## 3. JVM record mapping

The following mapping uses registered operations in
`futon3c/arxana/schema.clj` and the implemented F1/F3 record shapes rather
than inventing a second graph:

| thing | JVM representation | existing support / gap |
|---|---|---|
| code file | artifact plus immutable UTF-8 revision | `revision/ingest`, `revision/get`, `revision/head`; existing |
| definition | manifestation-scope endpoint: revision id, stable scope id/kind `code-definition`, symbol/quote and codepoint range as evidence | F3 uses the same endpoint pattern in `formal_f3_correspond/source-endpoint`; finder/registration materializer is missing |
| DocBook entry | its Org stub as a separate artifact revision, plus a manifestation-scope for the `def-doc` bullet/chunk | revision machinery fits; DocBook materializer and declared entry metadata are missing |
| code↔doc link | `[:correspondence 1]` clusion with role-labelled `source` code scope and `target` doc scope; asserted relation kind `code-documents` | `clusion/assert`, evidence/assessment and `correspondence/query` fit; type specialization and projector are missing |
| Lean declaration | entity keyed by `(Lean artifact revision, fully-qualified declaration name)`; range is revision evidence, not identity | implemented precedent in Formal Arxana F3 (`formal_f3.clj`) |
| Lean citation of code | correspondence clusion from Lean declaration entity to code-definition scope, relation kind `cites-code`; parser attestation records backtick method/version | F3 correspondence/evidence fits; the legacy parser must become a versioned materializer, and kernel standing must not be inferred |

`entity/get` and bounded `graph/query` can read these records.  `scope/resolve`
and `scope/at-position` provide lookup; `formal-f3/correspondence` is a useful
projection precedent but is fixture-specific.  Missing common pieces are:
(1) declared code-definition/DocBook scope materializers; (2) a versioned
Clojure-symbol finder; (3) a citation parser record; (4) a registered
code-docs projection returning chunks, Lean transclusions and all record ids;
and (5) the futon4a two-pane client.  The legacy `:options-key` finder remains
outside this key demo and should not block it.

## 4. Surviving edits

Legacy Tier 1 recomputes path/symbol rules over current files.  Renaming a
symbol is consequently a semantic change and will not silently match.  Tier 2
first checks the stored offset and content hash, then context plus hash, then
fuzzy surrounding context; its API returns one bounds pair or nil and does
not represent multiple candidates (`arxana-links-find-anchor`).  That makes
its ambiguity semantics weaker than JVM Arxana.

F1's `formal_f1_relocation/strategy` registers a content-addressed strategy
containing content hash, codepoint length, context and hint.  `relocate`
evaluates it against an explicit target revision and preserves
`:unique`, `:ambiguous` (all candidates), `:missing`, and stale-head outcomes;
`relocation-attestation` appends evidence without mutating the correspondence.
F2 adds application-hunk metadata for changed/deleted regions.  The same
machinery applies to code bytes without making code identity equal to an
offset.

The excursion's old/current check should run entirely on a private stack:

1. choose and record a historical futon2 commit (do not use a moving branch),
   obtain each source with `git show <sha>:<path>`, and ingest those exact bytes
   as historical artifact revisions;
2. ingest the current worktree bytes as successor revisions with expected-head
   CAS;
3. materialize the same named definition scopes and code↔doc/Lean citation
   clusions against the historical revisions;
4. register their hash/context/symbol strategies, call `scope/resolve` against
   both historical and current revisions, and retain every candidate;
5. report `N historical / N current / N both`, plus unique/ambiguous/missing
   per link and the revision/strategy ids.  A renamed or deleted function is
   an honest missing/semantic-change result, not a projector failure.

`git -C /home/joe/code/futon2 log -- ...full_loop_runtime.clj
...meta_self_heal_production.clj` confirms many candidate historical commits;
the exact baseline is **unchecked and must be selected/pinned in that packet**.
No live revision was written during this survey.

## 5. Small-packet demo plan

1. **Fixture and encoding gate:** pin the historical futon2 SHA, current code,
   two DocBook stubs and Lean file; record byte digests and report mojibake
   rather than repairing it implicitly.  Private data only.
2. **Code scopes:** ingest both code revisions and materialize named Clojure
   definitions with exact codepoint ranges; prove symbol lookup on the three
   self-heal functions.  Private stack only.
3. **Doc scopes:** ingest the two Org stubs and materialize each `def-doc`
   bullet; assert source-path/symbol correspondence candidates.  Private only.
4. **Lean citations:** ingest Lean, materialize FQ declarations and parser
   attestations for exact backticked citations; explicitly label them parser
   evidence, not kernel truth.  Private only.
5. **Correspondences:** assert role-labelled code↔doc and Lean→code clusions;
   add one bounded public projection returning doc chunks, Lean transclusions,
   standings and record ids.  Private only.
6. **Edit robustness:** run the historical/current resolution procedure above,
   including an intentional duplicate to prove ambiguity is not first-picked;
   publish the survives-edits count and trace.  Private only.
7. **futon4a two-pane client:** code buffer on the left and a JVM-projected
   `*Arxana Code Docs*` on the right; point invokes `scope/at-position`, both
   panes highlight every corresponding scope, Lean uses a distinct face, and
   record ids remain inspectable.  Recorded-response ERT plus real private
   end-to-end acceptance.
8. **Private demo service:** pid-owned launch/stop scripts on a newly censused
   loopback port, never a shared demo port; exact M-x steps and Android-sized
   two-pane acceptance.
9. **Optional promotion:** only after Joe accepts the trace, decide whether to
   copy fixture records into live futon1b.  **This is the sole planned step
   that writes live :7073 and requires Joe's explicit decision.** The demo
   itself does not require live writes.

The shortest faithful port is therefore not a translation of all 1,745 lines.
It preserves the useful UI interaction, promotes the existing file heuristics
to attributable materializers, and delegates identity, ambiguity and edit
relocation to the JVM service.
