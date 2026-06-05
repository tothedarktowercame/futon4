# Witness design: "VSATARCS is invariantly up to date"

**The foundational invariant of the new era (Joe, 2026-06-01).** D6 of
`M-vsatarcs-invariants-integration`. claude-2 owns this witness; codex-2 owns the
queue/projection it plugs into. **Design note — no code until the shape is agreed**
(model-before-code; this whole mission exists because invariants were built by
assertion).

## The key finding: "up to date" is not one witness — it is THREE chains

Investigating the real VSATARCS surfaces (2026-06-01) shows three distinct ways it
can be stale, and **only two are mtime-witnessable**. A single `mtime(source) >
mtime(derived)` check (like the existing `I-invariant-queue-freshness`) is
necessary but **not sufficient**, and naïvely applied it cries wolf.

### Chain 1 — generated-prose render staleness  (`.aif.edn` → `.aif.md`)

`.aif.md` files are *generated from* `.aif.edn` via `futon5a/scripts/render_*.clj`
(`render_aif2_prose.clj`, `render_leaf_prose.clj`). A `.aif.md` older than its
`.aif.edn` *can* mean a stale render.

**But strict mtime is too brittle here (verified):** a `-nt` test flagged 7 files
"stale" (devmap-futon0/3a/4/5/6, leaf-2, leaf-6-4-2) — yet `date -r` shows the
pairs share the SAME DAY (e.g. devmap-futon0: edn 04-22, md 04-22). The `.edn` was
touched seconds after the `.md` during the same regeneration. **Strict
`mtime(edn) > mtime(md)` false-positives on every co-regenerated pair.**

→ **Witness method for chain 1:** NOT strict mtime. Either (a) a tolerance window,
or — better, matching the feeder's existing dedup approach — (b) record a
**content-hash of the source `.edn`** in the rendered `.aif.md` (or a sidecar) and
witness `hash(edn) == recorded-hash`. Hash-based is drift-exact and immune to
mtime jitter. Recommended.

### Chain 2 — feeder-target currency  (live substrates → `vsatarcs-alignment-completeness.aif.edn`)

`futon3c/src/futon3c/vsatarcs/feeder.clj` polls live substrates and appends novel
entries to `vsatarcs-alignment-completeness.aif.edn` (currently dated 05-25). "Up
to date" here = the feeder has run recently enough relative to substrate change.

→ **Witness method for chain 2:** feeder-heartbeat freshness — last-feeder-run
timestamp vs substrate change, within a declared staleness bound. This is the
feeder's *own* currency; the witness reads the feeder's status, doesn't re-poll.
(Cadence/safety per the mission's D5: hourly+backoff, no OOM, no restart.)

### Chain 3 — CONTENT drift  (the hard one; mtime CANNOT catch it)

`leaf-invariants.md` asserts in prose **"9 operational + 10 candidate families."**
The live inventory now has **15 operational** family forms. The story is wrong —
and it could be *newer* than the inventory and still wrong, because the claim is
**hand-typed prose, not a regenerated projection.** mtime is blind to this.

This is the invariants' own disease, reproduced in VSATARCS: a hand-asserted count
drifted from source. **It is the most important chain to witness** because it's the
one that makes VSATARCS untrustworthy as "what the stack is."

→ **Witness method for chain 3:** the only real cure is codex-2's projection — the
count must be *derived from* the live inventory, not asserted in prose. The witness
is: **does the story's stated invariant-state match the derived projection?** If
the story says 9 and the projection says 15, currency violation. This requires the
story to either (a) be generated from the projection, or (b) carry a checkable
claim the witness compares against the projection. Prose assertions with no
checkable binding are, by this mission's rule #8, not allowed to stand.

## The witness, assembled

`witness :vsatarcs-up-to-date` returns the standard probe shape
`{:outcome :ok|:violation|:inactive :detail ...}`, and is really a **conjunction of
three sub-witnesses** — VSATARCS is up to date iff all three hold:

```
:vsatarcs-up-to-date =
  chain-1 (render-hash-current, all .aif.md match their .aif.edn source-hash)
  AND chain-2 (feeder-heartbeat within staleness bound)
  AND chain-3 (story invariant-claims match the derived projection)
:outcome :violation names WHICH chain(s) failed + the specific drifted artifact(s).
```

**Render-before-content (mission D6/decision-log #5):** when this witness is
`:violation`, VSATARCS shows the currency violation *first*, ahead of the stale
content — so a stale surface announces itself rather than masquerading as current.

## What I did NOT do (honest)
- No code. This is the witness *design*; the implementation plugs into codex-2's
  projection artifact (whose schema is still being fixed in INSTANTIATE), so
  building the witness before that schema lands would guess at its inputs.
- I did not "fix" the 7 chain-1 flags — they're mostly mtime false-positives, and
  fixing them is regeneration work, not witness work.
- Chain 3's real fix (story generated-from / checked-against projection) is
  codex-2's projection territory; this note specifies the witness, not the
  projection.

## Open questions for codex-2 (the projection owner)
- Does the derived projection expose a stable "invariant-state summary" (counts +
  per-family status) the chain-3 witness can compare a story's claims against?
- Should chain-1 hashes live in the `.aif.md` front-matter, a sidecar, or the
  projection? (Recommend: wherever the feeder already records dedup hashes, for
  consistency.)
- Is `:inactive` (feeder not running) a `:violation` for "invariantly up to date",
  or a distinct "can't currently witness" state? (Mission rule: missing witness ≠
  false; but for the *foundational* currency invariant, an un-running feeder
  arguably IS a failure of "invariantly up to date.")
