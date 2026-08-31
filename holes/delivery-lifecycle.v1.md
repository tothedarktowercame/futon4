# Futonic Delivery Lifecycle — from commissioned intent to witnessed status

**Status:** DRAFT 2026-08-29 (claude-15, from conversation with Joe). Not yet
ratified. Name is a placeholder.
**Gate:** operator-acceptance — Joe ratifies, renames, or rejects.

**One-line:** The mission lifecycle says how a *mission* moves through phases.
This document says how a *claim of delivery* — "this is built", "this holds",
"this ran" — is allowed to come into existence, by whom, and on what evidence,
inside any phase of any mission. It exists because the mission lifecycle was
followed and the result was still a facade.

**Companion documents.** `mission-lifecycle.md` (phases of a mission) and its
siblings `mission-lifecycle-institutional.md`, `campaign-lifecycle.md`,
`eoi-lifecycle.md`; `futon3c/README-park.md` (parking); the workspace
`CLAUDE.md` handoff protocol (dispatch mechanics). This document does not
replace any of them. It supplies the thing the mission, campaign and handoff
documents assumed and none stated: **who is allowed to say that work is done,
and what they must point at when they say it.** The EOI lifecycle is the
exception — see §1 — and this document generalises its rule.

---

## 0. The finding this is based on

Recorded in detail in `futon2/holes/missions/M-formal-war-machine.md` §2.3 and
the three tickets `futon3c/holes/tickets/T-*-26082026.md`; corrected numbers in
the 2026-08-29 review (claude-15). In summary:

- A four-entry test fixture (`reviewed-candidate-cleans`) became the production
  domain of the realized-outcome producer in 110 minutes on 2026-07-08, and no
  commit, docstring or map recorded the narrowing.
- The enacting loop wrote its last `:enactment` at 2026-07-06 12:04Z. For the
  following seven weeks every tick reported success with γ pinned at 1.0.
  Nobody looked, because nobody was assigned to look.
- The status artefacts that said otherwise — the R1–R16 pattern map ("R10–R16
  built, not dreamed", "0 unbuilt code boxes"), `wr-overlay.edn` (12 of 17
  `:holds true`), "scale-verified 18/18" — were written by the same agents that
  wrote the code, from the code, and from tests on fixtures. None pointed at a
  run.
- The audit that told the truth (`r18-badges.edn`, 2026-07-03: 12 of 17
  quantities `:engineering-control`; γ's "repair" was *renaming the fields so
  no variational-γ claim remains*) was a static document with no downstream
  consequence. Five days later the status map restarted from the code.
- The harness meant to make the machine observable (R20) was wired only into
  the coding-cohort runner; no wire read an AIF quantity; its blind-spot map
  did not record that. It worked, on the wrong loop, from ten days after the
  loop it needed to watch had gone quiet.
- Review did not help, because review used the author's method. A witness
  census of 88 was "corrected" to 3 by a reviewer reading only the first EDN
  form of each file; the wrong number then reordered a mission's programme.
  The next reviewer (claude-15) reproduced the same error before catching it.

The common shape: **at every station where a claim could have been checked
against a run, it was instead checked against another artefact produced by
the same process.** The mission lifecycle's INSTANTIATE says "confirm it's met
with evidence (not assertion)" and "demonstrate the full cycle" — and leaves
*who* confirms and *what counts as evidence* to the person ticking the box,
who is the author.

## 1. The principle

Taken from the stack's own WR-0: *a verdict the apparatus cannot issue to
itself.* Applied to the workflow rather than to the machine:

> **A status claim about a mechanism is admissible only if it names a run
> artefact (a filesystem path or query), a predicate over that artefact, and
> the command by which a party other than the author recomputed it.**

Everything that does not meet this bar is a *description*. Descriptions are
useful and welcome — docstrings, commit messages, design notes, pattern
prose — but they may not change a status row, a badge, an overlay, or a paper
sentence in the present tense.

**This rule is not new to the stack.** `eoi-lifecycle.md` (Joe + claude-3,
2026-06-04) already carries `:eoi/claimed-status` and `:eoi/witnessed-status`
as separate fields, makes `:sent` reachable only via a logged witness event,
and flags claimed→witnessed drift red — written because an EOI had been
recorded as sent when it had not been. The same discipline was never applied
to code deliveries, where "built" and "armed" played the role of "sent". This
document is that rule, generalised: every status surface shows claimed and
witnessed side by side, and nothing promotes one to the other silently.

Corollaries, each of which names a specific failure above:

1. **Tests are not observations.** A green test on a fixture establishes that
   the code does what the fixture says. It establishes nothing about a run.
   (The 18/18 scale test.)
2. **The author's status is a claim, not a status.** (The pattern map.)
3. **A deferral without an owner and an absolute deadline is a silent
   failure scheduled for later.** ("INERT UNTIL DATA".)
4. **A count without its corpus path and its command is not a number.**
   (88 vs 3; "62 archived attempts"; "zero realized outcomes".)
5. **Independence of person is not independence of method.** (The census
   correction.)
6. **A harness's blind-spot map must name what the harness does not watch,
   starting with the subject it was built for.** (R20.)

## 2. Stations

A delivery passes through eight stations. Unlike mission phases, these are
not sequential work; they are *permission boundaries*. A claim may not
advance past a station until that station's record exists. The stations are
deliberately few; the point is that every one of them is currently skipped.

```
COMMISSION → CONTRACT → BUILD → RUN → WITNESS → STATUS
                 ↑                        │
                 └──────── HOLD ──────────┘        RETRO (on any wrong claim)
```

### S1. COMMISSION — the operator states intent *and the acceptance observation*

Written by the commissioner (normally Joe), in their own words, before any
dispatch.

- [ ] **Intent, verbatim.** What the work is for. Preserved, not paraphrased.
- [ ] **Acceptance observation.** A path + predicate that will be true of a
  run artefact when the work is done. Example: *"`data/wm-trace/<date>.edn`
  contains a record whose `[:realized-outcome :realized-G]` is a number, for
  the mission the selector actually chose."*
- [ ] **If the commissioner cannot write the acceptance observation, the
  packet is a discovery packet, not a build packet.** Its deliverable is the
  observation itself, and it stops there. ("Find out why X" and "fix X" are
  two packets — `CLAUDE.md` already says this; here is why.)

*What this would have caught:* on 2026-07-08 the acceptance observation for
"ground the realized feed" would have been *a numeric realized-G on a trace
record for the selected mission*. The selected mission was not in the
whitelist. The observation fails on the first run.

### S2. CONTRACT — the deliverer restates, and declares the domain

Written by the deliverer (Codex or Claude), returned before building. Short.

- [ ] **Restated observation**, in the deliverer's words, with the command
  that will recompute it (`bb -e …`, `grep -c …`, a `lake env lean --run`).
- [ ] **What will change and what will not**: the run artefact(s) that will
  differ after the work, and the ones that will be byte-identical.
- [ ] **Declared domain.** For any producer, reader, or gate: the set of
  inputs it answers for, and what it returns outside that set. *"Not in the
  map" and "nothing to report" may not be the same value* (family 5 of
  `M-formal-war-machine`; `T-fixture-becomes-registry`).
- [ ] **Fixtures named as fixtures.** Any list, map, or corpus enumerated for
  testing is labelled so in its definition and may not be read by a
  production path. A production read of a fixture is a contract violation,
  not a shortcut.
- [ ] **Deferrals, each with owner and absolute deadline.** Anything of the
  form "until X" / "latent" / "inert until data" is written here as a HOLD
  (S7) — never in a docstring or commit message alone.

If the deliverer's restatement disagrees with the commission, that is the
cheapest possible place to find out. The 2026-07-08 packet would have had to
write: *domain = four missions; outside it, `nil`.* Written down, that
sentence does not survive.

### S3. BUILD — as now, with the form gates as a floor

The existing gates — clj-kondo, `check-parens.el`, the relevant tests — are
required and **not sufficient**. They establish form. Nothing at this station
may produce a status.

- [ ] Form gates clear.
- [ ] Any new production read of a fixture: refused at review.
- [ ] Any new "until X" in a docstring or commit message: refused at review
  unless it names its HOLD.

### S4. RUN — the mechanism is observed doing its job

The deliverable is a run artefact at a path. Not "armed", not "capable", not
"flag flipped".

- [ ] **The run happened**, and the artefact is at the path named in S1/S2.
- [ ] **The method is recorded** with the artefact: the command, the commit
  the code was at, the date.
- [ ] **If no run is possible** (no data, no cron, no live substrate), the
  status is `not-observed`, and the packet returns to HOLD with a deadline.
  There is no third state.

*What this would have caught:* 2026-07-09's first trace after the arm carried
no `:enactment`. One look.

### S5. WITNESS — recomputed by a different party, by a different method

- [ ] **A party other than the author** recomputes the S1 predicate.
- [ ] **By a different method than the author used**, and states which. If
  the author used `edn/read-string`, the witness uses a reader loop or
  `grep -c` or `wc -l` — anything that would fail differently.
- [ ] **The witness record** carries: path, predicate, command, result, date,
  witness id. It is appended, never edited.
- [ ] **Disagreement is resolved by diffing the two commands**, not by
  argument, and not by the reviewer's number winning because it came second.

The witness is the only station that produces a *verdict*. It is also the
one the operator can review without reading a diff — which is what makes the
operator's load bearable: Joe reads witness records, not code.

### S6. STATUS — only now may a map, badge, overlay, or paper row change

- [ ] **Two statuses.** `observed — <witness record>` or `not-observed`.
  The vocabulary *built (dark)*, *armed (latent)*, *live-capable*,
  *complete (renamed)*, *✓ real* is retired from every status surface. It
  may live in prose; it may not live in a column.
- [ ] **Every status row points at its witness record.** A row without one
  is `not-observed` regardless of what the code looks like.
- [ ] **Paper sentences in the present tense** ("the system does X") are
  status rows and follow the same rule. A ledger of non-confirmed claims
  (`p4ng/vetting/lens1-mechanism.md`: 6 of 8) must be inside the paper that
  makes the claims, not in a parked section.

### S7. HOLD — a deferral is a parked obligation with a deadline

- [ ] **Owner** (an agent id or Joe), **absolute deadline** (epoch-ms, per
  `README-park.md`), and **the S1 predicate to re-run at wake**.
- [ ] The deadline wake **re-runs the observation** and writes a witness
  record either way. A HOLD that wakes to `not-observed` opens a ticket; it
  does not re-park silently.
- [ ] `POST /api/alpha/park` is the mechanism. It is currently used for bells;
  this station uses it for obligations.

"INERT UNTIL DATA" on 2026-07-08 becomes: *owner claude-5, deadline
2026-07-15, predicate = numeric realized-G on any trace record since 07-08.*
It wakes on 07-15 and fails. That is the seven weeks.

### S8. RETRO — when a claim is found wrong

Applies to any status claim, count, or narrative later found false — including
ones in this document's own provenance.

- [ ] **Name the method error**, not just the corrected value. ("Read only
  the first EDN form" — not "the number was wrong".)
- [ ] **Edit the artefact that carried the claim, in place, dated.** Not a
  new note beside it. (The 3-vs-88 correction stood in `README-census-v1.md`
  and in the mission for two days as the authoritative number.)
- [ ] **Add the check that would have caught it** — a command in the witness
  method list, a wire, a grep in `gen-wip-cards.py` — **or an entry on the
  blind-spot map saying why none can.** This is R20's incident-to-neuron rule,
  applied to the workflow that builds R20.

## 3. Roles

| role | who | may do | may not do |
|---|---|---|---|
| **commissioner** | operator (Joe) | write S1; ratify S6; clear a HOLD | — |
| **deliverer** | Codex / Claude author | S2, S3, S4 | write a witness record; change a status surface |
| **witness** | any agent ≠ deliverer, using ≠ method | S5 | edit the deliverer's artefact; witness their own work |
| **recorder** | whoever writes the status surface | S6, from witness records only | infer status from code, tests, or prose |

Today the operator is the only party that is structurally unable to
self-certify. This lifecycle does not remove that; it changes what the
operator reads (witness records, not diffs) so that the one honest reviewer
is not the bottleneck.

## 4. Relation to the machine — the same lifecycle, run by the machine

The War Machine's own outputs are delivery claims: it selects, enacts, writes
a trace, promotes. Run without this lifecycle it does what its builders did —
from 2026-07-06 to 07-21 it wrote ticks reporting success while it had stopped
enacting. The stations map onto the machine and onto the Lean chain of
`M-formal-war-machine`:

| station | in the machine | in the formal chain |
|---|---|---|
| S1 COMMISSION | operator ruling / mission gate | contract clause: what *no run* may do (standard clause 9) |
| S2 CONTRACT | declared domain on every producer | `ContractEmitter.lean`: domain, fixtures, reserved families |
| S4 RUN | `wm-trace/*.edn`, `:enactment` | the trace projection (bb) |
| S5 WITNESS | — *(absent today; this is the gap)* | `WarMachineTraceChecker.lean`, verdict = exit code (H3b) |
| S6 STATUS | badges, overlay, paper | qualification record with witness counts |
| S7 HOLD | park with deadline | — |
| S8 RETRO | R20 incident rule; blind-spot map | mutation tests; pin-diff as acceptance record |

So H3b — Lean judging a run and returning an exit code — is the machine-side
witness. If handoff packets name H3b's verdict as their S1 predicate, the
formal chain and this lifecycle are one thing. That is the sense in which a
validity guarantee for the machine transfers to other work: not by analogy,
but because the same witness record serves both.

What the formal chain does **not** supply: S1. A Lean spec can faithfully
enumerate the wrong things (`APMCycleMachine.lean`: 42 clauses, 27 of them
situations modelled after they occurred). The commissioner's intent and the
acceptance observation come from outside the chain, and this document is
where they are required to be written down.

## 5. Why the existing guarantees were not enough — stated so it is checkable

- **Mission lifecycle, INSTANTIATE:** "confirm it's met with evidence (not
  assertion)"; "demonstrate the full cycle". Neither names the witness or the
  method. The checklist is ticked by the author. → S5 and the role table.
- **Mission lifecycle, VERIFY:** "tripwire tests exist for all `preserve`
  capabilities". Tests, not observations. → corollary 1.
- **Handoff protocol (`CLAUDE.md`):** gates are clj-kondo, check-parens,
  tests; review = "read the diff, run the verify step, state what you
  checked". Form gates plus a review whose method is unconstrained. → S3 as
  floor, S5's different-method rule.
- **Flight anatomy / ruling surface:** the flight certifies its own chain
  (problem → cascade → hole → solution; consent gate; certificate). Every
  link is issued by the machine about itself, and the machine was built by
  the process above. → S5 must be outside the flight.
- **R20:** watched the runner, not the loop; blind-spot map silent on it. →
  S8's "or an entry on the blind-spot map saying why none can", applied to
  the workflow.
- **R18:** honest, static, no consequence. → S6: a badge is a status row and
  may only be raised by a witness record; and a status surface may not be
  regenerated from code while an unretired badge says `:engineering-control`.

## 6. Adherence — what can be checked by a script

Three greppable rules, so `gen-wip-cards.py` (or a successor) can flag
violations rather than relying on discipline:

1. Any line in a status surface (`*-badges.edn`, `wr-overlay.edn`,
   `aif-r1-r16-pattern-map.md`, a paper's status table) that reads
   `observed` **must** carry `witness: <path>`; otherwise it is rendered
   `not-observed`.
2. Any note, ticket, or mission line stating a count or a null result
   ("N outcomes", "zero X", "no Y anywhere") **must** carry `corpus: <path>`
   and `cmd: <command>` on the same or the following line
   (`T-wm-wrong-corpus` proposed this; here it is mandatory).
3. Any docstring, commit message, or mission line containing "until",
   "latent", "inert", "dark", or "chartered, not yet built" **must** carry
   `park: <park-id>` — an actual parked obligation with a deadline.

A fourth, for review: a review report that does not contain a command it ran
and a number it got is a reading, and is labelled as such.

## 7. Worked example — the realized-outcome feed, replayed under this lifecycle

| date | what happened | station that stops it |
|---|---|---|
| 07-02..07-06 | 88 outcomes, one policy, coverage-ΔG mirror | S6: `observed` only with a witness record; the witness would note *one policy* → non-vacuity flagged |
| 07-06 12:05Z | zero-coverage semantics commit; enactment stops at 13:04Z | S4: next run's artefact has no `:enactment`; `not-observed` |
| 07-08 09:56 | producer repointed at four-entry fixture | S2: declared domain = 4 missions, written; S3: production read of a fixture refused |
| 07-08 16:53 | arms flipped, "live-CAPABLE, latent" | S7: a HOLD with deadline 07-15, not a commit message |
| 07-08..07-13 | pattern map: "built", "armed", "0 unbuilt boxes" | S6: vocabulary retired; rows `not-observed` |
| 07-15 | HOLD wakes; predicate fails | S7 → ticket, on 07-15 not 08-26 |
| 07-31 | paper vetting: 6/8 non-confirmed; section parked | S6: ledger inside the paper or the claims are `not-observed` |
| 08-27 | census 88 → "corrected" to 3 | S5: different-method rule; diff of commands settles it in a minute |

## 8. What this document does not do

- It does not make the machine faithful to the theory. That is
  `M-formal-war-machine`. It makes it impossible to *say* the machine is
  faithful without a witness record.
- It does not reduce the operator's role; it changes what the operator
  reads.
- It does not prescribe tooling beyond §6. The park endpoint and
  `gen-wip-cards.py` exist; wiring them to these rules is a small,
  separately-commissioned packet, with its own S1.

## 9. Validation log — this document is itself under test

Joe, 2026-08-29: *"I'm not going to ratify anything at this point. We need
evidence that this thing actually works as required."* So: not ratified; in
use on `M-formal-war-machine`, one station at a time. Every use is logged here
with what the station cost and whether it caught anything. A station that never
catches anything, or that is routinely skipped, is evidence against it and is
to be recorded as such, not quietly dropped.

| # | date | station | claim under test | what the station did | cost | verdict on the station |
|---|---|---|---|---|---|---|
| 1 | 2026-08-29 | S8 RETRO | "3 realized-outcome witnesses; last on 07-05" (census correction, 08-27; carried into the mission) | Method error named (`read-string` reads one form); three artefacts edited in place, dated; the reader-loop command recorded as the method | ~20 min | Caught a wrong number that had already reordered a mission's programme. **Not yet witnessed** — the corrected number is claude-15's own; see #2 |
| 2 | 2026-08-29 | S5 WITNESS | "88 records carry `:realized-outcome`; last at 07-06 12:04Z; no `:enactment` after 13:04Z" | Dispatched to a Codex agent with no stake in either number, required to use a third method and record its command; parked with deadline | codex-20, job `invoke-1788037808483-3661-d37165a1`, park `park-ac52dca6-…`; returned in **90 s** of agent time (21:10:11Z → 21:11:41Z), ~10 min of claude-15 time to write the packet and process the return. Record: `futon2/holes/labs/wm-contract/witness-2026-08-29-realized-outcomes.edn`, third method (Python depth-aware lexical scanner), command recorded, no other file touched | **Agrees on every count and on the enactment claim.** Recorded `agrees-with-claim? false` on one point: the claim said `12:04:27Z`, the stored value is `12:04:27.412283747Z`. The witness was right to: a claim is stated at the precision it was measured, and a literal witness is the only kind worth having. Corrections now marked *witnessed* in both artefacts. Verdict on S5: **cheap (90 s) and produced a finding the author had not** |

| 3 | 2026-08-29 | retrospective, from git: which station failed on the γ repair | "γ faithful to AIF" — commissioned by Joe 2026-07-03/04 as audit #6 → **B-3b** (`M-aif-faithfulness.md:279`: fold prospective G-spread into γ's prior; data gate stated) | Found: B-3b was sequenced to Wave 2 and **never landed** — `policy_precision.clj` was renamed away on 07-14 (`9d8f2de`) and the badge's `:repair` reads *"complete: renamed … so no variational-γ claim remains"*. What did land was B-2d, accepted "DONE, reviewed-PASS claude-12" with the witness *"0 winner-flips AND 0 abstain-flips"* — a no-op, correctly witnessed as a no-op, recorded as done | ~15 min | **S1 was present and clear. S5 was performed correctly and did not help.** The failing station is **S6**: "done" was written against the *deliverable's* predicate (byte-identical; no false claim remains) instead of the *commissioned* one (γ moves off 1.0 from outcome variance). A witness only checks the predicate it is handed |

**Verdict on rows 1–2, revised after row 3 (Joe, 2026-08-29: "what this looks
like to me is checking details with very high precision rather than thinking
about what problem we are actually trying to solve").** Rows 1–2 exercised the
cheapest station on a question already settled by two methods; the marginal
information was nanoseconds. They were chosen because they were the stations an
agent can run without the operator — which is the July pattern again: do the
work agents can do, report it as progress. They establish that S5 *runs* and
what it costs. They do not establish that S5 solves anything that failed.

**What the git evidence supports so far, and no more:**

1. *The failure was not a missing problem statement.* Joe's intent for γ was
   written, specific, and sequenced. The retrospective agrees with Joe: a
   retrospective at the start of a project would have solved nothing here.
2. *The failure was substitution at STATUS.* The deliverer's own predicate
   (B-2d: byte-identical; the rename: no claim remains) replaced the
   commissioner's (γ learns from outcomes), and "done" was written against the
   substitute. Review confirmed the substitute honestly. Nothing compared the
   two predicates.
3. *So the one rule with evidence behind it is narrow:* **"done" may be written
   only against the commissioner's predicate, verbatim, and a delivery that
   satisfies a different predicate is recorded as "did something else."** That
   is an S1→S6 coupling. S5 and S8 as drafted are unproven for the failure that
   actually occurred, and this document should not claim otherwise.
4. *The predicates that failed have a recognisable shape:* "no claim remains"
   (satisfiable by renaming); "byte-identical / 0 flips" (a no-op certified);
   "built (dark)" (no run); "for the live-test suite" (a fixture). Each states
   what the code is, not what problem it solves. Whether a predicate of the
   right shape can be written *before* the work — not just recognised after — is
   the open question, and it is the subject of `p4ng/futon-2026.tex`, *What
   Problems Are We Solving?*, not of this document.

Not ratified. Next use of this log, if any: not a build packet. A second
retrospective row on the fixture-as-registry case (07-08), to test whether
finding 2 generalises or is one case.

## Provenance

Drafted 2026-08-29 by claude-15 after a review, at Joe's direction, of how
`M-formal-war-machine`'s findings came about. Joe's framing, verbatim: *"It's
not just a handoff protocol. It's the overall system workings that need to be
done rigorously. And clearly following the mission life cycle is not enough
to guarantee that kind of high quality work. Similarly, the guarantees that
the machine … was supposed to supply, which we called the anatomy of a
flight, is not enough because the machine itself was constructed in a
completely erroneous way."* Evidence for §0 is in the 2026-08-29 claude-15
review (this REPL session), `M-formal-war-machine` §2.3, and the three
`T-*-26082026` tickets; the corrected trace numbers (88 outcomes; last
enactment 2026-07-06 12:04Z) were recomputed by reader loop over
`futon2/data/wm-trace/` and disagree with both E-R8 (07-09) and the
2026-08-27 census correction (3, 07-05).
