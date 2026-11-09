# TN — War Machine tokens and sessionista terms, side by side

claude-13, 2026-11-09, at Joe's request ("please write up a
TN-wm-tokens-and-sessionista-terms.md and we'll have that on the record").
Read-only comparison; nothing here is built. Related: `E-gcc-schema.md` §8
(futon4 `holes/excursions/`), WORKPLAN-INSTANTIATE (backlinks trial,
sessionista-mode packet 1).

Joe's question: "the way patterns are applied by the War Machine … they have
their own notion of 'tokens' for an interpretation. Maybe those tokens can be
aligned with the sessionista mode?"

## 1. The WM token, from the code

Sources: futon2 `resources/wm/observation-contract.edn`
(`:wm/observation-contract-v1`, written 2026-09-17, owner WM-04),
`src/futon2/aif/observation_checks.clj`, `declared_effect_record.clj`,
`mission_criteria.clj`, `kernel_example.clj`, `cascade_habit_reinforcement.clj`,
`token_outcome_pair.clj`. Lean carrier: mathlib4
`DarkTower.WarMachine.TokenObservation` (sha 889429e6bf, per the contract).

- **Definition.** "Each checkable class defines its own token: exactly the fact
  its check observes, nothing more" (contract `:token`). If an application states
  further conditions (correctness, adequacy, behaviour), those conditions are
  class **J**, judgement, and J is refused when a problem is assembled.
- **Identity.** A token is `[target wanted-token]`. Its meaning is
  `{:token, :declaration-sha256, :locator}`, checked against the target's
  declared snapshot (`kernel_example.clj:95`).
- **Locator.** This is a map giving the class plus the fields its check reads.
  The classes are:

  | Class | Token (the fact observed) | Reads |
  |---|---|---|
  | C3 | the named path exists at the pinned sha | git object |
  | C4 | a line of the file at the pinned sha starts with the declaration head, followed by whitespace, `:`, `(`, `{`, `[` or end of line | git object |
  | C5 | the contract bundle has the named entry, and its clojure loci resolve | git objects |
  | C6 | an EDN witness references an existing commit | git objects |
  | C8 | the test registry holds a passing warrant whose pinned shas equal the files now | test registry |
  | J | a judgement question with four verdicts and a must-not-see list | refused on the production path |

  `declared_effect_record.clj:62` interprets only `:C4`
  (`supported-locator-classes`). `mission_criteria.clj` derives a mission's
  wants and their C4 locators from its completion criteria.
- **Prediction and observation.** Before a run the WM gives each wanted token a
  probability. After the run the locator reports observed true or false, or a
  typed missing (`:observation-missing`). The comparison then classifies the token
  as predicted-and-observed, predicted-not-observed, not-predicted-observed,
  neither, or observation-missing (`:wm/token-outcome-comparison-v1`).
- **Discipline.** A missing observation is never counted as false. A verdict is
  never reused as an observation (`token_outcome_pair.clj`). Observations are
  taken at a recorded cutoff sha after each application (contract `:schedule`).
- **What no locator does.** Every class reads git objects at a pinned sha or the
  test registry. **None reads conversation text.**

## 2. The sessionista term, from the code

Sources: futon3c `src/futon3c/arxana/workplan_backlinks.clj`,
`resources/arxana/workplan/entity-backlink-rules.edn`; futon4a
`lisp/futon4a-sessionista.el`.

- **Rule.** A `[:workplan/entity-match-rule 1]` 'clusion per entity, whose
  endpoint is the entity record. The forms and their on/off switches are an
  attestation (basis `:human`), outside identity. In the trial the forms are
  Agency, Giraffe, kangaroo and Kangaroo.
- **Matcher.** Whole word, case-sensitive. Unicode letters, digits and `_` are
  word characters, so a hyphen is a boundary.
- **Two observers, one matcher.**
  - The backlinks builder runs the matcher over `evidence/body.text` of
    search hits. It records evidence id, codepoint range, role, session and
    time in a derived index, together with the rules' hash and the search
    `index-as-of`.
  - sessionista-mode runs it over the text in a REPL buffer as it is
    displayed, and records nothing.
- **What is dropped.** Hits where the word occurs only in metadata (for example
  `mission-id: M-giraffe`) are counted and dropped.

## 3. Field by field

| WM token | sessionista term | Comment |
|---|---|---|
| `[target wanted-token]` | `[entity-slug form]` | both pairs: what it is about, which trace |
| meaning `{:token :declaration-sha256 :locator}` | rule 'clusion id + forms attestation id (both content hashes) | both are hash-pinned declarations |
| locator class + fields (C3–C8) | matcher (whole word, case-sensitive) over `evidence/body.text` | a new class would be needed: none of C3–C8 reads text |
| pinned cutoff sha | evidence id (entries are immutable) + text sha256; for the index, `index-as-of` | evidence ids pin the way a sha does |
| observed true / false / typed missing | mark at a range / no mark / ? | **gap:** sessionista has no "missing". A turn outside the search index (after `index-as-of`), or a stack that is down, shows as no backlink, which looks like "not mentioned" |
| prediction before the run (probability) | none | sessionista observes and predicts nothing |
| "exactly the fact the check observes" | the word occurs as a whole word in that turn's body | **the important limit:** a mark does not mean the turn is about the entity. That claim is J |
| J refused on the production path | none yet | "is this turn about Agency?" would be J |

## 4. What aligning them would mean

1. **A text class.** Declare one more checkable class in the WM's own form, say
   `T1`: "the form occurs as a whole word in the `evidence/body.text` of entry
   E". Its inputs would be `{:evidence-id :form :text-sha256}` and its check is
   the sessionista matcher. The contract's own rule applies: a true reading says
   only that the word is there.
2. **Uses in one direction.** A WM pattern could predict that a turn, or a
   session, will come to name an entity. T1 then supplies the observation leg
   with no model call, and the comparison classes from §1 apply unchanged.
3. **Uses in the other direction.** sessionista inherits the WM's discipline:
   - declared meanings with hashes (it nearly has them already);
   - versioned locators (the rules' hash);
   - **typed missing:** "not in the index yet" and "stack down" are reported as
     missing, never shown as absence. This is the first change I would make to
     sessionista on its own merits.
4. **Not a rebuild of either.** This is a mapping plus one new class. WM
   classes C3–C8 are untouched. The Arxana rules stay the authority for the
   forms.

## 5. Beyond words (open)

Joe's tune: a player knows "Inisheer" from its shape, without the title. T1
finds a concept only when its word is said. Finding it by shape means a locator
that recognises a concept from what comes with it, for example bells, parks and
the roster for Agency, even where "Agency" is not said. In the contract's terms
that is either a composite of several T1 readings (still checkable) or a
judgement (J, with rates to measure). Which of the two applies is not settled.

## 6. Open questions

- Where would T1 be declared: in the WM contract (futon2, owner WM-04), in an
  Arxana type, or in both, with one as the authority?
- Should metadata-only hits be T1-false (the word is not in the body text) or
  outside T1's domain? Today they are counted and dropped.
- Does a session-level token ("this session names X") need its own class, or
  can it be read as any T1 over the session's entries?
