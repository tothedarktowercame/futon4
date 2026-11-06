# D4 — correspondence model and normative fixture

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: P4, D1–D3, ARGUE O-D4a.
Status: design checkpoint; fixture repair is specified, not executed.

## Decisions

**D4-DR1.** IF draft8 contains the richest committed correspondence evidence;
HOWEVER its scope hashes and anchors describe different source states; THEN
retain the original as a negative fixture and derive a new coherent manifest
before calling any pair normative; BECAUSE silently replacing hashes would
erase the defect the fixture must detect.

**D4-DR2.** IF source ranges, quotes and DOM IDs survive with different
strengths; HOWEVER none is semantic identity; THEN correspondence is a
provenanced assertion between revision-pinned manifestations with locators as
evidence; BECAUSE an offset or build-local ID cannot own identity.

**D4-DR3.** IF transformations may expand macros or resolve references;
HOWEVER exact text equality is still useful for literal prose; THEN assertion
methods explicitly distinguish exact-range, quote-context, build-anchor and
transformation evidence and may combine them; BECAUSE one Boolean "maps to"
would conceal the reason and direction of the match.

## Core values

- **Artifact** — stable logical resource (TeX file, HTML page).
- **Artifact revision** — immutable SHA-256 identity of canonical bytes.
- **Manifestation** — stable semantic role in an artifact family, e.g.
  `draft8/source/section-the-object` or `draft8/html/section-the-object`.
- **Locator** — revision-specific way to find a manifestation: codepoint range,
  quote/context, DOM selector/ID or source-map coordinate.
- **Correspondence assertion** — immutable, directional, revision-pinned claim
  relating two or more manifestations, locators and evidence items.

Manifestation IDs are minted by the fixture/model author, not copied from DOM
IDs. Minting is deterministic from the canonical tuple
`(manifestation-authority, artifact-family-id, semantic-role-key)` and yields
`arxana:manifestation:sha256:<digest>`. The authority and artifact family own
the namespace; a role key is unique within it. The registry refuses a second
definition for the same tuple unless it is byte-equivalent. Renames create an
explicit alias assertion; they do not recycle an old ID for a different role.
A build-local DOM ID is stored only as locator evidence.

The manifestation definition records the canonical tuple, human label, media
role and minting authority. Its stable identity spans revisions, while every
observation/correspondence still names an exact artifact revision. Two authors
cannot independently mint competing canonical IDs for one family without an
authority/alias decision.

## Assertion shape

```clojure
{:type :arxana/correspondence-assertion
 :id "arxana:correspondence:sha256:..."
 :source {:manifestation/id "..." :artifact/revision-id "..."
          :locator {:kind :codepoint-range :start 0 :end 20}}
 :target {:manifestation/id "..." :artifact/revision-id "..."
          :locator {:kind :dom-id :value "S2"}}
 :direction :source-to-target
 :cardinality :one-to-one
 :method [:source-map :quote]
 :verification :verified
 :evidence [{:type :oxide-sourcepos :value "0:316:1-0:316:21"}
            {:type :quote-digest :sha256 "..."}]
 :producer {:tool "latexml-oxide" :version "..." :build/id "..."}}
```

Identity digest covers endpoints, their revision IDs, direction, method and
semantic evidence; it excludes timestamps and transport metadata. Assertions
may be `verified`, `partial`, `ambiguous`, `unverified` or `stale`. Cardinality
is one-to-one, one-to-many, many-to-one or many-to-many. Reversing a directional
assertion is a query projection only when the method declares reversibility.

## Method semantics

| Method | Proves | Does not prove |
|---|---|---|
| exact range + digest | those source bytes existed at that range/revision | semantic identity across builds |
| quote/context | text can be rediscovered, possibly after movement | uniqueness unless all matches enumerated |
| source map | producer associated source coordinates with output node | literal equality or editability |
| DOM ID/selector | output node within one HTML revision | durable identity across builds |
| transformation | declared source-to-target normalization/expansion | reversibility unless specified |

Ambiguous locators retain every candidate. Partial paragraph anchors state the
known point and quote evidence; they do not manufacture an end range. Macro
`\sig`→`\sigma` is transformation evidence and is directional.

## Fixture manifest v1

The normative fixture is a manifest, not an assumed directory pairing:

```clojure
{:fixture/id "m-jvm-arxana/draft8-v1"
 :source-commit "245773fef526c36b040576add694ce774afb46f3"
 :artifacts [{:logical-id "draft8.tex" :path "sandbox/draft8.tex"
              :revision-id "arxana:revision:sha256:de7d8f..."}
             {:logical-id "intro-generated.tex"
              :path "sandbox/intro-generated.tex"
              :revision-id "arxana:revision:sha256:bccb33..."}
             {:logical-id "part3-exotype.tex" :path "sandbox/part3-exotype.tex"
              :revision-id "arxana:revision:sha256:3983ea..."}
             {:logical-id "draft8.html" :path "page/draft8.html"
              :revision-id "arxana:revision:sha256:ceab93..."}]
 :source-map {:path "sandbox/scopes.json" :status :must-be-regenerated}
 :cases ["paragraph-point" "section-range" "math-transformation"]}
```

Abbreviated hashes above are explanatory only; the generated manifest stores
full digests. It also records tool versions, canonicalization rules, build ID,
file-index mapping and full scope-table digest.

## Repair and rejection procedure

1. Hash every manifest artifact from committed bytes.
2. Reject the original `scopes.json`: each `source-sha` must equal its mapped
   artifact revision, and the observed `.pristine` values do not.
3. Rebuild the HTML/source map from a clean checkout of the named commit, or
   recover a producer log proving which exact bytes made the committed page.
4. Refuse mixed output: all page anchors and the map must name one build ID and
   the same source revision set.
5. Verify every source-map coordinate is in bounds. For the three normative
   cases, additionally verify expected quotes/transformation evidence.
6. Store the repaired map as a new fixture artifact; never overwrite the
   contradiction-bearing input.

If the committed HTML cannot be proven to derive from one coherent source set,
it remains only a negative fixture and a clean rebuild becomes the normative
positive fixture. Git commit plus quote is evidence, not a substitute for a
coherent build manifest.

## Normative cases

1. **Paragraph point:** source `intro-generated.tex` line 10 point anchor,
   leading literal quote, no DOM ID, partial correspondence. Expected query:
   one manifestation with a point locator plus quote/context; no invented end.
2. **Section range:** `draft8.tex` `\section{The Object}` to HTML build-local
   `S2`. Expected: verified source-map + quote assertion; DOM ID is locator only.
3. **Math transformation:** source `$P_\sig$` to HTML math `S1.p2.m2` with
   alttext `P_{\sigma}`. Expected: directional transformation assertion;
   reverse literal edit is unsupported.

Each expected response names source and target revision IDs, method,
verification, editability class and evidence digest.

## Query contract

`correspondence/query` accepts manifestation or revision-pinned locator,
direction, allowed methods and verification floor. It returns assertions and
all candidates; it never selects among ambiguous assertions. A stale assertion
remains queryable as historical evidence but is excluded from current-only
queries. Neighbourhood traversal is bounded and preserves direction/provenance.

## Extension to Lean and generated Python

New manifestations join by adding artifact revisions and correspondence
assertions; the draft8 cases do not wait for EmitPython. Lean→generated Python
and Lean→LaTeX producer evidence must name tool/version/build and exact input
and output revisions. Shared symbol names are candidate evidence, not identity.

## Tripwires

- Original scopes hashes cause `fixture/revision-mismatch`, not warning-only.
- Changing one source byte without rebuilding makes all dependent assertions
  stale and cannot mutate their historical records.
- Duplicate paragraph quotes return ambiguity.
- Paragraph case cannot acquire a fabricated end or DOM ID.
- Section DOM ID changes across rebuild while manifestation ID remains stable.
- Re-minting one canonical tuple returns the same ID; a conflicting definition
  or role-key reuse is refused, and cross-authority IDs cannot collide.
- Math assertion is directional and retains both source and rendered forms.
- Every assertion endpoint revision occurs in the same coherent build manifest.
- Full manifest regeneration is byte/digest deterministic for fixed inputs and
  pinned toolchain, or records the precise nondeterministic fields.

## Obligations

- **O-D4a closed at design level:** contradiction is a required negative
  fixture; positive adoption requires the rejection/repair procedure.
- **O-D4b closed at design level:** durable manifestation IDs are independent
  of revision-specific DOM locators.
- **O-D4c closed at design level:** IDs are deterministic authority/family/role
  tuple digests with uniqueness, conflict and alias rules.
- INSTANTIATE must create and commit the repaired manifest/map and demonstrate
  all tripwires; DERIVE does not rewrite futon5 artifacts.
- **O-D7c passed to D7:** assign fixture builder, manifestation minting
  authority and correspondence assertion authority. The repaired positive
  fixture must be the first implementation increment because D5/D6
  demonstrations depend on it.

No architecture is selected by D4.
