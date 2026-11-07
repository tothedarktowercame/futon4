# MAP Q9 — Geometry authority (unit circle, square map, fibre over -1)

**Question:** Which existing libraries can represent and verify S¹, complex
multiplication, conjugation/inversion, z ↦ z², and the fibre over −1 = {i, −i},
with a *query* (not a drawing) as authority?

## Answer

1. **Mathlib is the only candidate that can *certify* the whole F5 fixture, and
   it is already installed and prebuilt on Zone in two Lean projects**
   (`~/code/LeanEuclid` toolchain v4.19.0; apm-lean worktrees toolchain
   v4.29.0-rc8, Mathlib pinned master with build artefacts present).
2. The unit circle is **`Circle` = `Submonoid.unitSphere ℂ`**
   (`Mathlib/Analysis/Complex/Circle.lean:52`), a `CommGroup`
   (`Circle.instCommGroup`) — so multiplication, inversion, and the square map
   `fun z : Circle ↦ z * z` all typecheck against prebuilt oleans with no
   Mathlib rebuild (`lake env lean` on a scratch file in the apm-lean lemmas
   worktree).
3. Conjugation = inversion on the circle is an existing Mathlib lemma:
   **`Circle.coe_inv_eq_conj : ↑z⁻¹ = starRingEnd ℂ ↑z`**
   (`Mathlib/Analysis/Complex/Circle.lean:79`).
4. The fibre statement `{z : Circle | (z:ℂ)^2 = -1} = {⟨I,_⟩, ⟨-I,_⟩}` is
   **not a named Mathlib lemma** (greps for `sq_eq_neg_one_iff`, `x ^ 2 = -1`
   find only ZMod/Nat variants) but is provable today in ~20 lines, 0 sorries,
   via `sq_eq_sq_iff_eq_or_eq_neg` — verified by probe (see Evidence).
5. **JVM/Clojure: nothing verifies.** Only `commons-math3 3.6.1` on futon5's
   classpath (used for `Array2DRowRealMatrix`/`EigenDecomposition`/`ChiSquareTest`
   only). It ships `org.apache.commons.math3.complex.Complex` but *no futon code
   imports it* (grep of `math3.complex` across futon0/1/3c/5 src: zero hits) —
   and it is numeric, not proof. No sicmutils/emmy/core.matrix/symbolic-algebra
   dep in any futon `deps.edn`. No geometry library in futon3c.
6. **Python: sympy 1.14.0** in futon6's venv (exercised by 3 verify scripts)
   solves `z²=-1 → [-I, I]` symbolically — usable as an oracle, but it is a
   CAS, not a proof authority; answers are unverified computations.
7. **Frontend / diagrammatic:** futon0's System E stack
   (`scripts/euclid_engine.py` 5433 lines, `scripts/euclid_play.py`,
   `holes/excursions/E-euclid-tutor-data/viewer/play.html`) already makes the
   *engine*, not the drawing, the authority for Euclidean diagram facts —
   the architectural pattern F5 wants — but its `Circle` is the Euclidean
   diagram circle (centres, inside/on; cf. LeanEuclid
   `SystemE/Theory/Inferences/Diagrammatic.lean:18-33`), with no complex
   arithmetic and no square map. Rendering code is a projection there already.
8. Net: Mathlib gives query-authority verification now; a small
   `Circle`-fibre lemma file (the probe below, cleaned up) is the missing
   piece. All Clojure/frontend code can only *project* or *compute*, not
   certify.

## Evidence

- Probe (apm-lean lemmas worktree `/home/joe/worktrees/apm-lean/lemmas`,
  prebuilt Mathlib, no rebuild; `lake env lean /tmp/q9probe5.lean`, exit 0,
  no diagnostics):
  ```lean
  import Mathlib.Analysis.Complex.Circle
  open Complex
  example : {z : Circle | (z : ℂ)^2 = -1} =
      {⟨I, by simp [Submonoid.unitSphere]⟩, ⟨-I, by simp [Submonoid.unitSphere]⟩} := by
    ext ⟨z, hz⟩
    constructor
    · intro h
      have h' : z = I ∨ z = -I := by
        have : z^2 = I^2 := by simpa using h
        exact (sq_eq_sq_iff_eq_or_eq_neg (R := ℂ)).mp this
      rcases h' with rfl | rfl
      · left; rfl
      · right; rfl
    · intro h
      simp only [Set.mem_insert_iff, Set.mem_singleton_iff] at h
      rcases h with h | h
      · have : z = I := Subtype.ext_iff.mp h; simp [this]
      · have : z = -I := Subtype.ext_iff.mp h; simp [this]
  ```
- `#check` output (same worktree): `Circle : Type`;
  `fun z => z * z : Circle → Circle`; `Circle.instCommGroup : CommGroup Circle`;
  `Circle.coe_mul (z w : Circle) : ↑(z*w) = ↑z * ↑w`;
  `Circle.norm_coe (z : Circle) : ‖↑z‖ = 1`;
  `Circle.coe_inv_eq_conj (z : Circle) : ↑z⁻¹ = (starRingEnd ℂ) ↑z`;
  `sq_eq_sq_iff_eq_or_eq_neg : a^2 = b^2 ↔ a = b ∨ a = -b`.
- Mathlib toolchain: `cat /home/joe/worktrees/apm-lean/lemmas/lean-toolchain`
  → `leanprover/lean4:v4.29.0-rc8`; `lakefile.toml` requires mathlib master;
  `.lake/packages/mathlib/.lake/build/lib` populated.
  LeanEuclid: `~/code/LeanEuclid/lean-toolchain` → `v4.19.0`,
  `lake-manifest.json` includes mathlib, `.lake/packages/mathlib` present.
- Absence checks for a built-in fibre lemma:
  `grep -rn "x ^ 2 = -1" .lake/packages/mathlib/Mathlib/Data/Complex/ Mathlib/Analysis/Complex/` → no hits;
  `grep -rn "sq_eq_neg_one"` → only `ZMod.exists_sq_eq_neg_one_iff` /
  Nat-prime variants (LegendreSymbol/Basic.lean, SumTwoSquares.lean).
  (In-namespace `def` caveat noted; searched by statement shape, not `^def`.)
- JVM: `grep -i 'geometry|complex|math' futon*/deps.edn` → only
  `futon5/deps.edn` `org.apache.commons/commons-math3 3.6.1` (comment: "Linear
  algebra for eigenvalue-based hexagram extraction"); usages:
  `futon5/src/futon5/mmca/local_causal_states.clj:9` (ChiSquareTest),
  `futon5/src/futon5/hexagram/lift.clj:187-189` (real matrices).
  `grep -rn 'math3.complex' futon{0,1,3c,5}/src` → zero hits.
  `grep 'sicmutils|emmy|symbolic|core.matrix|neanderthal' futon*/deps.edn` →
  zero hits.
- Python: `~/code/futon6/.venv/bin/python -c "import sympy"` → 1.14.0;
  `sp.solve(sp.Eq(z**2,-1),z)` → `[-I, I]`; system python3 has no sympy.
  sympy used in `futon6/scripts/verify-p4-n4-case2-sturm.py`,
  `explore-p4-path2-A-structure.py`, `verify-p4-n4-critical-verify.py`.
- System E / frontend: `wc -l futon0/scripts/euclid_engine.py` = 5433,
  `futon0/scripts/euclid_play.py` = 865; viewer at
  `futon0/holes/excursions/E-euclid-tutor-data/viewer/play.html`.
  Euclidean-circle axioms (no complex numbers):
  `~/code/LeanEuclid/SystemE/Theory/Inferences/Diagrammatic.lean:18-33`.

## Ready / Missing

| Ready — no new code | Missing — actual work |
|---|---|
| Mathlib `Circle` (CommGroup) with mul, inv, conj=inv, norm lemmas, prebuilt in 2 Zone projects | Named fibre lemma `{z : Circle | z²=-1} = {i,-i}` (probe proof above works; needs a home file) |
| `sq_eq_sq_iff_eq_or_eq_neg` closes the fibre in ~20 lines | Roots-of-unity framing (`isPrimitiveRoot` for 4th roots) if F5 wants the general n-th-root story rather than the n=2 special case |
| apm-lean worktree pattern for cheap `lake env lean` `#check`/example probes without rebuilds | A bridge from Lean-certified facts to Arxana hypergraph records (no existing exporter from Lean decls to futon evidence found) |
| futon0 System E stack as architectural precedent: engine-as-authority, HTML/SVG as projection | Extending that engine pattern beyond Euclidean diagrams to complex arithmetic (engine has no ℂ) |
| sympy 1.14.0 (futon6 venv) as a quick symbolic oracle for cross-checking | Verification: sympy answers are unverified; cannot be the authority |

## Open doubts

- F5 wants "query is the authority": Lean `#check`/`example` proofs are
  batch-mode. Whether the mission wants a *live* query service (REPL server
  over the pinned Mathlib, e.g. a lean-repl wrapper) is a DERIVE question,
  flagged here because no such service exists on Zone today.
- Mathlib version skew: LeanEuclid v4.19.0 vs apm-lean v4.29.0-rc8 — lemma
  names above verified on v4.29.0-rc8; `Circle.coe_inv_eq_conj` etc. exist in
  both eras but a single pinned substrate should be chosen.
- commons-math3's `Complex` is implemented-but-unexercised; even if exercised
  it certifies nothing (floating point). Listed only for completeness.
