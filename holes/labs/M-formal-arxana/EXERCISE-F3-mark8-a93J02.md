# Exercise for Joe — F3: judge mark8's step-to-Lean links for a93J02

claude-13, 2026-11-08. For DERIVE-F3. mark8 linked each step of the
authored informal proof to a Lean declaration (or none), and a model judge
accepted or rejected each link. Those are model evidence only. Your
verdicts become the human assessments of these correspondences.

Source: `~/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json`;
Lean: `apm-lean/problems/a93J02/lean/Main.lean`.

## The three Lean declarations

- `integral_inv_one_plus_x` (l.22): ∫ x in 0..1, 1/(1+x) = log 2
- `tendsto_sum_alternating_harmonic` (l.58): the partial sums
  ∑_{i<n} (-1)^i/(i+1) tend to log 2 (proved by the alternating series
  test and Abel's limit theorem)
- `alternating_harmonic_eq_log_two` (l.111): the same statement, defined as
  `tendsto_sum_alternating_harmonic`

## Steps

For each step: is the link right? Answer `ok`, `wrong`, or the declaration
you would link instead (or `none`). One line per step is plenty, e.g.
`3 ok`, `5 wrong: none`.

### 1. ∫[0 to 1] 1 / (1 + x) dx = log(2)

- informal conclusion: $\int_{0}^{1} 1/(1+x) \mathrm{d}x = \log(2)$
- warrant in the informal proof: definition of f(x)
- mark8 link: `integral_inv_one_plus_x`
- judge's choice: `integral_inv_one_plus_x`
- judge's reason: The lemma's conclusion directly matches the step's conclusion.
- **your verdict:** 

### 2. for x ∈ [0, 1), 1 / (1 + x) = ∑[n=0 to ∞] (-1)^n * x^n

- informal conclusion: $1/(1+x) = \sum_{n=0}^{\infty} (-1)^{n}x^{n}$
- warrant in the informal proof: definition of f(x)
- mark8 link: `None` (no link)
- judge's choice: none
- judge's reason: None of the candidate lemmas directly prove the given step about the power series expansion of 1/(1+x).
- **your verdict:** 

### 3. ∫[0 to 1] (-1)^k * x^k dx = (-1)^k / (k + 1)

- informal conclusion: $\int_{0}^{1} (-1)^{k}x^{k} \mathrm{d}x = (-1)^{k}/(k+1)$
- warrant in the informal proof: direct computation
- mark8 link: `None` (no link)
- judge's choice: none
- judge's reason: None of the candidate lemmas directly compute the integral of (-1)^k * x^k from 0 to 1.
- **your verdict:** 

### 4. ∫[0 to 1] S_n(x) dx = ∑[k=0 to n] (-1)^k / (k + 1)

- informal conclusion: $\int_{0}^{1} S_{n}(x) \mathrm{d}x = \sum_{k=0}^{n} (-1)^{k}/(k+1)$
- warrant in the informal proof: definition of S_n(x)
- mark8 link: `None` (no link)
- judge's choice: none
- judge's reason: None of the candidate lemmas match the conclusion of the informal step.
- **your verdict:** 

### 5. pointwise convergence: for each x ∈ [0, 1), S_n(x) → 1 / (1 + x)

- informal conclusion: each $x \in [0,1)$, $S_{n}(x) \to 1/(1+x)$ (geometric series).
- warrant in the informal proof: definition of S_n(x)
- mark8 link: `tendsto_sum_alternating_harmonic`
- judge's choice: none
- judge's reason: None of the candidate lemmas mention the geometric series or its partial sum.
- **your verdict:** 

### 6. domination: |S_n(x)| ≤ 2 for x ∈ [0, 1]

- informal conclusion: $|S_{n}(x)| = |{1}-(-x)^{n+1}|/(1+x) \leq (1+x^{n+1})/(1+x) \leq (1+1)/(1+0) = 2$
- warrant in the informal proof: definition of S_n(x)
- mark8 link: `None` (no link)
- judge's choice: none
- judge's reason: None of the candidate lemmas directly relate to the partial sum S_n(x) and its absolute value.
- **your verdict:** 

### 7. ∫[0 to 1] S_n(x) dx → ∫[0 to 1] 1 / (1 + x) dx

- informal conclusion: $\int_{0}^{1} S_{n}(x) \mathrm{d}x \to \int_{0}^{1} 1/(1+x) \mathrm{d}x$
- warrant in the informal proof: dominated convergence theorem
- mark8 link: `None` (no link)
- judge's choice: none
- judge's reason: None of the candidate lemmas match the conclusion of the informal step.
- **your verdict:** 

### 8. the limit of the partial sums is equal to the integral of the function

- informal conclusion: $\sum_{k=0}^{n} (-1)^{k}/(k+1) = \int_{0}^{1} S_{n}(x) \mathrm{d}x \to \int_{0}^{1} 1/(1+x) \mathrm{d}x = [\log(1+x)]_{0}^{1} = \log 2$
- warrant in the informal proof: definition of limit
- mark8 link: `alternating_harmonic_eq_log_two`
- judge's choice: `integral_inv_one_plus_x`
- judge's reason: Its conclusion directly matches the step's conclusion, and its premise is implied by the step's premise.
- **your verdict:** 

### 9. the final answer is equal to the logarithm of two

- informal conclusion: $\sum_{n=0}^{\infty} (-1)^{n}/(n+1) = \log 2$
- warrant in the informal proof: definition of logarithm
- mark8 link: `None` (no link)
- judge's choice: `alternating_harmonic_eq_log_two`
- judge's reason: The lemma's conclusion directly states the equality of the alternating harmonic series with log 2.
- **your verdict:** 

## Things worth noticing (not answers)

- Steps 5 and 8–9: mark8's link and the judge's choice differ.
- Step 7 is the dominated-convergence step; the Lean proof has no
  counterpart (it uses Abel's theorem instead). DERIVE-F3 records this as
  a methodological divergence, not a contradiction.

## Joe's verdict (2026-11-08, turn `turn-YoF9El`, evidence `emacs-8fdea78f5fab37bcf3fc65aa4aded28e`)

> So, I looked at it, not deeply, but I agree with the judge's readings.
> The point at the end is crucial: "a methodological divergence". It looks
> like the mark8 reader was really reaching to find some connections that
> weren't there. Note also that there was never a request in the
> formalisation work that the formal proof *should* follow the informal
> proof, so divergence is expected. This puts some feedback pressure on
> mark8, but that's a separate issue.

Recorded for F3 as: nine human assessments agreeing with the judge's choice
for each step (`evidence/basis human`; Joe's own word "not deeply" kept as
the method note). mark8's raw links that the judge overruled (steps 5, 8)
are therefore rejected by both judge and operator; step 9's link comes from
the judge, not mark8.
