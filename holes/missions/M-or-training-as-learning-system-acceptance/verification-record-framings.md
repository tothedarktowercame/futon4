# Verification record: candidate-framing verdicts — M-or-training-as-learning-system

Every Working Paper candidate framing carried from the npt working paper, with
its final VERIFY verdict and its empirical evidence named. Sources: mission
VERIFY "Candidate-framing verdicts (final per VERIFY)" table, ARGUE
theoretical-coherence section, and simulation runs `notebooks/ukrn_mission_verify.clj`
(calibrated extensions over `notebooks/ukrn_cyberant_aif.clj`).

| Framing | Verdict | Empirical evidence |
|---|---|---|
| **NPT** (May & Finch 2009) | Robust | Codebook coded ORP T3 transcripts; population model encodes NPT factors (10 profiles × 16 factors, `assumptions-v2.edn`); design uses NPT as substrate throughout DERIVE. |
| **Pattern theory** (Alexander 1977 + WP adaptation) | Robust under stated conditions | Five institution-level patterns + three programme-level patterns survive into DERIVE as load-bearing; programme-level patterns flagged for PUR-driven revision after simulation runs. |
| **Active inference** (Friston + WP adaptation) | Instrumental (not a foundational claim) | Predictions 1–5 simulation runs are evidence that AIF is a useful *verification tool*; they are not evidence that UKRN-S is an AIF-natural system. Sector-facing materials must not advocate AIF as a UKRN-S claim. |
| **Krowne sustainability triangle / tetrahedral extension** | Set aside as architectural commitment; descriptively useful at MAP | DERIVE's role-and-coordination structure carries the design without the Krowne label; the evidence-apex framing helped WP1's third-evidence-layer point land at MAP, but no DERIVE commitment rests on it. |
| **MVSG** (geometric synthesis) | Set aside as load-bearing commitment | Prediction 2: role-coordination-centroid prior (D=0.78, A=0.77) performs equivalently — slightly better — than the MVSG-baked prior (D=0.73, A=0.76); both configurations reach 10/10 multiplied. Design is robust to the MVSG verdict either way; Convener may still use MVSG language descriptively. |

Supporting simulation results behind these verdicts (mean D = delivery
viability, mean A = architectural sustainability; viability threshold 0.5):

- Prediction 1 (evaluation timing): presence beats absence; timing-vs-presence
  is not sharp — V3 revision recorded.
- Prediction 2: MVSG conditional → set aside (above).
- Prediction 3 (coupling): all four configurations reach 10/10; decoupling is
  shape-determining, not threshold-determining (EV↔Convener loss costs A 0.13;
  Convener↔TM loss costs D 0.08).
- Prediction 4 (funder catalysis): funder-active raises both means
  (D 0.75 vs 0.68; A 0.76 vs 0.66) with dimension-specific variance
  (sd-D lower, sd-A higher) — V4 revision, a binding constraint the WP lacked.
- Prediction 5 (Evaluator configuration): distributed-p20 fails one
  institution (9/10); centralised and distributed-p ≥ 0.5 all reach 10/10 —
  configuration choice is a trade-off question, not a will-it-work question.
