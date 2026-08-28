# RB05: Which reliability coefficient feeds the HiTOP-SR score interval (M041)

- **Date:** 2026-08-28
- **Output required:** write findings to `cairn/reviews/RR05-hitopsr-interval-reliability.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package that scores questionnaire instruments distributed by
the HiTOP Society. `score_hitopsr()` scores the 405-item HiTOP Self-Report
into 76 primary scales and their subscales; each scored column is the
**mean** of its items, on a 1–4 response coding.

Milestone M041 adds `interval_hitopsr()`: a confidence interval around each
scored column's estimate of the respondent's true score. The method is fixed
(see Constraints): the regression-based estimated true score with scale
correction, from Schmukle (2026, *Assessment*, 33(5), 817–825), Eqs (10)–(12)
on p. 821:

```
RETS = M + sqrt(rel) * (x - M)
SERE = SD * sqrt(1 - rel)        # the paper notes this equals the SEM
CI   = RETS +/- z * SERE
```

The three inputs per scale are `M`, `SD`, and `rel` for a reference group.
`M` and `SD` come from Table 1 of the HiTOP-SR introduction manuscript
(Development Sample 2, N = 780), which M041 transcribes and ships as a new
exported dataset `hitopsr_devstats`. Table 1's columns are `# Items`,
`alpha`, `M`, `SD`, `Range`, `Skewness`, `Kurtosis`; its `M` and `SD` are on
the item-mean metric with `Range` printed as `[1.0, 4.0]`.

**The open question is what supplies `rel`.** Two candidates:

- **Cronbach's alpha**, printed in Table 1 beside the `M` and `SD` the same
  table prints, and transcribed with them.
- **Omega-ordinal**, not published anywhere for these scales, and therefore
  computable only by this package recomputing it from the raw responses.
  The raw response file `Prolific data HiTOP-SR.sav` (Development Sample 2,
  N = 780) is on the repository's gitignored source shelf.

The maintainer raised this at the implementation gate and chose to escalate
it rather than settle it in the implementing session.

The implementing session's own reading — stated here so you can attack it,
not so you can adopt it — was that alpha is correct on three grounds:

1. Schmukle's own validating simulation for six-category Likert items
   (p. 822, and the note to its Table 1) estimated all three approaches'
   confidence intervals "using Cronbach's alpha as an estimate of the
   reliability", and the scale-corrected approach covered .95 at every
   reliability level from .10 to .90.
2. `SD` in Eq (11) is the SD of the **observed** score, so `rel` must be a
   reliability of the observed score. Omega-ordinal is a coefficient for the
   latent response variable underlying the categories, not for the observed
   summed or averaged score, so pairing it with an observed-score SD mixes
   two metrics.
3. Alpha is printed, so it is transcribable and verifiable cell-by-cell
   against the source, which is what M041's verification criteria are built
   around. A recomputed omega-ordinal is a number this package produced.

Against that, some HiTOP-SR scales are markedly non-normal on a four-category
coding — Conversion Symptoms prints alpha 0.82, skew 4.10, kurtosis 19.84 —
which is exactly the regime where alpha is most often argued to attenuate.
Whether that attenuation matters enough to change what M041 ships is part of
what this brief asks.

## Materials

Sources are on the repository's **gitignored** shelf at
`cairn/references/sources/`; they are present in this checkout.

- `cairn/references/sources/schmukle2026.pdf` — the interval method. Read at
  least the derivation of Eqs (5)–(12) (pp. 820–821), the Simulation Studies
  section and its Tables 1–2 (pp. 821–823), Box 1, and Further Limitations.
- `cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf` — the HiTOP-SR
  introduction manuscript. Table 1 begins on the page whose text starts
  `Table 1. Descriptive Statistics and Internal Consistencies...`; the prose
  describing it is under the heading "Scale Descriptive Statistics and
  Internal Consistencies", which states that Development Sample 2's
  descriptives "provide preliminary reference norms for each scale and
  subscale" and that alpha is the internal-consistency coefficient reported.
  `data-raw/hitopsr_table1.R` is a working maintainer-run extractor for this
  table (`hitopsr_table1_rows()`); it needs `pdftotext` and verifies the
  file's SHA-256 before extracting. Use it rather than re-deriving the
  extraction.
- `cairn/references/sources/Prolific data HiTOP-SR.sav` — the raw item
  responses for Development Sample 2 (N = 780), readable with
  `haven::read_sav()`. It is the only route to any coefficient Table 1 does
  not print. You are not required to run an analysis on it; inspect it only
  as far as answering question 4 needs.
- `cairn/milestones/M041-hitopsr-score-intervals.md` — the milestone: goal,
  scope, seven acceptance criteria, tasks, and the work log (which records
  the plan-gate reasoning and the 2026-08-27 correction of the formula).
- `R/reliability.R` lines 129–215 — `calc_omega()`, the omega the package
  already exports: McDonald's omega-total from a **continuous** one-factor
  CFA fit with lavaan (MLR estimator, FIML), computed from unstandardized
  loadings and residual variances. Note what it is not: no `ordered=`
  argument, no threshold model, no categorical estimator.
- `R/reliability_engine.R`, `R/reliability_hitopsr.R` — the `alpha`/`omega`
  per-scale reliability family the package ships today, computed from the
  *user's own* data, not from any reference sample.
- `R/norm_pid5.R` — the closest existing analogue: it converts scored columns
  against a shipped published table and never rescores. `interval_hitopsr()`
  is planned on this pattern.
- `cairn/DESIGN.md` lines 94–97 — the four inviolable principles, IP1–IP4.
- `cairn/DECISIONS.md` — read the headings, then in full: **D-032**, which
  admits Table 1 as the reference-statistics source under IP3 and binds two
  conditions to it; **D-042**, which admits this manuscript version as the
  ingestion source. The `## Candidates` section of `cairn/ROADMAP.md`
  contains a standing row on recomputing Table 1's statistics from
  `Prolific data HiTOP-SR.sav`, listing the decisions such a recomputation
  would need; read it.

## Questions

1. **Which coefficient should supply `rel` in Eqs (10)–(12) for these
   scales — the alpha Table 1 prints, or an omega-ordinal recomputed from the
   raw responses?** Answer on the psychometrics, not on convenience. In
   particular: does Schmukle's derivation require `rel` to be a reliability
   of the *observed* score (the metric `SD` is on), and if so, is
   omega-ordinal such a coefficient? If the implementing session's metric
   argument (Background, point 2) is wrong, say so plainly and show why.

2. **Does alpha's behavior on these scales materially distort what M041
   ships?** Table 1's items are on a four-category coding and several scales
   are strongly non-normal (Conversion Symptoms: alpha 0.82, M 1.12, SD 0.31,
   range [1.0, 3.6], skew 4.10, kurtosis 19.84; contrast a scale like
   Distress-Dysphoria: 16 items, alpha 0.96, skew 0.37). Given that `rel`
   enters both as `sqrt(rel)` shrinkage toward the mean and as
   `sqrt(1 - rel)` half-width, characterize the direction and rough magnitude
   of the error an attenuated alpha would produce in each, and say whether it
   is large enough to matter for a scale score reported to a researcher. If
   it is, what should M041 do — document it, restrict the function, or
   something else?

3. **Is there a third option better than both?** Specifically, a
   categorical/nonlinear-SEM reliability for the observed summed or averaged
   score (e.g. Green & Yang, 2009), which is on the observed metric but
   models the item responses as ordered categories — a different thing from
   omega-ordinal, and a different thing from the continuous omega-total the
   package's `calc_omega()` computes. If such a coefficient is the
   psychometrically right answer, say so, and say whether the gain over alpha
   justifies the cost of this package computing a coefficient rather than
   citing one.

4. **Does IP2 bar a recomputed coefficient here?** IP2 (`cairn/DESIGN.md`
   line 95) requires that "every shipped numeric constant that affects output
   ... traces to a SOURCES.md-cited authority before it ships, machine-checked
   where feasible", and forbids tests that assert the code's own output as
   truth. A recomputed omega-ordinal or categorical omega would be a constant
   this package produced from raw data, verifiable only against this
   package's own procedure. Is a committed, scripted recomputation from the
   shelved raw file an acceptable form of the "independent recomputation" IP2
   permits, or does IP2 confine M041 to coefficients the source prints? If
   the answer is "acceptable but only under conditions", name the conditions.

## Constraints

Flag disagreement with any of these explicitly rather than working around
them silently.

- **The interval method is fixed.** Schmukle's regression approach with scale
  correction, Eqs (10)–(12), is settled and is not reopened by this brief.
  Only the `rel` input is in question. (If you believe the method itself is
  wrong for these data, say so under "Beyond the brief" — do not answer the
  numbered questions as though it were open.)
- **`M` and `SD` come from Table 1 and are not recomputed by M041** — D-032
  admits the table as the reference-statistics source under IP3 and binds two
  conditions: the reference group is named as the paper's development sample
  with its N wherever the numbers surface, and the ingestion is machine-diffed
  cell by cell against the source. D-042 makes the shelved manuscript version
  the admissible ingestion source. Neither is reopened here. A recommendation
  that `rel` be recomputed while `M` and `SD` stay transcribed should say
  whether the resulting mixture is coherent.
- **The dataset schema is already settled** by the maintainer at the
  2026-08-28 gate: `hitopsr_devstats` carries a generic `reliability` column
  plus a `reliabilityType` label naming the coefficient, rather than a column
  named `alpha`. Your answer determines what goes in those two columns for
  this release; it does not need to argue about their names.
- **Scope.** M041 covers the HiTOP-SR only. The HiTOP-BR spectrum scales and
  the PID-5 are explicitly out of scope and have their own roadmap rows. A
  recommendation that would only make sense package-wide should say so.
- **This is not a request to build anything.** Answer the questions; do not
  edit package code, `data-raw/`, or any tracking file other than writing
  your RR.

## Output format

In `RR05-hitopsr-interval-reliability.md`: answer each question by number
with your reasoning and evidence; list any additional findings separately
under "Beyond the brief"; end with concrete recommendations, each marked
apply / consider / reject-with-reason. Your report is advisory: this brief's
header slot says `not requested`, so emit no `## Binding criteria` section.
