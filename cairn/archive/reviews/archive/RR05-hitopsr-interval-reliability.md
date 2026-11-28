# RR05: Which reliability coefficient feeds the HiTOP-SR score interval (M041)

- **Date:** 2026-08-28
- **Brief:** `cairn/reviews/RB05-hitopsr-interval-reliability.md`
- **Reviewed:** `schmukle2026.pdf` in full (pp. 817–825, Eqs (1)–(12), both
  simulations, Box 1, Further Limitations); the HiTOP-SR manuscript's Table 1
  prose ("Scale Descriptive Statistics and Internal Consistencies");
  `M041-hitopsr-score-intervals.md`; DESIGN IP1–IP4; D-032, D-041, D-042; the
  ROADMAP recomputation candidate row; `R/reliability.R` 129–216,
  `R/reliability_engine.R`, `R/reliability_hitopsr.R`, `R/norm_pid5.R`,
  `data-raw/hitopsr_table1.R`. Status of the report: advisory; no binding
  criteria requested.

## Answers

### 1. Alpha or omega-ordinal?

**Alpha — or more precisely, an observed-score reliability, of which Table 1's
alpha is the only one in hand. Omega-ordinal is categorically the wrong kind of
coefficient for Eqs (10)–(12), not merely the less convenient one.**

The derivation settles the metric question. Schmukle's Eq (8) (p. 819) defines
the reliability entering the method as `r_xx = s_t² / s_x²` — the ratio of
true-score variance to *observed composite* variance — and everything downstream
is that same quantity: the shrinkage `sqrt(r_xx)` in Eq (10) is the ratio
`s_t / s_x`, and the half-width `s_x · sqrt(1 − r_xx)` in Eq (11) is the SEM of
the observed score. `s_x` and `r_xx` are two moments of one random variable, the
observed scale score. Substituting a reliability defined on any other variable
breaks the algebra that makes Eq (11) collapse to the SEM at all.

Omega-ordinal is defined on another variable. Whether computed as "ordinal
alpha"/"ordinal omega" from a polychoric correlation matrix (Gadermann, Guhn, &
Zumbo, 2012) or as omega from a WLSMV categorical CFA on the latent-response
scale, it estimates the reliability the composite *would* have if the continuous
latent response variates underlying the ordinal categories were observed and
summed. That hypothetical composite has a different variance and a different
reliability than the observed 1–4 item mean; Chalmers (2018, *Educational and
Psychological Measurement*, "On misconceptions and the limited usefulness of
ordinal alpha") makes exactly this point — ordinal reliability coefficients
describe an instrument that was not administered. Plugging omega-ordinal into
Eq (10) would shrink the observed deviation by the latent composite's
`sqrt(rel)`, and into Eq (11) would scale the observed SD by the latent
composite's error fraction: the resulting `SERE` is the SEM of nothing, and the
coverage guarantee Schmukle demonstrates has no reason to hold.

So the implementing session's metric argument (Background point 2) is correct,
and I will state its converse plainly too: this is not an argument that alpha is
the *ideal* observed-score coefficient. Under CTT with uncorrelated errors,
alpha is a lower bound on observed-score reliability, exact only under
(essential) tau-equivalence; a coefficient that models the observed score but
relaxes tau-equivalence (question 3) is closer to `rel`'s definition. Alpha is
the right *kind* and a mild underestimate within that kind; omega-ordinal is the
wrong kind. Schmukle's own validation also used alpha on 6-category Likert items
(p. 822 and the Table 1 note: CIs "estimated using all three approaches using
Cronbach's alpha as an estimate of the reliability") with scale-corrected
coverage .95 at every reliability from .10 to .90 — supporting evidence, though
see finding B1 for the caveat on what that simulation does and does not cover.

### 2. Does alpha's behavior on these scales materially distort what ships?

**No. The distortion from any plausible alpha attenuation is small and runs in
the conservative direction in both places `rel` enters. The genuinely visible
oddity on the skewed scales is a property of the fixed CTT method, not of
alpha, and calls for documentation, not restriction.**

First, a conflation to name. The "alpha attenuates on categorized, skewed
items" argument is almost always an argument that Pearson-based alpha
understates the reliability of the *latent* construct relative to a polychoric
coefficient. For the observed score — the thing Eqs (10)–(12) are about — alpha
is computed from the actual covariances of the actual observed items, and its
bias is the ordinary congeneric lower-bound gap (typically .01–.03, larger only
with markedly heterogeneous loadings), plus the possibility of *over*statement
if errors correlate. Coarse categorization lowers the observed score's true
reliability itself; alpha tracks that. There is no separate large
categorization bias against the observed-score target.

Direction and magnitude, taking the worst printed case. Suppose Conversion
Symptoms' true observed-score reliability were .88 where Table 1 prints alpha
.82 — a δ of .06, generous for a scale this internally consistent:

- **Half-width** `1.96 · SD · sqrt(1 − rel)`: 0.258 at .82 vs 0.210 at .88 —
  the interval is ~22% too wide (ratio `sqrt(.18/.12)` = 1.22). Coverage
  exceeds the nominal level. Conservative.
- **Shrinkage** `sqrt(rel) · (x − M)`: at a score 2 SD above the mean the
  estimate sits `(sqrt(.88) − sqrt(.82)) · 2 SD` = 0.020 raw units = 0.065 SD
  closer to the mean than ideal. Well inside the interval's own width.
  Conservative in the regression-to-the-mean direction.

A researcher reading `hsr_conversionSymptoms 1.35 [1.08, 1.60]` versus the
ideal `[1.13, 1.55]` draws the same substantive conclusion. For the high-alpha
scales (Distress-Dysphoria, .96) δ is necessarily tiny and the effect vanishes.
This is not large enough to matter, and it errs toward wider intervals — the
safe failure mode for a package whose IP4 posture is scores, never judgment.

What *will* look odd on the skewed scales is the method itself. For Conversion
Symptoms (M 1.12, SD 0.31, rel .82), a respondent at the floor x = 1.0 gets
RETS 1.011 with 95% CI [0.754, 1.269] — a lower bound below the instrument's
minimum, and below the printed sample range's floor of 1.0. This is not an
alpha artifact: it follows from CTT's constant, symmetric interval on a
distribution with skew 4.10, where most respondents sit at or near the floor.
Schmukle's Further Limitations section (pp. 823–824) concedes precisely this —
CTT intervals are identical across true scores while information is not, and
his dichotomous simulation (Table 2) shows the coverage cost at extreme scores
when item distributions depart from normality. Swapping coefficients cannot fix
it; only an IRT interval could, and the method is fixed.

**What M041 should do:** ship alpha, and document rather than restrict. Add to
the `interval_hitopsr()` help page (i) that bounds are symmetric, constant-width
CTT intervals and may extend below the scale minimum (or above the maximum) for
strongly skewed scales, with the manuscript's own prose as the pointer to which
scales those are (it names Antisocial Behavior, Conversion Symptoms, Purging,
Paraphilias, Excessive Exercise, Trauma Reactions, Reality Distortion,
Gambling, Delusions as substantially non-normal); and (ii) that coverage is
marginal over the reference population under a linear-normal measurement model,
per Schmukle's Further Limitations. Do not clamp the bounds to `srange`: the
formula would no longer be the cited Eq (12), AC4's closed-form oracle would
need carve-outs, and a bound outside the attainable range is itself informative
about the method's fit to that scale (IP1's spirit: leave the discrepancy
visible). Restricting the function — refusing the skewed scales — would draw an
arbitrary skew cutoff this package would then own; rejected.

### 3. Is a Green & Yang categorical omega better than both?

**It is the psychometrically best-defined candidate, and it is still not worth
it for this release.**

Green & Yang (2009, *Psychometrika*, "Reliability of summed item scores using
structural equation modeling: the case of congeneric items" / their nonlinear
SEM coefficient for ordered-categorical items) estimate the reliability of the
*observed* summed score from a categorical CFA — thresholds plus polychoric
loadings, mapped back through the normal ogive to the observed metric. Unlike
omega-ordinal it answers the right question for Eq (8), and unlike the
package's `calc_omega()` (a continuous MLR one-factor omega-total,
`R/reliability.R` 129–216 — no `ordered=`, no thresholds) it models the
4-category responses as categories. If M041 were choosing on psychometrics
alone with all coefficients printed in the source, this is the one to pick.

The gain over alpha is the congeneric-vs-tau-equivalent gap plus a correct
treatment of categorization — for scales with alphas mostly ≥ .80 and
reasonably homogeneous loadings, a few hundredths of reliability, i.e.
intervals a few percent narrower and shrinkage marginally weaker, in exactly
the region question 2 showed to be immaterial. The costs are concrete: the
package would be *producing* a shipped constant (question 4); the estimation is
fragile precisely where the argument for it is strongest (Conversion Symptoms'
items will have near-empty upper categories, sparse polychoric tables,
smoothing/convergence decisions across 93 scales); every analytic decision —
exclusions, estimator, smoothing — is a decision this package must make and
defend where the paper's alpha needed only transcription; and the number would
be verifiable against nothing but the script that made it. A conservative error
of a few percent in interval width does not buy that. If it is ever done, it is
package-wide by nature (the same argument applies verbatim to HiTOP-BR and
PID-5 intervals) and belongs downstream of the ROADMAP recomputation candidate,
not inside M041.

### 4. Does IP2 bar a recomputed coefficient here?

**IP2 does not flatly bar it, but the conditions under which it would be
admissible amount to promoting the ROADMAP recomputation candidate first — so
for M041 as scoped, the practical answer is that only printed coefficients are
admissible.**

Read closely, IP2 has two prongs. The test prong — no test asserts the code's
own output as truth — is not automatically violated by a scripted recomputation:
"independent recomputation" is a form of verification IP2 itself names. The
constants prong — every shipped constant "traces to a SOURCES.md-cited
authority before it ships, machine-checked where feasible" — is where the
tension lives. The raw `.sav` is an authority's artifact and can be shelf-cited
with a checksum, exactly as `data-raw/hitopsr_table1.R` pins the manuscript.
But a coefficient is not a cell of that file; it is the file *plus* this
package's analytic choices (exclusion rules, estimator, missing-data handling,
polychoric smoothing), and the only machine check available is re-running the
same script — self-reference in the precise sense IP2's test prong forbids. A
transcribed alpha is checkable cell-by-cell against print by AC2's independent
extraction; a recomputed omega is checkable against nothing external.

The conditions that would make a recomputation acceptable:

1. **Calibration against print first.** The recomputation pipeline must
   reproduce Table 1's printed alpha, M, and SD cell-by-cell within a stated
   tolerance on the same analytic sample before any unprinted coefficient from
   the same pipeline ships. Matching 93 × 3 printed cells is the independent
   machine check standing in for a printed value — it demonstrates the
   pipeline's exclusions and estimator match the paper's. This is exactly the
   scope of the standing ROADMAP candidate row ("the paper's exclusion rules,
   its alpha estimator, and how close counts as agreement"), which the M041
   plan gate deliberately kept out of this milestone.
2. **A D-entry** fixing the analytic decisions in advance, with the `.sav`
   SHA-256-pinned in SOURCES.md, mirroring the D-041/D-042 reconciliation
   pattern for when the accepted paper or a corrected data file arrives.
3. **Provenance travels with the value.** `reliabilityType` names the
   recomputed coefficient and the documentation states the package computed it
   from the development data, not that the paper reports it.
4. **Coherence of the mixture.** Recomputed `rel` beside transcribed `M`/`SD`
   is coherent only because condition 1 proves all three describe the same
   sample and scoring; without it the mixture is incoherent — Eq (8) ties
   `r_xx` and `s_x` to one score distribution, and a `rel` from a
   differently-filtered sample paired with the printed SD silently violates
   that.

Since condition 1 is out of M041's scope by the plan gate's own decision, M041
ships the printed alpha. This is also the answer AC2's design assumes, and it
keeps D-032's "machine-diffed cell by cell" condition meaningful.

## Beyond the brief

- **B1 — What Schmukle's ordinal simulation does not cover.** Its item
  thresholds were "chosen in a way to ensure approximately normally distributed
  item responses" (p. 822), so the .95-everywhere result validates the method
  under near-normality — not under Conversion Symptoms-like skew. The
  dichotomous simulation is the closer analogue to a floor-effect scale and
  shows coverage falling to ~.87 at extreme scores when reliability is high
  (Table 2, rel .90, T = 30/70). This is a limitation of the fixed method, not
  grounds to reopen it; it sharpens why the question-2 documentation should say
  coverage is model-conditional and marginal. AC5's coverage oracle, which
  generates data *under the source's own linear-normal model*, will and should
  pass — it verifies the implementation, and cannot certify robustness to skew;
  the help-page limitation is what carries that.
- **B2 — Bounds outside the response range are reachable on ordinary inputs.**
  Worked case in question 2 (Conversion Symptoms at x = 1.0 → CI lower bound
  0.754 on a 1–4 instrument). Worth a sentence in the help page and possibly a
  test asserting the function does *not* clamp, so the behavior is pinned as
  intended rather than discovered by a user.
- **B3 — AC1's wording.** AC1 still says the dataset carries "alpha", while the
  2026-08-28 gate fixed the schema as `reliability` + `reliabilityType`. With
  this report answering alpha, the held amendment is now one edit:
  `reliability` carries Table 1's printed Cronbach's alpha and
  `reliabilityType` is `"alpha"` for every row. The AC4/AC5 phrases "shipped
  alpha range" and "lowest, median, and highest alpha" remain correct in
  substance.
- **B4 — Two-decimal alphas.** Table 1 prints alpha to two decimals; the
  rounding perturbs `sqrt(1 − rel)` by at most ~0.7% of half-width at rel .82.
  Nothing to do; noted so no one mistakes it for a discrepancy when a future
  recomputation surfaces third-decimal differences.

## Recommendations

1. **Apply.** Ship Table 1's printed Cronbach's alpha as `rel`:
   `hitopsr_devstats$reliability` = the printed alpha,
   `reliabilityType = "alpha"`. (Questions 1, 4.)
2. **Apply.** Do not compute omega-ordinal for this purpose in any release —
   not a deferral: it is defined on the latent response metric and is the wrong
   kind of coefficient for Eqs (10)–(12) regardless of cost. (Question 1.)
3. **Apply.** Add the two help-page limitations from question 2: constant-width
   symmetric CTT intervals that can cross the scale bounds on strongly skewed
   scales (naming the manuscript's own list of non-normal scales, or pointing
   to the shipped skewness column if `hitopsr_devstats` ever carries one), and
   coverage that is marginal and model-conditional per Schmukle's Further
   Limitations. T7 is the natural home. (Question 2, B1, B2.)
4. **Apply.** Amend AC1's "alpha" wording to the `reliability`/
   `reliabilityType` schema now that the coefficient question is answered.
   (B3.)
5. **Consider.** A test pinning that bounds are not clamped to `srange` (or, if
   Jeff prefers clamping, a decision entry — it departs from the cited Eq (12)
   and touches AC4's oracle). Default recommendation: no clamping. (B2.)
6. **Consider.** A future package-wide milestone computing Green & Yang (2009)
   categorical observed-score omega, gated behind the ROADMAP recomputation
   candidate and the four conditions in question 4; the coefficient is the
   right one in principle, and the candidate row is where the prerequisite
   calibration already lives. No urgency — the expected change is a few percent
   of interval width, in the direction alpha already errs safely. (Questions 3,
   4.)
7. **Reject** restricting `interval_hitopsr()` on skewed scales: it would
   substitute this package's arbitrary skew cutoff for a documented,
   source-anchored limitation, and IP1's posture is to keep discrepancies
   visible rather than to gate them. (Question 2.)
8. **Reject** recomputing `rel` inside M041 under any coefficient, including a
   recomputed alpha: without the calibration-against-print step it produces an
   incoherent mixture with the transcribed `M`/`SD` and a constant with no
   external check, and with that step it is the recomputation candidate by
   another name. (Question 4.)
