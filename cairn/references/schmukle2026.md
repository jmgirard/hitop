# schmukle2026 — the regression-based true score with scale correction: the interval formula `interval_hitopsr()` computes

**Provenance.** Ingested 2026-08-28 by M041 from
`cairn/references/sources/schmukle2026.pdf` (gitignored) — the publisher's typeset
article, supplied by the maintainer. Pagination: journal pages 817–825, printed in
the running heads, so every anchor below is the printed journal page. Extraction:
verified 2026-08-28 against the source — each equation below was read from the
typeset page, and Box 1's two worked examples were recomputed from Equations
(10)-(12): both estimates and both standard errors reproduce the printed two
decimals exactly, and both recomputed upper bounds land 0.01 above the printed
figure because the article forms its bound from the rounded estimate and standard
error it has just printed (58.94 + 1.96 x 4.47 = 67.70, against 67.71 unrounded)
— observed 2026-08-28.

**Citation.** Schmukle, S. C. (2026). Unbiased confidence intervals for
psychological testing: The regression-based true score approach with scale
correction. *Assessment, 33*(5), 817–825.
https://doi.org/10.1177/10731911251362532. The article's copyright line reads
"© The Author(s) 2025" against the 2026 issue date; the issue is what this repo
cites. A preprint is at https://osf.io/preprints/psyarxiv/dsw5m and the simulation
code at https://osf.io/pfwav/ — neither is on the shelf, and neither was read —
observed 2026-08-28.

**Role.** The primary source for the estimate and the bounds `interval_hitopsr()`
returns. It settles which of the three classical-test-theory interval approaches
the package computes, the exact form of the point estimate and half-width, and what
the method's coverage claim does and does not cover. D-043 reads Equation (8) here
to fix which *kind* of reliability may enter Equation (11).

## Extracted values

### The three approaches, in the article's own notation

`x_j` is examinee *j*'s observed score, `M_x` and `s_x` the mean and standard
deviation of the observed score in the reference group, `r_xx` the reliability, `z`
the unit normal value for the confidence level ("z = 1.96 for the usual 95% CI",
p. 818).

- **Equation (1)**, p. 818 — `SEM = s_x * sqrt(1 - r_xx)`.
- **Equation (3)**, p. 818 — the traditional interval: `CI = x_j ± z * SEM`.
- **Equation (5)**, p. 818 — the regression-based estimated true score, "formula
  first introduced by Kelley, 1927": `ETS = M_x + r_xx * (x_j - M_x)`.
- **Equation (6)**, p. 818 — its standard error, the standard error of the
  estimate: `SEE = s_x * sqrt(r_xx) * sqrt(1 - r_xx)`.
- **Equation (7)**, p. 819 — the regression interval: `CI = ETS ± z * SEE`.
- **Equation (8)**, p. 819 — the definition of reliability the whole argument turns
  on: `r_xx = s_t^2 / s_x^2`, hence `s_t = sqrt(r_xx) * s_x`.
- **Equation (9)**, p. 821 — the rescaling, `RETS = M_x + ((ETS - M_t) / s_t) * s_x`.
- **Equation (10)**, p. 821 — the rescaled estimated true score in one step, using
  `M_t = M_x` and Equation (8):

      RETS = M_x + sqrt(r_xx) * (x_j - M_x)

- **Equation (11)**, p. 821 — the standard error of the rescaled estimate:

      SERE = (1 / sqrt(r_xx)) * SEE = s_x * sqrt(1 - r_xx) = SEM

  The article states the consequence in its own words: "That is, the SERE is
  identical to the traditional SEM."
- **Equation (12)**, p. 821 — the interval this package computes:

      CI = RETS ± z * SEM

These three are what the article tells a practitioner to use: "Confidence intervals
that are based on the regression approach with scale correction can be calculated in
the practice of psychological testing using Equations (10) to (12)" (p. 823).

**The shrinkage factor is `sqrt(r_xx)`, not `r_xx`.** Equation (5)'s uncorrected
ETS shrinks by `r_xx`; Equation (10)'s RETS shrinks by `sqrt(r_xx)`. The two are
different estimators and the article argues against the first: the ETS is on the
true-score metric, whose SD is `sqrt(r_xx) * s_x`, so it "cannot be interpreted like
observed scores" (pp. 819–820).

**The half-width is `s_x * sqrt(1 - r_xx)`, not `s_x * sqrt(r_xx * (1 - r_xx))`.**
The second is Equation (6), the standard error of the *uncorrected* ETS. Substituting
it into Equation (12) computes neither approach.

### Box 1, p. 823 — the article's own worked examples

Reproduced here because they are the closed-form check any implementation of
Equations (10)–(12) must pass:

- "Ms. A has an observed T-score of 60 (`M_x` = 50, `s_x` = 10) on a personality
  test with a reliability of .80." → `RETS = 50 + sqrt(0.80) * (60 - 50) = 58.94`;
  `SEM = 10 * sqrt(1 - 0.80) = 4.47`; `95% CI = 58.94 ± 1.96 * 4.47 = [50.18, 67.70]`.
- "Mr. B has an observed IQ score of 90 (`M_x` = 100, `s_x` = 15) on an intelligence
  test with a reliability of .90." → `RETS = 100 + sqrt(0.90) * (90 - 100) = 90.51`;
  `SEM = 15 * sqrt(1 - 0.90) = 4.74`;
  `95% CI = 90.51 ± 1.96 * 4.74 = [81.22, 99.80]`.

Both boxes are printed with the substitutions carried out at two decimals, and both
upper bounds are the value that arithmetic gives: 67.70 and 99.80 follow from the
rounded 58.94/4.47 and 90.51/4.74, where the same intervals computed without
rounding are 67.71 and 99.81. So a test that pins these two boxes to the last digit
pins the article's rounding, not its formulas — the package's closed-form oracle
recomputes the formulas at full precision instead.

### The validating simulation (Table 1, p. 822; method pp. 821–822)

What the article's coverage result was produced under, in its own terms:

- "N = 1,000,000 normally distributed true T-scores were simulated (M = 50,
  SD = 10). Then, for each true score, responses on 10 ordinal items with 6
  categories were simulated based on the specified reliability. Observed scores were
  calculated as the sum of responses to the 10 items." (Table 1 note, p. 822.)
- "Item thresholds were chosen in a way to ensure approximately normally distributed
  item responses." (p. 822.)
- "95% CIs were estimated using all three approaches using Cronbach's alpha as an
  estimate of the reliability." (p. 822.)
- The result for the scale-corrected approach: coverage of .95 at every reliability
  from .10 to .90, both at an observed T of 50 and at 30 or 70, and across all
  T-scores (Table 1, p. 822). "the regression approach with scale correction
  resulted in confidence intervals that always covered about 95% of the actual true
  scores, regardless of the reliability of the test and regardless of the observed
  T-score" (p. 823).

**The generative model, stated in the form a check can reproduce.** The article
describes the simulation in its own terms above; the two sentences that fix the
model are that true scores are drawn on the standard T scale (M = 50, SD = 10) and
that the observed item sum is "standardized to obtain observed scores that are
T-scaled (M = 50, SD = 10)" (p. 822). True and observed scores therefore sit on the
*same* metric with the *same* standard deviation, which makes their correlation the
reliability index `sqrt(r_xx)`:

    t ~ Normal(M, SD)
    x = M + sqrt(r_xx) * (t - M) + e,   e ~ Normal(0, SD * sqrt(1 - r_xx))

Equation (10) is then exactly the regression of `t` on `x`, and Equation (11)'s
`SD * sqrt(1 - r_xx)` is exactly that regression's residual standard deviation —
which is why Table 1's coverage is nominal at every reliability, and why Equation
(11) coincides with the SEM.

Note what `t` is here: the quantity drawn on the observed score's own metric, not
the classical true score of the composite, whose standard deviation is
`sqrt(r_xx) * SD` by Equation (8). Generating `t` at that smaller spread instead
produces a different result — Equation (7)'s uncorrected interval becomes the
nominal one and Equation (12)'s becomes conservative — so the metric `t` is drawn
on is the load-bearing detail of any reproduction.

**What the coverage claim is over.** The proportion is taken over the simulated
population — a million true scores drawn as above, with an observed score
generated from each. It is coverage over that population, and Table 1 also reports
it at fixed *observed* T-scores of 50 and of 30 or 70, where it is likewise
nominal. It is **not** coverage at a fixed *true* score, which this estimator does
not promise and which varies with how far that true score sits from the mean. The
dichotomous-item simulation (Table 2, p. 823), where the linear model does not
hold, is where the article's own coverage degrades at extreme scores.

### Stated limitations

- **Which group the mean belongs to.** "to apply the regression-based ETS approach
  (regardless of whether with or without scale correction), one needs to know which
  group the person being tested comes from" (p. 823). Absent other information the
  article takes the participant to come from the total population and regresses to
  the norming sample's mean; where a person is known to belong to a narrower group,
  "the mean value used in Equation (10) should be the specific group mean value and
  not the mean value of the total population" (p. 823).
- **Constant width.** "When tests are based on the CTT, confidence intervals are
  identical across different true scores. This assumption can reasonably be
  questioned" (p. 823) — an accurate score-dependent interval "would only be
  achieved using IRT instead of CTT" (p. 824).
- **Sampling variability in the norms is not modelled.** "all confidence intervals
  discussed so far in this article do not take into account the uncertainty in
  normed test scores due to sampling variability" (p. 824).
- The article gives **no rule for a bound that falls outside the response range**,
  and none of its examples is on a bounded raw metric — observed 2026-08-28.

## Traces to

- `R/interval_engine.R` — computes Equations (10)–(12).
- `R/interval_hitopsr.R` and its help page — the exported wrapper, the page anchor
  it cites, and the two limitations it prints.
- `tests/testthat/test-interval_hitopsr.R` — the closed-form oracle recomputes
  Equations (10)–(12) in hand arithmetic; the simulation-coverage oracle generates
  under the measurement model recorded above.
- `cairn/ORACLES.md` — both oracles' registry entries name this page.
- `cairn/DECISIONS.md`, D-043 — reads Equation (8) to fix which reliability may
  enter Equation (11).

## Open questions

- The simulation code at https://osf.io/pfwav/ is not on the shelf and was not read,
  so the exact threshold placement behind "approximately normally distributed item
  responses" is known only from the sentence quoted above. AC5's oracle generates
  under the model the article *describes*, not under a copy of its code — observed
  2026-08-28.
- The article's examples are all on unbounded standardized metrics (T, IQ). Whether
  its authors would endorse applying Equations (10)–(12) unchanged to a bounded 1–4
  item mean is not addressed anywhere in the text — observed 2026-08-28.
