# RB02: PID-5 raw ↔ T ↔ percentile lookup rules (M027)

- **Date:** 2026-07-30
- **Output required:** write findings to `cairn/reviews/RR02-pid5-norm-lookup-rules.md`

You are performing an independent expert review. This brief is fully
self-contained — do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package that scores HiTOP Society questionnaire instruments. Its
PID-5 family scores three versions of the Personality Inventory for DSM-5: FULL
(220 items), SF (100 items), BF (25 items). Items are coded 0–3 by default
(`srange = c(0, 3)`). `score_pid5()` returns scale scores as **item means**: a
FULL/SF domain is the mean of its three primary facet means, a BF domain is the
mean of its 5 items, and the BF `total` is the mean of all 25 BF items. Four
validity scales are integer sums: `INC`/`INCS` (sums of within-pair absolute
differences), `ORS` (a count of items answered at the range maximum), `PRD` (a
22-item raw sum).

Milestone M025 shipped the dataset `pid_norms` (1,056 rows;
`version`/`scale`/`tscore`/`raw`/`percentile`), transcribed from the normative
tables in Markon, Fossati, Somma & Krueger (2024), *Understanding the PID-5*
(APA Publishing), Appendix "Normative Score Distributions", pp. 113–219, and
verified cell-for-cell against the book by `data-raw/verify_norms_against_book.R`.
`percentile` is stored as a proportion in [0, 1]. The 16 domain/total scales
carry a `tscore`; the four validity scales carry `NA` there and are
score→percentile only.

Milestone M027 now builds `norm_pid5()`, which converts scored columns to T
scores and percentiles. Its acceptance criterion AC1 requires that three numeric
rules be settled by independent review **before** the conversion primitives
ship, because the book prints tables and no instruction for using them.

**What was already checked, so you need not.** The Appendix's "Background and
Methods" (pp. 113–115) describes the normative sample, the inclusion criteria,
and the census weighting, then proceeds directly to the tables. A search of all
eight chapters plus the Appendix prose for rounding, interpolation, or lookup
language returns exactly one relevant sentence, in Chapter 7: "Tables of T
scores and percentiles corresponding to raw scores are given in the Appendix,
'Normative Score Distributions.'" **The book states none of the three rules.**
They must therefore be chosen and defended (GP1's "where no published rule
exists" branch), which is why this brief exists.

## Materials

Read, in this order:

1. `cairn/references/markon2024.md` — the source note for the book: citation,
   which seven of the twelve Appendix tables ship, page anchors, the normative
   sample, and open questions.
2. `cairn/milestones/M027-pid5-norming-functions.md` — the milestone. AC1 is the
   criterion this brief serves; AC2–AC7 are the surrounding contract (signature,
   column naming, capping, `srange` reconciliation, documentation).
3. `cairn/DESIGN.md` lines 89–95 — principles IP2, IP3, IP4, GP1 (quoted under
   Constraints below).
4. `cairn/DECISIONS.md` — entries D-018, D-020, D-021 (read whole; each is
   short).
5. `R/score_pid5.R` (scale computation, proration) and `R/validity_pid5.R` lines
   88–205 (the four validity scales' integer arithmetic).
6. `data/pid_norms.rda` and the seven CSVs `data-raw/norms_pid5*.csv`.

The book itself is on the gitignored source shelf at
`cairn/references/sources/markon2024.epub`. It is an EPUB with its own pagebreak
anchors; the Appendix is `OEBPS/xhtml/20_Appendix.xhtml`. Consult it if you want
to check a printed cell or the prose above.

### Established facts — verify these before answering

Run this from the repo root (R ≥ 4.1, no packages beyond base):

```r
load("data/pid_norms.rda"); d <- as.data.frame(pid_norms)
m <- d[!d$scale %in% c("INC", "INCS", "ORS", "PRD"), ]      # 16 T-carrying scales
for (k in split(m[m$raw > 0, ], ~ version + scale, drop = TRUE)) {
  fit <- lm(raw ~ tscore, k)                                  # F1
  cat(k$version[1], k$scale[1], "exact 2dp matches:",
      sum(abs(round(predict(fit), 2) - k$raw) < 1e-9), "/", nrow(k),
      " M:", round(coef(fit)[1] + 50 * coef(fit)[2], 3),
      " SD:", round(coef(fit)[2] * 10, 4), "\n")
}
subset(m, version == "SF" & scale == "psychoticism")[1:14, ]  # F2, F3
subset(m, raw > 3)                                            # F4
```

- **F1 — the T-carrying tables are exactly linear.** `raw = M + (T − 50)/10 × SD`
  rounded to 2 dp reproduces 856 of the 863 printed non-zero rows exactly across
  all 16 scales; the 7 exceptions are single cells off by 0.01 at a rounding
  boundary. These are linear T scores, not normalized/area-transformed ones.
- **F2 — every tie is a floor artifact.** The only raw value repeated within any
  scale is 0.00, printed across runs of up to 13 consecutive T rows (SF
  psychoticism, T=30–42; BF psychoticism and BF antagonism, 8 rows each). Those
  are exactly the rows where the F1 line predicts a negative raw, i.e. clipping
  at zero.
- **F3 — percentile is empirical, not a transform of T.** Inside the SF
  psychoticism tie run, percentile is 0.00 at T=30–41 but 0.31 at T=42, so a raw
  of 0.00 is ambiguous on *both* output columns. Across scales, percentile
  diverges sharply from the normal CDF of the linear T (FULL negativeAffectivity
  T=37: printed 0.02, normal CDF 0.097) and saturates at 1.00 from roughly T=78
  upward.
- **F4 — 29 printed rows exceed the attainable 0–3 ceiling**: BF
  negativeAffectivity (11 rows, T=85–95), BF detachment (8, T=88–95), BF
  disinhibition (2, T=94–95), FULL negativeAffectivity (3, T=88–90), SF
  negativeAffectivity (5, T=86–90). Every one of them carries percentile 1.00,
  which the last attainable row already carries — so the unreachable region
  costs percentile information nowhere and T information only. Concretely: a BF
  negativeAffectivity of 3.00, the maximum a respondent can obtain, falls at
  T=84 (T=85 is printed at raw 3.05).
- **F5 — printed rows and attainable scores barely intersect.** A BF domain is a
  5-item 0–3 mean, so it takes only the 16 values 0, 0.2, …, 3.0; just 39 of the
  305 printed BF domain raws are such multiples. The BF `total` (25 items) can
  take multiples of 0.04, of which 17 of 61 printed rows are; an SF domain
  (mean of three 4-item facet means) takes multiples of 1/12, of which 65 of 305
  printed rows are. Between-rows lookup is therefore the normal case, not an
  edge case.
- **F6 — the validity scales are integer-valued and their tables are
  contiguous.** `INC` prints 0–23, `INCS` 0–15, `ORS` 0–8, `PRD` 0–55, one row
  per integer. `validity_pid5()` computes all four with `rowSums()` and no
  proration, so an observed value is always an integer. `PRD`'s attainable
  maximum is 66 (22 items × 3), above the table's last row of 55.

## Questions

1. **Between-rows raw → T, for the 16 T-carrying scales.** Given F1 and F5,
   which rule should `norm_pid5()` use for a raw score falling between two
   printed rows: (a) nearest printed row, (b) floor — the highest row whose raw
   is ≤ the observed value, (c) linear interpolation between the bracketing rows
   yielding a non-integer T, or (d) inversion of the F1 line rounded to an
   integer T? State the rule as an assertion checkable against the printed rows,
   and name what each rejected option would get wrong. Address the tie-breaking
   sub-case of (a) explicitly (a raw exactly midway between two rows).

2. **Between-rows raw → percentile, for those same 16 scales.** F3 shows the
   underlying percentile function is empirical, non-linear in T, and saturating.
   Should the percentile rule be the same as your answer to Q1 or different, and
   why? Should the returned percentile stay at the printed 2-dp resolution, or
   may it be interpolated to a finer value? Does your answer change for the four
   validity scales, given F6?

3. **The floor ties.** By F2/F3, an observed raw of 0.00 matches up to 13
   printed rows spanning two distinct percentiles. What T score and what
   percentile should such an observation receive, and on what principle — noting
   that 0.00 is not a rare value for a 5-item BF domain in real data. State the
   rule generally enough to cover any future tie, not only the zero case.

4. **The 29 above-ceiling rows.** AC4 requires that T → raw reproduce every
   printed row exactly, which includes these. Should raw → T (a) retain them,
   leaving the top of each affected scale unreachable, (b) treat the highest
   *attainable* row as the ceiling so the scale maximum returns the top T, or
   (c) something else? Under (a), a maxed BF negativeAffectivity returns T=84
   while 11 printed rows sit above it — is that the right user-facing behavior,
   and should `norm_pid5()` say anything about it? Note that AC5 already fixes
   the *out-of-table* behavior (cap to the nearest end's printed values); this
   question is about rows that are in the table but unreachable.

5. **Table authority vs. the linear form (IP3).** IP3 states "no norms without
   published tables." F1 means the shipped tables are a rounded rendering of 16
   linear functions whose M and SD the book does not print. Does IP3 permit
   `norm_pid5()` to compute a between-rows T by inverting a fitted line —
   constants derived here, not published — or must every returned value be
   derived only from printed rows? If inversion is numerically better but
   IP3-barred, state the defensible compromise. This question is the crux: your
   answers to Q1–Q3 should be consistent with your answer here.

6. **Oracles (IP2).** The book prints no worked example — no "a raw of X gives a
   T of Y" anywhere — so the printed rows are the only ground truth for exact
   matches, and none exists for between-rows values. What is the minimum
   evidence set that makes your Q1–Q4 rules verifiable without asserting the
   function's own output as truth? The book names an APA-referenced computerized
   scoring system at `https://pid5-us-en.pegasopoint.it` (p. 115) that "provides
   raw scores, T scores, and percentiles" — does that qualify as an independent
   second oracle type here, or should it be excluded (access, version drift,
   unverifiable provenance)? Recommend for or against, with reasons.

## Constraints

Fixed; do not relitigate. Flag disagreement with any of these explicitly in your
report rather than silently working around it.

- **IP2 — ground truth, never self-reference.** No test asserts the code's own
  output as truth; every shipped numeric constant traces to a cited authority.
- **IP3 — no scoring without a key; no norms without published tables.**
- **IP4 — scores never judgment.** No prose about what a T score *means*
  clinically may enter the package.
- **GP1 — published rules win the defaults; deviations are loud.** Where no
  published rule exists, defaults are chosen and documented on their merits.
- **`pid_norms` cell values are frozen.** The repo's CLAUDE.md forbids changing
  shipped keying/norming data without the maintainer's explicit sign-off. A
  recommendation that implies editing, adding, or dropping table rows must say
  so in those words rather than assume it.
- **D-020** settles the shifted-response-coding reconciliation per scale (item
  means shifted by `low`, `PRD` by `low × nItems`, `ORS` re-derived against the
  shifted maximum, `INC`/`INCS` unchanged). Not open here.
- **D-021** settles that a BF `total` prorates independently of the domains, so
  an `NA` total has no T to convert. Not open here.
- **D-018** settles the scale names (`INC`, not the book's "VRIN"). Not open
  here.
- **M027's AC2 signature and column contract** — `(data, scores, version, srange,
  prefix, append = TRUE)`, one `_t` column per T-carrying covered scale and one
  `_ptl` per covered scale — is fixed. AC5's out-of-table capping decision is
  likewise fixed.
- **Out of scope:** facet-level, sex/age-stratified, and Informant Form norms;
  HiTOP-SR/BR norms; profile plots and rendered reports; `validity_pid5()`'s cut
  scores under shifted codings (deferred as DESIGN Known issue #3).
- **No new package dependencies.** Dependency changes go through a separate
  maintainer gate. Internals are base R; output is a tibble.

## Output format

In `RR02-pid5-norm-lookup-rules.md`: answer each question by number with your
reasoning and evidence; list any additional findings separately under "Beyond
the brief"; end with concrete recommendations, each marked apply / consider /
reject-with-reason. Where findings bind implementation, also emit a
`## Binding criteria` section: numbered `BC1…`, each a measurable assertion
checkable against evidence, with any numeric projection stating its tolerance.
These are ingested VERBATIM into M027's acceptance criteria and mechanically
diffed against this file; departures are legal only through M027's shown
"Deviations from RR02" table.
