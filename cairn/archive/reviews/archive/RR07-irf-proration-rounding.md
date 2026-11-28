# RR07: Proration rounding for the PID-5 Informant Form (M159)

- **Date:** 2026-10-03
- **Brief:** `cairn/reviews/RB07-irf-proration-rounding.md`
- **Binding criteria:** not requested. None emitted.
- **Verdict in one line:** answer (b). `score_pid5(version = "IRF")` rounds
  the prorated raw to the nearest whole number, halves up, through the shared
  `apa_mean()`. The informant key's "round up" is a wording lapse in a document
  with other copy errors. The APA series' own worked example rounds 23.33 down
  to 23.

## Materials read

All read 2026-10-03. `pdftotext` (Homebrew poppler) was available. Every
quotation below was extracted from the PDF itself, not from a secondary source.

| Document | Where | sha256 (first 16) | Rounding sentence |
|---|---|---|---|
| PID-5-IRF Adult, DSM-5-TR edition (shelf copy) | `cairn/references/sources/apa2013pid5irf.pdf`, p. 9 | `be4a260a20706002` | "If the result is a fraction, round up to the nearest whole number." |
| PID-5-IRF Adult, DSM-5-TR edition (live) | <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/DSM-5-TR/APA-DSM5TR-ThePersonalityInventoryforDSM5FullVersionInformant.pdf> | `07036dad99aded28` | Same ("round up"). Text identical to the shelf copy except the page-1 permission-request URL. |
| PID-5-IRF Adult, DSM-5 edition (2013) | <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_The-Personality-Inventory-for-DSM-5-Full-Version-Informant.pdf> | `b5427fa08d4d97f8` | Same ("round up"). |
| PID-5 Adult, DSM-5-TR edition (the SOURCES.md "APA scoring key" URL) | <https://www.psychiatry.org/getmedia/594673a6-1b9b-4298-8b52-c4c652c4a4e2/APA-DSM5TR-ThePersonalityInventoryForDSM5FullVersionAdult.pdf> | `ce366db46f324ba7` | "If the result is a fraction, round to the nearest whole number." |
| PID-5 Adult, DSM-5 edition (2013) | <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_The-Personality-Inventory-For-DSM-5-Full-Version-Adult.pdf> | `8aa9125553dad868` | "round to the nearest whole number" |
| PID-5-BF Adult, DSM-5-TR edition | <https://www.psychiatry.org/getmedia/f65c4386-b2bc-44a5-9ace-d6fea2211506/APA-DSM5TR-ThePersonalityInventoryForDSM5BriefFormAdult.pdf> | `ed3203c6f7c04ee0` | "round to the nearest whole number" |
| PID-5 Child 11–17, DSM-5-TR (shelf and live) | `apa2013pid5child.pdf`. <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/DSM-5-TR/APA-DSM5TR-ThePersonalityInventoryForDSM5FullVersionChildAge11To17.pdf> | live `b58ae4f3d076f144` | "round to the nearest whole number" |
| PID-5-BF Child 11–17 (shelf) | `apa2013pid5bfchild.pdf` | (shelf) | "round to the nearest whole number" |
| LEVEL 2 Anxiety Adult (PROMIS) | <https://psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_Level-2-Anxiety-Adult.pdf> | `123f0bf6675e324e` | "round to the nearest whole number." Worked example: 6 of 7 items answered, sum 20, "20 X 7/ 6 = 23.33", rounded raw "23". |
| LEVEL 2 Depression Adult (PROMIS) | <https://psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_Level-2-Depression-Adult.pdf> | `336ffbbfd3f02ec4` | "round to the nearest whole number." Worked example: 6 of 8 answered, sum 20, "20 X 8/ 6 = 26.67", rounded raw "27". |
| LEVEL 2 Mania Adult (ASRM) | <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/APA_DSM5_Level-2-Mania-Adult.pdf> | `931919e9a8e77c53` | "round to the nearest whole number" |
| Severity Measure for Depression Adult (PHQ-9), DSM-5-TR | <https://psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/DSM-5-TR/APA-DSM5TR-SeverityMeasureForDepressionAdult.pdf> | `c227a9e072137bd9` | "round to the nearest whole number" |
| Severity Measure for Panic Disorder Adult, DSM-5-TR | <https://www.psychiatry.org/File%20Library/Psychiatrists/Practice/DSM/DSM-5-TR/APA-DSM5TR-SeverityMeasureForPanicDisorderAdult.pdf> | `3558dca2540c1342` | "round to the nearest whole number" |
| Markon, Fossati, Somma & Krueger (2024), *Understanding the PID-5* (shelf epub) | `cairn/references/sources/markon2024.epub` | (shelf) | No passage on proration or rounding. Text search for "round", "prorat", "25%", "unanswered" and "missing item" found nothing relevant. |

Repository files read: `cairn/references/apa2013pid5irf.md`. `cairn/SOURCES.md`
(the "Note on FULL/SF domain scoring", the Sources list, OQ-4). `R/util.R`
(`round_half_up()`, `apa_mean()`). `R/score_engine.R`. `R/score_pid5.R`.
`cairn/DESIGN.md` (IP2, GP1 to GP3) and D-009 in `cairn/DECISIONS.md`.
`cairn/DECISIONS.md` (D-089, D-088). `cairn/milestones/M159-pid5irf-scoring.md`.
`tests/testthat/test-util.R` and `test-score_pid5.R` (the existing proration
tests).

Not read: Markon, Quilty, Bagby & Krueger (2013, *Assessment*). It is not on
the shelf. A development paper does not fix a clinician's rounding step in any
case.

## 1. Which rounding rule

**Answer: (b).** Keep the package's round-half-up rule for the IRF, shared with
every other PID-5 version through `apa_mean()`.

**Plain meaning.** "Round up to the nearest whole number" is not a clean
instruction. In careful usage "round up" means a ceiling. "To the nearest whole
number" names a different operation. A sentence that meant a ceiling says
"round up to the next whole number". The printed sentence is the adult key's
sentence with one word inserted and the rule-naming phrase left in place. So
even on its own terms it does not prescribe a ceiling. It reads as a loose
"round off", not as a changed rule.

**The other APA keys.** Twelve APA emerging-measure documents were read (table
above). Every one except the two editions of the informant key says "round to
the nearest whole number". Two of them carry worked examples, and both are
decisive. Level 2 Anxiety rounds 23.33 to 23. A ceiling makes that 24. Level 2
Depression rounds 26.67 to 27. The proration paragraph is one house template.
It repeats across the series with only the measure name and item count
changed. "Round up" is not a house phrasing. It occurs in the informant key
alone.

**The adult self-report key's current text.** The URL in SOURCES.md resolves
to the DSM-5-TR edition (PDF metadata: created 2022-03-09, modified
2022-07-14). Its sentence is still "round to the nearest whole number". The
2013 DSM-5 edition says the same. The informant key is a light edit of this
text: same paragraph, same sentence order, "number of items on the measure"
for "number of items on the PID-5".

**The informant key's copy errors.** The brief names one. Step 1 lists 16
reverse items. The Facet Table does not mark two of them, and their wording is
not reversed (D-089(c)). The 2013 DSM-5 edition shows a second. Its page
headers on pp. 3 and 5 read "Name/ID (child receiving care)", a leftover from
the template form. The DSM-5-TR edition corrects this to "individual receiving
care". So the TR revision touched the form pages and the rights boilerplate.
It left both scoring-text errors in place: the 16-item list and "round up".
That revision was not a re-derivation of the scoring rule. Survival of the
word in the TR edition is not evidence that APA meant it.

**Comparison of informant and self-report scores.** The IRF is built as the
informant counterpart of the self-report form. It has the same 25 facets, the
same items minus two, the same domain triplets and the same 0 to 3 scale. Its
intended use is alongside the self-report form on the same person (Markon et
al., 2013). A ceiling rule pushes a prorated informant facet one raw point
above the self-report rule whenever its fractional part is below one half. I
enumerated all `(n, answered, partial)` combinations for facet sizes 4 to 14
at 1 to 25% missing. That gives 564 prorated cases. In 216 the fractional part
is below .5 and the rules differ. In 30 it is an exact half, and both rules
go up. In 216 it is above .5, and both rules go up. On the facet average the
discrepancy is 1/n: 0.25 on a 4-item facet and 0.071 on a 14-item facet,
always in the same direction. A direction-fixed informant-minus-self offset
confined to prorated facets is an artifact no user expects or wants.

**IP2 and GP1.** Both readings trace to an APA document. IP2 is satisfied
either way, provided SOURCES.md cites what the package relies on. The nearest
rule traces to the adult key the IRF parallels and to the series' worked
examples. The ceiling traces to one word in one sentence of a key with two
demonstrated copy errors. GP1 asks for the published rule as the default and
loud deviations. The published rule for this series is "nearest". Departing
from one printed word is not a deviation from the rule. It is a deviation from
the key's literal text, and GP1's spirit says to make it loud. Question 4
covers where.

**Rejected: (c) something else.** Three shapes were considered: a
version-specific `missing` default, a warning on every IRF call, and an extra
`missing` level ("apa_ceiling"). Each carries an unsupported reading into the
API surface. D-088 rejected a
version-dependent `missing` default for the BFPM on the same grounds.

## 2. Consequences of (b)

**What makes the wording a lapse rather than a rule.** Four things together.
The sentence keeps the template's rule-naming phrase "to the nearest whole
number". Every other document in the series states nearest rounding, and the
two with worked examples demonstrate it. The same key carries two other copy
errors in text that the TR revision did not re-read. And the key offers
nothing a ceiling rule needs: no worked example, no scoring-sheet column, no
rationale. A ceiling makes the IRF the only APA measure whose prorated scores
are biased upward.

**Evidence that reopens the choice.** Any of these: an APA erratum or a later
IRF edition whose text or worked example rounds a fraction below one half
upward. A publication by the form's authors that states a ceiling for the IRF
(the shelf copy of Markon et al. 2024 does not). IRF normative tables
documented as built under a ceiling rule. A second APA measure that adopts
"round up" with an example that shows it. The decision entry must list
these.

**For completeness, the (a) computation.** It is `ceiling(partial * n / a)`
with a guard. A prorated sum that is already a whole number stays as it is.
Floating-point results must be snapped first:

```r
v <- partial * n / a
v <- ifelse(abs(v - round(v)) < 1e-8, round(v), ceiling(v))
```

With whole-number responses no such noise arises. I checked every `(n, a, partial)` combination
for n = 4 to 14 and found zero cases where an integer-valued quotient was
inexact. But `score_pid5()` accepts decimal responses. An item value of 0.1
scores without error, because `srange` is checked as integerish and item
values are not. There `partial * n / a` can land at 1.4000000000000001 or
10.000000000000002, and a bare `ceiling()` turns that into one too many. The
guard belongs in a version-specific branch of `score_engine()`'s `scale_fun`
(a `round_fun` argument supplied by the wrapper), not in the shared
`apa_mean()`. None of this is needed under (b).

## 3. AC2

AC2 as written does not stand. It says "under the APA rules the key prints
(reverse, prorate, average)". The key prints "round up", and the implementer
already read "the rules the key prints" as pointing to a ceiling. The wording
also hides the rounding step inside "prorate". Replace it with:

> AC2: `score_pid5(version = "IRF")` returns the 25 facet and 5 domain columns
> that the FULL version returns, named the same way. Its values equal
> hand-computed values under the APA PID-5 rule as D-009 states it and D-090
> extends it to the IRF: reverse the Facet Table's 14 items. A facet with more
> than 25% of its items unanswered is `NA`. Otherwise the prorated raw is the
> partial sum times the item count over the items answered. That raw is
> rounded to the nearest whole number, halves up. The facet is the rounded raw
> over the item count. A domain is the mean of its 3 facets. If any of the 3 is
> `NA`, the domain is `NA`.
> The key's printed "round up" is not applied. The test fixture has at least 5
> respondents. One respondent has a facet at or just under the 25% missing
> limit. One has a facet over it. One has a domain that goes `NA`. At least
> one prorated facet has a prorated raw whose fractional part is below one
> half. Its expected value fails under a ceiling rule. At least one has a
> prorated raw that is an exact half. Its expected value fails under base
> `round()`. Items
> 98 and 176 take values other than 1.5 there. The expected values are typed
> into the test.

**What the fixture must contain to separate the rules.** A prorated facet
whose prorated raw has a fractional part strictly between 0 and .5. Two
concrete rows, hand-computed and checked against the current branch head:

- Callousness (14 items: 11, 13, 19, 54, 72, 73, 90R, 152, 165, 181, 196, 198,
  205, 206). Items 11, 13, 19 unanswered (3/14 = 21.4%, prorates). Every other
  item is 1, so 90 reverses to 2. Partial = 10 + 2 = 12. Prorated raw = 12 ×
  14 / 11 = 15.27. Nearest = 15. Facet = 15/14 = 1.0714. Ceiling gives 16/14 =
  1.1429.
- Submissiveness (4 items: 9, 15, 63, 200). Item 9 unanswered (1/4 = 25%, the
  inclusive boundary). The other three are 1, 0, 0. Partial = 1. Prorated raw
  = 1 × 4 / 3 = 1.33. Nearest = 1. Facet = 0.25. Ceiling gives 2/4 = 0.5, the
  largest separation any facet can show.

For the exact-half case: Withdrawal (10 items) with 2 unanswered and partial 2
gives 2 × 10 / 8 = 2.5. Half-up = 3, facet 0.3. Base `round()` gives 2, facet
0.2. Ceiling also gives 3. Keep this case for D-009's half-up assertion. It
does not separate (a) from (b). Only the fraction-below-half case does.

## 4. What the package must say, and where

- **`missing` help text** (`R/score_pid5.R`, the `@param missing` block). Add
  after "rounded to the nearest whole number before averaging":

  > The PID-5-IRF key prints this step as "round up to the nearest whole
  > number". The package reads it as the nearest-whole-number rule that every
  > other APA PID-5 key states, with halves rounded up. So an informant facet
  > prorates exactly as the matching self-report facet does.

- **IRF details section** (`@details ## The PID-5 Informant Form`, to be
  written in T5). One paragraph on the form: Markon et al. (2013), 218 items,
  the same 25 facets and 5 domains as the FULL version, one Anxiousness and
  one Suspiciousness item fewer, the 14 reverse items. Then the rounding
  sentence above with a pointer to D-090 in the package's decision log. Then a
  note on where the choice matters under `missing = "apa"`: only prorated
  facets (1 to 25% unanswered) whose prorated raw has a fractional part below
  one half. There the package's value is 1/n lower than a ceiling gives.

- **NEWS.md.** Inside the IRF "New features" bullet, one sentence: "The
  informant key says to 'round up' a fractional prorated raw score. The package
  applies the nearest-whole-number rule of the other APA PID-5 keys. So
  informant and self-report facets prorate the same way."

- **SOURCES.md.** (i) In the IRF rows of the verification summary, a new row:
  "IRF proration rounding | APA PID-5 Informant Form, p. 9 | key prints 'round
  up'. Package applies the series' nearest rule (D-090), see OQ-5". (ii) A new
  "OQ-5 [RESOLVED]" entry. It quotes the key verbatim. It lists the twelve APA
  documents and the two worked examples, with URLs and read date. It names the
  two copy errors and lists the reopening evidence. (iii) In "Note on FULL/SF
  domain scoring", a sentence that the adult key's URL was re-read 2026-10-03
  (sha256 `ce366db4…`) and its wording is unchanged. Add that the series'
  worked examples (23.33 → 23, 26.67 → 27) support "nearest". Neither
  tests a half.

- **References page** (`cairn/references/apa2013pid5irf.md`, "Scoring rule
  (p. 9)"). The paragraph now says "rounded up". That transcribes the key but
  reads as the package's rule. Quote the sentence verbatim, then add: "The
  package does not apply a ceiling. Every other APA key in the series,
  including two with worked examples, rounds to the nearest whole number.
  Resolved as OQ-5 / D-090." Also record the 2013 edition's "child receiving
  care" headers as a second copy error, with the DSM-5 URL.

- **DECISIONS.md.** A D-090 entry. Context: the sentence, the series, the copy
  errors. Decision: the IRF uses `apa_mean()` unchanged, and the printed
  "round up" is read as the nearest rule. Rejected: a ceiling, a
  version-specific rule, a warning. Consequences: no code change. Help, NEWS,
  SOURCES and the references page are updated. Reopening evidence: question 2.

- **Vignette** (`vignettes/pid5_scoring.Rmd`). The IRF section planned for
  T5 needs at most the one NEWS sentence.

## 5. FULL, SF, BF, BFPM

No change recommended, and none implied. The new evidence strengthens D-009.
The adult key's live text is unchanged. The series' worked examples show that
a fraction below one half goes down, which is what `round_half_up()` does.
Neither example lands on an exact half, so APA has still not said whether 2.5
is 2 or 3. D-009's half-up reading of "nearest" remains the conventional one
and stays. Under GP2 nothing changes silently, because nothing changes. The
M157/M159 characterization script (AC3) will show `identical()` output for the
existing versions. This report records that the rule was re-examined and kept.

## Beyond the brief

1. The shelf copy of the informant key and the live DSM-5-TR PDF differ only
   in the page-1 permission-request URL. The shelf has
   `websrvapps.psychiatry.org/requestform/default.aspx`. The live file has
   `webapps.psychiatry.org/RequestForm/`. The shelf copy's modification date
   (2022-07-14) is later than the live file's (2022-03-09). The references page
   can note that two TR builds exist and that their scoring text is identical.
2. The informant key's 2013 DSM-5 edition prints "Name/ID (child receiving
   care)" on its pp. 3 and 5. Record this on the references page as provenance
   for the "light edit of another form" reading that D-089(c) and this report
   both rest on.
3. `score_pid5()` accepts non-integer item responses without complaint.
   `srange` is checked for integerish values. Item values are not. This is
   pre-existing and outside M159. It is the only route by which floating-point
   noise reaches the rounding step. A future ceiling rule, for any version,
   needs that check first.
4. The references page's "Scoring rule" paragraph says "rounded up" without
   quotation marks. Until it is annotated, a reader takes it as the package's
   rule rather than the key's wording.
5. The `apa_mean()` comment in `R/util.R` cites "the PID-5 scoring key" in the
   singular. After M159 it serves five keys. Add a one-line note that the IRF
   key's "round up" is read as this rule (D-090). That keeps the next reader
   from reopening the question from the code side.

## Recommendations

1. **apply**: Score the IRF through the unchanged `apa_mean()`. No code change
   in `R/util.R` or `R/score_engine.R`.
2. **apply**: Replace AC2 with the wording in question 3. Build the fixture with
   the Callousness (15.27 → 15) and Submissiveness (1.33 → 1) rows, plus a
   Withdrawal exact-half row (2.5 → 3).
3. **apply**: Record D-090 in DECISIONS.md with the reopening evidence from
   question 2.
4. **apply**: Add the `missing` help sentence, the IRF details paragraph, the
   NEWS sentence, the SOURCES.md row and OQ-5, and the references-page
   annotation, as drafted in question 4.
5. **apply**: In SOURCES.md's "Note on FULL/SF domain scoring", record the
   2026-10-03 re-read of the adult key (sha256, unchanged wording) and the two
   APA worked examples as corroboration of "nearest".
6. **consider**: Add the 2013 edition's "child receiving care" headers and the
   two TR builds to the references page (Beyond the brief 1 and 2).
7. **consider**: A one-line pointer to D-090 in the `apa_mean()` comment.
8. **reject**: A ceiling rule for the IRF. It rests on one word in a key with
   two other copy errors. It contradicts the series' worked examples. It biases
   prorated informant facets upward relative to the self-report form.
9. **reject**: A version-specific `missing` default, an extra `missing` level,
   or a per-call warning for the IRF. Each carries an unsupported reading into
   the API. D-088 rejected the same shape for the BFPM.
10. **reject**: Any change to the FULL, SF, BF or BFPM rounding (question 5).
