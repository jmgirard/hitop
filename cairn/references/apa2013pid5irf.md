# apa2013pid5irf: the APA PID-5 Informant Form, its 218 items and its scoring key

**Provenance.** Ingested 2026-10-03 by M159 from
`cairn/references/sources/apa2013pid5irf.pdf` (gitignored, sha256
`be4a260a20706002a7ead4fd61261f86d2023cb6f221f7cfb04fb4d940f8af4d`). PDF metadata:
title "the personality inventory for DSM-5 - informant form", created 2022-03-09,
modified 2022-07-14. Pagination: PDF pages. Page 1 is the APA rights page. Pages 2
to 7 are the form, which numbers them "Page 1" to "Page 6". Page 8 is the scoring
key, and page 9 is the instructions to clinicians.
Extraction: verified 2026-10-03 by `data-raw/check_pid_irf_text.R` against pages 2 to 8, all 218 item texts and all 25 facets — observed 2026-10-03.

**Citation.** Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F. (2013).
*The Personality Inventory for DSM-5—Informant Form (PID-5-IRF)—Adult*. American
Psychiatric Association. The form's footnote cites the development paper by Markon
et al. (2013) as a manuscript in preparation. It appeared in *Assessment*, 20(3),
370–383.

**Role.** The keying and item-text source for the PID-5 Informant Form in
`data-raw/pid_irf_items.csv`. It gives the 218 items, the 25 facets, the 5 domains
and the scoring rule.

## Extracted values

### Form (pp. 2–7)

Each item completes the stem "He or she…". The response options are 0 "Very False
or Often False", 1 "Sometimes or Somewhat False", 2 "Sometimes or Somewhat True" and
3 "Very True or Often True". The first-page instructions begin "This is a list of
things different people might say about others." Later pages begin "Please continue
to complete the questionnaire."

`data-raw/pid_irf_items.csv` stores each item without the leading ellipsis and the
final period, with ASCII apostrophes and quotes, as `pid_items.csv` stores the
self-report text. `data-raw/check_pid_irf_text.R` applies the same normalization to
the PDF and finds all 218 texts equal.

### Item alignment with the self-report form

The informant form has no counterpart to two reverse-keyed self-report items. Item
96 is "I rarely worry about things" (Anxiousness). Item 177 is "I rarely feel that
people I know are trying to take advantage of me" (Suspiciousness). IRF item n is
self-report item n through 95, n + 1 from 96 to 175, and n + 2 from 176 to 218.
Under this mapping, every IRF item has the facet of its self-report item. Anxiousness
and Suspiciousness have one item fewer than on the self-report form.

### Scoring key (p. 8)

Step 1 lists 16 items to reverse: 7, 30, 35, 58, 87, 90, 96, 97, 98, 130, 141, 154,
163, 176, 208 and 213. The page-9 instructions repeat the same 16. Step 2 says that
the Step 1 items are marked R in the Facet Table, but the table marks 14: 7, 30, 35,
58, 87, 90, 96, 97, 130, 141, 154, 163, 208 and 213. Items 98 and 176 carry no R.
Item 98 is "sometimes hears things that are not really there" (Unusual Beliefs &
Experiences). Item 176 is "mentions that they will commit suicide sooner or later"
(Depressivity). The
self-report reverse flags, carried across the mapping above, give the same 14
items. Self-report items 98 and 177 are reverse-keyed, so the Step 1 list matches
the self-report numbers at those two places.

The Facet Table prints these items per facet (R marks shown):

| Facet | IRF items |
|---|---|
| Anhedonia | 1, 23, 26, 30R, 123, 154R, 156, 187 |
| Anxiousness | 79, 93, 95, 108, 109, 129, 140, 173 |
| Attention Seeking | 14, 43, 74, 110, 112, 172, 189, 209 |
| Callousness | 11, 13, 19, 54, 72, 73, 90R, 152, 165, 181, 196, 198, 205, 206 |
| Deceitfulness | 41, 53, 56, 76, 125, 133, 141R, 204, 212, 216 |
| Depressivity | 27, 61, 66, 81, 86, 103, 118, 147, 150, 162, 167, 168, 176, 210 |
| Distractibility | 6, 29, 47, 68, 88, 117, 131, 143, 197 |
| Eccentricity | 5, 21, 24, 25, 33, 52, 55, 70, 71, 151, 171, 183, 203 |
| Emotional Lability | 18, 62, 101, 121, 137, 164, 179 |
| Grandiosity | 40, 65, 113, 177, 185, 195 |
| Hostility | 28, 32, 38, 85, 92, 115, 157, 169, 186, 214 |
| Impulsivity | 4, 16, 17, 22, 58R, 202 |
| Intimacy Avoidance | 89, 96R, 107, 119, 144, 201 |
| Irresponsibility | 31, 128, 155, 159, 170, 199, 208R |
| Manipulativeness | 106, 124, 161, 178, 217 |
| Perceptual Dysregulation | 36, 37, 42, 44, 59, 77, 83, 153, 190, 191, 211, 215 |
| Perseveration | 46, 51, 60, 78, 80, 99, 120, 127, 136 |
| Restricted Affectivity | 8, 45, 84, 91, 100, 166, 182 |
| Rigid Perfectionism | 34, 49, 104, 114, 122, 134, 139, 175, 194, 218 |
| Risk Taking | 3, 7R, 35R, 39, 48, 67, 69, 87R, 97R, 111, 158, 163R, 193, 213R |
| Separation Insecurity | 12, 50, 57, 64, 126, 148, 174 |
| Submissiveness | 9, 15, 63, 200 |
| Suspiciousness | 2, 102, 116, 130R, 132, 188 |
| Unusual Beliefs & Experiences | 94, 98, 105, 138, 142, 149, 192, 207 |
| Withdrawal | 10, 20, 75, 82, 135, 145, 146, 160, 180, 184 |

The Domain Table names the same three primary facets per domain as the self-report
key: Negative Affect (Emotional Lability, Anxiousness, Separation Insecurity),
Detachment (Withdrawal, Anhedonia, Intimacy Avoidance), Antagonism
(Manipulativeness, Deceitfulness, Grandiosity), Disinhibition (Irresponsibility,
Impulsivity, Distractibility) and Psychoticism (Unusual Beliefs & Experiences,
Eccentricity, Perceptual Dysregulation).

### Scoring rule (p. 9)

The rule is the self-report key's rule. A facet's average is its raw sum divided by
its item count. If more than 25% of a facet's items are unanswered, the facet score
is not used. At 25% or less, the raw score is prorated. The partial raw score is
multiplied by the item count, divided by the items answered and rounded up. A domain score is the mean
of its 3 facet averages and is not computed if any of the 3 cannot be computed.

## Traces to

- `data-raw/pid_irf_items.csv` (item text, facet, self-report number) and
  `data-raw/check_pid_irf_text.R` (the check against this PDF).
- `cairn/SOURCES.md`: the IRF rows of the verification summary and OQ-4.

## Open questions

- Which reverse list governs, Step 1's 16 items or the Facet Table's 14 R marks. Recorded as OQ-4 in `cairn/SOURCES.md` — observed 2026-10-03.
