# markon2024 — the published PID-5 normative tables (raw ↔ T ↔ percentile) and their sample

**Provenance.** Ingested 2026-07-30 by M25 from
`cairn/references/sources/markon2024.epub` (gitignored) — an EPUB supplied by the
maintainer, not a PDF. Pagination: book pages, carried by the EPUB's own pagebreak
anchors, so every anchor below is the printed page rather than an estimate.
Extraction: verified 2026-07-31 against the source — every numeric cell of the nine
ingested tables was extracted from the book's table markup by
`data-raw/verify_norms_against_book.R` and diffed against the committed CSVs, which
now match cell for cell — observed 2026-07-31. The seven M25 tables were transcribed
by hand, so for those the diff is a transcription check; A–6 and A–8 (M33) are
machine-extracted by `data-raw/extract_facet_norms.R`, so for those it is a
cross-check of two independently structured reshapings, backed by hand-read spot
values in `tests/testthat/test-norms.R` — observed 2026-07-31.

**Citation.** Markon, K. E., Fossati, A., Somma, A., & Krueger, R. F. (2024).
*Understanding the Personality Inventory for DSM-5 (PID-5).* Washington, DC:
American Psychiatric Association Publishing. ISBN 9781615375127. The cover lists the
authors with degrees ("Kristian E. Markon, Ph.D., Andrea Fossati, Ph.D., Antonella
Somma, Ph.D. and Robert F. Krueger, Ph.D."); the normative tables are the book's
Appendix, "Normative Score Distributions", pp. 113–219.

**Role.** The authoritative published source for the PID-5 normative tables the
package ships as `pid_norms` (IP3: no norms without published tables). It settles
the raw ↔ T ↔ percentile correspondence for the domain scales of all three scored
versions and the percentile distributions of the four validity scales. It is also
the **scoring key for the PID-5-BF total** (Ch. 3, p. 23, below) — the APA BF form
defines only the five domains, so this book is the sole published authority for how
the total is computed (IP3: no scoring without a key). It is *not* the authority for
validity-scale naming — see D-018 and "Open questions" below.

## Extracted values

Anchors are the printed page on which each table begins. The nine tables the package
ships are listed; the Appendix runs to twelve (see Open questions).

- **Table A–1**, p. 116 — "Personality Inventory for DSM-5—Self-Report Form Variable
  Response Inconsistency (VRIN) scale score percentiles". 24 rows, score 0–23.
  → package `INC` (FULL). CSV `data-raw/norms_pid5_vrin.csv`.
- **Table A–2**, p. 117 — "Personality Inventory for DSM-5—Self-Report Form (100-item
  form) Variable Response Inconsistency (VRIN) scale score percentiles". 16 rows.
  → package `INCS` (SF). CSV `data-raw/norms_pid5sf_vrin.csv`. **The caption's name
  is not the book's own name for this scale** — see Open questions.
- **Table A–3**, p. 117 — "Personality Inventory for DSM-5—Self-Report Form
  Over-Reporting Scale score percentiles". 9 rows. → package `ORS` (FULL).
  CSV `data-raw/norms_pid5_ors.csv`.
- **Table A–4**, p. 118 — "Personality Inventory for DSM-5—Self-Report Form Positive
  Impression Management Response Distortion scale (underreporting) score percentiles".
  56 rows. → package `PRD` (FULL). CSV `data-raw/norms_pid5_pimrd.csv` (the filename
  predates this ingest and follows a label the book does not use; kept as-is by M25-D4).
- **Table A–5**, p. 120 — "…Self-Report Form normative tables: domain scales". 56 rows
  × (T, and Raw/Percentile for each of Negative affect, Detachment, Antagonism,
  Disinhibition, Psychoticism). → FULL domains. CSV `data-raw/norms_pid5_domains.csv`.
- **Table A–6**, p. 124 — "…Self-Report Form normative tables: trait scales". All 25
  facets in one `<table>`, printed as five stacked blocks of five facets, each block
  71 rows (T = 30–100) × (T, and Raw/Percentile per facet) = 1,775 rows. → FULL
  facets. CSV `data-raw/norms_pid5_facets.csv` (M33).
- **Table A–7**, p. 147 — "…Self-Report Form (100 item) normative tables: domain
  scales". 61 rows, same five domains. → SF domains.
  CSV `data-raw/norms_pid5sf_domains.csv`.
- **Table A–8**, p. 151 — "…Self-Report Form (100 item) normative tables: trait
  scales". Same shape as A–6: five blocks of five facets, 71 rows each, 1,775 rows.
  → SF facets. CSV `data-raw/norms_pid5sf_facets.csv` (M33).
- **Table A–9**, p. 174 — "…Brief Form normative tables: total score and domain
  scales". 61 rows, Total plus the five domains. → BF total + domains.
  CSV `data-raw/norms_pid5bf_domains.csv`.

**The PID-5-BF total's computation rule** (Chapter 3, p. 23 — prose, not a table;
ingested 2026-07-30 by M26). The book states it once, verbatim: *"Unlike the other
versions of the PID-5, the PID-5-BF total score can be computed by averaging the
overall score by the total number of items in the measure (i.e., 25)."* So the total
is the item-level mean over all 25 BF items, not the mean of the five domain means.
The page anchor is the EPUB's own `page23` pagebreak span, read directly from the
chapter markup rather than inferred from surrounding text — verified against the
source 2026-07-30. Chapter 3's instrument-comparison table (p. 22) describes the same
quantity as "1 total score (domain profile elevation)", which is a characterization
of what the total represents rather than a competing rule. The book gives **no**
missing-data or proration rule for the total anywhere; the Appendix's only mention of
missingness (p. 114) is a sample-inclusion criterion for deriving the norms, not a
scoring instruction — observed 2026-07-30.

**The normative sample** (Appendix, "Background and Methods", pp. 113–114), in the
book's own terms:

- Norms for the 220-item, 100-item, and brief forms come from "a sample of 1,082
  individuals from a U.S. Census–matched panel (Krueger et al. 2012)", which the book
  states is "the same panel reported in the original PID-5 development article
  (Krueger et al. 2012), although the sample is different" (p. 113).
- **Validity-scale** distributions (Tables A–1 to A–4) use "the entire sample of 1,082
  individuals" (p. 114).
- **Domain and trait** distributions (Tables A–5 to A–8) use only respondents whose
  "PID-5 Variable Response Inconsistency (VRIN) scale scores were lower than 17, no
  more than a quarter of their responses were missing, and they did not endorse both
  infrequency items", giving **N = 995** (p. 114).
- "All self-report form descriptive statistics, T scores, and percentiles were
  computed using sampling weights reflecting U.S. Census data" (p. 114).
- Weighted demographics: 52% female, 48% male; ages 18–92, mean 46.6, median 47
  (p. 114).

**Corroboration of the A–1 mapping.** The book's own domain-norm inclusion criterion
is a VRIN score "lower than 17" (p. 114), and `R/validity_pid5.R`'s documented cut
score is "a score of 17 or higher on the INC is indicative of inconsistent
responding". The identical threshold is independent evidence that the book's VRIN and
the package's `INC` are one scale, beyond the shared attribution to Keeley et al.
(2016).

## Traces to

- `data-raw/norms_pid5_vrin.csv`, `norms_pid5sf_vrin.csv`, `norms_pid5_ors.csv`,
  `norms_pid5_pimrd.csv`, `norms_pid5_domains.csv`, `norms_pid5sf_domains.csv`,
  `norms_pid5bf_domains.csv` — the seven hand-transcribed tables;
  `norms_pid5_facets.csv`, `norms_pid5sf_facets.csv` — the two machine-extracted
  facet tables. One per anchor above.
- `data-raw/extract_facet_norms.R` — writes the two facet CSVs from this source.
- `data-raw/verify_norms_against_book.R` — extracts all nine tables from this source
  and diffs them against those CSVs; its `spec` list carries the table→CSV
  correspondence.
- `cairn/SOURCES.md`, "PID-5 normative tables" section — the provenance record and
  the book-label → package-column mapping.
- `pid_scales[["BF"]]`'s `total` row and `score_pid5(version = "BF")`'s total column,
  via `cairn/SOURCES.md`'s "Note on the BF total score" — the p. 23 rule above is the
  IP3 key they ship against (M26).

## Open questions

- The Appendix carries **twelve** tables; the package ships nine (M25's seven plus
  A–6 and A–8 at M33). A–10 and A–11 are the Informant Form norms and A–12 the
  SRF/IRF descriptive statistics and T-score differences; those three are captured as
  ROADMAP candidates rather than ingested here — observed 2026-07-31.
- **A–6 and A–8 print raw scores a 0–3 item mean cannot reach.** A facet score is a
  mean of items coded 0–3, so it stops at 3.00, yet 42 of the 50 facet columns print
  raws above 3.00 and 19 of them clamp at exactly 4.00, repeating that value across
  up to 12 consecutive T rows (short-form anxiousness, T = 89–100). The book states
  no ceiling rule and no erratum is known, so the package ships the rows verbatim
  (M33) and reads them by its ordinary tie rule. Whether the 4.00 clamp is a
  deliberate tabulation choice or an error in the source is **unresolved and worth
  putting to the authors** — a 4.00 ceiling is what a 1–4 coding would produce, which
  would make these columns tabulated on a different response coding from the domain
  tables beside them — observed 2026-07-31.
- The book's naming of the 100-item inconsistency scale is internally inconsistent:
  the Table A–2 caption (p. 117) calls it "Variable Response Inconsistency (VRIN)",
  while Chapter 4 (pp. 34–35, six occurrences) calls it "PID-5-INC-S" and credits
  Lowmaster et al. (2020). The package maps A–2 to `INCS` on the strength of the
  chapter text, Lowmaster et al. (2020), and the item-pair content. See D-018.
- Whether Keeley et al. (2016) *itself* abbreviates the scale "VRIN" is unverified:
  that paper is not on the source shelf, so `INC` is not attributed to Keeley's own
  text anywhere in this repo — observed 2026-07-30.
- No sex- or age-stratified norms appear anywhere in the Appendix; the stratified
  half of the facet/stratified ROADMAP candidate is not satisfied by this source —
  observed 2026-07-30.
