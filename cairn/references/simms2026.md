# simms2026 — the HiTOP-SR introduction paper: Table 1's development-sample statistics and the HiTOP-SR scale names

**Provenance.** Ingested 2026-08-28 by M041 from
`cairn/references/sources/ASMNT-26-0390_Proof_hi.pdf` (gitignored; sha256
`1c211219b7fe13f8ed172f9210c152a642a9be77d790e08d795843c25da8e425`, pinned in
`data-raw/hitopsr_table1.R`) — a proof of a manuscript under peer review, supplied
by the maintainer, admitted as this package's source by D-042. Pagination: PDF
pages of that shelf copy. The document also prints its own manuscript page number
one lower in the running head, so Table 1 begins on shelf page 49 and prints
"Assessing the HiTOP Model 48"; every anchor below is the shelf page.
Extraction: verified — Table 1's 93 primary-scale and subscale rows were
transcribed by reading the three table pages rendered at 200 dpi, and were
re-read against the independent coordinate extraction
`data-raw/verify_hitopsr_devstats.R` performs at the M041 review, 2026-08-28:
372 cells against the committed CSV and 372 against the shipped
`hitopsr_devstats`, none differing, plus the 17 indented labels against the 17
`subscale` rows. The eight Superspectra and Spectra rows are not transcribed and
so are not covered by that check — observed 2026-08-28.

**Citation.** Simms, L. J., Naragon-Gainey, K., Wright, A. G. C., Bornovalova, M.,
Cicero, D. C., Clark, L. A., Forbes, M. K., et al. *Assessment of the Hierarchical
Taxonomy Of Psychopathology (HiTOP): Introducing the HiTOP Self-Report (HiTOP-SR)
and Brief Report (HiTOP-BR).* Manuscript submitted to *Assessment*; Manuscript ID
`ASMNT-26-0390`, Original Research Article. No volume, issue, pages or DOI: the
document is a review proof, not a published article. The running head is
"Assessing the HiTOP Model".

**Role.** The published-source authority (IP3) for two things this package ships.
First, HiTOP-SR **scale names**, already read by M058 and M059 through
`cairn/SOURCES.md`'s "HiTOP-SR scale names" section. Second, the **development-sample
descriptive statistics** `hitopsr_devstats` carries and `interval_hitopsr()` reads:
per scale and subscale, the item count, Cronbach's alpha, mean and SD. It is *not*
a norming source — see "Open questions".

## Extracted values

### Table 1, pp. 49–51

Caption, p. 49, as printed: "Table 1. *Descriptive Statistics and Internal
Consistencies for all HiTOP-SR Primary Scales, Subscales, and Superspectra and
Spectra Scales in Development Sample 2.*"

Columns, in printed order: `Scale / Subscale`, `# Items`, `alpha`, `M`, `SD`,
`Range`, `Skewness`, `Kurtosis`. 101 data rows in 13 sections; the first 12
sections hold the 93 primary scales and subscales, and the last, "Superspectra and
Spectra Scales", holds the 8 HiTOP-BR scales. Subscale rows are indented under
their parent scale; the table itself carries no other scale/subscale marker.

- **The metric.** Every `Range` cell opens at `[1.0,` and the widest close at
  `4.0]`, so `M` and `SD` are on the HiTOP-SR's own four-option 1–4 item-mean
  coding — the coding `score_hitopsr()` defaults to (`srange = c(1, 4)`). Scores
  are item means, not sums: a 16-item scale's printed `M` is 2.19.
- **`alpha` is Cronbach's alpha.** The text on p. 24 calls these "internal
  consistency reliabilities" and "alpha coefficients", and states the target the
  scales were built against: "evaluating internal consistency with a target
  Cronbach's alpha of at least .80 per scale" (p. 16). No omega, ordinal or
  otherwise, is reported anywhere for these scales — observed 2026-08-28.
- **Range of the printed alphas.** p. 24: "77 out of 93 primary scales and
  subscales (82.8%) demonstrated alpha coefficients at or above .80"; "The
  remaining 16 scales (17.2%) fell below this threshold, with most of these alphas
  still in the acceptable range (between .66 and .79)". The lowest three are named
  in the same paragraph: "Blood-Injection Phobia (α = .66), Trichotillomania
  (α = .67), and Purging (α = .66)". The composite scales' range is given as "αs
  ranged from .82 to .96".
- **The Note**, p. 51, verbatim in its first sentence: "*Note.* N = 780." It
  continues that the scales "are arranged rationally and based on consensus
  judgment with respect to the HiTOP model (Kotov et al., 2017, 2021) and related
  literature, not according to a structural analysis of these data", and that the
  Superspectra and Spectra Scales (HiTOP-BR) "were developed independently from the
  primary scales (other than using the same item pool)" and so "form an independent
  set of scales, not a short form of the collection of primary scales".
- **The one footnote marker.** The row `Manic Energy†` (p. 50) is the table's only
  label carrying a marker; the footnote reads "† Manic Energy is defined in the
  model to split between the Thought Disorder and Internalizing spectra." The
  marker is a placement note about the HiTOP model, not part of the scale's name,
  and `hitopsr_scales$Scale` prints `Manic Energy`.

### Development Sample 2 (pp. 15–16)

The reference group behind every Table 1 cell, in the paper's own terms:

- "additional data were collected from a new sample of 780 Prolific Academic
  participants, who were stratified by sex and age to approximate a
  community-representative population from the United States" (p. 15).
- "The mean age of participants was 41.4 years (SD = 14.9; range = 18 to 80)"; of
  biological sex, "50.5% identified as female, and 48.6% as male" (p. 15).
- "A history of psychiatric treatment was reported by 63.1% of participants, with
  20.3% currently receiving psychiatric treatment" (p. 16).
- Development Sample 1 carries no comparable table: "Similar statistics for
  Development Sample 1 are difficult to calculate given the planned missingness
  design that resulted in incomplete item-level data within scales" (p. 23).

The paper describes what Table 1 supplies as "preliminary reference norms for each
scale and subscale" (p. 23). It is a development sample, not a norming sample: no
weighting to a census frame is reported, and no raw-to-T or percentile table
appears anywhere in the document — observed 2026-08-28.

## Traces to

- `data-raw/hitopsr_devstats.csv` — the transcription of Table 1's `# Items`,
  `alpha`, `M` and `SD` columns for the 93 primary-scale and subscale rows.
- `data-raw/hitopsr_devstats.R` — builds `data/hitopsr_devstats.rda` from that CSV,
  and carries the Table 1 label → package name map and its exception set.
- `data-raw/hitopsr_table1.R` — the shared extractor: `hitopsr_table1_rows()` reads
  the labels, `hitopsr_table1_cells()` reads the labels and all seven numeric
  columns by word coordinates.
- `data-raw/verify_hitopsr_devstats.R` — diffs that extraction cell by cell against
  the CSV and against the built `hitopsr_devstats`.
- `data-raw/verify_hitopsr_names.R`, `data-raw/verify_hitopsr_scale_name.R` — the
  M058/M059 name verifiers, which read the same table.
- `cairn/SOURCES.md`, "HiTOP-SR scale names" and "HiTOP-SR development statistics"
  sections.
- `R/interval_hitopsr.R` and its help page — the mean, SD and reliability it reads,
  and the development-sample caveat it prints.

## Open questions

- The document is a **review proof, not the accepted version**. D-041's
  reconciliation commitment, widened by D-042, stands: when the accepted version is
  shelved, every value read from here is re-verified against its Table 1 cell —
  observed 2026-08-28.
- **[RESOLVED 2026-08-30, corrected M68]** Table 1 and `hitopbr_scales` disagreed
  on the item counts of **Detachment** (printed 5, package 6) and
  **Internalizing** (printed 8, package 7), which is why M041 left the
  Superspectra and Spectra block untranscribed. One item accounts for both:
  `hitopbr_items` row 36, `HiTOP_69`, "I had a hard time asserting myself to
  others.", was keyed `Detachment`. Four statements put it under Internalizing —
  the development workbook `B-HiTOP overview.xlsx`'s `item-to-scale` sheet and,
  independently, the same workbook's `scoring syntax` sheet (`BInternalizing =
  MEAN.7(HiTOP_69, ...)` against a five-item `BDetachment`); this paper's Table 4
  (p. 57 of the shelf PDF), which loads "Hard to assert self" at .67 on INT and
  prints no DET loading; and the item's own HiTOP-SR home, the `Submissiveness`
  scale. Across all 45 HiTOP-BR items it was the only place the workbook and
  `hitopbr_items` disagreed. M068 corrected the keying at Jeff's sign-off, and
  the printed counts now agree with the package's own for all eight rows — see
  `cairn/SOURCES.md`, "HiTOP-BR item-to-scale membership".
- The raw responses behind Table 1 are on the shelf as `Prolific data HiTOP-SR.sav`
  (Development Sample 2, N = 780), so every printed cell could in principle be
  recomputed rather than transcribed. M041 does not: the recomputation needs its own
  analytic decisions and is the standing ROADMAP candidate D-043 makes condition one
  for shipping any coefficient this table does not print — observed 2026-08-28.
- **The paper's prose contradicts its own Table 1 on the lowest alpha.** p. 24
  states that of the 16 scales below .80, "most of these alphas [are] still in
  the acceptable range (between .66 and .79)", and names the lowest three as
  Blood-Injection Phobia (.66), Trichotillomania (.67) and Purging (.66). Table 1
  prints **.61** for Situational Phobias, below all three, and the independent
  coordinate extraction confirms that cell. Exactly 16 rows do fall below .80, as
  the prose says, so the disagreement is over which is lowest and not over the
  count. Nothing shipped depends on the prose — `hitopsr_devstats` carries the
  printed cell — but 0.61 is the constant anchoring the closed-form oracle O-001
  and the discriminating cell of the coverage oracle O-003, so the discrepancy is
  recorded rather than reconciled (IP1). Reconcilable against the accepted
  version, or by the raw-data recomputation the ROADMAP carries as a candidate —
  observed 2026-08-28 (M041 review).
- The paper prints **no reliability coefficient other than alpha** for these scales,
  so nothing here bears on the omega-ordinal question D-043 settles — observed
  2026-08-28.
