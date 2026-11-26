# andrews2009 — the 12-item WHODAS 2.0 norms: a simple sum from 0 to 48

**Provenance.** Ingested 2026-11-25 by M174 from
`cairn/references/sources/andrews2009.pdf` (gitignored), sent by Katherine Jonas with
the authors' second answers (SOURCES.md "The authors' second answers (2026-11-07)").
sha256 `6422686774cae9ac6826ab77a27cdc5ff2ac0f11c791dca9ff7260c68860cac5`.
Pagination: the article's own page numbers, 1 to 6, printed in the PLoS ONE footers.
Extraction: verified 2026-11-25 against the source, read from the `pdftotext -layout`
output of the shelf copy — observed 2026-11-25.

**Citation.** Andrews, G., Kemp, A., Sunderland, M., Von Korff, M., & Ustun, T. B.
(2009). Normative data for the 12 item WHO Disability Assessment Schedule 2.0.
*PLoS ONE, 4*(12), e8343. https://doi.org/10.1371/journal.pone.0008343

**Role.** The source of the WHODAS scoring rule that `score_hitopdat()` applies, and of
the WHODAS norms the authors named (Table 1's total).

## Extracted values

- p. 2: the paper scores the 12 items with "a simple sum scoring method", in place of
  the original IRT-weighted method. "The sum score for global disability therefore
  ranges from 0 (no disability) to 48 (complete disability)." Each item scores 0 to 4,
  the battery's `whodas` answer set.
- p. 3, Table 1, "Total" row, "Total" column: N 8,824, M 3.1, SD 5.3. Table 1 also
  splits the means by sex and by seven age groups.
- p. 4: 17 of 8,841 respondents had missing data on one or more items and were
  excluded. The paper states no rule for scoring a form with missing items.

## Traces to

- `R/score_hitopdat.R`: the WHODAS raw score is the sum of its 12 items.
- `tests/testthat/test-score_hitopdat.R`: the helper's sum rule for the WHODAS.
- `cairn/SOURCES.md` "HiTOP-DAT", "The authors' second answers": Table 1's total as
  the WHODAS norm.
- `data-raw/hitopdat_norms.csv` and `tests/testthat/test-hitopdat_norms.R`: the `whodas`
  row of `hitopdat_norms`, Table 1's total (corrected M175).

## Open questions

- The paper states no proration rule. `score_hitopdat()` prorates by the authors' rule
  (SOURCES.md "The authors' joint answers").
