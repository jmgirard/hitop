# bergman2002 — the AUDIT in a Swedish population sample: a sum from 0 to 40, and T scores by sex and age

**Provenance.** Ingested 2026-11-25 by M174 from
`cairn/references/sources/bergman2002.pdf` (gitignored), sent by Katherine Jonas with
the authors' second answers (SOURCES.md "The authors' second answers (2026-11-07)").
sha256 `02205e7ce2c338d14f737aad630f0b8d9800892e21d1c2211cc6350f8ebd4388`.
Pagination: journal pages 245 to 251, printed in the page headers of the shelf copy.
Extraction: verified 2026-11-25 against the source, read from the `pdftotext -layout`
output of the shelf copy — observed 2026-11-25.

**Citation.** Bergman, H., & Källmén, H. (2002). Alcohol use among Swedes and a
psychometric evaluation of the Alcohol Use Disorders Identification Test. *Alcohol &
Alcoholism, 37*(3), 245-251. The shelf copy prints no DOI.

**Role.** The source of the AUDIT scoring rule that `score_hitopdat()` applies, and of
the AUDIT T scores and thresholds the authors' REDCap version uses.

## Extracted values

- p. 245: the AUDIT "consists of 10 items with five response alternatives (the last
  two items only three), scored from 0 to 4 points". "The maximum score is 40." So the
  total is the sum of the 10 items. The battery's `audit_harm` answer set (0, 2, 4)
  carries the last two items' three alternatives.
- p. 247: the cutoffs of 8 or more and, for women, 6 or more (the authors' second
  answers cite this page).
- p. 249, Table 6: non-normalized T scores by sex and three age groups. Each column
  prints T 0 at raw 0 (SOURCES.md "The authors' joint answers": keep it).

## Traces to

- `R/score_hitopdat.R`: the AUDIT raw score is the sum of its 10 items.
- `tests/testthat/test-score_hitopdat.R`: the helper's sum rule for the AUDIT.

## Open questions

- The paper states no rule for missing items. `score_hitopdat()` prorates by the
  authors' rule (SOURCES.md "The authors' joint answers").
