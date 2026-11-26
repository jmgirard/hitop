# berman2005 — the DUDIT evaluation: a sum from 0 to 44, and T scores by sex and age

**Provenance.** Ingested 2026-11-25 by M174 from
`cairn/references/sources/berman2005.pdf` (gitignored), sent by Katherine Jonas with
the authors' second answers (SOURCES.md "The authors' second answers (2026-11-07)").
sha256 `0cf6c24fb7cfad20c976b918bfcfea80b87bf471788972c03fdfd969e601f109`.
Pagination: journal pages 22 to 31, printed in the page headers of the shelf copy.
Extraction: verified 2026-11-25 against the source, read from the `pdftotext -layout`
output of the shelf copy — observed 2026-11-25.

**Citation.** Berman, A. H., Bergman, H., Palmstierna, T., & Schlyter, F. (2005).
Evaluation of the Drug Use Disorders Identification Test (DUDIT) in criminal justice
and detoxification settings and in a Swedish population sample. *European Addiction
Research, 11*(1), 22-31. https://doi.org/10.1159/000081413

**Role.** The source of the DUDIT scoring rule that `score_hitopdat()` applies, and of
the DUDIT T scores and thresholds the authors' REDCap version uses.

## Extracted values

- p. 23, "Measures and Scoring": "The first 9 items with five response alternatives
  each were coded 0, 1, 2, 3, or 4, and items 10 and 11 with three response
  alternatives were coded 0, 2 or 4, giving a maximum score of 44 points." So the
  total is the sum of the 11 items. The battery's `dudit_harm` answer set (0, 2, 4)
  carries items 10 and 11.
- p. 24, "Missing Values and Item Characteristics": in the drug-user sample the
  authors read a missing answer as an intended 0. They say that another population
  can need another rule.
- p. 26, Table 2: T scores by sex for three age groups and all ages, printed up to
  about T 100 (SOURCES.md "Berman Table 2's short columns").
- p. 26: the thresholds of 6 points for men and 2 for women.

## Traces to

- `R/score_hitopdat.R`: the DUDIT raw score is the sum of its 11 items.
- `tests/testthat/test-score_hitopdat.R`: the helper's sum rule for the DUDIT.

## Open questions

- The paper's missing-as-zero reading is for its drug-user sample only.
  `score_hitopdat()` prorates by the authors' rule (SOURCES.md "The authors' joint
  answers").
