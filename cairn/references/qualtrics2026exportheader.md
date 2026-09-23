# qualtrics2026exportheader — a Qualtrics data export names each column by the `[[ID:]]` tag of the imported question

**Provenance.** Ingested 2026-09-22 by M108 from
`cairn/references/sources/qualtrics2026exportheader.csv` (gitignored). That file holds
the three header rows of a first-hand Qualtrics export, copied from the full file Jeff
Girard exported on 2026-09-22. The response row was left off the shelf because it
carries the preview's location. Procedure: Jeff ran
`generate_qualtrics_hitopsr(file = "qtest.txt", module = hitop_module("hitopsr",
c("Agoraphobia", "Appetite Loss")))` at the M108 branch point. He imported `qtest.txt`
into a new Qualtrics survey, answered one preview response, and exported the responses
as CSV from Data & Analysis. The export used the default header, not "Use internal IDs
in header".
Pagination: none. The anchors are CSV rows.
Extraction: verified 2026-09-22 against the source, each header row read directly from the shelf copy — observed 2026-09-22.

**Citation.** Girard, J. M. (2026). Qualtrics CSV export of a survey imported from a
`generate_qualtrics_hitopsr()` Advanced Format file [first-hand observation,
2026-09-22]. The Qualtrics account, data center and product release were not recorded.

**Role.** Settles that the `[[ID:...]]` value of an imported Advanced Format question
is that question's column name in a Qualtrics CSV export. Qualtrics' own help does not
state this. Its import page says only that the tag "Specifies the question ID"
(https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/,
read 2026-09-22). M108 relies on this page for the descriptor's `columns` field from
`generate_qualtrics_hitopsr()`. Scope: the observation covers one generated two-scale
module with the default `id_prefix`, exported with the default header.

## Extracted values

- Row 1, the column names, ends with the eight item columns `HSR_066`, `HSR_109`,
  `HSR_118`, `HSR_144`, `HSR_202`, `HSR_260`, `HSR_291`, `HSR_389`. These equal the
  `[[ID:...]]` tokens in `qtest.txt`, in the same order. The first 19 columns are
  Qualtrics metadata, such as `StartDate` and `ResponseId`.
- Row 2 holds each question's text, for example "My appetite was poor." for `HSR_144`.
- Row 3 holds Qualtrics' internal IDs as `ImportId` values: `QID3` to `QID10` for the
  eight items. So the `[[ID:]]` tag set the column name, and Qualtrics assigned its own
  internal question IDs apart from it.
- The `start_instructions` descriptive block has no column in the export.
- Another header setting: Qualtrics' dataset help says "You can also find QIDs in the
  question's column header in your exported data, if you exported with Use internal IDs
  in header"
  (https://www.qualtrics.com/support/survey-platform/data-and-analysis-module/data/download-data/understanding-your-dataset/,
  read 2026-09-22). With that setting, the columns carry `QID` names, such as the
  `QID3` to `QID10` in row 3, not the `[[ID:]]` values. This is the vendor's statement,
  not observed here.

## Traces to

- `R/generate_qualtrics.R`, the `descriptor` argument of `generate_qualtrics_hitopsr()`:
  the claim that the descriptor's `columns` are the export's column names.
- `R/module_file.R`, `?write_module` and `?read_module`: the `columns` field.
- `tests/testthat/test-generator-descriptor.R`: the test compares `columns` to the
  generated file's `[[ID:]]` tokens. This page carries the step from those tokens to the
  export's columns.

## Open questions

- What breaks the link: a question whose export tag is renamed in the Qualtrics editor
  after import. Nobody tested a rename, and the tag's rename behavior rests on the
  editor showing the same value as the question number — observed 2026-09-22.
- The export held choice text ("Not at all") rather than numeric values, so the export
  was made with "Use choice text". Scoring needs numeric values. M108 added this to the
  modules article, the `generate_qualtrics_hitopsr()` help and NEWS. The import
  instructions article was not checked — observed 2026-09-22.
- This is one observation, from one account, on one date. Nobody checked whether
  another Qualtrics release names columns the same way — observed 2026-09-22.
