# simms2026catpdworkbook — Len Simms's CAT-PD and IDAS-II scoring workbook: the CAT-PD community norms

**Provenance.** Ingested 2026-11-25 by M175 from
`cairn/references/sources/CAT-PD and IDAS scoring.xlsx` (gitignored), attached to Katherine
Jonas's email received 2026-10-15 (`jonas2026email.txt`: "I've attached Len's CAT-PD/IDAS
spreadsheet, on which the Redcap battery was based"). sha256
`8643147c2178f0f53dbb083a9ac398116a8fb19eab02646d35f35d116dec9467`. The file's properties
name Len Simms as creator (2016-05-16) and Katherine Jonas as last editor (2025-05-14).
Pagination: — (sheet and cell addresses).
Extraction: verified 2026-11-25 against the source, cells E3:F35 of sheet "CAT-PD Scoring" read with `readxl::read_excel()` and from the sheet's XML, both giving the same values — observed 2026-11-25.

**Citation.** Simms, L. J. (n.d.). *CAT-PD and IDAS scoring* [Unpublished Excel workbook].
Sent by K. G. Jonas, 2026-10-15. Not published (D-098).

**Role.** The source of the 33 CAT-PD facet norms. Jonas says these norms come from Simms's
workbook and were never published (`jonas_ringwald2026email.txt`); D-098 admits them as the
battery authors' norms.

## Extracted values

- Sheet "CAT-PD Scoring" (`xl/worksheets/sheet10.xml`), rows 3 to 35. Row 2's headers:
  "Trait", "# Items", "Items in Score", "Raw Score", "M Community", "SD Community",
  "T Score". Column E is "M Community" and column F "SD Community". Each cell is a typed
  constant, not a formula, stored at 7 significant digits. As (trait as printed, E, F):
  Affective Lability (2.140302, 0.8457127), Anger (2.108249, 0.8440041), Anhedonia
  (2.00097, 0.8156681), Anxiousness (2.408313, 0.8564947), Callousness (1.676236,
  0.6857011), Cognitive Problems (2.031049, 0.6946615), Depressiveness (2.026511,
  0.9627519), Domineering (2.366881, 0.8826642), Emotional Detachment (2.638858,
  0.8487732), Exhibitionism (2.475595, 0.9222846), Fantasy Proneness (2.422023,
  0.9844998), Grandiosity (1.987954, 0.8116232), Health Anxiety (2.151155, 0.8604635),
  Hostile Aggression (1.411761, 0.5424737), Irresponsibility (1.761346, 0.669054),
  Manipulativeness (1.570993, 0.7097781), Mistrust (2.241225, 0.8245), Non-Perseverance
  (2.360963, 0.839808), Non-Planfulness (2.248438, 0.7819425), Norm Violation (1.753451,
  0.7595038), Peculiarity (2.034683, 0.9407927), Perfectionism (3.283652, 0.8309183),
  Relationship Insecurity (2.015666, 0.8540606), Rigidity (2.643119, 0.6727987), Risk
  Taking (1.858946, 0.8576587), Romantic Disinterest (2.066845, 0.905436), Rudeness
  (1.960858, 0.8197296), Self Harm (1.242576, 0.6058149), Social Withdrawal (2.183413,
  0.8346834), Submissiveness (2.12, 0.79341), Unusual Beliefs (1.665727, 0.7388285),
  Unusual Experiences (1.452366, 0.7239242), Workaholism (2.775229, 0.9211361).
- The 33 traits run in the order of the CAT-PD facets in `hitopdat_scales`. "Self Harm"
  is the manual's Self-Harm (`selfHarm`). The "Raw Score" column is an item mean, so the
  norms are on the item-mean metric `score_hitopdat()` uses for the CAT-PD.
- Column G is a formula, T = 50 + 10 × (raw − M) / SD, unrounded.
- The sheet names no sample beyond "Community" and gives no N.
- Row 8, Cognitive Problems, prints "7" items in column B and lists eight item numbers in
  column C; `hitopdat_scales` gives it 8 items. The norm cells do not depend on the count.

## Traces to

- `data-raw/hitopdat_norms.csv`: the 33 CAT-PD rows, M and SD transcribed from E3:F35.
- `tests/testthat/test-hitopdat_norms.R`: those 33 rows, typed from this page.
- `R/data.R` `?hitopdat_norms` and `R/norm_hitopdat.R` `?norm_hitopdat`: named as the
  authors' unpublished workbook.
- `cairn/SOURCES.md` "HiTOP-DAT", DAT-15 (b): the workbook's CAT-PD values equal the
  Qualtrics file's at its two decimals.

## Open questions

- The "Community" sample is not described in the workbook. Ringwald offered to ask Len
  Simms how the CAT-PD scores were standardized (`jonas_ringwald2026email3.txt`) —
  observed 2026-11-25.
