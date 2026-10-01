# jonas2021dat — the HiTOP-DAT manual: the battery's measures and scale names

**Provenance.** Ingested 2026-09-29 by M143 from
`cairn/references/sources/HiTOP Manual 10.5.21.pdf` (gitignored), supplied by Jeff.
sha256 `0f770c4f7c3b8360c0e9bfe2e79d71e7ab4d55e9d924b49debdb0572846c96d6`. The PDF's
metadata gives its creation date as 2021-10-05. A copy is posted at https://osf.io/8hngd/
(found by web search 2026-09-29, not compared with the shelf copy).
Pagination: the manual's printed page numbers ("HiTOP-DAT Manual Page N" at the top of each page, equal to the PDF page).
Extraction: verified 2026-09-29 against the source, each name and page below read from the pdftotext output of the shelf copy — observed 2026-09-29.

**Citation.** Jonas, K. G., Stanton, K., Simms, L., Mullins-Sweatt, S. N., Gillett, D.,
Dainer, E., Nelson, B. D., Cohn, J. R., Guillot, S. T., Kotov, R., Cicero, D., &
Ruggero, C. (2021). *HiTOP Digital Assessment and Tracker (HiTOP-DAT) Manual*. The file
prints no year on its title page. The year is the PDF's creation date and matches the
file name's "10.5.21".

**Role.** Settles the name of each scale in `hitopdat_scales$Scale`. The manual's
"Scale definitions" section lists 57 scales with the parent measure of most in
parentheses. The battery's Qualtrics file scores 57 categories, and M143 maps each to
one of these names.

## Extracted values

- The battery's name: "HiTOP Digital Assessment and Tracker (HiTOP-DAT)", p. 1.
- The IDAS-II citation the manual gives: "Inventory of Depression and Anxiety Symptoms
  (IDAS-II; Watson et al., 2012)", p. 12. The manual cites no item key for it.
- The CAPE is listed as "positive symptom scale only", pp. 38 and 40. Only "Positive
  Symptoms (CAPE)" has a definition.
- "Scale definitions", pp. 20-25, in the order printed. The section says the scales
  "are ordered as they are presented in the report, with their parent measure in
  parentheses, if relevant." The list, by printed page:
  - p. 20: WHODAS, General Depression (IDAS-II), Dysphoria (IDAS-II), Lassitude
    (IDAS-II), Suicidality (IDAS-II), Insomnia (IDAS-II).
  - p. 21: Appetite Loss (IDAS-II), Appetite Gain (IDAS-II), Ill Temper, Panic (IDAS-II),
    Traumatic Intrusions (IDAS-II), Traumatic Avoidance (IDAS-II), Claustrophobia
    (IDAS-II), Social Anxiety (IDAS-II), Cleaning (IDAS-II), Ordering (IDAS-II),
    Checking (IDAS-II).
  - p. 22: Affective Lability (CAT-PD), Anger (CAT-PD), Anxiousness (CAT-PD),
    Depressiveness (CAT-PD), Self-Harm (CAT-PD), Mistrust (CAT-PD), Submissiveness
    (CAT-PD), Relationship Insecurity (CAT-PD), Cognitive Problems (CAT-PD), Mania
    (IDAS-II), Euphoria (IDAS-II).
  - p. 23: Well-Being (IDAS-II), Health Anxiety (CAT-PD), Physical Symptoms (PHQ),
    Alcohol Use (AUDIT), Drug Use (DUDIT), Non-Premeditation (CAT-PD), Non-Perseverance
    (CAT-PD), Risk Taking (CAT-PD), Irresponsibility (CAT-PD), Perfectionism (CAT-PD),
    Workaholism (CAT-PD).
  - p. 24: Rigidity (CAT-PD), Callousness (CAT-PD), Manipulativeness (CAT-PD),
    Grandiosity (CAT-PD), Domineering (CAT-PD), Norm Violation, Hostile Aggression
    (CAT-PD), Rudeness (CAT-PD), Positive Symptoms (CAPE), Unusual Beliefs (CAT-PD).
  - p. 25: Unusual Experiences (CAT-PD), Fantasy Proneness (CAT-PD), Peculiarity
    (CAT-PD), Anhedonia (CAT-PD), Exhibitionism (CAT-PD), Social Withdrawal (CAT-PD),
    Emotional Detachment (CAT-PD), Romantic Disinterest (CAT-PD).
- Three definitions print no parent measure: WHODAS, Ill Temper and Norm Violation.
  Ill Temper is an IDAS-II scale and Norm Violation a CAT-PD facet, by the file's
  score categories ("IDAS - Ill Temper", "CAT - Norm Violation").
- The manual says Non-Premeditation where the IPIP key and the file say
  Non-Planfulness (p. 23).

## Traces to

- `data-raw/hitopdat_info.R`, `dat_scale_names`: the crosswalk from the file's category
  names to these names.
- `tests/testthat/test-keying-hitopdat.R`, `manual_scales`: the 57 names and their
  measures, checked against `hitopdat_scales` in both directions.
- `R/data.R`, `?hitopdat_scales`: "`Scale` is the name the HiTOP-DAT manual (2021)
  gives the scale."

## Open questions

- The manual's reference samples and T-score material are not ingested. They belong to
  the HiTOP-DAT scoring candidate row — observed 2026-09-29.
