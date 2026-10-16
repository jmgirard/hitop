# jonas2026dat — the revised HiTOP-DAT manual: the battery's scale names

**Provenance.** Ingested 2026-10-16 by M173 from
`cairn/references/sources/HiTOP-DAT Manual revised.docx` (gitignored). Katherine Jonas sent it to Jeff
with her email of 2026-10-15 (`cairn/references/sources/jonas2026email.txt`). sha256
`dec767f2b504cd5cb664bd1d9f53b962dd2041621418cc12cc391705a0df9031`. The file's metadata gives
its creation date as 2021-10-04 and its last change as 2026-10-15, by "Jonas, Katherine".
The revision is not posted anywhere that M173 found. The 2021 PDF is
[jonas2021dat.md](jonas2021dat.md).
Pagination: none. A .docx prints no page numbers, so anchors are section headings.
Extraction: verified 2026-10-16 against the source, each name below read from the `pandoc -t plain` output of the shelf copy — observed 2026-10-16.

**Citation.** Jonas, K. G., Stanton, K., Simms, L., Mullins-Sweatt, S. N., Gillett, D.,
Dainer, E., Nelson, B. D., Cohn, J. R., Guillot, S. T., Kotov, R., Cicero, D., &
Ruggero, C. *HiTOP Digital Assessment and Tracker (HiTOP-DAT) Manual*, revised
version of 2026-10-15. Unpublished. The title page prints no year and no version.

**Role.** Settles the name of each scale in `hitopdat_scales$Scale` (D-085, as annotated
by M173's decision entry). It replaces the 2021 manual for that purpose. The revision
changes one name: the 2021 "Non-Premeditation" (p. 23) is "Non-Planfulness" here.

## Extracted values

- "Scale definitions", in the order printed, under its subheadings. The section says the
  scales "are ordered as they are presented in the report, with their parent measure in
  parentheses, if relevant." 57 names in all:
  - Functioning: WHODAS.
  - Internalizing, Distress: General Depression (IDAS-II), Dysphoria (IDAS-II), Lassitude
    (IDAS-II), Suicidality (IDAS-II), Insomnia (IDAS-II), Appetite Loss (IDAS-II), Appetite
    Gain (IDAS-II), Ill Temper, Panic (IDAS-II), Traumatic Intrusions (IDAS-II), Traumatic
    Avoidance (IDAS-II).
  - Internalizing, Fear: Claustrophobia, Social Anxiety, Cleaning, Ordering, Checking (each
    IDAS-II).
  - Internalizing, Negative Emotionality: Affective Lability, Anger, Anxiousness,
    Depressiveness, Self-Harm, Mistrust, Submissiveness, Relationship Insecurity, Cognitive
    Problems (each CAT-PD).
  - Internalizing, Mania: Mania, Euphoria, Well-Being (each IDAS-II).
  - Somatoform: Health Anxiety (CAT-PD), Physical Symptoms (PHQ).
  - Disinhibited Externalizing: Alcohol Use (AUDIT), Drug Use (DUDIT), Non-Planfulness,
    Non-Perseverance, Risk Taking, Irresponsibility, Perfectionism, Workaholism, Rigidity
    (each CAT-PD).
  - Antagonistic Externalizing: Callousness, Manipulativeness, Grandiosity, Domineering
    (CAT-PD), Norm Violation, Hostile Aggression, Rudeness (CAT-PD).
  - Thought Disorder: Positive Symptoms (CAPE), Unusual Beliefs, Unusual Experiences,
    Fantasy Proneness, Peculiarity (each CAT-PD).
  - Detachment: Anhedonia, Exhibitionism, Social Withdrawal, Emotional Detachment,
    Romantic Disinterest (each CAT-PD).
- As in 2021, WHODAS, Ill Temper and Norm Violation print no parent measure.
- The text holds no form of "premeditation". "Non-planfulness" also appears in "High
  disinhibition externalizing", under "Interpreting the report".
- "Measures and reference data", CAPE: only the positive symptom factor is used, because
  the CAT-PD and IDAS capture the negative and depressive factors.
- "Scale definitions", Physical Symptoms (PHQ): the definition still lists "menstrual cramps
  or other pain (Women only)". The battery leaves out PHQ-15 item 4 (SOURCES.md
  "HiTOP-DAT", DAT-6 and DAT-15).

## Traces to

- `data-raw/hitopdat_info.R`, `dat_scale_names`: the crosswalk from the Qualtrics file's
  category names to these names.
- `tests/testthat/test-keying-hitopdat.R`, `manual_scales`: the 57 names and their measures,
  checked against `hitopdat_scales` in both directions.
- `R/data.R`, `?hitopdat_scales`: the source of `Scale`.

## Open questions

- The manual's reference samples are described in "Measures and reference data", but their
  means and SDs are not in it. They belong to the HiTOP-DAT scoring candidate row — observed
  2026-10-16.
