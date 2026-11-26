# galletly2016 — the CAPE positive symptom norms the HiTOP-DAT authors named

**Provenance.** Ingested 2026-11-25 by M175 from
`cairn/references/sources/galletly2016.pdf` (gitignored), sent by Katherine Jonas with
the authors' second answers (SOURCES.md "The authors' second answers (2026-11-07)").
sha256 `1cfe00d304b5978c175bb0a3ec30c70fda2f391b5058bb4c30bec19f0e42eddd`.
Pagination: journal pages (Psychiatry Research 239, 54-61); Table 1 is on p. 56, the PDF's third page.
Extraction: verified 2026-11-25 against the source, Table 1 read from the `pdftotext -layout` output of the shelf copy — observed 2026-11-25.

**Citation.** Galletly, C., Clark, L., McFarlane, A., Searle, A., Sawyer, M., Sim, M.,
Baghurst, P., & van Hooff, M. (2016). Childhood lead exposure, childhood trauma,
substance use and subclinical psychotic experiences: A longitudinal cohort study.
*Psychiatry Research, 239*, 54-61. https://doi.org/10.1016/j.psychres.2016.02.066
The title prints an en dash between "experiences" and "a longitudinal cohort study".

**Role.** The source of the CAPE positive symptom norm. Jonas's answer 5 names it:
"CAPE-positive norms are in Table 1 of Galletly (I believe we took the weighted mean of
the male and female values here)" (`jonas_ringwald2026email.txt`).

## Extracted values

- p. 56, Table 1, "Descriptive statistics and correlation matrix between childhood and
  adult variables and CAPE for males (M) and females (F)", row "CAPE – positive": n 125
  (M) and 180 (F); "Means (and standard deviations)" 26.26 (3.52) male and 26.58 (3.52)
  female.
- p. 55, section 2.3.3: the CAPE is "a 42 item self-report measure"; "the frequency of
  symptoms subscale was the focus of the current study". The sample is the Port Pirie
  Cohort Study's adults, aged 25-29, followed up in 2008-2009 (p. 55, section 2.1).
- Arithmetic on those cells, as `data-raw/hitopdat_norms.R` does it: the n-weighted mean
  (26.26 × 125 + 26.58 × 180) / 305 = 26.4489, and per item over the battery's 20 positive
  items 1.32244; the common SD 3.52, per item 0.176. The battery scores CAPE positive
  symptoms as an item mean (`?score_hitopdat`).

## Traces to

- `data-raw/hitopdat_norms.R`: the `positiveSymptoms` row of `hitopdat_norms`, computed
  from the four printed cells above.
- `tests/testthat/test-hitopdat_norms.R`: that row's M and SD, typed from this page.
- `R/data.R` `?hitopdat_norms` and `R/norm_hitopdat.R` `?norm_hitopdat`: the citation.

## Open questions

- Table 1 prints no pooled SD. The package uses the common printed SD (3.52), as the
  authors' "weighted mean of the male and female values" implies; a pooled SD would add a
  between-sex term the authors did not name — observed 2026-11-25.
