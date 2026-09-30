# watson2012 — the IDAS-II paper: scale names and item counts, but no item key

**Provenance.** Ingested 2026-09-29 by M143 from
`cairn/references/sources/watson2012Development.pdf` (gitignored), uploaded by Jeff on
2026-09-29. sha256 `3108806a113da0cffb6cd3cd0b7fdd09263059fc45562e67dc43c712f9423ee2`.
The copy was downloaded from asm.sagepub.com on 2015-12-07, as its page footers say.
Pagination: journal pages (Assessment 19(4), 399-420).
Extraction: verified 2026-09-29 against the source, Table 1 read from the pdftotext output of the shelf copy — observed 2026-09-29.

**Citation.** Watson, D., O'Hara, M. W., Naragon-Gainey, K., Koffel, E., Chmielewski,
M., Kotov, R., Stasik, S. M., & Ruggero, C. J. (2012). Development and validation of
new anxiety and bipolar symptom scales for an expanded version of the IDAS (the
IDAS-II). *Assessment, 19*(4), 399-420. https://doi.org/10.1177/1073191112449857

**Role.** The DAT manual cites this paper for the IDAS-II. It settles the names and item
counts of the 18 non-overlapping IDAS-II scales. It does not settle which items each
scale holds or which it reverses, because it prints no item numbers.

## Extracted values

- Table 1, p. 406, "Internal Consistencies (Coefficient Alphas) and Average Interitem
  Correlations (AICs) for the IDAS-II Scales". The note says "The number of items in
  each scale is shown in parentheses." The counts: Dysphoria (10), Well-Being (8),
  Panic (8), Cleaning (7), Lassitude (6), Insomnia (6), Suicidality (6), Social Anxiety
  (6), Ill Temper (5), Mania (5), Euphoria (5), Claustrophobia (5), Ordering (5),
  Traumatic Avoidance (4), Traumatic Intrusions (4), Checking (3), Appetite Loss (3),
  Appetite Gain (3). They sum to 99, the item count the paper gives for the IDAS-II
  (p. 404).
- General Depression is not in Table 1. The discussion (p. 418) says the instrument
  "also includes a General Depression scale". The paper prints no item count for it.
- The paper prints example items in quotation marks but no item numbers, no scoring
  key and no appendix of items — observed 2026-09-29.

## Traces to

- `tests/testthat/test-keying-hitopdat.R`, `watson_counts`: the 18 counts, checked
  against `hitopdat_scales$nItems`.

## Open questions

- The IDAS-II item key (which items each scale holds, and which General Depression
  reverses) is not on the shelf. Jeff is getting the authors' key. Until then the
  `hitopdat_scales` IDAS-II memberships follow the Qualtrics file, checked only by the
  counts above — observed 2026-09-29.
