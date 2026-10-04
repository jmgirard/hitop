# niemeyer2022: the PID-5 Forensic Faceted Brief Form (PID-5-FFBF), its items and keying

**Provenance.** Ingested 2026-10-04 by M162 from four files on the gitignored shelf, listed below.
Pagination: journal pages for the article (pp. 30–43). PDF pages for Table S3, which
numbers its pages 1 to 11.
Extraction: verified 2026-10-04 by `data-raw/check_pid_ffbf_text.R` against Table S3 (all 400 texts, the 100 facets, the reverse marks and the a, b and c footnote marks) and the authors' code (the self and informant facet lists for the FFBF and the original form, the self and informant recodes, the two four-factor domains for both forms, and the unadapted items) — observed 2026-10-04.

- `cairn/references/sources/niemeyer2022.pdf`, the article (14 pages, sha256
  `43bfac3d6582fb5b87e370a3c7d40dbce6c87dde124cbea6e5c8a6a61ef3cb2f`).
- `cairn/references/sources/niemeyer2022_tableS3.pdf`, Supplement Table S3, "Item
  Content of Self- and Informant Report Version of the PID-5-FFBF" (11 pages, sha256
  `d0a04a14237d4ff6cd54c9cf97cd761a32080a84061c8d90a5eb9c5e231330b4`). Downloaded
  2026-10-04 with Jeff's permission from the paper's OSF project, Supplement component
  (osf.io/fzyvr, file `Table_S3_Item_Content_PID-5-FFBF.pdf`, https://osf.io/download/p23t8/).
- `cairn/references/sources/niemeyer2022_code.R`, the authors' analysis script (sha256
  `41547644d7ad0b0e59993e25c01cca63c281482d71e77fadeba42661629bbcdc`). Downloaded
  2026-10-04 with Jeff's permission from the OSF "Data and Statistical Code" component
  (osf.io/m42gn, file `PID_FC_Statistical Code.R`, https://osf.io/download/579qk/).
  It has CRLF line ends. Line numbers below count its lines as R's `readLines()` does.
- `cairn/references/sources/niemeyer_durchblick_codebook.pdf`, the study codebook
  ("DURCHBLICK, Validation study", 64 pages, Word file dated 2017-05-10, sha256
  `a8eb7357e8e1bdb4c9f78cd2171f224d2356feba4a6a7dc2312a32e76c745b38`). Jeff put it on
  the shelf before 2026-10-03. It is a draft of the study materials: its English item
  text differs from Table S3, so the package does not take text from it.

**Citation.** Niemeyer, L. M., Grosz, M. P., Zimmermann, J., & Back, M. D. (2022).
Assessing maladaptive personality in the forensic context: Development and validation
of the Personality Inventory for DSM-5 Forensic Faceted Brief Form (PID-5-FFBF).
*Journal of Personality Assessment, 104*(1), 30–43.
https://doi.org/10.1080/00223891.2021.1923522

**Role.** The keying and item-text source for `data-raw/pid_ffbf_items.csv` and the
FFBF domain rule. The ROADMAP row before M162 said that OSF held only a pre-final item
pool. That was wrong (corrected M162): Table S3 and the code are on OSF.

## Extracted values

### The form (article pp. 31–33)

The authors adapted the 100-item PID-5 Faceted Brief Form (Maples et al., 2015) for
prisoners, in German, for self and informant report. The study gave all items in
German (p. 33). Table S3 prints each item in four versions: self report German and
English, informant report German and English. The English text is the authors' English version, and the paper validated only the German version (p. 40; corrected M162). Items are answered from 0 ("very false") to 3 ("very true"), as in the
German PID-5 (p. 33). Informant reports from two raters were averaged item by item
before scoring (p. 33). Two items are reverse-coded (p. 32). The form was used "with
permission from Hogrefe and the APA" (p. 32).

### Facets and item order (Table S3)

Table S3 groups the items under 25 facet headings, in the alphabetical order of the
facet names. Each heading equals a `pid_items$Facet` name. Facet k (k = 1 to 25 in that
order) holds items k, k + 25, k + 50 and k + 75. The authors' code lists the same four items for
each facet at lines 109 to 133. Lines 3311 to 3335 repeat the lists for the original form.

### Reverse items

Item 12 is "I usually think before I act" (Impulsivity). Item 26 is "I enjoy
life to the extent it is possible to do so in prison" (Anhedonia). Table S3 marks both with "(-)"
in its two German columns only. The code recodes the same two items as 3 minus the
response (lines 382 to 388).

### Domains (article p. 33; code lines 161 to 190)

The five APA domains are each the unweighted mean of their three primary facets
("APA-three scales only scoring", p. 33), the facets of `pid_domains`. The paper's
four-factor solution names four domains: Antagonism, Detachment, Disinhibited
Aggression and Insecurity (pp. 30, 38). The code scores each from three facets (lines
182 to 185 for self report, 187 to 190 for informant report):

- Antagonism: Deceitfulness, Grandiosity, Manipulativeness (the APA domain).
- Detachment: Anhedonia, Intimacy Avoidance, Withdrawal (the APA domain).
- Disinhibited Aggression: Emotional Lability, Hostility, Impulsivity.
- Insecurity: Separation Insecurity, Anxiousness, Perceptual Dysregulation.

### Missing data (code only)

The paper prints no missing-data rule. The code (lines 395 to 423) scores a facet as the
unrounded mean of the answered items, with at most 1 of its 4 items missing. It
scores a domain as the mean of its 12 items when at most 3 are missing, whatever
facets they fall in. The package uses its SF rule instead (M162 work log).

### Table S3 notes

Footnote letters on item numbers mark items not adapted (a), not adapted for informant
reports (b) and not adapted for self-reports (c). The other letters mark items of the SD-TD (d) and PRD
(e) underreporting scales (Williams et al., 2019) and of the Response Inconsistency Scale
(f) (Lowmaster et al., 2019). Parenthetical notes such as "(G-PID-5 Item 95)" give an
item's source in the German or English 220-item PID-5 or PID-5-IRF. The table's note
ends with this notice: "Copyright © 2013 American Psychiatric Association, German
Version © 2015. All Rights Reserved. The PID-5-FFBF was developed and used in this
study with permission from Hogrefe and the APA."

Letters by item, read from the `pdftotext -raw` output on 2026-10-04 and matching RR08: a on items 11, 14, 15, 27, 28, 32, 34, 57, 60, 62, 65, 68, 70, 78, 82, 83 and 90; b on 8, 41 and 99; c on 33; d (SD-TD) on 11, 46, 56 and 84; e (PRD) on 11, 65, 82 and 91; f (INC-S) on 28, 32, 34, 57 and 78. The source notes stay in the shelf PDF and are not re-transcribed.

## How the transcription was made and checked

`data-raw/pid_ffbf_items.csv` was built from the word positions of `pdftotext -bbox`.
Each word went to the cell that its column and row place it in. The facet came from
the heading above the item. The texts then follow one rule. The rule removes source
notes in parentheses that begin "(G" or "(E-", the "(-)" mark, a leading ellipsis and a final period. It also removes the stray markers E14,
E18 and E77 (in items 3, 84 and 16). It turns
typographic quotes, apostrophes and the acute accent of "doesn´t" (item 50) into ASCII.
In a German text it removes a hyphen inside a word, for example "Ge-fühle" (item 93)
and "Be-ziehungen ein- gehen" (item 38). In an English text it joins "day-to- day"
(item 51) at the line break and keeps the hyphen. Printed wording is kept, typos
included ("does't", item 30). Hyphenation and the accent are typesetting, so the rule corrects them. Misspellings are wording, so IP1 keeps them, and SOURCES.md OQ-7 lists them (RR08). Table S3 prints item 10 as "To be honest: I am just more important than other inmates". The article (p. 32) quotes it in other words, and Table S3 governs (OQ-6).

`data-raw/check_pid_ffbf_text.R` reads Table S3 a second way, with `pdftotext -raw`.
For each item, it finds a split of the item's words into four runs that equal the CSV's four
texts under the same rule. It also checks the facets, the reverse marks and the code's
facet lists, recodes, forensic domains and unadapted items (extended by M162 T6 after RR08). Run 2026-10-04: PASS. Five defects were planted one at a time in a copy of the
CSV, and each made it exit 1. They were a word moved between cells, a changed word, a dropped word, a wrong facet and an
extra reverse flag.
