# RB08: PID-5-FFBF keying, forensic domains and missing-data rule (M162)

- **Date:** 2026-10-04
- **Output required:** write findings to `cairn/reviews/RR08-ffbf-keying.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained. Do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package that scores questionnaires of the HiTOP Society. Among
them is the Personality Inventory for DSM-5 (PID-5). `score_pid5(data, items,
version, ...)` returns facet and domain scores for several PID-5 forms. The
keying (which items form which facet, which items are reverse-keyed, which
facets form which domain) is held in exported tables built by `data-raw/`
scripts. The package treats keying as inviolable instrument content: every
cell needs a primary source, and the maintainer signs off any change (design
principle IP1).

Milestone M162 adds the PID-5 Forensic Faceted Brief Form (PID-5-FFBF) of
Niemeyer, Grosz, Zimmermann and Back (2022). It is a 100-item adaptation, for
prisoners, of the PID-5 Faceted Brief Form (the 100-item "SF" of Maples et al.,
2015), with a self-report and an informant-report version. The study gave the
items in German, and Table S3 of the supplement prints German and English text.

M162 has built, on branch `m162-pid5ffbf-scoring`:

- `pid_ffbf_items`: 100 rows, each with the item number, facet, reverse flag
  and four texts. Facet k of the 25 facets (alphabetical order) holds items
  k, k + 25, k + 50 and k + 75. Items 12 and 26 are reverse-keyed.
- `pid_ffbf_domains`: 7 domains. Rows 1 to 5 are the APA domains, each the
  mean of its 3 primary facets, the same as the package's `pid_domains`.
  Row 6 is Disinhibited Aggression (Emotional Lability, Hostility,
  Impulsivity). Row 7 is Insecurity (Separation Insecurity, Anxiousness,
  Perceptual Dysregulation).
- The maintainer chose at the plan gate to score the 25 facets, the 5 APA
  domains and these 2 forensic domains. The paper's four-factor Antagonism and
  Detachment have the same facets as the APA domains, so they are not
  repeated.

The scoring code is not written yet. The plan is to score FFBF with the rules
the package uses for the 100-item SF. A reverse item scores 3 minus the
response on the 0 to 3 scale. A 4-item facet with 1 item missing is prorated:
the partial sum times 4 over the number answered, rounded to the nearest whole
number with halves up, then divided by 4. A facet with 2 or more items missing
is `NA`. A domain is the mean of its 3 facets, and it is `NA` if any of its
facets is `NA`. This is the APA full-form rule (Krueger et al., 2013, p. 8),
which the package applies to the SF by analogy because Maples et al. (2015)
print none.

The maintainer asked for this independent review because the keying and the
domain definitions touch IP1. The two forensic domains come from the authors'
analysis code, not from a printed scoring key. The missing-data rule departs
from the authors' code.

## Materials

Read these. Paths are relative to the repository root.

- `cairn/references/niemeyer2022.md`: the package's source note. It records
  the four shelf files, their hashes, page and line anchors, the transcription
  rule and the check results.
- `cairn/references/sources/niemeyer2022.pdf`: the article (14 pages, journal
  pages 30 to 43). Use `pdftotext -layout` to read it. The measures section is
  on p. 33, the item adaptation on p. 32, and the four-factor results on pp.
  34 to 38.
- `cairn/references/sources/niemeyer2022_tableS3.pdf`: Table S3, the item
  content of all 100 items under 25 facet headings (11 pages). `(-)` marks a
  reverse-coded item. Its final note explains the footnote letters.
- `cairn/references/sources/niemeyer2022_code.R`: the authors' R analysis
  script (3,761 lines, CRLF line ends). Read lines 44 to 190 (labels, facet
  item lists for self and informant report, APA and four-factor domain lists,
  including the commented-out lines 177 to 180), lines 379 to 423 (the reverse
  recodes and the facet and domain scoring with missing data), and lines 3300
  to 3390 (the same lists for the original, unadapted form).
- `data-raw/pid_ffbf_items.csv` and `data-raw/pid_info.R` (search for "FFBF"):
  the transcription and the build of the tables.
- `tests/testthat/helper-fixtures.R` (the block headed "PID-5 Forensic Faceted
  Brief Form"): the key tables typed from Table S3 and the code.
- `data-raw/check_pid_ffbf_text.R`: the check script. You can run it with
  `Rscript data-raw/check_pid_ffbf_text.R` from the repository root. It needs
  `pdftotext`. It exits 0 on a pass.
- `cairn/SOURCES.md`, the section "Note on FULL/SF domain scoring": the SF
  missing-data rule and its source.

## Questions

1. **Facet map.** Do Table S3's facet headings, the code's self-report lists
   (lines 109 to 133), its informant lists (lines 135 to 159) and its lists
   for the original form (lines 3311 to 3335) all support "facet k holds items
   k, k + 25, k + 50 and k + 75" for all 25 facets? Report any item whose
   facet differs between any two of these sources, and name the source that governs
   if one does.

2. **Reverse items.** Are items 12 and 26 the only reverse-keyed items of both
   the self-report and the informant form? Table S3 prints `(-)` only in its
   German columns. Is there any reason to think the English or informant
   versions are keyed differently? The package's 100-item SF has no reverse
   items. Is any other FFBF item worded in reverse (for example, a rewritten
   item whose wording now points the other way) without a mark?

3. **Forensic domains.** The code scores Disinhibited Aggression and
   Insecurity from three facets each (lines 184 and 185, and 189 and 190 for
   informants). Commented-out lines 177 to 180 list wider facet sets from the
   factor analysis. Which definition do the paper's reported domain scores
   use (for example, in its Tables 3 to 5 and the text on pp. 33 to 38)? Is
   three-facet scoring the authors' published scoring rule, or only an
   analysis choice in the script? Is it sound for the package to ship these
   two domains as scored scales, and under these names, given that the paper
   prints no scoring key for them?

4. **Missing-data rule.** The authors' code (lines 392 to 423) scores a facet
   as the unrounded mean of the answered items when at most 1 of 4 is
   missing. It scores a domain as the mean of its 12 items when at most 3 are
   missing, wherever they fall. The package plans the SF rule given in the
   Background (rounded proration, and a domain is `NA` if any facet is `NA`).
   The paper prints no rule. Which rule is the right default for the FFBF,
   and why? If the SF rule stays, what must the help page say about the
   difference?

5. **Informant scoring.** The paper averages the item responses of two
   informants before scoring. The plan scores self and informant data with
   the same version and leaves the averaging to the user. Does anything in
   the sources make the informant form's keying or scoring differ from the
   self-report form's (beyond the averaging)?

6. **Transcription rule.** To compare text with Table S3, the package removes
   source notes in parentheses, the `(-)` mark, the stray markers E14, E18 and
   E77, a leading ellipsis and a final period. It turns typographic quotes,
   apostrophes and an acute accent into ASCII. In German text it removes
   hyphens inside a word (for example "Ge-fühle" becomes "Gefühle"), and it
   keeps typos as printed. Is any of these changes a change of item wording
   that IP1 forbids? Is any printed mark that the rule removes a part of
   the item?

## Constraints

These are fixed. Flag disagreement explicitly rather than working around it.

- The maintainer's plan-gate choices: a separate `pid_ffbf_items` table
  (not rows in `pid_items`), one `version = "FFBF"` for self and informant
  data, and output of 25 facets, the 5 APA domains and the 2 forensic domains.
- IP1 (instrument content is sacrosanct: keying and item text need a primary
  source and maintainer sign-off), IP2 (tests check against ground truth,
  never the code's own output) and IP3 (no scoring without a key, no norms
  without published tables), in `cairn/DESIGN.md`.
- D-018: a scale's name follows its development paper.
- D-088: a form whose domains are not those of `pid_domains` gets its own
  exported domain table with the four `pid_domains` columns.
- D-090 and the package's SF rule: proration rounds to the nearest whole
  number, halves up, in every version so far.

## Output format

In `RR08-ffbf-keying.md`: answer each question by number with your reasoning
and evidence (page, table, or file and line). List any additional findings
separately under "Beyond the brief". End with concrete recommendations, each
marked apply / consider / reject-with-reason. Your report is advisory: emit a
`## Binding criteria` section ONLY if this brief's header slot says
`requested`.
