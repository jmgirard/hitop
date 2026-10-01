# watson2011idas — the IDAS-II items and scale key: item numbers, text, membership and reverse keys

**Provenance.** Ingested 2026-10-01 by M143 from
`cairn/references/sources/IDAS-II (Items + Scoring).doc` (gitignored), sent to Jeff by a
HiTOP-DAT user, received on 2026-10-01 and put on the shelf that day. sha256
`dc77c5feee5563465612e30302fc3fdab9fefbd257979fdce34d7721c4e256dc`. Jeff signed it off
on 2026-10-01 as the IDAS-II authors' item key that D-082 admits.
Pagination: the document prints no page numbers. Anchors are the item numbers 1 to 99
and the heading "COMPOSITION OF THE IDAS-II SCALES".
Extraction: verified 2026-10-01 against the source, the text that macOS `textutil` extracts from the shelf copy parsed into 99 items and 19 scale lists and read against the test's lists — observed 2026-10-01.

**Citation.** Watson, D. (2011). *The Inventory of Depression and Anxiety Symptoms,
Second Version (IDAS-II)* [Items and scoring key]. The document is headed
"© David Watson, 2011".

**Role.** The IDAS-II key for `hitopdat_scales` and `hitopdat_items`: which items each
of the 19 IDAS-II scales holds, which General Depression reverses, and each item's
number and text.

## Extracted values

- The instructions ask how the respondent felt "during the past two weeks, including
  today", on a 1 to 5 scale from "not at all" to "extremely".
- Items 1 to 99, each printed with its number, its text and its scale in parentheses,
  for example "1. I did not have much of an appetite (Appetite Loss)". Some items
  carry a doubled space before the scale tag. Items 69, 75, 77, 78 and 92 use curly
  quotation marks or apostrophes.
- "Composition of the IDAS-II scales" lists 19 scales with their item counts and item
  numbers. General Depression (20 items) is 1, 2, 5, 6, 8, 9, 11, 13, 21, 26, 27*, 30,
  31, 40, 48, 51, 52, 57, 61, 64*. The note "*reverse-keyed item" marks 27 and 64. The
  other 18 scales hold each of the 99 items once and reverse none. Their counts equal
  Watson et al. (2012, Table 1, p. 406).
- The key names the scales as the DAT manual does (p. 12 and pp. 20-25), with no
  spelling differences.

## Traces to

- `tests/testthat/test-keying-hitopdat.R`, `watson_idas_scales`: the 19 lists, checked
  against `hitopdat_scales$itemNumbers`, `$reverseNumbers` and `$nItems`.
- `tests/testthat/test-keying-hitopdat.R`, `watson_idas_text`: the 99 texts, checked
  against `hitopdat_items$Text` at each IDAS-II `MeasureItem`.

## Open questions

- The document gives no scoring rule beyond the scale lists (for example, sums or
  means, or how to handle missing answers) and no norms. Scoring the IDAS-II needs
  another source (HiTOP-DAT candidate row) — observed 2026-10-01.
