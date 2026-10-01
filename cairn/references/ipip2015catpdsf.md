# ipip2015catpdsf — the IPIP CAT-PD-SF v1.1 key: facet membership, reverse keys and item text

**Provenance.** Ingested 2026-09-29 by M143 from https://ipip.ori.org/newCAT-PD-SFv1.1Keys.htm,
fetched with `curl` by the M143 session on 2026-09-29 and saved to
`cairn/references/sources/ipip-catpd-sfv1.1-keys.htm` (gitignored), sha256
`eee0861985978e6d0c4d5cf1e21096916cb665106c5edbad0b1d6c76190e4a23`. The server's
`Last-Modified` header reads 2015-12-18. The page is Windows-1252 text.
Pagination: none. Anchors are the facet headings and the page's "SF #" column.
Extraction: verified 2026-09-29 against the source, the saved page parsed into 33 facets and 216 rows and read against the test's lists — observed 2026-09-29.

**Citation.** International Personality Item Pool. *CAT-Personality Disorder Scales
Keys: CAT-Personality Disorder Scales Static Form* (CAT-PD-SF, v1.1). Oregon Research
Institute, https://ipip.ori.org/newCAT-PD-SFv1.1Keys.htm, last modified 2015-12-18,
read 2026-09-29. The page cites the CAT-PD to Simms et al. (2011).

**Role.** The published key for the 33 CAT-PD facets in `hitopdat_scales`: which static
form items each facet holds, which it reverses, and each item's text.

## Extracted values

- Each facet heading gives its name and item count, for example "Affective Lability
  (6 items; ...)". Each item row gives "SF #", "CAT #" and the item text, with "(RK)"
  after a reversed item.
- 33 facets and 216 rows. Each SF number from 1 to 216 appears once.
- The page names the facets "Self Harm" and "Non-Planfulness". The DAT manual says
  "Self-Harm" and "Non-Premeditation".
- The note under the key: "SF scales are scored by averaging item responses within
  each scale. RK item responses must be reversed prior to averaging (i.e., keyed = 6 –
  item response)."
- Four item texts start with "I" and the other 212 do not, for example SF 213 "I have
  intentionally done myself physical harm." and SF 1 "Have frequent mood swings." SF 170
  ends without a period.

## Traces to

- `tests/testthat/test-keying-hitopdat.R`, `ipip_catpd_facets` and `ipip_catpd_text`:
  every facet's items and reverse keys, and every item's text.
- `data-raw/hitopdat_info.R`: CAT-PD item 194's text takes "someone" from this key where
  the Qualtrics file cuts it short.

## Open questions

- None open. The file's 33 CAT-PD facets equal this key in items and reverse keys, and
  its item text equals this key's under the first-person rule the test states — observed
  2026-09-29.
