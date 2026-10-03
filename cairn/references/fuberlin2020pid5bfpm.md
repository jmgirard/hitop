# fuberlin2020pid5bfpm — the PID5BF+M key sheet: PID-5 numbers, facets, domains and scoring rule

**Provenance.** Ingested 2026-10-03 by M157 from
`cairn/references/sources/fuberlin_pid5bfpm_de.pdf` (gitignored). This is the German
PID5BF+M form that the Freie Universität Berlin distributes. The web survey that
planned M157 to M161 found it. PDF metadata: title "PID5BF+ M DE", author André
Kerber, created 2020-11-30. Pagination: PDF pages, because the sheet prints no page
numbers. Page 1 is the 36-item form. Page 2 is the "PID5BF+ M Kodierschema" (coding
scheme).
Extraction: verified 2026-10-03 against page 2 and against the page-1 German text of all 36 items — observed 2026-10-03.

M157 matched each PID-5 number to the German item at its BF+M position. It
compared the meaning with the English `pid_items$Text` of that PID-5 item. All 36
agree in meaning and in pair order.

**Citation.** Kerber, A. (2020). *Persönlichkeitsinventar für DSM-5 und ICD-11:
Kurzform Modifiziert (PID5BF+ M)* [Questionnaire and coding scheme, German]. Freie
Universität Berlin. The sheet cites Kerber et al. (2020, *Assessment*,
doi:10.1177/1073191120971848) for the PID5BF+. It cites Bach et al. (2020,
*Psychopathology*, doi:10.1159/000507589) for the modified form. See
[bach2020](bach2020.md).

**Role.** The keying source for the `BFPM` entries of `pid_items` and `pid_scales`.
M157's plan chose it over waiting for the online Appendix A of Bach et al. It gives
the PID-5 item at each of the 36 BF+M positions. It gives the two items of each of
the 18 facets and the three facets of each of the 6 domains. It also says that no
item is reverse-keyed.

## Extracted values

### Scoring rule (p. 2, paragraph under the domain diagram)

In English, the sheet says the following. Each of the 18 facets has 2 items. The
item values within each facet are summed, and no item is reversed. A domain average
is the average of the 3 facets that contribute to that domain. Higher averages
indicate greater dysfunction. The sheet states no missing-data or proration rule.

### Coding scheme table (p. 2, "PID5BF+ M Kodierschema")

English facet names follow Bach et al. (2020, Table 2, p. 183). The German name is
in parentheses. Item pairs are in the order of the sheet. The first BF+M number of a
pair goes with the first PID-5 number.

| Domain | Facet | BF+M items | PID-5 items |
|---|---|---|---|
| Negative Affectivity (Negative Affektivität) | Emotional Lability (Emotionale Labilität) | 1, 19 | 62, 122 |
| | Anxiousness (Ängstlichkeit) | 7, 25 | 109, 110 |
| | Separation Insecurity (Trennungsangst) | 13, 31 | 50, 64 |
| Detachment (Verschlossenheit) | Withdrawal (Sozialer Rückzug) | 4, 22 | 82, 136 |
| | Anhedonia (Anhedonie) | 10, 28 | 23, 189 |
| | Intimacy Avoidance (Vermeidung von Nähe) | 16, 34 | 89, 108 |
| Antagonism (Antagonismus) | Manipulativeness (Neigung zur Manipulation) | 2, 20 | 162, 219 |
| | Deceitfulness (Unehrlichkeit) | 8, 26 | 126, 218 |
| | Grandiosity (Grandiosität) | 14, 32 | 187, 197 |
| Disinhibition (Disinhibition) | Irresponsibility (Verantwortungslosigkeit) | 3, 21 | 129, 160 |
| | Impulsivity (Impulsivität) | 9, 27 | 4, 17 |
| | Distractibility (Ablenkbarkeit) | 15, 33 | 6, 132 |
| Anankastia (Anankasmus) | Perfectionism (Perfektionismus) | 6, 18 | 123, 176 |
| | Rigidity (Rigidität) | 12, 24 | 140, 220 |
| | Orderliness (Ordnungszwang) | 30, 36 | 34, 115 |
| Psychoticism (Psychotizismus) | Unusual Beliefs (Ungewöhnliche Überzeugungen und innere Erlebnisse) | 5, 23 | 194, 209 |
| | Eccentricity (Exzentrizität) | 11, 29 | 25, 185 |
| | Perceptual Dysregulation (Denk- und Wahrnehmungsstörungen) | 17, 35 | 44, 77 |

On the sheet, each domain cell spans its three facet rows. The 36 BF+M numbers are
1 to 36 with no repeat. The 36 PID-5 numbers are all different.

## Traces to

- `data-raw/pid_bfpm_key.csv` and `data-raw/pid_info.R`: the `BFPM` column of
  `pid_items` and the `BFPM` element of `pid_scales`.
- `tests/testthat/test-keying.R`: the BF+M keying test, which types this table in
  apart from `pid_items`.
- `cairn/SOURCES.md`: the verification-summary row for `BFPM`.

## Open questions

- The online supplementary Appendix A of Bach et al., the published 36-item scoring key, is not on the shelf — observed 2026-10-03.
- The body of the paper confirms only the six anankastia items and the domain rule. See [bach2020](bach2020.md).
