# References index

<!-- One line per committed page under cairn/references/. Sources themselves
     live on the gitignored sources/ shelf. Keying provenance for `pid_items`
     etc. is the declared repo-specific file cairn/SOURCES.md (pre-dates this
     index). -->

- [markon2024.md](markon2024.md) — Markon, Fossati, Somma & Krueger (2024), *Understanding the PID-5*: the published normative tables behind `pid_norms`, their sample, and the book's scale labels.
- [online-collection.md](online-collection.md) — Synthesis note: a feasibility comparison of four ways to collect questionnaire responses online without Qualtrics or REDCap, costed under two operators, with a Sources table of the vendor and regulator pages read on 2026-09-20.
- [qualtrics2026exportheader.md](qualtrics2026exportheader.md) — First-hand Qualtrics CSV export (2026-09-22) of a survey imported from `generate_qualtrics_hitopsr()`: each column is named by the question's `[[ID:]]` tag.
- [schmukle2026.md](schmukle2026.md) — Schmukle (2026), *Assessment* 33(5), 817-825: the regression-based true score with scale correction, Equations (10)-(12), and what its coverage result covers.
- [vanderbilt2015redcapdd.md](vanderbilt2015redcapdd.md) — Vanderbilt University (2015), *Creating a Data Dictionary in REDCap*: a dictionary's field names are the data export's column names, and renaming a field breaks that link.
- [simms2026.md](simms2026.md) — Simms et al., the HiTOP-SR/HiTOP-BR introduction manuscript: Table 1's Development Sample 2 statistics behind `hitopsr_devstats` and `hitopbr_devstats`, and the sample they describe.
- [prolific2026help.md](prolific2026help.md) — Prolific's help center and API reference (read 2026-09-23): the `PROLIFIC_PID`, `STUDY_ID` and `SESSION_ID` parameters, the `{{%…%}}` placeholders, the completion URL shape and the preview's 24-character ID behind the hitop-form Prolific fields.
- [sona2026help.md](sona2026help.md) — SONA's researcher documentation (read 2026-09-27): the `%SURVEY_CODE%` placeholder, `id` in its Qualtrics guide, the client-side completion URL's `survey_code`, and the security note on that URL, behind hitop-form's `participantParam` and `{participant}` token.
- [jonas2021dat.md](jonas2021dat.md) — Jonas et al. (2021), the HiTOP-DAT manual: the 57 scale definitions (pp. 20-25) behind `hitopdat_scales$Scale`, and the IDAS-II citation it gives.
- [ipip2015catpdsf.md](ipip2015catpdsf.md) — The IPIP CAT-PD-SF v1.1 key page (read 2026-09-29): the 33 facets' items and reverse keys and the 216 item texts that `hitopdat_scales` and `hitopdat_items` are checked against.
- [watson2011idas.md](watson2011idas.md) — Watson (2011), the IDAS-II items and scale key (received 2026-10-01): the 19 scales' items and reverse keys and the 99 item texts that `hitopdat_scales` and `hitopdat_items` are checked against.
- [watson2012.md](watson2012.md) — Watson et al. (2012), the IDAS-II paper: Table 1's item counts for the 18 non-overlapping scales, and the absence of an item key.
- [github2026rawheaders.md](github2026rawheaders.md) — First-hand `curl -sI` of GitHub raw-file addresses (2026-10-01): `access-control-allow-origin: *` and `cache-control: max-age=300`, behind the hitop-form README's hosted setup file section.
- [fastly2026limits.md](fastly2026limits.md) — Fastly's resource limits and a first-hand `curl` of GitHub Pages (2026-10-01): an 8 KB URL limit answered with 414, measured as 8,192 characters of path and query, behind the Study Link Builder's refusal of a link longer than the host accepts.
- [cloudresearch2026help.md](cloudresearch2026help.md) — CloudResearch Connect's researcher help (read 2026-09-27): the `participantId` parameter, the optional `assignmentId` and `projectId`, and the completion code or redirect, behind hitop-form's Connect builder choice.
- [postgresql2026limits.md](postgresql2026limits.md) — PostgreSQL 18 documentation, Appendix K (read 2026-10-01): 1,600 columns per table and a row that must fit in one 8,192-byte page, behind hitop-form's Supabase column check.
- [fuberlin2020pid5bfpm.md](fuberlin2020pid5bfpm.md) — The FU Berlin PID5BF+M key sheet (German, 2020), p. 2: each of the 36 items' PID-5 number, its facet and domain, and the scoring rule behind the `BFPM` keying.
- [bach2020.md](bach2020.md) — Bach et al. (2020), *Psychopathology* 53, 179-188: the PID5BF+M development paper, its six anankastia items, its domain rule (p. 181) and its English facet names (Table 2).
- [apa2013pid5irf.md](apa2013pid5irf.md) — The APA PID-5 Informant Form (Markon et al., 2013): its 218 item texts, its Facet and Domain Tables, the self-report alignment, and the two reverse lists that disagree on items 98 and 176.
- [apa2013pid5child.md](apa2013pid5child.md) — The APA PID-5 child forms for ages 11 to 17 (Krueger et al., 2013), full and brief: their instructions, and a scripted check that their items and keying equal the adult `FULL` and `BF` tables.
- [niemeyer2022.md](niemeyer2022.md) — Niemeyer et al. (2022), *JPA* 104, 30-43, with its OSF Table S3 and analysis code: the PID-5-FFBF forensic form's 100 items in four versions, its facets, its two reverse items and its domain rules.
- [rfc9110.md](rfc9110.md) — RFC 9110, *HTTP Semantics*, section 4.1 (read 2026-10-01): senders and recipients are asked to support URIs of at least 8,000 octets, behind the Study Link Builder's long-link warning.
