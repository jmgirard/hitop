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
- [cloudresearch2026help.md](cloudresearch2026help.md) — CloudResearch Connect's researcher help (read 2026-09-27): the `participantId` parameter, the optional `assignmentId` and `projectId`, and the completion code or redirect, behind hitop-form's Connect builder choice.
