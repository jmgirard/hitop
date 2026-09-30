# M143: HiTOP-DAT item, answer and scale tables

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3
- **Resolves:** —
- **Surface tier:** user-facing — three new exported datasets
- **Branch/PR:** m143-hitopdat-tables

## Goal

The package ships the 382 HiTOP-DAT items, their answer options, their scales and their instructions as documented datasets, with keys checked against published sources.

## Scope

**In:** `hitopdat_items`, `hitopdat_choices` and `hitopdat_scales` as exported datasets, and `hitopdat_instructions` as internal data. All four are built by `data-raw/` scripts from the shared Qualtrics file `data-raw/HiTOP-DAT.qsf`, which stays uncommitted. The CAT-PD keys are checked against the IPIP CAT-PD-SF v1.1 key and the IDAS-II keys against Watson et al. (2012). Scale names come from the DAT manual (2021). SOURCES.md records the provenance and the open questions. The item column stem is `hdat_`, one battery number per item from 1 to 382 in the order of the file.

**Out:**
- Word and REDCap exports go to M144, and the Qualtrics export goes to M145.
- Scoring, T scores and the critical-item alert go to the HiTOP-DAT scoring candidate row. IP3 needs a published key and norm tables first.
- hitop-form and builder support also go to that candidate row.
- The clinic screens are not DAT content, so they stay out (plan gate, 2026-09-29). These are the UNT intro, the 90-minute check, the crisis notice, the feedback grid, the thank-you page, the suicide prompt and the patient ID fields.
- The "Skip" answer that the file adds to every item also stays out.

## Acceptance criteria

- [ ] AC1: `data-raw/hitopdat_info.R` builds `hitopdat_items` and `hitopdat_choices` from the shared file, and both ship as exported, documented datasets. `hitopdat_items` has 382 rows, one per item in the seven measure blocks. These are WHODAS 12, IDAS-II 99, AUDIT 10, DUDIT 11, CAPE 20, CAT-PD 216 and PHQ-15 14. Each row carries an integer battery number, the measure, the measure's own item number, the item text and its answer set. The battery numbers run from 1 to 382 in survey-flow order: WHODAS, IDAS-II, AUDIT, DUDIT, CAPE, CAT-PD, PHQ-15. `hitopdat_choices` gives the labels of each answer set with the forward (not reversed) value that the file grades each label. It holds no "Skip" answer. A test asserts the row count per measure, the battery numbers, and each measure's own-number vector. It also asserts that no item text matches `<`, `&[a-z]+;` or `^[0-9]+\.\s`, that no label reads "Skip", and each answer set's values.
- [ ] AC2: `hitopdat_scales` ships as an exported, documented dataset with one row per scale the file scores. That is 57 rows: 19 IDAS-II scales, 33 CAT-PD facets, and one total each for WHODAS, AUDIT, DUDIT, CAPE positive and PHQ-15. Each row carries `Measure`, `Scale`, `camelCase`, `itemNumbers`, `reverseNumbers` (the items reversed in that scale) and integer `nItems`. `Measure` comes from the prefix of the file's scoring category. `Scale` is the name of the matching scale definition in the DAT manual.
- [ ] AC3: Each CAT-PD facet's `itemNumbers` and `reverseNumbers` equal the IPIP CAT-PD-SF v1.1 key. Each IDAS-II scale's equal the key of Watson et al. (2012). Each of the five totals holds exactly the battery numbers of its measure. The set of `Scale` names equals the manual's list of definitions in both directions. A crosswalk hardcoded in the test maps each manual name to its key name, for example the manual's Non-Premeditation to IPIP's Non-Planfulness. Where the file differs from a published key, the table follows the key. `tests/testthat/test-keying-hitopdat.R` asserts each point against lists hardcoded from those sources, and each list cites its page or URL. For each key, the test also asserts that the key's text for an item matches `hitopdat_items` at that number.
- [ ] AC4: `cairn/SOURCES.md` gains a HiTOP-DAT provenance section. It names the file (sha256, date received), the manual (citation, sha256), the IPIP key (URL, date read) and Watson et al. (2012). `cairn/references/` holds a source note for each of the last three. The section compares the 57 scoring categories of the file with the keys and the manual. It lists each difference in membership, keying or name as an open question. It also lists these defects, known at planning:
  - Well-Being is prorated by 5, but the scale has 8 items.
  - The CAPE-Negative score and its completion field point at a missing scoring category.
  - The 20 CAPE items are graded into two categories that the file does not define.
  - IDAS-II item 99 grades "Skip" as 6 and leaves "Extremely" ungraded.
  - The file drops the PHQ-15 menstrual item.
  - The file has 382 items, but the Society page says 405.
  - The AUDIT picture of a standard drink is not shipped.
  - The file spells "Sucidality", "Claustraphobia", "Affective Liability", "Non-Perserverance" and "Traumatic Instrusions".
  - The manual says Non-Premeditation where IPIP and the file say Non-Planfulness.
  - The answer values come from the file's grades and are not yet traced to each measure's published scoring rule.
- [ ] AC5: `data-raw/sysdata.R` builds `hitopdat_instructions` into `R/sysdata.rda`. It holds the instruction text of WHODAS, IDAS-II, AUDIT, DUDIT, CAT-PD and PHQ-15, with no HTML. Line and list breaks stay as `\n`, and other runs of spaces become one space. It records that CAPE has none. A test compares each entry with text hardcoded from the file.
- [ ] AC6: NEWS.md names the three datasets, and the `_pkgdown.yml` reference index lists them. `pkgdown::check_pkgdown()` passes. `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T6
- AC2 → T3
- AC3 → T1, T3, T6
- AC4 → T1, T7
- AC5 → T4, T6
- AC6 → T5, T7

## Tasks

- [ ] T1: Gather the keys. Jeff uploads Watson et al. (2012) to `cairn/references/sources/`. If it is not available, stop for a gated amendment. Read the IPIP CAT-PD-SF v1.1 key page. Write source notes for the manual (already on the shelf, sha256 `0f770c4f…`), the IPIP key and Watson et al. (2012), with their `INDEX.md` lines.
- [x] T2: Write `data-raw/hitopdat_info.R`. It reads the live measure blocks of the file in flow order and removes HTML and the "n. " prefixes. It writes `data-raw/hitopdat_items.csv` and `data-raw/hitopdat_choices.csv` with integer columns, leaves out "Skip", and saves both with `usethis::use_data()`. The CAPE own-numbers come from the tags (`CAPE2` is 2). The PHQ-15 own-numbers keep the gap at 4.
- [ ] T3: Extend the script to build `hitopdat_scales` from the scoring categories of the file. Leave out the "Skipped" categories and the CAPE-Negative score. Take the names from the manual, and put the file's spellings in SOURCES. Where the file and a published key differ, follow the key.
- [ ] T4: Add `hitopdat_instructions` to `data-raw/sysdata.R`. It holds the six instruction texts and a CAPE entry that records none. Rebuild `R/sysdata.rda`.
- [ ] T5: Document the three datasets in `R/data.R` and add them to the `_pkgdown.yml` reference index. Add a NEWS entry. Update the instrument list in DESIGN.md "Purpose & scope" and "Instrument data model". Run `devtools::document()`.
- [ ] T6: Write `tests/testthat/test-keying-hitopdat.R` (AC3), with the name crosswalk, and the dataset shape tests (AC1, AC5). Plant a wrong facet item, a wrong reverse key, a renamed scale, a wrong total member, and an HTML entity in one item text. See each go red.
- [ ] T7: Write the SOURCES.md HiTOP-DAT section and its open questions, and record how to get the file (it is not committed). Run `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-09-29: created by /milestone-plan. Plan gate: the file supplies item text and answers, and the manual checks the scales. Jeff confirms that publishing the text is allowed. The clinic screens and "Skip" stay out. The work splits into three milestones, and scoring waits as a candidate.
- 2026-09-29: criteria audit (full mode, fresh [O] reader) returned 13 findings. Seven clear fixes were applied: names from the manual, a two-way scale check, the explicit defect list, the CAPE instruction case, and three export fixes carried to M144/M145. Four judgment calls went to the gate (source, manual, scope, content). Two were settled in the plan: keying per scale, and answers as scoring values without "Skip".
- 2026-09-29: plan chose reverse keying per scale (`reverseNumbers`) over an item-level `Reverse` flag. IDAS-II items 27 and 64 are forward in Well-Being and reversed in General Depression. Falsified by a published IDAS-II key that keys each item in one direction.
- 2026-09-29: plan chose one battery stem `hdat_001` to `hdat_382` over one stem per measure (`idas_01`, `catpd_001`). The form reader and the D-052 naming take one stem per instrument. Falsified by a researcher who needs per-measure column names to reuse an existing scoring pipeline.
- 2026-09-29: plan chose the published keys over the file where they differ, with the file's value kept as an open question. IP1 names the published source as the authority. Falsified by the DAT team stating that a difference is deliberate.
- 2026-09-29: second criteria audit (full mode, fresh [O] reader) on the post-gate wording returned 13 findings, 9 clear fixes and 4 judgment calls, all applied. M143 gained forward values, flow order, text and value asserts, and a name crosswalk. It also gained an item-text check per key, a full defect list, and kept line breaks. M144/M145 gained six instruction texts placed per measure, the legend wording, named bad inputs per argument, Qualtrics IDs, value checks, and a footer criterion.
- 2026-09-29: plan chose answer values from the file's grades, recorded as an open question, over tracing each of the five totals' values to its publication now. No scoring ships in M143 to M145, so the values are export codes. Falsified by a published rule for one of the measures that grades its answers differently from the file.

- 2026-09-29: implement started on branch m143-hitopdat-tables. Question gate: Jeff uploaded Watson et al. (2012) to the shelf. The item table takes the HiTOP-HSUM column names `Item`, `Measure`, `MeasureItem`, `Text` and `Choice_Set`. The answer table takes `Choice_Set`, `Value` and `Label`.
- 2026-09-29: T1 found that Watson et al. (2012) prints the IDAS-II scale names and item counts (Table 1, p. 406) but no item numbers or reverse keys. At the amendment gate Jeff chose to get the authors' IDAS-II item key later, and asked for all work that does not need it now. AC3 and AC4 stay as planned until that gate closes.
- re-audit: AC3 (full) — 9 findings on the draft "authors' IDAS-II item key" wording: MeasureItem mapping, text-match scope and rule, key versus Table 1 precedence, General Depression count, crosswalk names, T6 plants, "published" for a sent key, a blocked-or-split stop, and the test-file sentence as an instrument property.
- re-audit: AC4 (full) — 2 findings: the new key needs identifying fields (URL or sender, sha256, date received), and Scope and T1 still name Watson et al. (2012) as the IDAS-II key.
- 2026-09-29: T2 done. `data-raw/hitopdat_info.R` builds 382 items and 13 answer sets from the file. IDAS-II item 99 is the one allowed grade defect. Item text collapses the file's hard wraps to spaces. CAT-PD item 194 takes "someone" from the IPIP key where the file cuts it to "someon". `devtools::test()` passes (0 failed).

## Decisions

## Review
