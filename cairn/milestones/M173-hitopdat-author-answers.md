# M173: HiTOP-DAT names, records and announcement after the authors' answers

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1
- **Resolves:** —
- **Surface tier:** user-facing — renames a scale in the exported `hitopdat_scales` and announces the three HiTOP-DAT tables
- **Branch/PR:** m173-hitopdat-author-answers

## Goal

The three HiTOP-DAT tables ship announced, with scale names and provenance taken from the revised HiTOP-DAT manual and Katherine Jonas's answers of 2026-10-15.

## Scope

**In:**
- Rename the CAT-PD scale "Non-Premeditation" to "Non-Planfulness" in `hitopdat_scales`, with the `camelCase` stem `nonPlanfulness`. Its items and reverse keys do not change.
- Point the scale-name test and `?hitopdat_scales` at the revised manual.
- Write the revised manual's source note. The plan put the revised manual, Len Simms's CAT-PD and IDAS scoring workbook, and the email (`jonas2026email.txt`) on the shelf `cairn/references/sources/`.
- Record the email in SOURCES.md "HiTOP-DAT" and close the open questions it answers.
- A decision entry that admits the revised manual as the source of the scale names.
- Announce `hitopdat_items`, `hitopdat_choices` and `hitopdat_scales`: no internal keyword, a NEWS entry, rows in the pkgdown reference index and a README capability row.

**Out:**
- Scale scores, T scores and the critical-item alert stay in the HiTOP-DAT candidate row, which now names the workbook and Watson et al. (2012) Table 2 as norm sources. The workbook's source note is written by the milestone that scores with it.
- `hitopdat_instructions` stays internal data, like the other `*_instructions`.
- The Word, REDCap and Qualtrics exports are M144 and M145.
- The Society's web page, the DAT report and the shared Qualtrics file belong to the Society. The package ships none of them, so fixes there go to Katherine.

## Acceptance criteria

- [ ] AC1: `hitopdat_scales` has one row whose `Scale` is "Non-Planfulness" and whose `camelCase` is "nonPlanfulness". Its `Measure`, `nItems` and the integer vectors in `itemNumbers` and `reverseNumbers` equal those of the "Non-Premeditation" row at commit `bdb2b153`, and the list-element names follow `camelCase`. The other 56 rows equal their rows at `bdb2b153` in every column. A test asserts the renamed row's `Scale`, `camelCase` and items.
- [ ] AC2: `manual_scales` in `tests/testthat/test-keying-hitopdat.R` holds the 57 names of the "Scale definitions" section of the revised manual (sha256 `dec767f2…`), and its test passes against `hitopdat_scales$Scale` in both directions. `key_names` has no Non-Planfulness entry, and the IPIP comparison test passes for Non-Planfulness.
- [ ] AC3: `?hitopdat_scales` names the revised HiTOP-DAT manual as the source of `Scale`. `git grep -i premeditation -- R man data-raw tests vignettes inst` prints no line.
- [ ] AC4: SOURCES.md "HiTOP-DAT" lists the revised manual (sha256, received 2026-10-15) and the email from Katherine Jonas received 2026-10-15 (the date is the attachments' download time; shelved as `jonas2026email.txt`) as sources. It marks DAT-2, DAT-3, DAT-4, DAT-6, DAT-7, DAT-10, W1, W2 and W3 resolved, each with a one-line answer that names its source: the email or a section heading of the revised manual. A new DAT-15 records the two facts found at the plan (the menstrual item in the revised manual's Physical Symptoms definition, and the Qualtrics file's IDAS-II norms against Watson et al. (2012) Table 2). `cairn/references/` holds the revised manual's source note with its `INDEX.md` line, and `cairn_validate.py` reports no failure that names M173 or `references/`.
- [ ] AC5: DECISIONS.md holds an entry that annotates D-085. It admits the revised manual (sha256 `dec767f2…`, sent by Katherine Jonas) under IP1 as the source of `hitopdat_scales$Scale`, with Jeff's sign-off and its date.
- [ ] AC6: The help pages of `hitopdat_items`, `hitopdat_choices` and `hitopdat_scales` carry no `internal` keyword, and a test asserts it for each. The "Instrument Data" section of `_pkgdown.yml` lists the three, and `pkgdown::check_pkgdown()` passes. NEWS.md has an entry that names the three tables. The README's "What the package covers" table has a HiTOP-DAT row with 382 items and no scoring, reliability, tutorial or forms, `test-readme-capabilities.R` checks that row, and the paragraph above the table names the HiTOP-DAT. A case-insensitive search of `cairn/DESIGN.md` for "announced" and "internal" finds no passage that calls the three tables unannounced or internal.
- [ ] AC7: `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3
- AC2 → T3
- AC3 → T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: The plan copied `HiTOP-DAT Manual revised.docx` (sha256 `dec767f2b504cd5cb664bd1d9f53b962dd2041621418cc12cc391705a0df9031`) and `CAT-PD and IDAS scoring.xlsx` (sha256 `8643147c2178f0f53dbb083a9ac398116a8fb19eab02646d35f35d116dec9467`) to `cairn/references/sources/`. Confirm both sha256 values. Write `cairn/references/jonas2026dat.md` from the source-note template, with its `INDEX.md` line. The .docx prints no page numbers, so anchors are section headings. In `jonas2021dat.md`, mark that the revised manual is now the source of the names (corrected M173), and move its "Traces to" lines to the new note.
- [x] T2: In `data-raw/hitopdat_info.R`, map "CAT - Non-Planfulness" to "Non-Planfulness". Rerun the script. Compare the rebuilt `hitopdat_scales` with the one at `bdb2b153` and confirm that only the renamed row changed. Also grep the bare stem `remeditation` in every `data-raw/` script (LESSONS M077).
- [x] T3: In `test-keying-hitopdat.R`, rename the `manual_scales` entry, drop the `key_names` line that is now an identity, and fix the comment above it. Add the test of AC1. In `R/data.R`, cite the revised manual in `?hitopdat_scales`.
- [x] T4: In SOURCES.md "HiTOP-DAT", add the revised manual and the email as sources. Resolve DAT-2 (the file's Well-Being prorating by 5 is an error, and the scale prorates over its 8 items), DAT-3 and DAT-4 (the CAPE negative items were removed on purpose), DAT-6 and W2 (PHQ-15 item 4 is out on purpose, with the reason), DAT-7 and W1 (382 items, and the web page's counts are wrong), DAT-10 (the revised manual says Non-Planfulness) and W3. Record the email's other statements: the battery holds no IPIP measure, every scale is public, the manual cites every norm source, and T 65 is the alert point the authors chose, which clinics may change. Add DAT-15: the revised manual's Physical Symptoms definition still lists menstrual cramps, and the Qualtrics file's IDAS-II norms differ from Watson et al. (2012) Table 2's adult column on all 19 scales, where the workbook's 18 Table 2 scales equal it.
- [x] T5: Append the decision entry of AC5.
- [x] T6: Remove `@keywords internal` from the three tables in `R/data.R`, and turn the test "the three tables' help pages are marked internal" in `test-data-hitopdat.R` into a test that none is. Add the three to "Instrument Data" in `_pkgdown.yml`. Write the NEWS entry from the help pages as built. Add the README row, name the HiTOP-DAT in the paragraph above the table, extend `test-readme-capabilities.R` to check it, and rebuild the README. In `cairn/DESIGN.md`, change the two passages that call the tables not yet announced.
- [x] T7: Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-16: created by /milestone-plan from Katherine Jonas's email of 2026-10-15 and its two attachments.
- 2026-10-16: tier check: milestone — renames a scale in an exported dataset and needs Jeff's keying sign-off.
- 2026-10-16: question set: rename Non-Premeditation to Non-Planfulness and take the 57 names from the revised manual — Jeff signed off (Rename).
- 2026-10-16: question set: announce the three HiTOP-DAT tables — Jeff chose to announce them in M173.
- 2026-10-16: full criteria audit (fresh Opus reader) returned 6 findings, all fixed. A new AC5 binds the decision entry. AC1 allows the renamed list-element names. AC2 names the `key_names` test. AC4 gets a shelved email, a source per answer, DAT-15, and a validator clause that ignores the unregistered M172 file. The received date is 2026-10-15, from the attachments' download time.
- 2026-10-16: re-audit of the answer-changed criteria (same reader): AC6 passes, with two findings, both fixed. AC6 now binds the DESIGN.md passages and the README paragraph above the table. AC4 states where the received date comes from.
- 2026-10-16: plan dates the email 2026-10-15 from the download time of its attachments, since the pasted text carries no date.
- 2026-10-16: plan commit edits the ROADMAP. M144 and M145 go back to planned, M144 now depends on M173, and the HiTOP-DAT candidate row gains the email's facts and ` — started M173`. The release row's HiTOP-DAT clause names M173 inline but takes no started token, because the ROADMAP is at its 24,000-byte budget. Two wide rows (form text, PID-5 norms) were shortened with no fact removed. Post-merge hygiene trims the clause.
- 2026-10-16: no open GitHub issue or PR overlaps this scope (1 open issue, #87, on hosting data).
- 2026-10-16: plan puts the announcement in this milestone rather than a separate one, at Jeff's choice. Falsified if the rename and the announcement need separate reviews.
- 2026-10-16: implement started on `m173-hitopdat-author-answers`. The untracked M172 file and `devel/` files of other work stay unstaged.
- 2026-10-16: T1 done. Both shelf files match their sha256. `jonas2026dat.md` and its INDEX line are written, and `jonas2021dat.md` now points its traces there (corrected M173). cairn_validate fails only on the unregistered M172 file.
- 2026-10-16: T2 done. The crosswalk maps to Non-Planfulness, and the script reran from the shelved .qsf. Against `bdb2b153`, `hitopdat_items` and `hitopdat_choices` are identical, and `hitopdat_scales` differs in row 42 alone, with the same Measure, items, reverse keys and nItems. The committed CSVs did not change. No `data-raw/` script holds the stem `remeditation`.
- 2026-10-16: T3 done. The new test reads the facet's items from the IPIP key, not from the table, and checks for the old stem as `remeditat`, so AC3's grep finds no line. With the `bdb2b153` table planted back it fails (10 failures in the file). Restored by copy and compared with `cmp`. `devtools::test()` at that state: 0 failures, 15 skips.
- 2026-10-16: T4 done. SOURCES.md "HiTOP-DAT" lists the revised manual and the email as sources, resolves DAT-2, -3, -4, -6, -7 and -10 and W1 to W3 with their sources, and adds DAT-15. The workbook's 33 CAT-PD means and SDs equal the Qualtrics file's after rounding to 2 decimals, and its 18 IDAS-II pairs equal Watson et al. (2012) Table 2's adults.
- 2026-10-16: T5 done. D-097 annotates D-085. M172's draft plans a decision entry too. If it lands on main first and takes D-097, this branch renumbers at merge.
- 2026-10-16: T6 done. The internal keyword is gone from the three help pages, and the test now asserts its absence and that each page parsed. The tables have `_pkgdown.yml` rows, a NEWS entry, a README row with the HiTOP-DAT named in the paragraph above, and a `dat` stem in `test-readme-capabilities.R`. The two DESIGN.md passages are updated. Plants: dropping the README row gives 5 failures, and adding the keyword back gives 1. Both were restored by copy.
- 2026-10-16: claim audit: 36 claims read, 1 corrected — R/data.R (`?hitopdat_scales` cited "revised 2026", a year the title page does not print. It now says "revised version of 2026-10-15", the date from the file's metadata).
- 2026-10-16: T7 done. On `e815ec63` plus the claim fix: `devtools::test()` gave 0 failures, 15 skips and 29,068 passes. `pkgdown::check_pkgdown()` found no problems. `devtools::check()` gave 0 errors, 0 warnings and 0 notes. Status set to review.
- 2026-10-16: implementation choice: the NEWS entry goes under "New features", because 0.2.0 shipped without these tables.

## Decisions

## Review
