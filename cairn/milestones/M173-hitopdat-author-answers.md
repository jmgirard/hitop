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

- [x] AC1: `hitopdat_scales` has one row whose `Scale` is "Non-Planfulness" and whose `camelCase` is "nonPlanfulness". Its `Measure`, `nItems` and the integer vectors in `itemNumbers` and `reverseNumbers` equal those of the "Non-Premeditation" row at commit `bdb2b153`, and the list-element names follow `camelCase`. The other 56 rows equal their rows at `bdb2b153` in every column. A test asserts the renamed row's `Scale`, `camelCase` and items.
- [x] AC2: `manual_scales` in `tests/testthat/test-keying-hitopdat.R` holds the 57 names of the "Scale definitions" section of the revised manual (sha256 `dec767f2…`), and its test passes against `hitopdat_scales$Scale` in both directions. `key_names` has no Non-Planfulness entry, and the IPIP comparison test passes for Non-Planfulness.
- [x] AC3: `?hitopdat_scales` names the revised HiTOP-DAT manual as the source of `Scale`. `git grep -i premeditation -- R man data-raw tests vignettes inst` prints no line.
- [x] AC4: SOURCES.md "HiTOP-DAT" lists the revised manual (sha256, received 2026-10-15) and the email from Katherine Jonas received 2026-10-15 (the date is the attachments' download time; shelved as `jonas2026email.txt`) as sources. It marks DAT-2, DAT-3, DAT-4, DAT-6, DAT-7, DAT-10, W1, W2 and W3 resolved, each with a one-line answer that names its source: the email or a section heading of the revised manual. A new DAT-15 records the two facts found at the plan (the menstrual item in the revised manual's Physical Symptoms definition, and the Qualtrics file's IDAS-II norms against Watson et al. (2012) Table 2). `cairn/references/` holds the revised manual's source note with its `INDEX.md` line, and `cairn_validate.py` reports no failure that names M173 or `references/`.
- [x] AC5: DECISIONS.md holds an entry that annotates D-085. It admits the revised manual (sha256 `dec767f2…`, sent by Katherine Jonas) under IP1 as the source of `hitopdat_scales$Scale`, with Jeff's sign-off and its date.
- [x] AC6: The help pages of `hitopdat_items`, `hitopdat_choices` and `hitopdat_scales` carry no `internal` keyword, and a test asserts it for each. The "Instrument Data" section of `_pkgdown.yml` lists the three, and `pkgdown::check_pkgdown()` passes. NEWS.md has an entry that names the three tables. The README's "What the package covers" table has a HiTOP-DAT row with 382 items and no scoring, reliability, tutorial or forms, `test-readme-capabilities.R` checks that row, and the paragraph above the table names the HiTOP-DAT. A case-insensitive search of `cairn/DESIGN.md` for "announced" and "internal" finds no passage that calls the three tables unannounced or internal.
- [x] AC7: `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

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
- 2026-10-16: step-7 approval: m173-hitopdat-author-answers approved for merge
- 2026-10-16: implementation choice: the NEWS entry goes under "New features", because 0.2.0 shipped without these tables.

## Decisions

## Review

Evidence, gathered 2026-10-16 on branch head `940bde79` (synced: `origin/main` 4d93d75c is an ancestor).

- AC1: against `git show bdb2b153:data/hitopdat_scales.rda`, one row has Scale "Non-Planfulness" and camelCase "nonPlanfulness". Measure, nItems and the unnamed itemNumbers and reverseNumbers vectors are identical to the old Non-Premeditation row. The list names equal camelCase in both list columns, and the other 56 rows are `identical()`. The test "the CAT-PD planfulness facet carries the revised manual's name" (test-keying-hitopdat.R) asserts the name, stem and IPIP-derived items, and the keying file passes.
- AC2: a scripted read of the revised manual's "Scale definitions" (pandoc plain text, shelf copy sha256 dec767f2…) gives 57 names, identical in order to `names(manual_scales)`. `key_names` holds only "Self-Harm". The keying test file passes, the IPIP facet comparison included.
- AC3: `man/hitopdat_scales.Rd` line 27 names "the revised HiTOP-DAT manual". `git grep -i premeditation -- R man data-raw tests vignettes inst` prints nothing (exit 1).
- AC4: SOURCES.md "HiTOP-DAT" lists "The revised manual" (sha256 dec767f2…, received 2026-10-15) and "The authors' email" (received 2026-10-15, download-time date stated, shelved as jonas2026email.txt). DAT-2, -3, -4, -6, -7 and -10 carry `[RESOLVED 2026-10-16]`, and W1 to W3 carry resolved markers, each with "Resolved by" or "Confirmed by" the email or a revised-manual heading. DAT-15 records both plan facts. `references/jonas2026dat.md` exists with its INDEX line. `cairn_validate.py` has one FAIL, `roadmap<->disk orphans` naming only `M172-hitopsr-text-matching.md`, and `references index<->disk` passes.
- AC5: D-097 in DECISIONS.md annotates D-085. It admits the revised manual (full sha256, sent by Katherine Jonas) under IP1, with "Jeff signed off on 2026-10-16".
- AC6: no `\keyword{internal}` in the three Rd files (grep count 0 each). `_pkgdown.yml` "Instrument Data" lists the three, and `check_pkgdown()` reports no problems. NEWS.md line 5 is the HiTOP-DAT entry. README.Rmd and README.md have the row `| HiTOP-DAT | 382 |  |  |  |  |`, and README.Rmd line 43 names the HiTOP-DAT above the table. `test-readme-capabilities.R` maps "HiTOP-DAT" to `dat`, and a plant that dropped the row failed it (5 failures, implement log). DESIGN.md search: line 14 says "announced by M173", line 35 says announced, and line 36's "internal" is `hitopdat_instructions`, not the three tables.
- AC7: a fresh `devtools::test()` at `5d3e45a2` gave FAIL 0, WARN 0, SKIP 15, PASS 29,068. `devtools::check()` at `bef57b30` gave 0 errors, 0 warnings and 0 notes. Code outside `cairn/` was the same at `bef57b30` and `5d3e45a2` (empty `git diff --stat`). After the fix-now commits (`3560c436`, `0cf4918c`, which change only roxygen text and records), `document()` plus the HiTOP-DAT and README tests gave FAIL 0, PASS 433.
- Consistency gate: `devtools::document()` gives no diff. `build_readme()` leaves README.md unchanged.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: DAT-15 (b) said the file's IDAS-II norms differ "on all 19 scales", but Table 2 prints 18. Fix now, fixed 0cf4918c (now "all 18 scales that Table 2 prints", with p. 407).
- diff-bug #2: SOURCES.md stated "The manual cites every norm source" as fact, where the email hedges. Fix now, fixed 0cf4918c ("She believes…", with a pointer to DAT-15 (b)).
- diff-bug #3: DAT-7 said the email gives 382, but it gives no total. Fix now, fixed 0cf4918c.
- diff-bug #4: DAT-15 cited Table 2 values that `watson2012.md` did not hold. Fix now, fixed 0cf4918c (the Table 2 adult column is added to the note's extracted values and traces).
- diff-bug #5: D-097 did not address D-085's reason (users read the DAT report, which still prints the old name). Fix now, fixed 0cf4918c (a Consequences sentence on the gap between report and package).
- diff-bug #6: `jonas2026dat.md`'s Antagonistic Externalizing line misplaced the CAT-PD tags. Fix now, fixed 0cf4918c.
- diff-bug #7: SOURCES.md inferred the surname "Kotov" and promised "four facts" but listed five. Fix now, fixed 0cf4918c.
- diff-bug #8: DAT-10's M143 sentence "The package uses the manual's name" no longer said which manual. Fix now, fixed 0cf4918c.
- diff-bug #9: the AC1 test hardcodes the IPIP numbers that `ipip_catpd_facets` also holds, and its `remeditat` check is redundant. Reject (planned change): the independent statement is deliberate, since the check-discrimination rule keeps one fact apart from the artifact, and `ipip_catpd_facets` is defined later in the file. A drift fails the test and never passes it silently. The redundant check is a style point.
- diff-bug #10: `?hitopdat_scales` cites a manual users cannot get. Fix now, fixed 0cf4918c (now "unpublished revised version of 2026-10-15").
- diff-bug #11: (a) the ROADMAP HiTOP-DAT row still lists announcing as pending. Reject (planned change): post-merge hygiene trims it. (b) DESCRIPTION's Description does not name the HiTOP-DAT. Follow-up: the HiTOP-DAT candidate row (name the battery in DESCRIPTION when its forms ship, since the field says forms exist "for each instrument"). (c) D-097 can collide with M172's draft entry. Reject (false as a defect): logged, and checked against `origin/main` before the merge.
- blame-history #1: no D-entry records the reversal of M143's hold on announcing. Reject (planned change): the hold was a deferral, which the rules keep out of DECISIONS, and Jeff chose to announce at the plan gate (question-set work-log line).
- blame-history #2: AC boxes unticked and Review empty. Reject (false): the reviewer read before checkpoint `5d3e45a2`, which ticks AC1 to AC6 against evidence lines.
- blame-history #3: the ROADMAP row still lists announcing. Reject (planned change): post-merge hygiene, as diff-bug #11 (a).
- blame-history #4: D-085 carries no pointer to D-097. Reject (planned change): DECISIONS is append-only (IP4), and D-097's heading names D-085, so the bounded read's back-reference finds it.
- blame-history #5: D-097 does not cite D-082 and D-048. Fix now, fixed 3560c436.
- blame-history #6: D-097 can collide with M172's draft. Reject, as diff-bug #11 (c).
- blame-history #7 and #8: reported clean (the `key_names` change and the 2021 note's history). Noted, no change.
- prior-review #1: D-085 has no pointer to D-097. Reject, as blame-history #4.
- prior-review #2: `watson2011idas.md` cites the 2021 manual's pages. Fix now, fixed 3560c436 (names the 2021 manual and points to the revision).
- prior-review #3: `ipip2015catpdsf.md` says the DAT manual says Non-Premeditation. Fix now, fixed 3560c436 (2021 and revised named apart, corrected M173).
- prior-review #4: the ROADMAP hygiene stamp still names M170. Reject (planned change): step 9 replaces it. `check_pkgdown()` is clean. The NEWS entry is present, and no new top-level files exist.
