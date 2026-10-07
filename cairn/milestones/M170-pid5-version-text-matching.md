# M170: PID-5 version refusals and item-text matching

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — error behavior and text matching of seven exported functions
- **Branch/PR:** m170-pid5-version-text-matching

## Goal

Researchers get a clear, classed refusal for a bad PID-5 `version` or an ambiguous item text, and `rename_pid5_items()` matches item text copied with common typographic differences.

## Scope

**In:**
- One `version` check for `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()`, `label_pid5()`, `validity_pid5()`, `norm_pid5()` and `plot_pid5()`. It accepts full names only, in any case, and refuses everything else under the class `hitop_unknown_version` (D-094). Prefixes such as `"S"` and `"F"` stop working. This is a pre-1.0 break, listed in NEWS.
- The proration help text and SOURCES.md say that a prorated half rounds away from zero, which is what `round_half_up()` does (R/util.R:708-710). No function body changes.
- `rename_pid5_items(method = "text")` refuses two columns that match one item for every version, under the class `hitop_duplicate_item_match` (D-094). It also ignores four typographic differences.
- The internal JSON writer reads item text from a column its spec names, so an informant spec can read `TextIRF`. No new JSON file ships.
- Test reach: the six child generators join the hand-kept generator lists, and stale counts in test comments are fixed.

**Out:**
- The APA notice in online exports and adult Word footers goes to M171.
- The APA Word layout (identity lines, instruction label, continuation text, "Clinician Use" column) goes to part (d) of the candidate row "Form text awaiting a source, sign-off or permission", which waits on APA's written permission.
- An informant JSON export and hitop-form support stay on the "Generalize modularization to BR/PID-5" row.
- IRF norms and the guard against norming IRF scores as FULL stay on the "PID-5 norms not yet shipped" row.

## Acceptance criteria

- [x] AC1: Each of the seven functions accepts `version` only as one of its full names, in any letter case. The four functions that take FFBF have six names, and `validity_pid5()`, `norm_pid5()` and `plot_pid5()` have three. An omitted `version` still means `"FULL"`, and so does the function's whole default vector of names passed in any case. Each function refuses any other value that is not one string equal to a full name after `toupper()`. The refusal is an error of class `hitop_unknown_version` whose call is the function itself. A test runs these refused values in each function: `"XYZ"`, `"S"`, `"F"`, `"B"`, `"FF"`, `""`, `NA`, `NULL`, `1` and `c("FULL", "SF")`. It asserts the class, and that the message names `version` and each of that function's full names. A second test resolves each full name in upper case, lower case and one mixed case (for example `"Full"` and `"bFpM"`), and the omitted argument, in each function. Each probe runs on input that passes every earlier check, and the `plot_pid5()` probes skip without ggplot2 3.4.0 or later.
- [ ] AC2: The `score_pid5()` help text that describes proration states that a prorated half rounds away from zero. This text is the roxygen of the `missing` argument and the IRF section in `R/score_pid5.R`. A search for `halves up` and `half up` over `R/`, `man/`, `vignettes/` and `cairn/SOURCES.md` finds no hit. A test scores the FULL Manipulativeness facet (items 107, 125, 162, 180 and 219) with item 219 missing. Answers -1, -1, 0, 0 under `srange = c(-3, 0)` give -0.6, and answers 1, 1, 0, 0 under `srange = c(0, 3)` give 0.6. The rounding code does not change: `git diff main...HEAD -- R/util.R` shows no change inside the bodies of `round_half_up()` and `apa_mean()`.
- [x] AC3: For each of the six versions, a test gives `rename_pid5_items(method = "text")` two columns whose texts match the same item. It asserts an error of class `hitop_duplicate_item_match` whose message names both columns and the item number. Before this milestone, only `"FFBF"` refused, with no class.
- [x] AC4: Under `method = "text"`, if a column text differs from one of an item's stored texts only in these four ways, alone or together, it matches that item. (a) Typographic quotes stand for straight ones: ‘ or ’ for `'`, and “ or ” for `"`. (b) The text starts with an ellipsis, `...` or `…`. (c) The text ends with a period. (d) Spaces, tabs or line breaks surround the text. For every stored text of every version, a test applies each difference alone and then all four together, and asserts that the text renames to its own item. The quote probes are four: every `'` made ‘, every `'` made ’, every `"` made “, and every `"` made ”. The `method` help names the four differences and still says that the "He or she…" stem must be removed first.
- [x] AC5: `write_instrument_json()` reads item text from a column that its spec names, and the default is `Text`. A test writes a spec numbered by `IRF` with the text column `TextIRF`. It finds each item's text equal to `pid_items$TextIRF` for that number. The five shipped JSON files stay byte-identical to their manifest rows.
- [x] AC6: NEWS.md lists AC1 under breaking changes and describes AC3 and AC4. A search for the pattern `version *= *['"]` over `R/`, `data-raw/`, `vignettes/`, `README.Rmd` and `tests/` finds no prefix or other non-full name passed to a PID-5 function, except in the refusal tests. `devtools::test()` passes, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T3
- AC5 → T4
- AC6 → T5, T6

## Tasks

- [x] T1: Add the `version` check to `R/util.R` and use it at the seven `match.arg(version)` sites (R/score_pid5.R:358, R/reliability_pid5.R:96, R/rename_pid5_items.R:100, R/label_pid5.R:76, R/validity_pid5.R:85, R/norm_pid5.R:195, R/plot_pid5.R:117). Delete the unreachable `switch()` defaults (R/score_pid5.R:369, R/reliability_pid5.R:105). Rewrite each `@param version` and the "F" note in NEWS. Replace the tests that expect a prefix to resolve or a prose refusal: test-score_pid5ffbf.R:157-181, test-score_pid5.R:648-658 and :925-931, and test-plot_pid5.R:581-587. Write AC1's tests.
- [x] T2: Fix the rounding text (R/score_pid5.R:46-51, 136-143, 197-201, the `apa_mean()` comment in R/util.R, and SOURCES.md, marked `corrected M170`). Append D-095, which annotates D-090: the rule rounds a half away from zero, which is "up" for non-negative codings. Write AC2's test.
- [x] T3: In R/rename_pid5_items.R:201-225, normalize both sides before `match()`, refuse duplicate matches for every version under the new class, and name the columns in the message. Update the `method` help (:19-26). Check that normalization joins no two items of one pool. Write AC3's and AC4's tests.
- [x] T4: Add a text-column field to the JSON spec (R/json_export.R:64, data-raw/json_export.R:24-60) with `Text` as the default. Write AC5's test.
- [x] T5: Add the six child generators to the hand-kept lists the M158 lesson names: test-generate_docx.R:61-72 and :257-263, test-generate_qualtrics.R:88-96 and :141-150, test-generate_redcap.R:97-105. Fix the stale counts at test-json-export.R:9 and :464-465, test-export-arg-guards.R:113-115, test-generate_docx.R:57, test-generate_qualtrics.R:85 and test-generate_redcap.R:94.
- [x] T6: Write NEWS, run AC6's search, then `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Work log

- 2026-10-07: created by /milestone-plan, together with M171. Lineage: the form and generator parts of the "PID-5 norms not yet shipped" candidate row (M159 to M162 reviews).
- 2026-10-07: question set: keep short `version` spellings? Answer: full names only. Jeff chose the option that said scripts using "S" or "F" break, so this plan reads it as the pre-1.0 waiver of the deprecation cycle (D-094).
- 2026-10-07: plan gate chose full names only (Jeff) over keeping `match.arg()`'s prefixes, because "F" already changed meaning when FFBF arrived and the HiTOP functions use `rlang::arg_match()`. Falsified by user reports of broken scripts that used prefixes.
- 2026-10-07: plan chose classed refusals over unclassed ones, because D-034(c) says tests assert by class and message text carries no promise. Falsified by a class that no caller or test can use.
- 2026-10-07: plan chose to fix the rounding wording over changing `round_half_up()` to round halves up for negative values, because a code change moves scored output (GP2) and a negative coding is rare. Falsified by a published PID-5 key that rounds a negative half toward zero.
- 2026-10-07: plan chose a spec-named text column for the JSON writer over a guard that refuses an IRF spec, because a guard blocks a later informant export. Falsified by no informant JSON export ever being planned.
- 2026-10-07: criteria audit (full mode, fresh Opus reader) returned 17 findings on the three drafted milestones. All were applied or carried. For M170: AC1 lists the full names and probes the omitted argument, "B", "FF", empty, NA, NULL and two values, with a class (findings 1 to 3). AC2 names the Manipulativeness facet, adds SOURCES.md and the D-090 annotation, and says "no function body" (4, 17). AC4 probes combined differences and each quote mark, defines whitespace, and updates the help (5, 6). The Word layout findings (7 to 13) went to the "APA PID-5 Word layout" candidate row. Findings 14 to 16 shaped M171.
- 2026-10-07: re-audit of AC1 after the full-names answer (fresh Opus reader, full mode, together with M171) returned 5 findings on M170, all applied. D-093 was already taken, so the classes and the waiver are D-094 and the rounding note is D-095. The whole default vector counts as omitted. The resolution test adds a mixed case, and the refusals add `1`. Probes run on valid input, and `plot_pid5()` skips without ggplot2. T1 lists the four test files that expect prefixes, and AC6's search covers `data-raw/` and both quote marks.
- 2026-10-07: test-generate_redcap.R:97-105 is edited by both M170 (T5) and M171 (T3). M170 goes first, and M171 rebases onto it.
- 2026-10-07: implement started on branch m170-pid5-version-text-matching. Untracked `devel/hitopdat_*` files are not this milestone's and stay unstaged.
- 2026-10-07: T1 done. `resolve_pid5_version()` in R/util.R replaces the seven `match.arg()` sites, and the unreachable `switch()` defaults are gone from score, reliability and validity. New test-pid5-version.R (AC1). Eight older tests now expect the class. Planting `match.arg()` back turned the refusal test red (108 failures). Full suite: 0 failed, 15 skipped.
- 2026-10-07: T2 done. The `score_pid5()` help, the `round_half_up()` comment and four SOURCES.md passages (marked corrected M170) say half away from zero. D-095 annotates D-090. The Manipulativeness test asserts -0.6 and 0.6. The AC2 search finds no hit. The test passes on unchanged code, because it pins a documented claim, not a fix.
- 2026-10-07: T3 done. New `normalize_item_text()` drops any leading or trailing run of periods, ellipses and whitespace. This is wider than AC4's four differences: a leading single period also goes. No pool has two items that normalize to one text (0 in all six pools, 400 FFBF texts included). The duplicate refusal covers every version and names the columns. Planted defects: exact matching gave 66 failures, and the old FFBF-only refusal turned the duplicate test red. Full suite: 0 failed.
- 2026-10-07: T4 done. `write_instrument_json()` takes an optional `text_col` and defaults to `Text`, so the five specs did not change. The new test writes an IRF spec with `TextIRF`. Its control, the same spec without `text_col`, writes self-report text that differs on every IRF row. The byte-for-byte rebuild test of the five shipped files passes.
- 2026-10-07: T5 done. The six child generators joined the five hand-kept lists. The Word smoke and legend lists, the Qualtrics smoke and width lists, and the REDCap smoke list pass with them. The stale counts in six test comments now match. test-generate-pid5irf.R's Society-footer list stays without the child forms, because their footer is the APA notice.
- 2026-10-07: T6 done. NEWS rewrites the M157 "B" breaking entry as the full-names entry and adds the duplicate refusal under breaking changes, plus two entries under improvements. The AC6 search finds only refusal tests, a format string and a regex. The first `check()` warned on non-ASCII in R/util.R, because the Edit tool wrote literal curly quotes in place of `\u` escapes. The escapes are fixed, and `check()` now gives 0 errors, 0 warnings and 0 notes. Full suite before the escape fix: 0 failed. The tests under `check()` after it: clean.
- 2026-10-07: claim audit: 54 claims read, 4 corrected — NEWS.md (which abbreviations used to resolve in which functions, and what the old error named), R/rename_pid5_items.R help and comment (normalization drops any leading or trailing run of periods, ellipses and whitespace), R/json_export.R (no IRF spec exists, so "would name").
- 2026-10-07: implement complete; status set to review.
- 2026-10-07: amendment routed: AC2 — its last clause, "No function body in `R/` changes", reads as a claim over the whole branch, and AC1, AC3 and AC5 change function bodies in R/ by design. The clause meant the scoring code. Status back to in-progress for the amendment alone.
- 2026-10-07: re-audit: AC2 (full) — nothing blocking. The two named functions hold all proration rounding (`apa_mean()` is called only from score_engine.R:103), the branch meets the clause, and the test tells away-from-zero from half up and from base `round()`. One optional rewording ("between the `function` line and the closing brace") was not taken, so no second reader is owed.
- 2026-10-07: amendment return: AC2 — "The rounding code does not change: `git diff main...HEAD -- R/util.R` shows no change inside the bodies of `round_half_up()` and `apa_mean()`."
- 2026-10-07: amendment done; status set to review.

## Review

- 2026-10-07 sync: main at 32f5dfca, which the branch contains; no merge needed.
- AC1 evidence: test-pid5-version.R passes (2 tests). The refusal test runs the 10 listed values in all 7 functions and asserts the class `hitop_unknown_version`, the call's function name, and `version` and each full name in the message. The resolution test runs upper, lower and one mixed case of each full name, the omitted argument and the lower-case default vector. Inputs are valid per version, and `plot_pid5()` skips without ggplot2 3.4.0. A planted `match.arg()` gave 108 failures (T1 work-log line).
- AC2 evidence: the `missing` and IRF-section roxygen in R/score_pid5.R say a prorated half rounds away from zero. The search for `halves up` and `half up` over R/, man/, vignettes/ and cairn/SOURCES.md finds no hit. The Manipulativeness test passes with -0.6 and 0.6. The clause "No function body in `R/` changes" fails as written: `git diff main...HEAD -- R/` changes the bodies of seven PID-5 functions, `rename_pid5_items()` and `write_instrument_json()`, as AC1, AC3 and AC5 require. `round_half_up()` and `apa_mean()` are unchanged. Not ticked; routed as an amendment.
- AC3 evidence: the duplicate test in test-rename_pid5_items.R passes for all six versions. It asserts the class `hitop_duplicate_item_match` and that the message names both columns and the item number. The old FFBF-only code failed it (T3 work-log line).
- AC4 evidence: the normalization test passes. It applies 9 variants to every stored text of every version: the four quote probes, `...`, `…`, a final period, surrounding spaces, tabs and line breaks, and all four differences together. The FFBF pool runs in four calls of 100. The `method` help names the quotes, the leading ellipsis, the final period and the whitespace, and says to remove the "He or she..." stem first.
- AC5 evidence: test-json-export.R passes (19 tests). The `text_col` test writes an IRF spec with `TextIRF` and matches every item to `pid_items$TextIRF`, and its no-`text_col` control writes the self-report text. The byte-for-byte rebuild test of the five shipped JSON files passes.
- AC6 evidence: NEWS.md lists the full-names change and the duplicate refusal under Breaking changes, and the text matching under Improvements. The search for `version *= *['"]` over R/, data-raw/, vignettes/, README.Rmd and tests/ finds only full names, refusal-test values, a `sprintf` template whose values are full names, and a regex that reads ggplot2's version. `devtools::test()`: 0 failed, 0 errors, 15 skipped. `devtools::check()`: 0 errors, 0 warnings, 0 notes.
