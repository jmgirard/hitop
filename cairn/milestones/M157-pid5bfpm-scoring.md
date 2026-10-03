# M157: PID5BF+M keying and scoring

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2
- **Resolves:** —
- **Surface tier:** user-facing — a new `version` value on exported scoring functions and new keying data
- **Branch/PR:** m157-pid5bfpm-scoring

## Goal

Researchers can score the 36-item PID5BF+M (Bach et al., 2020) with `score_pid5()` and `reliability_pid5()`.

## Scope

**In:**
- The BF+M keying enters `pid_items` and `pid_scales` through `data-raw/pid_info.R`. That is a 36-position map onto PID-5 item numbers, plus 18 facets with 2 items each and 6 domains: Negative Affectivity, Detachment, Antagonism, Disinhibition, Anankastia and Psychoticism.
- The keying source is the FU Berlin key sheet on the shelf (`cairn/references/sources/fuberlin_pid5bfpm_de.pdf`, p. 2). Its table maps each BF+M item to its PID-5 item number and gives the scoring rule.
- `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` accept the new version.
- A references page, a SOURCES.md row, tests, NEWS, and a scoring vignette section.

**Out:**
- Word, Qualtrics and REDCap forms go to M158.
- The 34-item PID5BF+ waits on Kerber et al. (2022) Figure 1 or its supplement (candidate row).
- BF+M norms (Rek et al., 2022) get a candidate row.
- `validity_pid5()`, `norm_pid5()` and `plot_pid5()` keep their current versions. Each refuses the new version through its existing `match.arg()`. The plot gets a candidate row, and no validity scale exists for this form.
- Module Builder, hitop-form and the Study Link Builder get a candidate row.

## Acceptance criteria

- [x] AC1: `pid_items` gains a BF+M column. Its 36 non-missing values sit on exactly the PID-5 rows that the key sheet (p. 2) lists, each at its BF+M position. `pid_scales` gains a BF+M entry whose 18 facets list exactly the BF+M item pairs the key sheet gives. The package also holds the BF+M facet-to-domain map, 6 domains with 3 facets each. A keying test compares all three with a transcription of the key-sheet table typed into the test file, not derived from `pid_items`.
- [x] AC2: `score_pid5(version = <BF+M string>)` returns 18 facet and 6 domain columns. Its values equal hand-computed values under the scoring metric and missing-data rule the pre-implementation gate settles. The test fixture has at least 5 respondents. It covers each `missing` mode the gate keeps for the BF+M, with a missing item under each mode. The expected values are typed into the test, not computed by package code. (RB tripwire: irreversible-api)
- [x] AC3: The existing versions do not change. A `data-raw/` characterization script in the `characterize_calc_se.R` pattern runs at the merge base and at the branch head. It shows `identical()` output for `score_pid5()` over version (FULL, SF, BF) × `missing` × `calc_se` × `append` on `sim_pid5`, `sim_pid5sf`, `ku_pid5sf` and `sim_pid5bf`. It does the same for `reliability_pid5()` over version × `alpha` and `omega`, for `rename_pid5_items()` over version × its matching methods, and for `label_pid5()` over version.
- [x] AC4: `reliability_pid5()` returns one row per BF+M facet and domain. `rename_pid5_items()` maps the 36 BF+M items, and `label_pid5()` labels them. Each has a test on the new version.
- [x] AC5: The `score_pid5()` help page and `vignettes/pid5_scoring.Rmd` name the form and cite Bach et al. (2020). They state the scoring metric and missing-data rule. They also say that no BF+M item is reverse-keyed. NEWS.md has an entry. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T4
- AC4 → T3, T4
- AC5 → T5

## Tasks

- [x] T1: Write the references page for the key sheet: extracted table, scoring rule, provenance. If Jeff uploads Bach et al. (2020), check the key against it and record the result. Add the SOURCES.md row.
- [x] T2: Add the BF+M column and the `pid_scales` entry in `data-raw/pid_info.R`, and regenerate the data. Build both from a key CSV with its own facet and domain labels, in the key sheet's domain-grouped order. Add `pid_bfpm_domains` (D-088(c)) and its docs. Write the keying test (AC1). Keying content needs Jeff's sign-off before merge.
- [x] T3: Pre-implementation gate (settled by RR06 and D-088), then code. The gate's questions (RB tripwire: irreversible-api):
  - The version string, and the item-name stem for `rename_pid5_items()` and the M158 exports.
  - The output column names, including `anankastia`.
  - Where the facet-to-domain map lives, since `pid_domains` holds only the 5 FULL and SF domains.
  - The scoring metric. The key sheet sums each facet's 2 items and averages the 3 facet sums for a domain. The package reports item means elsewhere.
  - The missing-data rule. The key sheet states no proration rule.

  Then thread the version through `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()`. If `score_engine()` cannot express the chosen metric, teach it the metric. `reliability_pid5()` adds the 6 domain rows from `pid_bfpm_domains`. `label_pid5(target = "scales")` reads that table for BFPM. The reliability engine reports omega as `NA` for a scale with fewer than 3 items (D-088(f)), with a test that BFPM reliability emits no warning.
- [x] T4: Write the hand-computed fixture tests (AC2, AC4). Write the characterization script (AC3) and run it at the merge base before the version code lands.
- [x] T5: Update the help page, the vignette section and NEWS. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M158–M161. Sources come from a web survey (shelf files dated 2026-10-03). Johannes Zimmermann's original request is not on file yet.
- 2026-10-03: plan chose a new `version` value on `score_pid5()` over a separate `score_pid5bfpm()`. CLAUDE.md sets one function per task with a version argument. Falsified by a BF+M rule that the shared engine cannot express without per-version branches outside `score_pid5()`.
- 2026-10-03: plan chose the FU Berlin key sheet as the keying source over waiting for Bach et al. (2020). The sheet maps every item to a PID-5 number and states the rule, and it is on the shelf. Falsified by a disagreement between the sheet and the paper.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 2 judgment findings, all applied. AC1 now binds the facet-to-domain map. AC2 covers each kept `missing` mode. AC3 names a `data-raw/` characterization script, because `test-score_pid5.R` has no argument matrix. The T3 gate adds the item-name stem, and `label_pid5()` joins the scope.
- 2026-10-03: implement started on branch `m157-pid5bfpm-scoring`. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit.
- 2026-10-03: T1 done. Pages `references/fuberlin2020pid5bfpm.md` and `references/bach2020.md`, and a `BFPM` row in SOURCES.md. Bach et al. (2020) was already on the shelf as `bach2020a.pdf`. Its p. 181 confirms the 6 anankastia PID-5 items (123, 176, 140, 220, 34, 115) and the domain rule. Its Appendix A, the full key, is not on the shelf. All 36 key-sheet PID-5 numbers match the German item at their BF+M position in pair order.
- 2026-10-03: minor amendment: T2 now runs after the T3 gate, because the `pid_items` column name is the version string that the gate settles.
- 2026-10-03: T3 gate posed with the agent's five recommendations. Jeff chose escalation. Blocked on RB06 (`cairn/reviews/archive/RB06-pid5bfpm-api.md`). The brief commit lands on the milestone branch, not main, because a status edit on main conflicts with the branch's ROADMAP row.
- 2026-10-03: RR06 ingested (Fable subagent). It accepts the five gate recommendations and adds a domain-grouped row order, omega `NA` under 3 items, and no total score. Promoted as D-088. Triage is in Decisions. Minor amendment: T2 and T3 name the new work. Status back to in-progress.
- 2026-10-03: T2 done. `data-raw/pid_bfpm_key.csv` feeds `pid_items$BFPM` (after `BF`), `pid_scales$BFPM` and the new `pid_bfpm_domains`. The regenerated FULL, SF and BF elements and `pid_domains` are `identical()` to the old files, and every old `pid_items` column is unchanged. AC1's keying test failed on two planted key errors (a swapped pair, a facet moved to another domain) and passes on the real key.
- 2026-10-03: T2 choice: `tibble::add_column()` keeps readr's class and `spec` on `pid_items`. The spec stays the record of `pid_items.csv` and does not list BFPM.
- 2026-10-03: T2 found that `test-plot_pid5.R` built its cases from `names(pid_scales)`, which RR06 missed. The cases now come from `plot_pid5()`'s own `version` choices.
- 2026-10-03: T3 done. `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` take `"BFPM"`. `score_engine()` needed no change, because its `domain_map` path already scores a domain as the mean of its facet means. The reliability engine skips omega for a scale under 3 items. Its no-warning test passes with the guard and fails on a lavaan warning without it.
- 2026-10-03: T3 choices: `validity_pid5()` gets its own `version` doc, because it inherited `score_pid5()`'s, which now lists BFPM. The `version` lookups use `if`/`else` rather than a `switch()` fall-through, which `test-warning-classes.R`'s body walker cannot read.
- 2026-10-03: T4 started early: `data-raw/characterize_bfpm.R` ran at the merge base `75a93d1b` (a scratch worktree) before the T3 code, and captured 80 calls.
- 2026-10-03: T4 done. `fx_pid5bfpm()` has 5 respondents with hand-worked values. Tests cover all 24 columns under the three `missing` modes, a key-pair recomputation, reliability rows, rename and label. Oracles O-008 to O-010 are in `cairn/ORACLES.md`. A rotated domain map failed 30 score assertions, and `pid_domains` in place of `pid_bfpm_domains` failed the label test on Anankastia.
- 2026-10-03: AC3 run: `characterize_bfpm.R` at the branch head gave 80 of 80 calls `identical()` to the merge-base run. A planted omega threshold of 6 items made it report the 8 omega calls, and the restored code passed.
- 2026-10-03: T5 in progress (checkpoint). Help pages, the vignette section, NEWS, README roadmap rows, and the DESIGN.md and CLAUDE.md form lists are written. `pkgdown::check_pkgdown()` passes. `devtools::check()` is running, and `build_readme()` waits for it.
- 2026-10-03: T5 done. `devtools::check()` gave 0 errors, 0 warnings and 0 notes, with the vignette rebuilt. `build_readme()` changed only the PID5BF+M rows. Four help-page statements were run on `fx_pid5bfpm()` before they were written. They are the factor of 2, the `"apa"` rule, the domain from one facet and the one-item `NA` standard error.
- 2026-10-03: claim audit: 95 claims read, 3 corrected — R/label_pid5.R, R/data.R, vignettes/pid5_scoring.Rmd
- 2026-10-03: The claim audit's one re-read cleared the label and data fixes. It also corrected the vignette's warning source to these fits, not lavaan alone. Status is now review.

## Decisions

- 2026-10-03 (RR06, triage): recommendations 1 to 6 and 8 applied as D-088 (a) to (e) and (g). Recommendation 7 applied as D-088(f), with the omega guard in the reliability engine rather than in `calc_omega()`. `calc_omega()` is exported, and a guard there changes its result for a direct 2-item call. No shipped scale has fewer than 3 items, so the engine guard changes no existing output. Recommendation 9 (the rejected alternatives) is recorded in D-088. Recommendation 10: the `calc_se` note on 1-item facets goes into the help text in T5, and the scoring-table note goes to M158's work log.

## Review

- AC1 evidence (2026-10-03, head a83caaa4): `test-keying.R` passes, 166 assertions and 1 old skip. Its four BF+M tests hold the key sheet's table typed in. They check `pid_items$BFPM` (5 assertions), the `pid_scales$BFPM` facets, pairs and order (5), the `pid_bfpm_domains` map (6) and shared-facet membership (18), with 0 failures. The keying test failed on two planted key errors during T2.
- AC2 evidence (2026-10-03, head a83caaa4): the 18 facet and 6 domain columns come out in key order (1 assertion). "BFPM scores match hand-computed values under each missing mode" passes 72 assertions with 0 failures. That is 24 columns times `"available"`, `"apa"` and `"complete"`, on `fx_pid5bfpm()`'s 5 respondents. R4 has 1 missing item and R5 has 2. The expected values are typed literals with the arithmetic in `helper-fixtures.R`. The gate settled all three `missing` modes for the form (D-088(e)). The key-pair recomputation passes 48 assertions. The `"apa"` versus `"complete"` identity test passes on this whole-number fixture.
- AC3 evidence (2026-10-03, review run): `data-raw/characterize_bfpm.R` ran fresh at the merge base `75a93d1b` (scratch worktree, since removed) and at head `a83caaa4`. All 80 of 80 entries are `identical()`. The `score_pid5` calls are 48: 4 dataset-version pairs × 3 `missing` × `calc_se` × `append`. The other calls are `reliability_pid5` 16 (× `alpha` × `omega`), `rename_pid5_items` 8 and `label_pid5` 8. Each entry includes the conditions it raised. During T4 a planted omega threshold made the script report the 8 omega calls.
- AC4 evidence (2026-10-03, head a83caaa4): `test-reliability.R` passes. "returns 18 facet rows, then 6 domain rows" (5 assertions) types the 24 names and the item counts. "fits no 2-item omega and raises no warning" (5) passes, and during T3 it failed without the guard. `test-rename_pid5_items.R` passes, with BFPM renaming of all 36 by number and one by text (8 assertions). `test-label_pid5.R` passes, with 36 item labels (38 assertions) and the 24 typed scale labels (1).
- AC5 evidence (2026-10-03, head a83caaa4): `man/score_pid5.Rd` names the PID5BF+M, cites Bach et al. (2020) under its references, and states the item-mean metric with the factor of 2 against the key. It states the missing-data rule and says no item is reverse-keyed (subsection "The PID5BF+M"). `vignettes/pid5_scoring.Rmd` section "The PID5BF+M" does the same, with the DOI. NEWS.md has a New features entry. `devtools::check()` gave 0 errors, 0 warnings and 0 notes in 5m 53s, with tests and the vignette rebuilt. `pkgdown::check_pkgdown()` reported no problems.
- Gate (2026-10-03): `cairn_validate.py` exits 0, with 27 advisory warnings, all from older records. `devtools::document()` leaves no diff. README.md was knitted after the last README.Rmd change. NEWS.md has the entry, and no new top-level file was added. No DESIGN.md principle changed, so `cairn_impact` was skipped. Check and pkgdown results are under AC5.
- spawned: diff-bug (Opus), blame-history (Sonnet), prior-review (Sonnet)
- diff-bug #1: adding `"BFPM"` stops `version = "B"` from resolving to `"BF"` (it now matches two choices and errors), and lets `"bfp"` resolve to BFPM. Fix now: a NEWS breaking-change line. The waiver of a deprecation cycle goes to the merge question. RR06 had said `"B"` already failed, which a run disproved.
- diff-bug #2: `"apa"` equals `"complete"` only for whole-number responses, because `apa_mean()` rounds the sum (items 1.4 and 1.4 give 1.5 against 1.4). Fix now: the help page, vignette and NEWS say "with whole-number responses".
- diff-bug #3: under `"available"`, a facet with both items missing is `NaN`, not `NA`, and so is a domain with all 3 facets missing. Follow-up: new candidate row. This happens on every form already, and 2-item facets make it common.
- diff-bug #4: no BFPM `calc_se` test, though the help makes two claims about it. Fix now.
- diff-bug #5: the text method of `rename_pid5_items()` is tested on 1 BFPM item. Fix now: a test on all 36 texts.
- diff-bug #6: the label item test reads its expected text through `pid_items$BFPM`, the lookup under test. Fix now: the expected text goes through typed PID-5 numbers.
- diff-bug #7: the reliability alpha test checks 2 of 24 rows. Fix now: all 24 rows against typed item sets.
- diff-bug #8: the new help subsection put the existing "Errors" paragraph under "The PID5BF+M". Fix now: the subsection moves above the BF total section.
- diff-bug #9: `validity_pid5()`, `norm_pid5()` and `plot_pid5()` refuse BFPM with the bare `match.arg()` error. Reject: planned change. Scope Out says each refuses it through its existing `match.arg()`.
- diff-bug #10: `pid_bfpm_domains`'s `@source` cites only Bach et al., but the keying came from the FU Berlin key sheet. Fix now: the key sheet is added.
- diff-bug #11: comments in `R/score_engine.R` and `R/json_export.R` still say only FULL/SF have a domain map and that `pid_items` has three form columns. Fix now.
- diff-bug #12: one long roxygen line in `R/label_pid5.R`. Reject: style.
- diff-bug #13: no BFPM example in `?score_pid5`. Fix now: an example built from `sim_pid5`, as the vignette does.
- diff-bug #14: keying content needs Jeff's sign-off before merge (CLAUDE.md). Noted: the merge question asks for it.
- blame-history #1: the comment above `profile_cases()` in `test-plot_pid5.R` still says the list comes from `pid_scales`. Fix now.
- blame-history #2: the omega guard returns `NA` silently for any future 2-item scale. Reject: planned change, D-088(f), and the help states it.
- blame-history #3: no test pins the BFPM refusal by `validity_pid5()`, `norm_pid5()` and `plot_pid5()`. Fix now.
- blame-history #4: the `R/json_export.R` comment. Fix now, with diff-bug #11.
- blame-history #5: BFPM reverse-keying is read from `pid_items$Reverse`, so a later change there flows in silently. Reject: false. `test-keying.R` asserts that no BF+M item is reversed and fails on that change.
- blame-history #6: `pid_items$Facet` no longer names a form-independent facet. Reject: planned change, D-088(c), documented in `?pid_items`.
- blame-history #7: D-088(a) says no other spelling is accepted, but case folding and partial matching accept more. Fix now, with diff-bug #1: NEWS names the matching.
- blame-history #8: the keying sign-off. Noted, with diff-bug #14.
- prior-review #1: the vignette's BFPM reliability chunk prints 10 of 24 rows, so the domain rows it describes are hidden (the M073 lesson). Fix now: print all 24.
- prior-review #2: BFPM is not in the label and rename loops, and has no `calc_se` test. Fix now: the `calc_se` test (diff-bug #4) and a BFPM mis-padding and out-of-range label test.
- prior-review #3: stale file headers in `helper-fixtures.R` and `R/json_export.R`. Fix now.
- prior-review #4: NEWS says the omega rule holds for all `reliability_*()` functions, but only `?reliability_pid5` says so. Fix now: `?reliability_hitopsr` and `?reliability_hitopbr` get the sentence.
- Fix-now landed (2026-10-03): all 18 fix-now dispositions above are on the branch. The new tests pass. They check BFPM standard errors, the `"B"` and `"bfp"` abbreviations, all 24 alphas and all 36 rename texts. They also check typed-number item labels, BFPM mis-padding, and the refusals by validity, norm and plot. The NaN follow-up is a new ROADMAP candidate row. To stay under the 60-line cap, that row's commit clusters the HiTOP-SR/BR norms row and validity-scales row into one.
- prior-review #5: `"BFPM" = 36` is hardcoded in two `switch()` calls. Reject: false risk. A wrong count fails every BFPM test, and it matches the other versions' entries.
