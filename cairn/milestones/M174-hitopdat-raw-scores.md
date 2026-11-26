# M174: HiTOP-DAT raw scale scores

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — a new exported scoring function
- **Branch/PR:** m174-hitopdat-raw-scores

## Goal

`score_hitopdat()` returns a raw score on each HiTOP-DAT scale other than General Depression, by the rule its measure's source states.

## Scope

**In:**
- `score_hitopdat()` in a new `R/score_hitopdat.R`, reusing the `R/util.R` validators and `round_half_up()`'s file for any helper it needs.
- Sums for the IDAS-II, WHODAS, AUDIT, DUDIT and PHQ-15 scales. Item means for the CAT-PD facets and CAPE positive symptoms. Reverse keys from `hitopdat_scales$reverseNumbers`.
- Proration of every scale with at least `min_complete` of its items answered (default 0.8), and a `_complete` column per scale (the authors' answers, SOURCES.md "The authors' joint answers").
- A refusal of answer values outside an item's answer set, under a new public class.
- Source notes for Andrews et al. (2009), Bergman & Källmén (2002), Berman et al. (2005) and Hinz et al. (2017), the sources of the scoring rules.
- SOURCES.md: DAT-5 and DAT-17 (c), (e) and (h) resolved by the authors' answers.

**Out:**
- T scores and the alert: M175.
- AUDIT and DUDIT T scores and the cutoff flags, CAT-PD domain scores, and scored REDCap and Qualtrics files: the candidate row "HiTOP-DAT after M143 to M145", waiting on the authors or on M175.
- General Depression: the authors dropped it (2026-11-18). Its `hitopdat_scales` row stays, as the IDAS-II key's record.
- A reliability function for the battery. Nobody asked for one.

## Acceptance criteria

- [x] AC1: `score_hitopdat(data, items, prefix = "dat_", min_complete = 0.8, append = TRUE)` returns a tibble. With `append = FALSE` its names equal, for the 56 rows of `hitopdat_scales` other than General Depression in table order, `prefix` + `camelCase`, followed by `prefix` + `camelCase` + `"_complete"` in the same order. A test compares the names with a list of the 56 stems typed in the test.
- [x] AC2: On fully answered responses drawn with a fixed seed from each item's `hitopdat_choices` set, plus one row at every set's minimum and one at its maximum, every score equals a recomputation in a test helper. The helper's item lists and reverse keys are typed from the shelved keys: Watson (2011) for the 18 IDAS-II scales, the IPIP CAT-PD-SF v1.1 key for the 33 CAT-PD facets, and the battery's item lists for the WHODAS, AUDIT, DUDIT, CAPE and PHQ-15. The rule is a sum for the IDAS-II scales (the metric of Watson et al., 2012, Table 2), the WHODAS (Andrews et al., 2009, p. 2), the AUDIT (Bergman & Källmén, 2002), the DUDIT (Berman et al., 2005) and the PHQ-15 (Hinz et al., 2017, 0 to 28). It is an item mean for the CAT-PD facets (the metric of the authors' workbook) and CAPE positive symptoms (the metric of the authors' REDCap version). A reversed item scores (min + max) − answer on its answer set.
- [x] AC3: With items missing, a scale whose answered share is at least `min_complete` scores its answered sum times its item count divided by its answered count (sum scales), or its answered mean (mean scales). A scale below that share scores `NA`. A test checks every scale at the smallest answered count k with k / n ≥ `min_complete` and at k − 1, with a reversed item among the missing where the scale has one. It runs at the default, at `min_complete = 1`, and at `min_complete = 0.7` on Rigidity (10 items), where 0.7 × 10 is not exact in floating point.
- [x] AC4: Each `_complete` column equals the scale's answered item count divided by its item count, tested on the AC3 fixtures.
- [x] AC5: An `NA` answer counts as missing. A zero-row `data` returns a zero-row tibble with the AC1 names. `score_hitopdat()` refuses each of these, and a test fires each one and asserts its condition class (or its message, for the unclassed validators in `R/util.R`): `data` not a data frame; `items` not of length 382, holding a duplicate, or naming a column absent from `data`; a non-numeric item column, a factor included (`hitop_nonnumeric_items`); an answer that is not a value of its item's answer set in `hitopdat_choices` (the new public class `hitop_invalid_answer`); `min_complete` not one number in (0, 1]; `prefix` not one string; `append` not `TRUE` or `FALSE`; an output name already in `data` (`hitop_append_collision`).
- [x] AC6: `?score_hitopdat` states each measure's rule and source, the 80% default as the authors' choice, that the PHQ-15 prorates where Hinz et al. (2017, p. 28) filled one missing item with the other items' rounded mean, and that General Depression is not scored at the authors' request. NEWS.md has an entry, `_pkgdown.yml` lists the function, and the README capability table marks HiTOP-DAT scoring. `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T2, T3
- AC2 → T1, T2, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T2, T3, T4
- AC6 → T5

## Tasks

- [x] T1: Write the source notes `andrews2009.md`, `bergman2002.md`, `berman2005.md` and `hinz2017.md` from the shelved PDFs, each with its sha256 and INDEX line, holding the scoring rule the milestone uses. Resolve DAT-5 and DAT-17 (c), (e) and (h) in SOURCES.md, citing `jonas_ringwald2026email3.txt`.
- [x] T2: Write `tests/testthat/test-score_hitopdat.R` first, with a helper that types the item lists and reverse keys from the keys (LESSONS 2026-10-04 (M159): random answers with `NA`, never a periodic pattern). Run it on the base and log that it fails for the missing function.
- [x] T3: Write `score_hitopdat()` in `R/score_hitopdat.R`. Read memberships and reverse keys from `hitopdat_scales` and answer sets from `hitopdat_items` and `hitopdat_choices`; hardcode no item number (CLAUDE.md).
- [x] T4: Append two D-entries: the signature departs from the family's (`min_complete` in place of `missing`, no `srange`, no `calc_se`), because the battery's answer sets differ by measure and the authors set the proration rule; and the new public class `hitop_invalid_answer` (D-034(c)).
- [x] T5: Write the roxygen, NEWS entry, `_pkgdown.yml` entry and README row (`devtools::build_readme()`). Run `devtools::document()`, `devtools::test()` and `devtools::check()`.

## Work log

- 2026-11-25: created by /milestone-plan, together with M175.
- 2026-11-25: tier check: milestone — new exported scoring function with design decisions.
- 2026-11-25: idea vet: gap — IDAS-II metric: the REDCap version scores item means and the Qualtrics file and Table 2 sums; reading taken: sums, the metric Table 2 prints, which gives the same T. Gap — CAPE metric: item mean, as both of the authors' versions use. Gap — "report both thresholds" was read as covering cutoffs only; the reading for T scores goes to the authors (candidate row). No flaw: D-098 settles the unpublished-norms concern.
- 2026-11-25: lessons used: 2026-10-04 (M159), 2026-07-31 (M032), 2026-08-21 (M043).
- 2026-11-25: collision check: promotes the parts "scale scores by each measure's rule" and the M143 review's reverse keys and stems of the candidate row "HiTOP-DAT after M143 to M145"; D-098 governs; no open issue overlaps (one open, #87).
- 2026-11-25: question set: how should the parts waiting on the authors wait? Answer: ask them, plan later. Jeff sends the follow-up drafted in this session.
- 2026-11-25: question set: when to plan the scored REDCap and Qualtrics files? Answer: after the R scoring ships (candidate row).
- 2026-11-25: plan chose proration with a minimum share (Jonas 2026-11-18) over the REDCap version's unprorated sums and means, because the latest answer overrides the file. Falsified by a later author answer.
- 2026-11-25: criteria audit (fresh Opus reader, full mode, with M175 and a dropped M176 draft) returned 21 findings. Applied to M174: draws from each answer set with min and max rows (AC2), the CAPE metric cited to the REDCap version (AC2), a 0.7 probe on Rigidity (AC3), the PHQ-15 proration departure stated (AC6), `NA` as missing and zero rows (AC5), the signature departure as a D-entry (T4).
- 2026-11-25: implement started on branch m174-hitopdat-raw-scores, cut from main at 79e81556; six untracked `devel/hitopdat_*` files left unstaged, not milestone work.
- 2026-11-25: T1 done: source notes andrews2009, bergman2002, berman2005, hinz2017 with INDEX lines, read from the shelf PDFs' text with page anchors; DAT-5 resolved from Jonas's second answers (answer 2, item 99 is battery item 111) and DAT-17 (c), (e), (h) from her joint answers (answers 1, 2).
- 2026-11-25: T2 choice: the typed IPIP and IDAS-II keys and `battery_numbers()` moved from test-keying-hitopdat.R to a new helper-hitopdat-keys.R, so the scoring test reads the same transcriptions; the keying test passes unchanged (300 expectations).
- 2026-11-25: T2 done: test-score_hitopdat.R written; on the base it fails 10 tests, each on `could not find function "score_hitopdat"`, 2 expectations passing (the typed stem count and key names).
- 2026-11-25: T4 done: D-099 (the signature without `srange`, `missing` and `calc_se`, with `min_complete`) and D-100 (`hitop_invalid_answer`) appended; committed before T3 while the T3 suite ran.
- 2026-11-25: T3 checkpoint, not yet verified: `score_hitopdat()` and the util.R helpers `validate_share()` and `validate_answer_sets()` written; new tests pass (392); the first full suite stopped at its failure cap on four export-wide sweeps (append collision, error prose, README capabilities, vignette coverage), now given HiTOP-DAT entries with a shared `hitopdat_answer_ends()` fixture, plus T5 docs, the download-page scoring card, and DESIGN.md/CLAUDE.md lines naming the function; full suite rerun pending. DESCRIPTION's roxygen version line moved to 8.1.1 by the local `document()`.
- 2026-11-25: T3 done: full `devtools::test()` 0 failures, 31,393 passed, 15 skipped. Choices: proration compares the answered share with `min_complete` less `sqrt(.Machine$double.eps)`, so a share such as 1 - 0.3 meets 7 of 10; reverse keys are applied per scale, so items 39 and 76 stay forward in Well-Being (tested).
- 2026-11-25: claim audit (fresh Opus reader) flagged 5 of about 85 claims. All 5 were corrected in code or wording. One was a defect: with `min_complete` below about 1.5e-8 a scale with no answered item scored NaN. It now scores NA, with a test. Re-read by the same reader is pending. The first `devtools::check()` was stopped because it predated the fix.
- 2026-11-25: claim audit: 85 claims read, 5 corrected — R/score_hitopdat.R, R/util.R, tests/testthat/test-score_hitopdat.R, tests/testthat/helper-hitopdat-keys.R, vignettes/articles/download-hitopdat.Rmd (the reader's one re-read found all 5 corrections hold).
- 2026-11-25: T5 done: roxygen, NEWS entry, `_pkgdown.yml` row, README row (`build_readme()`), and, added as a minor sub-task, the download page's scoring card, which replaces its stale "does not score" sentence and covers the export for the vignette-coverage test. `pkgdown::check_pkgdown()` reports no problems; `devtools::check()` gives 0 errors, 0 warnings, 0 notes. Status set to review.
- 2026-11-25: correction to the T3 line: `1 - 0.3` equals 0.7 exactly in R, so it shows nothing about the tolerance; `0.1 * 7` is slightly above 0.7 and still scores 7 of 10 (run at review). The code comment and `?score_hitopdat` now use that example (ff3443f0).

## Review

- Sync: `origin/main` is 79e81556, the cut point; no merge needed. No PR yet; the Copilot round is off (no `# Copilot review:` line in PROFILE.md).
- AC1: test "the output names are the 56 scored scales, then their _complete columns" passes (2026-11-25, `devtools::test(filter = "score_hitopdat")`, 0 failed); it compares names with 56 stems typed in the test, for `append = FALSE`, `prefix = "x."` and `append = TRUE`. A direct run on the two-row answer-ends frame gave a `tbl_df` of 112 columns whose names equal `dat_` + the 56 non-General-Depression `camelCase` values in table order, then the same with `_complete`.
- AC2: test "fully answered scores equal the recomputation from the typed keys" passes (same run): 40 rows drawn with seed 1743 from each item's `hitopdat_choices` set plus a minimum row and a maximum row, compared whole with `recompute_hitopdat()`. The helper reads Watson (2011) and IPIP keys from helper-hitopdat-keys.R and types the WHODAS 1:12, AUDIT 112:121, DUDIT 122:132, CAPE 133:152 and PHQ-15 369:382 lists; it reads `hitopdat_scales` nowhere (grep: only comments name it). Sums for WHODAS, IDAS-II, AUDIT, DUDIT, PHQ-15, means for CAT-PD and CAPE, reversal 6 − answer on the 1-to-5 CAT-PD set. The same test pins the source ranges 0–48, 0–40, 0–44, 0–28.
- AC3: tests "each scale prorates at the default 80% and is NA one item below it", "min_complete = 1 scores only fully answered scales" and "min_complete = 0.7 scores Rigidity at 7 of 10 items, not 6" pass (same run). For each scale they build a row at the smallest k with k/n ≥ the share (k typed as integer arithmetic, `(4n + 4) %/% 5` at the default) and one at k − 1, reversed items dropped first, and compare the whole output with the recomputation (sum × n / answered, or answered mean). Discrimination: a loaded copy with the threshold planted as `share <= min_complete` failed all three (12, 57 and 2 expectations) and the typed proration test (1); the source was not edited.
- AC4: the three AC3 tests also assert, per scale, `_complete` equal to `c(k, k - 1) / n` from the typed k (`check_proration()`, `expect_equal(..., info = s)`), and compare every `_complete` column with the recomputation's answered / n; all pass (same run). The typed case checks `dat_whodas_complete` = 10 / 12.
- AC5: tests "NA answers are missing, and zero rows give the named empty tibble" (6 expectations), "score_hitopdat() refuses bad data, items and arguments" (17) and "an answer outside its item's answer set is refused by name" (9) pass (same run). An all-`NA` frame scores `NA` with `_complete` 0, also at `min_complete = 1e-9`; a zero-row frame returns 0 rows with the AC1 names. Refusals fired: a list for `data` ("must be a data frame"), 381 items ("wrong length"), a duplicate ("distinct column"), an absent column ("must all be columns"), a factor column (`hitop_nonnumeric_items`), answers 6 on a CAT-PD item, 1 on an `audit_harm` item, 2.5 on an IDAS-II item and −1 on a PHQ-15 item (`hitop_invalid_answer`, message naming the column), `min_complete` of 0, 1.1, −0.5, length 2, "0.8", `NA` and `Inf` (message names `min_complete`), `prefix` 1 and length 2 ("single string"), `append = "yes"` (its exact message), and a `dat_whodas_complete` column in `data` (`hitop_append_collision`), with `append = FALSE` passing.
- Consistency gate (2026-11-25): `cairn_validate.py` exit 0 (advisories only: dangling ids in older records, one references-staleness on schmukle2026.md, none from this branch); `devtools::document()` leaves no diff; README.md equals README.Rmd's row (knitted in bd2fdbdd); `pkgdown::check_pkgdown()` no problems; NEWS.md has the entry, no milestone numbers; no new top-level file; `devtools::check()` at 8ea68567 0 errors, 0 warnings, 0 notes, and only `cairn/` changed since (`git diff --stat 8ea68567 HEAD`). No principle changed, so `cairn_impact` was skipped.
- spawned: diff-bug, blame-history, prior-review, ripple
- diff-bug #1: logical item columns pass the answer-set check as 0/1 — reject (false): D-068 reads a logical column as its 0/1 values by decision, and those values are then checked against the set like any number; no HiTOP-DAT form or export writes logical answers.
- diff-bug #2: NaN counts as missing without notice — reject (false): the family reads NaN as missing (`unparsed_value()`, D-068), and the scale's `_complete` column reports the missing item.
- diff-bug #3: the rounding tolerance admits a share up to 1.5e-8 below `min_complete` while the docs say "at least" — fix now: the `min_complete` doc states the comparison is within rounding error, fixed ff3443f0.
- diff-bug #4: the non-numeric refusal is tested only with a factor — reject (false): the refusal is the shared `validate_item_columns()`, whose text, Date, list and haven kinds test-nonnumeric-items.R covers; the factor case pins this function's call of it.
- diff-bug #5: the NaN regression is pinned on one scale — reject (false): every scale runs the same line, as the reviewer says.
- diff-bug #6: duplicate non-item column names abort with tibble's unclassed error under `append = TRUE` — reject (edge): only a data frame with two same-named non-item columns reaches it, outside documented and common use; the effect is an error, as in `score_engine()`.
- diff-bug #7: DESCRIPTION's roxygen version moved to 8.1.1 — reject (false): `document()` writes it and the gate requires a no-diff `document()`; the T3 work log records it.
- diff-bug #8: the Well-Being forward test repeats what the exact recomputation covers — fix now: removed, fixed ff3443f0.
- diff-bug #9: the Workaholism and Social Withdrawal values repeat the exact recomputation — fix now: removed, fixed ff3443f0.
- diff-bug #10: the hand-typed proration case runs the path `check_proration()` compares — reject (false): it is the one expectation typed independently of both the code and the helper (check discrimination).
- diff-bug #11: the `_complete` equality repeats the exact comparison — reject (false): it uses the typed k, independent of the helper's threshold, and AC4 names it.
- diff-bug #12: seven `min_complete` refusal values share one assertion — fix now: kept 0, 1.1, length 2, "0.8" and `NA`; dropped −0.5 and `Inf`, fixed ff3443f0.
- diff-bug #13: two `prefix` refusal cases share one path — fix now: kept `prefix = 1`, fixed ff3443f0.
- diff-bug #14: four invalid answers share one path — fix now: kept 6 on a CAT-PD item and 1 on an `audit_harm` item (inside the range, outside the set), fixed ff3443f0.
- diff-bug #15: the accepted min, max and `NA` block repeats the recomputation and the all-`NA` test — fix now: removed, fixed ff3443f0.
- diff-bug #16: the stem-count and key-name checks test the fixture — reject (false): they keep the typed stem list and key from emptying silently (check discrimination).
- diff-bug #17: three test comments restate code — fix now: the threshold comment and the 1e-9 comment removed (the third went with #15), fixed ff3443f0.
- blame-history #1: the README call hardcodes the HiTOP-DAT branch — reject (false): it checks what the other branches check, as the reviewer says.
- blame-history #2: the moved keys — noted: verified identical.
- blame-history #3: the error-prose counts — noted: correct.
- blame-history #4: the removed "no scoring yet" sentences — noted: the authors' joint answer met D-098's wait.
- blame-history #5: the roxygen version line — reject (false), as diff-bug #7.
- blame-history #6: README.md edited by hand — reject (false): `build_readme()` knitted it in bd2fdbdd.
- blame-history #7: the signature departure notes — noted: they cite D-099.
- prior-review #1: the item-order warning fires before the collision refusal, against M060's "ahead of its warnings" — reject (false): `score_engine()` runs the same validation warning first; M060's rule concerns the warnings of building the output.
- prior-review #2: the HiTOP-DAT row marks Scoring with an empty Tutorial cell — reject (false): the column links tutorials that exist, none exists for the HiTOP-DAT, and the README test agrees.
- ripple #1: R/data.R `?hitopdat_items` says the battery has no scoring function — fix now, fixed ff3443f0.
- ripple #2: R/generate_docx.R says the package ships no HiTOP-DAT scoring key — fix now, fixed ff3443f0.
- ripple #3: CLAUDE.md's opening line omits the HiTOP-DAT — follow-up: candidate row "Name HiTOP-DAT in the CLAUDE.md summary" (low: no user reads it; left for Jeff since CLAUDE.md is his instruction file).
- ripple #4: DESIGN.md's Function families signature line has no departure note — fix now, fixed ff3443f0.
- ripple #5: ipip2015catpdsf.md and watson2011idas.md trace the key lists to test-keying-hitopdat.R — fix now, fixed ff3443f0.
- ripple #6: test-append-collision.R says "an eighth appending export" — fix now, fixed ff3443f0.
- ripple #7: SOURCES.md's machine-check pointer omits the helper — fix now, fixed ff3443f0.
- Return floor: no finding shows a criterion failing or a load-bearing defect; no status change.
- AC6: man/score_hitopdat.Rd names each measure's rule with its source (Andrews 2009 p. 2, Watson 2012 Table 2, Bergman & Källmén 2002 p. 245, Berman 2005 p. 23, Hinz 2017 p. 28, the CAT-PD workbook and the authors' REDCap version for CAPE), calls the 80% default the authors' choice (line 31), states the PHQ-15 departure from Hinz's rounded one-item fill (line 83), and says General Depression is not scored at the authors' request (line 88). NEWS.md:14 has the entry, `_pkgdown.yml`:82 lists the function, README.Rmd:52 and README.md mark HiTOP-DAT scoring "Yes". On ff3443f0, after the fix-now commit, `devtools::test()` gave 0 failures, 31,384 passed, 15 skipped, and `devtools::check()` gave 0 errors, 0 warnings, 0 notes; only the milestone file changed after it.
- Snapshot files: `git diff --name-only main...HEAD` lists no `_snaps` path; the list is empty.
- PR conversation: no PR yet, so the read does not run. Copilot round off.
