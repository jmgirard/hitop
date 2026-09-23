# M109: Scoring refuses an item column it cannot read as numbers

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — changes what seven exported functions accept
- **Branch/PR:** m109-nonnumeric-items

## Goal

If a researcher scores an export of choice text or factor codes, the call aborts and names the bad columns. It does not return all-`NA` or wrong scores.

## Scope

**In:** One value rule for item columns. `prep_items()` (`R/util.R:505`) and `validity_pid5()` (`R/validity_pid5.R:133`) check it. The rule covers `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, `reliability_pid5()`, `reliability_hitopsr()`, `reliability_hitopbr()` and `validity_pid5()`. The abort has the new public class `hitop_nonnumeric_items`. The milestone also updates the `items` documentation and NEWS, and adds a D-entry for the class. It adds haven to Suggests for the AC4 tests.

**Out:** A class for the unclassed refusals of score columns in `norm_pid5()`, `plot_pid5()` and `interval_*()` goes to a candidate row. Numeric types that pass the rule and still score wrong (`haven_labelled_spss` user-missing codes, `integer64`) go to a candidate row. The coercion of `calc_alpha()` and `calc_omega()` stays as their help page documents it, with no row.

## Acceptance criteria

- [x] AC1: The seven functions are `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, `reliability_pid5()`, `reliability_hitopsr()`, `reliability_hitopbr()` and `validity_pid5()`. An accepted item column is numeric or logical. A character column is also accepted when `as.numeric()` parses every value that is not blank and not `NA` after `trimws()`. Values such as `"Inf"` and `"0x1A"` parse, and they score as the same values in a numeric column do. If a column that a call scores is not accepted, each of the seven functions aborts with condition class `hitop_nonnumeric_items`. A call scores the columns in `items`. If `items` is omitted, it scores the columns in the module's `columns` (D-067(e)). Any `layout = "printed"` reorder comes first. Tests cover each function with five refused columns: a factor, an ordered factor, choice text, a Date and a list column.
- [x] AC2: The abort names the first five refused columns by their names in `data`, in the order of the caller's `items` (or the module's `columns`). Each named column shows its class. A character column also shows its first value that does not parse. If more than five columns are refused, the abort states how many more. An `i` line tells the caller to export numeric values rather than choice text. `conditionCall()` names the exported function.
- [x] AC3: The abort comes after the argument checks of `data`, `items`, `srange`, `prefix` and the flags. It comes before the output-column collision check, before the `srange` warning of `validity_pid5()`, and before item values are converted for scoring. A refused call raises no "NAs introduced by coercion" warning.
- [x] AC4: These accepted columns score as before: a double, an integer, a logical, a `haven::labelled()` double with one `NA` cell, and a character column of digit text with blank and `NA_character_` cells. The labelled column sits on a reverse-keyed item where the instrument version has one (PID-5 FULL, HiTOP-SR), and on any item otherwise. Tests feed each of them to each function. Each scoring result equals the result for the matching all-double data with those cells set to `NA`, and each reliability result equals the reliability of that data. The full `devtools::test()` suite passes with no existing expectation edited.
- [x] AC5: The `items` documentation of the seven functions states the accepted-column rule and that a choice-text export is refused. NEWS.md carries one entry that names the refusal, the seven functions and the class `hitop_nonnumeric_items`.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T2, T3, T4
- AC5 → T4

## Tasks

- [x] T1: In `R/util.R`, add a helper that applies the AC1 rule to a list of columns and aborts with class `hitop_nonnumeric_items` and the AC2 message. Its parse test muffles its own coercion warnings. Leave `validate_numeric_columns()` unchanged.
- [x] T2: Call the helper in `prep_items()` after `validate_range()` and before the `as.numeric()` at `R/util.R:505`. In `validity_pid5()`, call it after the checks at lines 77-93 and before the collision check at line 111. Pass `call` so the abort blames the exported function.
- [x] T3: Write the tests first. Probe axes: offender type (AC1 list), position (first item, last item, and a reverse-keyed item where the version has one), offender count (1, 2, 21), all three PID-5 versions, `items` omitted with module `columns`, and `layout = "printed"`. Add one probe per function that combines a refused column with an output collision and a non-default `srange`, to test the AC3 order. Capture warnings with `withCallingHandlers()` (LESSONS M032). Add the AC4 comparisons, with the haven case under `skip_if_not_installed("haven")`. The review evidence for AC4 must show that the haven case ran and did not skip.
- [x] T4: Update the `items` roxygen of the seven functions, run `devtools::document()`, and add the NEWS entry. Add haven to Suggests in `DESCRIPTION`. Append a D-entry that records `hitop_nonnumeric_items` as public on the terms of D-034(c), the value rule, and the check order. The same entry records haven in Suggests for tests only, with no use in `R/` (GP4).
- [x] T5: Run `devtools::test()` and `devtools::check()` clean.
- [x] T6: Fix review F1 and F2: `validate_item_columns()` reads each scored column by position, as `prep_items()` does, and uses names only in the message. Add regression tests that fail first: duplicated names with positional `items` and a choice-text duplicate, and an empty or `NA` column name with positional `items`.
- [x] T7: Fix review F6 and F7: list `hitop_nonnumeric_items` in the Errors sections of the seven functions, and show the factor hint only when a refused column is a factor.
- [x] T8: Fix review F8: test that `"Inf"` and `"0x1A"` score as `Inf` and `26`, and make the argument-order test assert which error each bad argument raises. Then run `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-09-22: created by /milestone-plan. Promotes the M108 review F3 candidate row.
- 2026-09-22: full criteria audit, run by an [O] reader in two passes. Pass one returned 9 findings. Four were fixed without a question: scored-column paths, the coercion-warning test, the D-entry moved to T4, and a candidate row for `haven_labelled_spss` and `integer64`. Three went to the gate, and two were no-finding. Pass two returned 5 wording fixes, all applied: `NA` strings, `Inf` and hex values, `items` order, "before conversion", and a combined-order probe.
- 2026-09-22: plan gate chose a value rule (digit text accepted) over a type rule like `norm_pid5()` because a Qualtrics CSV with its header rows removed leaves digit-text columns that score correctly today; falsified by a report of a character column that parses but scores wrong.
- 2026-09-22: plan gate chose to name the first five refused columns and a count over one bullet per column because a choice-text export refuses 220 or 405 columns; falsified by a caller who needs the full list to fix the export.
- 2026-09-22: plan gate chose to class only the new abort over also classing the three score-column refusals, to keep the D-entry and test surface small; falsified by a caller who needs to catch a score-column refusal by class.
- 2026-09-22: amendment (user-chosen at a mini gate): AC4's classed-double probe cannot pass without haven loaded, because the `vctrs_vctr` class sends `as.numeric()` to `vctrs::vec_cast()`, which aborts. haven goes to Suggests and AC4 tests a real `haven::labelled()` column. Scope In, T3 and T4 edited to match.
- 2026-09-22: re-audit: AC4 (full) — 4 findings: T4 lacked the Suggests edit and D-entry, "scores as before" was wider than its tests, the labelled probe named no position, and the skip clause bound the harness. All four applied.
- 2026-09-22: re-audit: AC4 (full) — 2 findings: SF, BF and HiTOP-BR have no reverse-keyed item, and reliability results have no cells. Both applied, and the user approved the final text (second line, so further AC4 churn goes to the user).
- 2026-09-22: question gate skipped, nothing open. T3 tests written first: 13 blocks red on the refusal, AC4 blocks green on the old code. T1 `validate_item_columns()` and `unparsed_value()` in `R/util.R`. T2 wired into `prep_items()` and `validity_pid5()`, and a `caller_items` argument threads the caller's vector through both engines and the two HiTOP-SR wrappers so the report follows the caller's order under `layout = "printed"`. A plant passing the permuted vector turned the printed-order test red (3 failures). haven added to Suggests. `devtools::test()`: 18629 passes, 0 failures, 15 skips.
- 2026-09-22: T4 done: `items` docs of the six scoring and reliability functions state the accepted-column rule (`validity_pid5()` inherits it), NEWS entry under Breaking changes, D-068 appended, `devtools::document()` run.
- 2026-09-22: claim audit: 30 claims read, 2 corrected — NEWS.md, R/util.R, the six `@param items` roxygen blocks and their man pages. The NEWS error sentence now says only a character column shows a value, and the docs now say the text `"NA"` is refused. The reader's probe also found the text `"NaN"` refused, against AC1's "parses", so `unparsed_value()` now accepts it, with a new test. The same reader re-read the corrections once and all held.
- 2026-09-22: T5 done. `devtools::test()`: 18632 passes, 0 failures, 15 skips. `devtools::check()` after the last code change: 0 errors, 0 warnings, 1 note for the untracked `qtest.txt`. Status set to review.
- 2026-09-22: review return 1 (defect): F1 fails AC1 and F2 fails AC4, both reproduced. The user chose at the gate to send the milestone back with F6, F7 and F8 fixed too. T6 to T8 added. Status set to in-progress.
- 2026-09-22: T6 done. `validate_item_columns()` reads columns by position and labels an empty or `NA` name as "Column <n>". Two new tests failed first (26 failures on duplicate names, and the `[.data.frame` error on an empty name) and now pass. `devtools::test()`: 18747 passes, 0 failures, 15 skips.
- 2026-09-22: T7 done. The Errors sections of `score_pid5()`, `score_hitopsr()`, `score_hitopbr()` and `validity_pid5()` name `hitop_nonnumeric_items`. The three reliability functions have no Errors section, so their `items` text stays the only mention. The factor tip shows only when a refused column is a factor, and a new test failed first. `devtools::test()`: 18751 passes, 0 failures, 15 skips.
- 2026-09-22: T8 done. New test: `"Inf"` and `"0x1A"` score as `Inf` and `26` in all 13 cases. The argument-order test now asserts the message of each bad argument. A plant that moved the refusal before the `srange` check turned it red (20 failures). `devtools::test()`: 18764 passes, 0 failures, 15 skips. `devtools::check()`: 0 errors, 0 warnings, 1 note for the untracked `qtest.txt`.
- 2026-09-22: claim audit: 19 claims read, 1 corrected — R/util.R (the comment above `validate_item_columns()` now names `validity_pid5()` as the second caller). This audit covered only lines added after c3da774e, because the first audit covered the rest. The same reader re-read the correction once, and it held. The change touches only the comment, so the T8 test and check results still apply. Status set to review.

## Decisions

## Review

The branch is up to date with `origin/main` (9b8d995c), so no merge was needed. `devtools::test()` gives 18617 passes, 0 failures, 0 errors and 15 skips. All 14 blocks of `test-nonnumeric-items.R` pass with 0 skips. The haven block ran 13 expectations. `devtools::check()` gives 0 errors, 0 warnings and 1 note for the untracked `qtest.txt`, which is not in the diff.

- AC1 (not ticked): the tests cover the five refused types in all seven functions, the three PID-5 versions, module `columns` and `layout = "printed"`. A probe showed `"Inf"` and `"0x1A"` score as `Inf` and `26` in `score_pid5()`, `validity_pid5()` and `score_hitopbr()`. Finding F1 fails it: with duplicated names and positional `items`, a refused column is scored.
- AC2: the message names the first five refused columns in `items` order with their classes. It shows the first bad value of a character column, counts the rest and carries the `i` line. `conditionCall()` names the exported function (blocks 1, 2, 5, 6 and 8, and a probe of seven refused columns).
- AC3: the refusal comes after the `data`, `items`, `srange`, `prefix` and flag checks. It comes before the collision check, the `srange` warning of `validity_pid5()` and conversion, with no coercion warning (blocks 9 to 11).
- AC4 (not ticked): the double, integer, logical, labelled and digit-text cases equal their all-double results in every function (blocks 12 and 13). No existing expectation was edited. Finding F2 fails it: a double column in data with an empty column name, scored by position, now stops with an unclassed error.
- AC5: the `items` docs of the seven functions state the rule and the choice-text refusal. `validity_pid5()` inherits the text. NEWS.md has one entry under Breaking changes that names the refusal, the seven functions and the class.

Consistency gate: `cairn_validate.py` exits 0 with 24 old advisory warnings. `devtools::document()` gives no diff. `pkgdown::check_pkgdown()` finds no problems. The README is untouched. No principle text changed, so no impact report runs.

Independent review ran three lenses. The blame-history lens found nothing. The prior-review lens found nothing, and no inline PR comments exist. The diff-bug lens reported these, most severe first:

- F1: the check turns positions into names. With duplicated names it reads the first column of that name, but `prep_items()` converts the column at the position. Choice text then scores with a coercion warning. Reproduced, and it fails AC1.
- F2: `data[names_scored]` fails on an empty or `NA` column name when `items` are positions. The error is unclassed and blames `[.data.frame`. The same call scored before this branch. Reproduced, and it fails AC4.
- F3: a `haven::labelled()` character column passes the check and then stops inside the cast of haven. It also failed before this branch.
- F4: the accepted spellings (`"inf"`, `"+Inf"`, `"-NaN"`, `"1."`) are wider than the docs say. The text `"TRUE"` is refused, but a logical column is accepted. This matches the AC1 wording.
- F5: the `calc_se = TRUE` deprecation warning still comes before a refusal. That conflicts with a comment in `validity_pid5()`, but it conforms to AC3.
- F6: the Errors sections of the seven functions do not list the new class, but they list the other public classes.
- F7: the `i` hint always names a factor of digits, even when no refused column is a factor.
- F8: no test compares `"Inf"` or `"0x1A"` scores. The argument-order test asserts only that the error is not this class. No test uses positional `items` with duplicated or empty names.

Triage at the gate (user-chosen, 2026-09-22): F1 and F2 fix now (T6), and they return the milestone to in-progress. F6 and F7 fix now (T7). F8 fix now (T8). F3 follow-up, added to the existing candidate row on numeric types that pass the rule. F4 rejected, because the accepted spellings are what AC1 states. F5 rejected, because AC3 forbids only the coercion warning. The `validity_pid5()` comment stays true for that function, which has no `calc_se`.

### Pass 2 (2026-09-22, after T6 to T8)

The branch is up to date with `origin/main` (9b8d995c), so no merge was needed. `devtools::test()` gives 18779 expectations, 0 failures, 0 errors and 15 skips. All 18 blocks of `test-nonnumeric-items.R` pass with 0 skips, and the haven block ran 13 expectations. `devtools::check()` gives 0 errors, 0 warnings and 1 note for the untracked `qtest.txt`, which is not in the diff.

- AC1: block 1 refuses the five types in all 13 function and version cases with the class. Blocks 3 to 5 cover positions and counts 1, 2 and 21. Blocks 10 and 11 cover module `columns` and `layout = "printed"`. Block 17 shows `"Inf"` and `"0x1A"` score as `Inf` and `26` in all 13 cases. Block 7 (F1: duplicated names, choice-text duplicate) passes, and a fresh probe gave `hitop_nonnumeric_items` naming `"x"` and `"Very true"`.
- AC2: blocks 1, 2, 5 and 6 pass. They assert the names in `items` order, each class, the first bad value, the count of the rest (`"16 more"`), the `i` line and `conditionCall()`. Block 9 shows the factor tip only for a factor.
- AC3: blocks 12 to 14 pass. Block 13 asserts the message of each bad argument, block 14 the collision and `srange` order, and block 12 no coercion warning.
- AC4: blocks 15 and 16 pass (double, integer, logical, digit text and haven), and the haven block ran 13 expectations with no skip. Block 8 (F2) passes: a double column with an empty or `NA` name scores by position. No existing expectation was edited on the branch (`git diff origin/main...HEAD -- tests/` touches only the new file).
- AC5: all seven `man/*.Rd` pages carry the rule, the choice-text refusal and the class. NEWS.md has one entry under Breaking changes that names the refusal, the seven functions and the class.

Consistency gate: `cairn_validate.py` passes with 24 old advisory warnings. `devtools::document()` gives no diff. `pkgdown::check_pkgdown()` finds no problems. The README is untouched. No principle text changed, so no impact report runs.

Independent review ran three new lenses. The blame-history lens found nothing, and it found that the diff matches D-068 and D-045(a). The prior-review lens found nothing. It found that this diff resolves M108 review F3, and it found no inline PR comments. The diff-bug lens found that F1, F2, F6, F7 and F8 are fixed, and it reported these, most severe first:

- G1: the comment at `R/validity_pid5.R:98-100` says a refused call "hears about its columns alone". But `warn_item_order()` runs first, in `validity_pid5()` and in `prep_items()`. Reproduced: with a factor column and item names in reverse order, `score_hitopbr()` gave the refusal and 1 order warning. AC3 counts that warning as part of the `items` checks, so AC3 holds.
- G2: pass 1 note F5 is wrong. `deprecate_calc_se()` runs after `prep_items()`, so a refused call with `calc_se = TRUE` gives no warning. Reproduced (0 warnings). The code is correct, and only the pass 1 note is wrong.
- G3: NEWS names "a factor, a date, a list, or character text", but the code also refuses complex, difftime, POSIXct and raw columns. Reproduced for complex. A complex column with a zero imaginary part and a difftime column scored as numbers before. The `items` docs say "Any other column", which is correct.
- G4: the argument-order test checks one flag per function. It does not check `omega`, `calc_se` or a bad `data`. The order is correct today.
- G5: `trimws()` does not strip a non-breaking space, so a cell that looks blank is refused. The message then shows a value that prints as blank. Not reproduced for a non-breaking space. The rule refuses such a cell as AC1 states, so only the message is unclear.
