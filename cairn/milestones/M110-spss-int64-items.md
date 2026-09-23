# M110: Scoring refuses SPSS missing codes and 64-bit integers it misreads

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — it changes which item columns seven exported functions accept, and their error message
- **Branch/PR:** m110-spss-int64-items

## Goal

The scoring functions abort and name an item column that holds SPSS user-missing codes or 64-bit integers, instead of scoring wrong numbers in silence.

## Scope

**In:** M109 added a value rule in `validate_item_columns()` and `unparsed_value()` (`R/util.R:339` and `:405`). This milestone adds two refusals and one acceptance to that rule. All use the existing class `hitop_nonnumeric_items` and cover the seven functions D-068(a) names.

1. A `haven_labelled_spss` column that holds a value its `na_values` or `na_range` attribute declares missing is refused.
2. An `integer64` column is refused.
3. A `haven_labelled` character column whose values all parse is converted as plain text. Today it stops inside the haven cast.

The milestone also closes two M109 review gaps. A refused value with no `[[:graph:]]` character is shown by its code points. The argument-order test covers `omega`, `calc_se` and a bad `data`. A new D-entry narrows the accepted-type rule of D-068(a). The `items` docs, the Errors sections and NEWS change to match.

**Out:** A condition class for the score-column refusals of `norm_pid5()`, `plot_pid5()` and `interval_*()` stays a candidate row. Its promotion condition (a caller who needs to catch one by class) did not occur. The package does not score declared missing codes as `NA` by itself (see the work log). The caller converts with `haven::zap_missing()`. No dependency is added, so bit64 and callr stay out of `DESCRIPTION`.

## Acceptance criteria

- [x] AC1: For each of the seven functions, a `haven::labelled_spss()` item column that holds a declared-missing value is refused with class `hitop_nonnumeric_items`. The message names the column, shows the first declared-missing value in row order, and names `haven::zap_missing()`. The test covers a double column and a character column, a code outside `srange` and a code inside it, a code declared through `na_values` and one through a `na_range` with an infinite bound. It puts the column once first and once on a reverse-keyed item.
- [x] AC2: For each of the seven functions, a `haven::labelled_spss()` item column that declares missing codes but holds none of them scores as its plain double copy does. The test calls with `append = FALSE` (and `omega = FALSE` for `reliability_*()`) and compares the two results with `expect_identical()`.
- [x] AC3: For each of the seven functions, an item column of class `integer64` is refused with class `hitop_nonnumeric_items`, including a column that holds an `NA`. The message names the column and its class.
- [x] AC4: For each of the seven functions, a `haven::labelled()` character item column whose values all parse scores as its plain character copy does. The test calls with `append = FALSE` (and `omega = FALSE` for `reliability_*()`) and compares with `expect_identical()`. A column that holds a value that does not parse is refused with class `hitop_nonnumeric_items`, and the message shows that value.
- [x] AC5: The message shows a refused character value made only of Unicode separator (`\p{Z}`), control (`\p{Cc}`) and format (`\p{Cf}`) characters by its Unicode code points. The test covers `\v`, `U+00A0`, `U+2009`, `U+3000`, `U+FEFF`, `U+200B`, a mix of `U+00A0` and `U+2009`, and a mix of `U+00A0` and `\v`, and asserts that each code point is in the message. It also asserts that the refused values `U+00A0` followed by `x` and `1` followed by `U+00A0` are shown as text and not as code points.
- [x] AC6: A bad `omega` in the three `reliability_*()` functions and a bad `calc_se` in the three `score_*()` functions each abort with their own argument error and not `hitop_nonnumeric_items`, although an item column also fails the value rule. A `data` that is not a data frame aborts with its own argument error in all seven functions.
- [x] AC7: `Rscript -e 'devtools::test()'` reports 0 failures. `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [x] T1: Write the tests for AC1 to AC4 in `tests/testthat/test-nonnumeric-items.R` with its seven-function case list. Build the `integer64` column by hand as `structure(<double>, class = "integer64")`. This is the shape `readRDS()` returns when bit64 is not loaded, so bit64 stays out of Suggests. Make sure that each new-behavior test fails for the reason its criterion names. AC2 passes today, so it is a regression guard.
- [x] T2: In `unparsed_value()`, refuse a `haven_labelled_spss` column that holds a declared-missing value. Read the codes from the `na_values` and `na_range` attributes and call no haven function. Return the first such value. In `validate_item_columns()`, print that value with `{.val}` for any type, because the detail branch now prints only a character value. Add the `haven::zap_missing()` tip when such a column is refused.
- [x] T3: Refuse an `integer64` column in `unparsed_value()` before the `is.numeric()` acceptance.
- [x] T4: Convert a `haven_labelled` character column through `unclass()` before `as.numeric()` in `prep_items()` and in `validity_pid5()` (`R/validity_pid5.R:141`). Run the parse rule on its unclassed values.
- [x] T5: Write the AC5 test first. Then show a refused value made only of `\p{Z}`, `\p{Cc}` and `\p{Cf}` characters by its code points.
- [x] T6: Add the AC6 cases to the test "argument checks run before the refusal" (`test-nonnumeric-items.R:325`). The checks already run first (`R/score_engine.R:68`, `R/reliability_engine.R:55`), so this task adds tests only, unless one fails.
- [x] T7: Append the D-entry that narrows D-068(a). It cites the new evidence: numeric-typed columns that score wrong. Update the `items` docs and the Errors sections, run `devtools::document()`, and extend the M109 NEWS entry. NEWS states that calls which returned wrong numbers now stop. Run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-22: created by /milestone-plan.
- 2026-09-22: full criteria audit ([O], fresh context) returned 9 findings. Eight were fixed in the draft (message branch for numeric values, SPSS probe axes, `append = FALSE`, regression-guard labels, `[[:graph:]]` predicate, AC6 argument map, `integer64` with `NA`, D-entry narrowing). Finding 3 (conversion aborts without haven) was rejected: a measured `readRDS()` column converted to `1 99` without haven loaded.
- 2026-09-22: plan gate chose to refuse an SPSS column that holds a declared-missing code over scoring the codes as `NA`, because the package does not change data without a message (D-068); falsified by users who expect SPSS missing codes to be honored without a step.
- 2026-09-22: plan gate chose to score a labelled digit-text column over refusing it, because plain digit text scores today; falsified by a labelled character column whose labels change what its digits mean.
- 2026-09-22: plan gate chose to show a whitespace-only value by code points over treating it as blank, because treating it as blank needs a second strip in the conversion step; falsified by a user export where such cells are routine.
- 2026-09-22: plan chose a hand-built `integer64` test column over adding bit64 to Suggests, because the refusal reads only the class; falsified by a refusal rule that needs bit64 values.
- 2026-09-22: T1 minor edit: the refusal half of AC4 does not pass today. `unparsed_value()` aborts with haven's `vctrs_error_cast` when it calls `as.numeric()` on labelled values, so the test is red and T4 fixes it. T1's regression-guard sentence now names AC2 only.
- 2026-09-22: T1 done. Five test blocks added. Red before the fix: SPSS refusal 72 of 72 (a 99 code scored `pid_disinhibition` 21.6), `integer64` 13 of 13, labelled choice text 13 of 13 (haven cast error), labelled digit text errors (haven cast error). The AC2 guard passes (26 of 26).
- 2026-09-22: T2-T4 done in one commit. `unparsed_value()` now returns a reason with a kind (text, missing, type). The new helpers `declared_missing()`, `item_values()` and `item_numbers()` are in `R/util.R`, and `item_numbers()` replaces `as.numeric()` in `prep_items()` and `validity_pid5()`. All haven classes are unclassed, not only character ones. The test file is 0 failures. Full `devtools::test()` is 0 failures.
- 2026-09-22: amendment gate: AC5's `[[:graph:]]` predicate depends on the regex engine (TRE counts U+00A0 and U+FEFF as graph, PCRE UCP counts U+FEFF and U+200B), so the criterion contradicted its own U+FEFF probe. Jeff adopted the Unicode-category wording (`\p{Z}`, `\p{Cc}`, `\p{Cf}`) over also marking invisible characters inside a visible value. That wider option went to a candidate row. T5's wording follows.
- re-audit: AC5 (full) — two probe gaps (no mix across categories, no trailing invisible character after a visible one). Both were folded into the adopted text.
- 2026-09-22: T5 done. The AC5 test was red at 130 of 312 and is green after the fix. `is_invisible()` and `code_points()` were added to `R/util.R`. A first build rendered "character U+00A0 and U+2009" in the singular (the M030 cli quantity lesson), so `cli::qty()` now sits just before the plural marker.
- 2026-09-22: T6 done (tests only). The argument-order test grew from 94 to 140 expectations and passes. Removing `validate_flag(omega)` from `R/reliability_engine.R` as a plant turned it red (10 failures), and the file was restored.
- claim audit: 33 claims read, 3 corrected — R/util.R (two comments: the value is shown after `trimws()`, and a `\v` prints as an escape, not blank), tests/testthat/test-nonnumeric-items.R (the `integer64` stand-in holds real bit patterns only at -0).
- 2026-09-22: T7 done. The `items` text changed in the six R/ sources (`validity_pid5()` inherits it), and the Errors sections point to `items`, so they needed no edit. NEWS extended, D-069 appended. `devtools::test()` 0 failures. `devtools::check()` 0 errors, 0 warnings, 1 note (the untracked `qtest.txt`). The test file was rerun after the comment-only audit fixes: 0 failures. Status set to review.
- 2026-09-23: review fixed F1 and F3 at the gate (details in the Review section).
- 2026-09-23: step-7 approval: m110-spss-int64-items approved for merge

## Review

Sync: on 2026-09-22 the branch contained `origin/main` (ffba8a24). No merge was needed. No PR exists yet.

Evidence: on 2026-09-22, `testthat::test_file("tests/testthat/test-nonnumeric-items.R")` ran 24 blocks with 0 failures.

- AC1: "an SPSS column holding a declared-missing code is refused" passes with 288 expectations. It loops the seven functions, with the PID-5 at three versions. It covers four variants: double `na_values` outside `srange`, double `na_values` inside it, double `na_range = c(90, Inf)`, and character `na_values`. It puts the column first and on the first reverse-keyed item where one exists. It asserts the class, the column name, `holds 99,` and `haven::zap_missing()`. Row 2 holds 99 and row 4 holds 98, so a message that showed the lowest code fails.
- AC2: "an SPSS column declaring codes it does not hold scores as its double does" passes with 26 expectations. It compares `na_values` and `na_range` columns with the double copy through `expect_identical()`. `run_case()` sets `append = FALSE` and `omega = FALSE`.
- AC3: "an integer64 column is refused" passes with 39 expectations. The column starts with -0, the bit64 `NA` pattern. The test asserts the class, the column name and `integer64`.
- AC4: "a haven::labelled() digit-text column scores as its plain text does" passes with 13 `expect_identical()` expectations. "a haven::labelled() choice-text column is refused and shows the value" passes with 26 expectations on the class and `"Moderately"`.
- AC5: "a refused value made only of invisible characters is shown by its code points" passes with 312 expectations. It covers the eight listed values and asserts each code point. The values `U+00A0` + `x` and `1` + `U+00A0` show as text (`x"`, `"1`) with no `U+00A0`.
- AC6: "argument checks run before the refusal" passes with 140 expectations. An item column holds choice text in each call. A bad `omega` (three `reliability_*()`), a bad `calc_se` (three `score_*()`) and a list `data` (all seven) each raise their own message. None raises `hitop_nonnumeric_items`.
- AC7: `devtools::test()` ran 19,529 expectations with 0 failures, 0 errors and 15 skips. `devtools::check()` reported 0 errors, 0 warnings and 1 note. The note names the untracked `qtest.txt`, which is not part of the branch.

Consistency gate: `cairn_validate.py` passed with exit 0. Its 24 advisory warnings were there before this branch. `devtools::document()` made no diff. `pkgdown::check_pkgdown()` found no problems. The branch does not touch `README.Rmd` or `README.md`. NEWS.md extends the M109 entry and names no milestone. No DESIGN.md principle changed, so `cairn_impact.py` was not run.

Independent review: three fresh reviewers read the diff. The history reviewer and the prior-review reviewer found nothing. The prior-review probe found no PR review comments. The diff reviewer found seven items, ranked below. Dispositions are set at the merge gate.

- F1: A declared-missing code made only of invisible characters prints as blank, for example `holds " "`. The code-point display covers only the text kind.
- F2: A character SPSS column that declares `""` missing is refused, although blank cells already score as `NA`.
- F3: An `integer64` or SPSS refusal also gets the choice-text hint. An `integer64` refusal gets no tip on how to convert it.
- F4: Columns saved under haven before 2.0 carry the class `labelled_spss`, which the check does not catch. Their codes score as answers.
- F5: A hand-set `na_values` attribute of the wrong type can differ from haven's own `is.na()`. haven's constructors prevent this input.
- F6: A character column with invalid UTF-8, such as a latin1 `"\xa0"`, stops with a base R error instead of the refusal. This code came from M109.
- F7: The SPSS tip is chosen over all refused columns. If none of the five named columns holds a code, the tip can still show.

Triage at the gate (2026-09-23, Jeff):

- F1: fixed now. A declared-missing code made only of invisible characters is shown by its code points. The test "an invisible declared-missing code is shown by its code points" failed 13 of 26 before the fix and passes after it.
- F3: fixed now. `unparsed_value()` returns the kind `integer64`. The choice-text tip shows only for text and other refused types. A new tip names `as.numeric()` after `library(bit64)`. A saved `integer64` read back without bit64 converted to about 4.9e-324 and, after `library(bit64)`, to 1, 4, NA. The test "each refusal gets only the tips that fit its columns" failed 65 of 195 before the fix and passes after it.
- F2, F4, F6: follow-up. One candidate row on the roadmap holds all three.
- F5: rejected, because haven's constructors cannot make a missing-code attribute of the wrong type.
- F7: rejected, because the tip is still true of the refused set.

After the fixes, the test file ran 26 blocks and 1,826 expectations with 0 failures. `devtools::test()` ran 19,750 expectations with 0 failures, 0 errors and 15 skips. `devtools::document()` made no diff. `devtools::check()` on the fixed code reported 0 errors, 0 warnings and 1 note, which names the untracked `qtest.txt`.
