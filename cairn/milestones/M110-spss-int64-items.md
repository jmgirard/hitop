# M110: Scoring refuses SPSS missing codes and 64-bit integers it misreads

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — it changes which item columns seven exported functions accept, and their error message
- **Branch/PR:** —

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

- [ ] AC1: For each of the seven functions, a `haven::labelled_spss()` item column that holds a declared-missing value is refused with class `hitop_nonnumeric_items`. The message names the column, shows the first declared-missing value in row order, and names `haven::zap_missing()`. The test covers a double column and a character column, a code outside `srange` and a code inside it, a code declared through `na_values` and one through a `na_range` with an infinite bound. It puts the column once first and once on a reverse-keyed item.
- [ ] AC2: For each of the seven functions, a `haven::labelled_spss()` item column that declares missing codes but holds none of them scores as its plain double copy does. The test calls with `append = FALSE` (and `omega = FALSE` for `reliability_*()`) and compares the two results with `expect_identical()`.
- [ ] AC3: For each of the seven functions, an item column of class `integer64` is refused with class `hitop_nonnumeric_items`, including a column that holds an `NA`. The message names the column and its class.
- [ ] AC4: For each of the seven functions, a `haven::labelled()` character item column whose values all parse scores as its plain character copy does. The test calls with `append = FALSE` (and `omega = FALSE` for `reliability_*()`) and compares with `expect_identical()`. A column that holds a value that does not parse is refused with class `hitop_nonnumeric_items`, and the message shows that value.
- [ ] AC5: The message shows a refused character value that has no `[[:graph:]]` character by its Unicode code points. The test covers `\v`, `U+00A0`, `U+2009`, `U+3000`, `U+FEFF` and a mix of `U+00A0` and `U+2009`, and asserts that each code point is in the message.
- [ ] AC6: A bad `omega` in the three `reliability_*()` functions and a bad `calc_se` in the three `score_*()` functions each abort with their own argument error and not `hitop_nonnumeric_items`, although an item column also fails the value rule. A `data` that is not a data frame aborts with its own argument error in all seven functions.
- [ ] AC7: `Rscript -e 'devtools::test()'` reports 0 failures. `Rscript -e 'devtools::check()'` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T3
- AC4 → T1, T4
- AC5 → T5
- AC6 → T6
- AC7 → T7

## Tasks

- [ ] T1: Write the tests for AC1 to AC4 in `tests/testthat/test-nonnumeric-items.R` with its seven-function case list. Build the `integer64` column by hand as `structure(<double>, class = "integer64")`. This is the shape `readRDS()` returns when bit64 is not loaded, so bit64 stays out of Suggests. Make sure that each new-behavior test fails for the reason its criterion names. AC2 and the refusal half of AC4 pass today, so they are regression guards.
- [ ] T2: In `unparsed_value()`, refuse a `haven_labelled_spss` column that holds a declared-missing value. Read the codes from the `na_values` and `na_range` attributes and call no haven function. Return the first such value. In `validate_item_columns()`, print that value with `{.val}` for any type, because the detail branch now prints only a character value. Add the `haven::zap_missing()` tip when such a column is refused.
- [ ] T3: Refuse an `integer64` column in `unparsed_value()` before the `is.numeric()` acceptance.
- [ ] T4: Convert a `haven_labelled` character column through `unclass()` before `as.numeric()` in `prep_items()` and in `validity_pid5()` (`R/validity_pid5.R:141`). Run the parse rule on its unclassed values.
- [ ] T5: Write the AC5 test first. Then show a refused value with no `[[:graph:]]` character by its code points.
- [ ] T6: Add the AC6 cases to the test "argument checks run before the refusal" (`test-nonnumeric-items.R:325`). The checks already run first (`R/score_engine.R:68`, `R/reliability_engine.R:55`), so this task adds tests only, unless one fails.
- [ ] T7: Append the D-entry that narrows D-068(a). It cites the new evidence: numeric-typed columns that score wrong. Update the `items` docs and the Errors sections, run `devtools::document()`, and extend the M109 NEWS entry. NEWS states that calls which returned wrong numbers now stop. Run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-22: created by /milestone-plan.
- 2026-09-22: full criteria audit ([O], fresh context) returned 9 findings. Eight were fixed in the draft (message branch for numeric values, SPSS probe axes, `append = FALSE`, regression-guard labels, `[[:graph:]]` predicate, AC6 argument map, `integer64` with `NA`, D-entry narrowing). Finding 3 (conversion aborts without haven) was rejected: a measured `readRDS()` column converted to `1 99` without haven loaded.
- 2026-09-22: plan gate chose to refuse an SPSS column that holds a declared-missing code over scoring the codes as `NA`, because the package does not change data without a message (D-068); falsified by users who expect SPSS missing codes to be honored without a step.
- 2026-09-22: plan gate chose to score a labelled digit-text column over refusing it, because plain digit text scores today; falsified by a labelled character column whose labels change what its digits mean.
- 2026-09-22: plan gate chose to show a whitespace-only value by code points over treating it as blank, because treating it as blank needs a second strip in the conversion step; falsified by a user export where such cells are routine.
- 2026-09-22: plan chose a hand-built `integer64` test column over adding bit64 to Suggests, because the refusal reads only the class; falsified by a refusal rule that needs bit64 values.
