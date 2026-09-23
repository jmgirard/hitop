# M109: Scoring refuses an item column it cannot read as numbers

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — changes what seven exported functions accept
- **Branch/PR:** —

## Goal

If a researcher scores an export of choice text or factor codes, the call aborts and names the bad columns. It does not return all-`NA` or wrong scores.

## Scope

**In:** One value rule for item columns. `prep_items()` (`R/util.R:505`) and `validity_pid5()` (`R/validity_pid5.R:133`) check it. The rule covers `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, `reliability_pid5()`, `reliability_hitopsr()`, `reliability_hitopbr()` and `validity_pid5()`. The abort has the new public class `hitop_nonnumeric_items`. The milestone also updates the `items` documentation and NEWS, and adds a D-entry for the class.

**Out:** A class for the unclassed refusals of score columns in `norm_pid5()`, `plot_pid5()` and `interval_*()` goes to a candidate row. Numeric types that pass the rule and still score wrong (`haven_labelled_spss` user-missing codes, `integer64`) go to a candidate row. The coercion of `calc_alpha()` and `calc_omega()` stays as their help page documents it, with no row.

## Acceptance criteria

- [ ] AC1: The seven functions are `score_pid5()`, `score_hitopsr()`, `score_hitopbr()`, `reliability_pid5()`, `reliability_hitopsr()`, `reliability_hitopbr()` and `validity_pid5()`. An accepted item column is numeric or logical. A character column is also accepted when `as.numeric()` parses every value that is not blank and not `NA` after `trimws()`. Values such as `"Inf"` and `"0x1A"` parse, and they score as the same values in a numeric column do. If a column that a call scores is not accepted, each of the seven functions aborts with condition class `hitop_nonnumeric_items`. A call scores the columns in `items`. If `items` is omitted, it scores the columns in the module's `columns` (D-067(e)). Any `layout = "printed"` reorder comes first. Tests cover each function with five refused columns: a factor, an ordered factor, choice text, a Date and a list column.
- [ ] AC2: The abort names the first five refused columns by their names in `data`, in the order of the caller's `items` (or the module's `columns`). Each named column shows its class. A character column also shows its first value that does not parse. If more than five columns are refused, the abort states how many more. An `i` line tells the caller to export numeric values rather than choice text. `conditionCall()` names the exported function.
- [ ] AC3: The abort comes after the argument checks of `data`, `items`, `srange`, `prefix` and the flags. It comes before the output-column collision check, before the `srange` warning of `validity_pid5()`, and before item values are converted for scoring. A refused call raises no "NAs introduced by coercion" warning.
- [ ] AC4: An accepted column scores as before. Tests feed each function a double, an integer, a logical, and a classed double (the class vector that `haven::labelled()` returns, built without haven). They also feed a character column of digit text with blank and `NA_character_` cells. Each result equals the result for the matching all-double data, with `NA` where a cell was blank or `NA`. The full `devtools::test()` suite passes with no existing expectation edited.
- [ ] AC5: The `items` documentation of the seven functions states the accepted-column rule and that a choice-text export is refused. NEWS.md carries one entry that names the refusal, the seven functions and the class `hitop_nonnumeric_items`.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1, T3
- AC3 → T2, T3
- AC4 → T2, T3
- AC5 → T4

## Tasks

- [ ] T1: In `R/util.R`, add a helper that applies the AC1 rule to a list of columns and aborts with class `hitop_nonnumeric_items` and the AC2 message. Its parse test muffles its own coercion warnings. Leave `validate_numeric_columns()` unchanged.
- [ ] T2: Call the helper in `prep_items()` after `validate_range()` and before the `as.numeric()` at `R/util.R:505`. In `validity_pid5()`, call it after the checks at lines 77-93 and before the collision check at line 111. Pass `call` so the abort blames the exported function.
- [ ] T3: Write the tests first. Probe axes: offender type (AC1 list), position (first item, last item, a reverse-keyed item), offender count (1, 2, 21), all three PID-5 versions, `items` omitted with module `columns`, and `layout = "printed"`. Add one probe per function that combines a refused column with an output collision and a non-default `srange`, to test the AC3 order. Capture warnings with `withCallingHandlers()` (LESSONS M032). Add the AC4 comparisons.
- [ ] T4: Update the `items` roxygen of the seven functions, run `devtools::document()`, and add the NEWS entry. Append a D-entry that records `hitop_nonnumeric_items` as public on the terms of D-034(c), the value rule, and the check order.
- [ ] T5: Run `devtools::test()` and `devtools::check()` clean.

## Work log

- 2026-09-22: created by /milestone-plan. Promotes the M108 review F3 candidate row.
- 2026-09-22: full criteria audit, run by an [O] reader in two passes. Pass one returned 9 findings. Four were fixed without a question: scored-column paths, the coercion-warning test, the D-entry moved to T4, and a candidate row for `haven_labelled_spss` and `integer64`. Three went to the gate, and two were no-finding. Pass two returned 5 wording fixes, all applied: `NA` strings, `Inf` and hex values, `items` order, "before conversion", and a combined-order probe.
- 2026-09-22: plan gate chose a value rule (digit text accepted) over a type rule like `norm_pid5()` because a Qualtrics CSV with its header rows removed leaves digit-text columns that score correctly today; falsified by a report of a character column that parses but scores wrong.
- 2026-09-22: plan gate chose to name the first five refused columns and a count over one bullet per column because a choice-text export refuses 220 or 405 columns; falsified by a caller who needs the full list to fix the export.
- 2026-09-22: plan gate chose to class only the new abort over also classing the three score-column refusals, to keep the D-entry and test surface small; falsified by a caller who needs to catch a score-column refusal by class.

## Decisions

## Review
