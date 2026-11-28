# M085: The `rename_*`/`label_*` family's warning contract

**Status:** done (2026-09-02, PR #92 https://github.com/jmgirard/hitop/pull/92)

**Goal:** Every warning the two `rename_*()` and three `label_*()` helpers raise carries a condition
class a caller can catch, with the family's unpinned edge paths pinned by class rather than prose.

**Outcome:** The eight nothing-matched reports raise `hitop_no_columns_matched` (both rename helpers'
primary methods; each `label_*()` under `target = "items"` and `"scales"`); both "only N of M items
renamed" reports raise `hitop_incomplete_rename`; `rename_hitopsr_items(method = "text")` reports
unmatched item text through `warn_unmatched_items()`, adopting `hitop_unmatched_items` and escaping
braces, so an `item_text` holding `{...}` warns instead of erroring in cli. `rename_pid5_items(method
= "number")` reads item numbers through `item_col_numbers()`, so a past-integer-range column leaks no
base-R coercion warning and is still named unmatched. New `tests/testthat/test-warning-classes.R` pins
each path by class and guards the family by walking `body()` of the five exported functions, failing
any `warning()`/`rlang::warn()`/`cli_warn()` whose `class` is not a literal `hitop_*`. The five
`@return` sections name the classes raised; NEWS and D-059 record the contract.

**Decisions:** D-059, superseding D-057's classless-completeness clause.

**Review:** Three lenses; nine findings, all from the diff-bug lens. Fixed at the gate: the guard
skipping under `R CMD check`, its `cli_warn`-only domain, its present-not-usable `class` check, its
count-equality assertion, and a NEWS sentence claiming no message changed. Deferred to a candidate row:
the class's reach over each rename helper's `method = "text"` path, and two test-reach gaps. One
rejected as plan-called-for. Nothing graduated or retired.
