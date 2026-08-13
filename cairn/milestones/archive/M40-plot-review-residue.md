# M40: Retire the M38/M39 plot-review residue

**Status:** done (2026-08-13, PR #44 https://github.com/jmgirard/hitop/pull/44)

**Goal:** Clear the four mechanical follow-ups the M38 and M39 reviews left on
`plot_pid5()` and its tests.

**Outcome:** `plot_pid5()`'s score axis takes the label-side 12% expansion only
when `show_labels`, 3% both ends otherwise. `validate_numeric_columns()` in
`R/util.R` carries the non-numeric-column predicate and per-column class bullets
for both `norm_pid5()` and `plot_pid5()`, each supplying its own headline and
closing line as functions of the offending count. `test-plot_pid5.R` asserts over
built layer data only, via `built_profile()` and `panel_names()`, with two
AST-walk self-checks over the file enforcing that and `info =` on in-loop
expectations.

**Decisions:** M40-D1 drawn order over internal row order; M40-D2 the self-check
discriminates by syntactic shape, not dataflow; M40-D3 callers supply headline
and closing line as functions of the count. None cross-cutting.

**Review:** Two passes. Pass 1 returned it (84/AC2, 84/AC3, 87/AC4; 21 logged
under 80). Pass 2 actioned B6 (82, internal `panel`-column coupling) in-pass; 15
logged under 80, A1 (74) disclosed at the gate. Other two lenses: zero findings.
