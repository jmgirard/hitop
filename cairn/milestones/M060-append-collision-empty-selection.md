# M060: The scoring and conversion family refuses two argument shapes it lets fall through

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** —

## Goal

Every exported function that appends columns to `data` refuses an output-column
collision, and every function taking a variable-length column selection refuses
an empty one, each with a classed condition naming what the caller wrote.

## Scope

Surface tier: **user-facing** — the deliverable is the condition an exported
function signals to a caller, and the help text describing it.

**In:** Two shared validators in `R/util.R`, wired into the five sites that
build appended output (`R/score_engine.R:135`, `R/validity_pid5.R:233`,
`R/rank_scales.R:135`, `R/norm_pid5.R:335`, `R/interval_engine.R:144`), covering
the seven exports carrying an `append` formal. Two new public condition classes.
Tests, NEWS, roxygen `@details` **Errors** prose, and D-045, recording the
contract and the two public classes (the D-034(c) requirement for a new exported
condition, as D-044 met it). Promotes the `data-raw`-adjacent candidate row
lineaged to M041's review findings 3 and 4, absorbed whole.

**Out:** Any change to a value a succeeding call returns — no arithmetic is
touched. The `_se`-beside-`NA` divergence between `score_pid5()` and the SR/BR
scorers → its own candidate row. Format-rule checks on `id_prefix`/`form_name`
→ its own candidate row. Whether `append = FALSE` should return zero columns or
zero rows on a selection that is legal but matches nothing → not reachable once
an empty selection aborts.

## Acceptance criteria

- [ ] **AC1.** Every exported function of this package whose `formals()` carry an
      `append` argument aborts, before constructing any output column, when
      `append = TRUE` and `data` already holds a column that call would produce.
      The condition is classed `hitop_append_collision` and its message names
      every colliding column and no other. The domain is enumerated by a test
      reading `getNamespaceExports("hitop")` and keeping the exports whose
      `formals()` carry `append`. Probes vary the collision's form as well as its
      location: for each enumerated export, one colliding column and several; and
      on the exports that emit them, a `_se` column and a validity abbreviation
      (`pid_PNA`) as well as a scale column.
- [ ] **AC2.** `norm_pid5()` and `interval_hitopsr()` on a zero-length `scores`,
      and `rank_scales()` on a zero-length `scales`, each abort with a condition
      classed `hitop_empty_selection` whose message names that argument. For a
      call combining the empty selection with any other invalid argument of the
      selection family — `top`, `srange`, `prefix`, `level`, `append` — the
      signalled condition is `hitop_empty_selection`, so the reported cause is
      the empty selection and not a consequence of it (today `rank_scales()`
      reports `top` out of range, "between 1 and 0"). `data` is exempt: an
      invalid `data` is still reported first.
- [ ] **AC3.** No returned value and no accept/reject verdict changes for the
      calls a characterization harness enumerates: one call per export the AC1
      sweep names — the four scorers on their instrument's `sim_*` dataset, the
      three conversion functions on that dataset's `score_*()` output — each
      recording the returned object and any signalled condition together, and
      each compared against the same call run against the merge base.
- [ ] **AC4.** Both refusals are documented where a caller reads them. `NEWS.md`
      records both under the current version; the rendered Rd of every export the
      AC1 sweep names states the collision refusal in its **Errors** details, and
      the Rd of the three AC2 exports states the empty-selection refusal. Each
      export's name is resolved to its Rd through the package's alias index, and
      an unresolved name fails rather than dropping out of the domain.
- [ ] **AC5.** `Rscript -e 'devtools::document()'` produces no diff and
      `Rscript -e 'devtools::test()'` is clean.

## Coverage

- AC1 → T2, T4
- AC2 → T3, T5
- AC3 → T1, T6
- AC4 → T7
- AC5 → T8

## Tasks

- [ ] **T1.** Build the characterization harness under `data-raw/` or
      `tests/testthat/helper-*`: enumerate the seven exports by the AC1 sweep,
      call each as AC3 specifies, record value and condition together per call
      (M031/M037 — a value-only probe is blind to which inputs are accepted), and
      capture the merge-base baseline via `git archive` to a temp dir. Assert the
      enumerated call list is non-empty before comparing.
- [ ] **T2.** Write the failing tests for AC1 first: a `getNamespaceExports()`
      sweep filtered on an `append` formal, with `expect_true(length(x) > 0)` on
      the enumerated set (the `test-export-arg-guards.R` pattern, guarding the
      silently-emptying domain), asserting condition class and the exact set of
      named columns per probe. Confirm red on all seven before T4.
- [ ] **T3.** Write the failing tests for AC2: class, argument named, and the
      precedence cases over `top`/`srange`/`prefix`/`level`/`append`, plus the
      `data`-first exemption. Confirm red.
- [ ] **T4.** Add `validate_no_output_collision()` to `R/util.R` following the
      family's `arg`/`call` convention (`cli_assert()`'s `caller_env()` default —
      M043/M054/M057: resolve the value into a local before any enclosing call so
      `conditionCall()` is not `NULL`), and call it at the five append sites.
- [ ] **T5.** Add `validate_nonempty_selection()` and call it in `norm_pid5()`,
      `interval_engine()` and `rank_scales()` at the position AC2's precedence
      requires — after `validate_data()`, before the rest.
- [ ] **T6.** Run T1's harness against the branch; record the comparison.
- [ ] **T7.** Roxygen **Errors** prose on the affected exports, `NEWS.md`, and the
      AC4 Rd test — assert both cut boundaries of the Errors passage are found and
      exclude the anchor from the passage (M041/M046: a guard that cuts what it
      asserts over widens to the whole file when the anchor misses, and this
      package's Rd files carry Errors in two forms, `**Errors.**` inline and
      `\section{Errors}`).
- [ ] **T8.** `devtools::document()` + `devtools::test()` clean; confirm the
      shipped behavior matches D-045(a)-(d) as written at the plan gate.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader, over the step-2 draft at final wording. Returned findings on all four drafted criteria: AC1 unguarded empty sweep domain, an unobservable "instead of reaching cbind()" clause, and no probe variation over collision form; AC2's "ahead of every other validator" binding implementation ordering and, read literally, preceding `validate_data()`; AC3 unsatisfiable for the three conversion functions at defaults, its universal a proxy for its 7-call procedure, and value-only where the change is about acceptance; AC4 promising both refusals while checking one, naming no alias→Rd resolution, and cutting an Errors passage that exists in two forms. Ten disposed as clear repairs and folded into the wording above; the AC3 narrow-vs-grid finding went to the gate as a question.
- 2026-08-28: plan gate chose aborting on an empty selection over returning `data` unchanged, because a mistyped prefix that matches nothing would otherwise return silently unconverted data (GP2/GP3, and M029's stated goal on this same function); falsified by a report of a legitimate pipeline whose column filter matches none.
- 2026-08-28: plan gate chose aborting on an output-column collision over overwriting (with or without a warning), because an overwrite destroys a same-named column that need not have come from this package; falsified by a report that re-running a call is common enough that a two-step re-run is the worse cost.
- 2026-08-28: plan gate chose all seven `append`-carrying exports over the two the candidate row named, because the collision reproduces on all seven from one shared check at five sites; falsified by the shared validator proving not to fit a site's output shape.
- 2026-08-28: plan gate chose narrowing AC3's promise to its enumerated calls over an argument grid over `append`/`missing`/`calc_se`/`srange`/`version`, because the change adds refusals and touches no arithmetic; falsified by a value difference the existing suite catches that the 7-call harness does not.

## Decisions

## Review
