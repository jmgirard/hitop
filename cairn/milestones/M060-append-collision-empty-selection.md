# M060: The scoring and conversion family refuses two argument shapes it lets fall through

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m060-append-collision-empty-selection` / https://github.com/jmgirard/hitop/pull/67

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
- [x] **AC2.** `norm_pid5()` and `interval_hitopsr()` on a zero-length `scores`,
      and `rank_scales()` on a zero-length `scales`, each abort with a condition
      classed `hitop_empty_selection` whose message names that argument. For a
      call combining the empty selection with any other invalid argument of the
      selection family — `top`, `srange`, `prefix`, `level`, `append` — the
      signalled condition is `hitop_empty_selection`, so the reported cause is
      the empty selection and not a consequence of it (today `rank_scales()`
      reports `top` out of range, "between 1 and 0"). `data` is exempt: an
      invalid `data` is still reported first.
- [x] **AC3.** No returned value and no accept/reject verdict changes for the
      calls a characterization harness enumerates: one call per export the AC1
      sweep names — the four scorers on their instrument's `sim_*` dataset, the
      three conversion functions on that dataset's `score_*()` output — each
      recording the returned object and any signalled condition together, and
      each compared against the same call run against the merge base.
- [x] **AC4.** Both refusals are documented where a caller reads them. `NEWS.md`
      records both under the current version; the rendered Rd of every export the
      AC1 sweep names states the collision refusal in its **Errors** details, and
      the Rd of the three AC2 exports states the empty-selection refusal. Each
      export's name is resolved to its Rd through the package's alias index, and
      an unresolved name fails rather than dropping out of the domain.
- [x] **AC5.** `Rscript -e 'devtools::document()'` produces no diff and
      `Rscript -e 'devtools::test()'` is clean.

## Coverage

- AC1 → T2, T4
- AC2 → T3, T5
- AC3 → T1, T6
- AC4 → T7
- AC5 → T8

## Tasks

- [x] **T1.** Build the characterization harness under `data-raw/` or
      `tests/testthat/helper-*`: enumerate the seven exports by the AC1 sweep,
      call each as AC3 specifies, record value and condition together per call
      (M031/M037 — a value-only probe is blind to which inputs are accepted), and
      capture the merge-base baseline via `git archive` to a temp dir. Assert the
      enumerated call list is non-empty before comparing.
- [x] **T2.** Write the failing tests for AC1 first: a `getNamespaceExports()`
      sweep filtered on an `append` formal, with `expect_true(length(x) > 0)` on
      the enumerated set (the `test-export-arg-guards.R` pattern, guarding the
      silently-emptying domain), asserting condition class and the exact set of
      named columns per probe. Confirm red on all seven before T4.
- [x] **T3.** Write the failing tests for AC2: class, argument named, and the
      precedence cases over `top`/`srange`/`prefix`/`level`/`append`, plus the
      `data`-first exemption. Confirm red.
- [x] **T4.** Add `validate_no_output_collision()` to `R/util.R` following the
      family's `arg`/`call` convention (`cli_assert()`'s `caller_env()` default —
      M043/M054/M057: resolve the value into a local before any enclosing call so
      `conditionCall()` is not `NULL`), and call it at the five append sites.
- [x] **T5.** Add `validate_nonempty_selection()` and call it in `norm_pid5()`,
      `interval_engine()` and `rank_scales()` at the position AC2's precedence
      requires — after `validate_data()`, before the rest.
- [x] **T6.** Run T1's harness against the branch; record the comparison.
- [x] **T7.** Roxygen **Errors** prose on the affected exports, `NEWS.md`, and the
      AC4 Rd test — assert both cut boundaries of the Errors passage are found and
      exclude the anchor from the passage (M041/M046: a guard that cuts what it
      asserts over widens to the whole file when the anchor misses, and this
      package's Rd files carry Errors in two forms, `**Errors.**` inline and
      `\section{Errors}`).
- [x] **T8.** `devtools::document()` + `devtools::test()` clean; confirm the
      shipped behavior matches D-045(a)-(d) as written at the plan gate.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier), fresh-context [O] reader, over the step-2 draft at final wording. Returned findings on all four drafted criteria: AC1 unguarded empty sweep domain, an unobservable "instead of reaching cbind()" clause, and no probe variation over collision form; AC2's "ahead of every other validator" binding implementation ordering and, read literally, preceding `validate_data()`; AC3 unsatisfiable for the three conversion functions at defaults, its universal a proxy for its 7-call procedure, and value-only where the change is about acceptance; AC4 promising both refusals while checking one, naming no alias→Rd resolution, and cutting an Errors passage that exists in two forms. Ten disposed as clear repairs and folded into the wording above; the AC3 narrow-vs-grid finding went to the gate as a question.
- 2026-08-28: plan gate chose aborting on an empty selection over returning `data` unchanged, because a mistyped prefix that matches nothing would otherwise return silently unconverted data (GP2/GP3, and M029's stated goal on this same function); falsified by a report of a legitimate pipeline whose column filter matches none.
- 2026-08-28: plan gate chose aborting on an output-column collision over overwriting (with or without a warning), because an overwrite destroys a same-named column that need not have come from this package; falsified by a report that re-running a call is common enough that a two-step re-run is the worse cost.
- 2026-08-28: plan gate chose all seven `append`-carrying exports over the two the candidate row named, because the collision reproduces on all seven from one shared check at five sites; falsified by the shared validator proving not to fit a site's output shape.
- 2026-08-28: plan gate chose narrowing AC3's promise to its enumerated calls over an argument grid over `append`/`missing`/`calc_se`/`srange`/`version`, because the change adds refusals and touches no arithmetic; falsified by a value difference the existing suite catches that the 7-call harness does not.
- 2026-08-28: implementation gate chose a maintainer-run `data-raw/` script for the characterization harness over a testthat helper, because the comparison needs a git checkout of the merge base and `R CMD check` runs against a built copy with no repository, where the helper would skip silently.
- 2026-08-28: implementation gate chose reporting the existing argument checks ahead of the output-column collision over the collision first, because the collision check adds a refusal and the alternative would change what an already-invalid call reports today.
- 2026-08-28: T1 — `data-raw/verify_m060_characterization.R` records value, conditions and abort-flag per call for the seven enumerated exports, on the working tree and on a `git archive` of the merge base, in separate subprocesses. Control clean (7 same, 0 differ). Proven able to fail on four planted defects, one at a time, each dimension reported independently; recorded in the script header.
- 2026-08-28: T2 — `tests/testthat/test-append-collision.R` written. Confirmed red on all seven exports individually, each aborting with `tibble_error_column_names_must_be_unique` rather than a classed condition. The message assertion reads whole quoted tokens, not substrings, because every scale column is a prefix of its own `_se` column. Three tests already green: the sweep-matches-probe-table check, `append = FALSE`, and the existing-checks-first ordering control.
- 2026-08-28: T3 — `tests/testthat/test-empty-selection.R` written. Confirmed red on all three exports, each reaching base R's "arguments imply differing number of rows" or `validate_count()`'s "between 1 and 0". The precedence test asserts each competing argument is invalid on its own before asserting the empty selection outranks it. The `data`-first and wrong-type tests are green already. Tests-first tasks are red by construction; the profile's verify slot is run clean at T4/T5, when the guards land.
- 2026-08-28: T4 — `validate_no_output_collision()` in `R/util.R`, called at the five append sites, each after that site's existing argument checks and before any output column is built. `validity_pid5()`'s four cutoff abbreviations are now resolved once into `cutoff_vars` and read by both the enumeration and the four scale blocks, so the two cannot drift.
- 2026-08-28: T5 — `validate_nonempty_selection()` in `R/util.R`, called in `rank_scales()`, `norm_pid5()` and `interval_engine()` after the selection's own type check and before the rest of the selection family. Full suite clean after both: FAIL 0, WARN 0, SKIP 5, PASS 15087.
- 2026-08-28: T6 — harness run against the branch at 50698d2, merge base 108f126: 7 calls compared, 0 differ. Every call kept its returned object, its accept/reject verdict and its conditions; `validity_pid5()`'s six alerts are byte-identical on both sides.
- 2026-08-28: T7 — **Errors** prose added to all seven help pages (five new passages, `norm_pid5()`'s and `interval_hitopsr()`'s existing ones extended) and a `NEWS.md` entry under 0.2.0 naming both classes. `tests/testthat/test-error-prose.R` resolves each swept export to its Rd through the `\alias{}` index across `man/`, cuts the Errors passage between anchors the Rd owns with both ends asserted found, and drops the opening anchor. `\examples{` terminates six pages; `norm_pid5()`'s Errors passage is followed by a further bold heading, which is the tighter cut. Proven able to fail on three planted defects with a clean control: a reversed sentence in one Rd (1 failure), a removed class name in NEWS (1), and a terminator matching nothing (7 — the widening trap fails rather than passing on later text).
- 2026-08-28: T7 — a fourth test asserts the four non-selection exports do *not* promise an empty-selection refusal, so a page cannot claim a refusal its function does not make.
- 2026-08-28: T8 — `devtools::document()` reproduces the committed `man/` with no further diff and leaves `NAMESPACE` unchanged (both validators are internal). `devtools::test()`: FAIL 0, WARN 0, SKIP 5, PASS 15179. Shipped behavior matches D-045(a)-(d): aborts on collision naming every colliding column; aborts on an empty selection naming the argument, ahead of the other selection arguments and behind `data`; both conditions classed and public; no returned value changed (T6).
- 2026-08-28: all eight tasks done; status set to `review`. `devtools::document()` no diff, `devtools::test()` FAIL 0 / PASS 15179, `cairn_validate` all checks passed (21 pre-existing advisories, none from this branch).
- 2026-08-28: review returned to `in-progress` (defect return 1). AC1 fails: the collision message names only the first 20 colliding columns (cli's `vec_trunc` default) — 20 of 30 on `score_pid5()`, 20 of 76 on `score_hitopsr()`; and the multi-column probe covers 2 of 7 exports, because `skip_if` inside the loop aborts the whole `test_that` at `rank_scales()`. Consistency gate fails: `devtools::check()` 1 ERROR — `test-error-prose.R:169` reads `DESCRIPTION` outside the `artifact_text()` skip guard, so it errors under `R CMD check` instead of skipping. AC2-AC5 verified with fresh evidence. Eight further findings logged in the Review section for triage during the repair.

## Decisions

## Review

Reviewed 2026-08-28 against PR #67, branch at 5acc9ed, merge base 108f126.
**Outcome: returned to `in-progress`.** AC1 fails on two counts and the
profile's consistency gate fails on `devtools::check()`.

### Acceptance criteria

- **AC1 — FAILS.** The sweep does enumerate exactly the seven exports carrying
  an `append` formal, and each aborts with `hitop_append_collision` before any
  output column is built. Two failures against the criterion's wording:
  (a) the message does **not** name every colliding column. cli's inline vector
  collapse elides at `cli.vec_trunc = 20`. Measured:
  `score_pid5(cbind(sim_pid5, score_pid5(sim_pid5, items = 1:220, append = FALSE)), items = 1:220)`
  collides on 30 columns and the message names 20 with an elision; the 76-column
  `score_hitopsr()` case names 20 of 76.
  (b) the "and several" probe does not run for each enumerated export. The
  `skip_if(length(collide) < 2)` inside the loop of "a collision message names
  every colliding column and no other" aborts the whole `test_that`, not that
  iteration. Sweep order is `interval_hitopsr, norm_pid5, rank_scales,
  score_hitopbr, score_hitopsr, score_pid5, validity_pid5`; `rank_scales()`
  produces one column at its probe's arguments, so the block exits at position 3
  and the last four exports never receive a multi-column collision. Measured:
  that test reports `skipped = TRUE` after 7 passes.
- [x] **AC2 — verified.** `tests/testthat/test-empty-selection.R`, 5 tests /
  71 passes, 0 failures. Each of the three exports aborts on a zero-length
  selection with `hitop_empty_selection` naming `scores`/`scales`; each
  competing argument (`top`, `srange`, `prefix`, `level`, `append`, per that
  function's family) is asserted invalid on its own before being asserted
  outranked; the `data`-first exemption and the wrong-type case are asserted
  separately.
- [x] **AC3 — verified.** `Rscript data-raw/verify_m060_characterization.R`
  re-run this session: 7 probes recorded on the merge-base `git archive` and 7
  on the working tree, 7 calls compared, 0 differ. Every call kept its returned
  object, its accept/reject verdict and its conditions; `validity_pid5()`'s six
  alerts identical on both sides.
- [x] **AC4 — verified.** `tests/testthat/test-error-prose.R`, 5 tests / 92
  passes, 0 failures under `devtools::test()`. Each swept export resolves to an
  Rd through the `\alias{}` index (an unresolved name fails the setequal check);
  the Errors passage is cut between two anchors with both ends asserted found and
  the opening anchor dropped; all seven pages carry the collision sentence, the
  three selection pages the empty-selection sentence naming their argument, and
  the four others are asserted not to carry it. The `NEWS.md` section is cut at
  the current version's heading and names both classes.
- [x] **AC5 — verified.** `Rscript -e 'devtools::document()'` leaves the tree
  clean (`git status` shows only this milestone file). `Rscript -e
  'devtools::test()'`: FAIL 0, WARN 0, SKIP 5, PASS 15179.

### Consistency gate

Universal: `cairn_validate.py` — all checks passed, 21 advisories, all
pre-existing (20 dangling id tokens in DESIGN/SOURCES/DECISIONS, 1 references
staleness on `schmukle2026.md`); none introduced by this branch. No `DESIGN.md`
principle changed, so `cairn_impact.py` was not run.

Toolchain (`r-package` profile): `document()` no diff — pass. No generated file
hand-edited; `NAMESPACE` unchanged (both validators internal) — pass. `README.Rmd`
untouched — pass. `pkgdown::check_pkgdown()` "No problems found" — pass. `NEWS.md`
carries an entry under 0.2.0 naming both classes, no milestone number — pass. No
new top-level file, so no `.Rbuildignore` entry owed — pass.
**`devtools::check()` — FAILS: 1 ERROR, 0 warnings, 0 notes.**
`test-error-prose.R:169` calls `read.dcf(file.path(root(), "DESCRIPTION"))`
directly, outside the `artifact_text()` helper that carries the
`skip_if(!file.exists(...))` guard. Under `R CMD check` the source tree is
absent, so the test errors instead of skipping — contradicting the file's own
header comment that these tests skip under check. CI on PR #67 will be red for
the same reason. This is branch-introduced; the file is new here.

### Findings

Three fresh-context lenses ran against distinct evidence bases; the review
session added three of its own. Ranked, with disposition.

1. **[O] The collision message truncates the column list at 20.** `R/util.R:372`
   renders `{.val {collide}}` through cli's inline collapse, whose default
   `cli.vec_trunc` is 20. Breaks AC1, D-045(a), the NEWS entry and the identical
   Errors sentence on all seven pages, in exactly the case the milestone exists
   for. → **return: AC1 failure.**
2. **[review session] `devtools::check()` errors on `test-error-prose.R:169`.**
   See the consistency gate above. → **return: gate failure.**
3. **[review session] The multi-column collision probe covers 2 of 7 exports.**
   `skip_if` inside the loop aborts the block at `rank_scales`. → **return: AC1
   failure.**
4. **[O, and independently the prior-review lens] The collision headline is
   always singular.** `R/util.R:371` — `{cli::qty(n)}` is cancelled by the
   intervening `{.arg {arg}}` substitution, so `{?holds a column/holds columns}`
   always takes the singular. Measured at n = 2, 3, 30 and 76. This is the trap
   the `LESSONS.md` M030 line (extended M027) states verbatim, and the same
   function's other two markers place `qty()` correctly. → for the implementer.
5. **[O] The AC1 tests cannot detect findings 1 or 4.** No assertion counts
   named columns against `length(collide)`, no probe exceeds 20 columns, and
   nothing reads the headline. → for the implementer, with 1 and 4.
6. **[O] The empty-selection prose misdescribes the behavior it replaces.**
   `R/interval_hitopsr.R:99`, `R/norm_pid5.R:100`, `R/rank_scales.R:56` and the
   three rendered Rd lines say the refusal replaces "a silent return of `data`
   unchanged". No released version did that — per D-045's own Context these
   calls hit `data.frame()`'s differing-number-of-rows error, and `rank_scales()`
   reported `top` "between 1 and 0". The sentence describes the rejected design
   alternative as if it were prior behavior. → for the implementer.
7. **[O] Neither new condition class is named on any help page.** D-045(c) makes
   both public; D-044's precedent documents `hitop_interval_uncovered` by name in
   `man/interval_hitopsr.Rd`. A caller writing `tryCatch(..., hitop_append_collision =)`
   can only find the name in a release note. AC4 as worded does not require it.
   → for the implementer.
8. **[O] `validity_pid5()` emits its `srange` warning before the collision
   abort.** `R/validity_pid5.R:100` warns, `:119` aborts; the four other sites are
   placed ahead of their warnings with comments stating a colliding call is told
   about the collision alone. → for the implementer.
9. **[O] `norm_pid5()` lists collisions grouped by suffix.** `R/norm_pid5.R:253`
   builds every `_t` before every `_ptl`, and `intersect()` preserves that order.
   Cosmetic. → for the implementer.
10. **[O] `validate_nonempty_selection()`'s `arg` has no default.**
    `R/util.R:394`, unlike every sibling validator. All three call sites pass it,
    so nothing fails today. → for the implementer.
11. **[O] `between()` in `test-error-prose.R` takes an unused `info`.** Its four
    `expect_gt` assertions report failures without naming the export. → for the
    implementer.

**[S] blame-history lens: no findings.** It read the touched lines' `git blame`
history, the milestones those commits name, and D-045/D-044/D-034/D-025, and
found nothing the branch silently undoes, resurrects, or contradicts.

**[S] prior-review lens: one finding**, listed as 4 above, cited to the
`LESSONS.md` M030 line. Its GitHub probe
(`gh api repos/jmgirard/hitop/pulls/comments?per_page=1`) returned `[]`, so the
per-PR walk was skipped.

### Return

Defect return 1 of this milestone (thrash rule: none prior; no re-cut, no
amendment return). Findings 1, 2 and 3 are the return; 4 through 11 are logged
for triage during the repair.
