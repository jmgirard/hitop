<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M085: The `rename_*`/`label_*` family's warning contract

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate -->
- **Principles touched:** GP3   <!-- owner: plan · create/amend-via-gate -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate -->
- **Branch/PR:** `m085-rename-label-warning-contract` · https://github.com/jmgirard/hitop/pull/92   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

Every warning the two `rename_*()` and three `label_*()` helpers raise carries a
condition class a caller can catch, with the family's unpinned edge paths
covered by tests asserting that class rather than message prose.

## Scope
<!-- owner: plan · create/amend-via-gate -->

Surface tier: **user-facing** — condition classes are a public contract
(D-034(c)), and the change ships in docs and NEWS.

**In:** the eleven inline `cli::cli_warn()` sites in `R/rename_pid5_items.R`,
`R/rename_hitopsr_items.R`, `R/label_pid5.R`, `R/label_hitopsr.R` and
`R/label_hitopbr.R`. The eight nothing-matched reports gain
`hitop_no_columns_matched`; the two completeness reports gain
`hitop_incomplete_rename`; `rename_hitopsr_items()`'s unmatched item-text
report adopts the existing `hitop_unmatched_items` through
`warn_unmatched_items()` (`R/util.R:675`), the retrofit D-057 deferred. The
stray base-R coercion warning `rename_pid5_items()` leaks on a past-integer
item number is silenced. Tests pin each path by class, plus the out-of-range
classification of `<prefix>000` and a past-integer-range column.

**Out:** classing the warnings outside this family (`norm_pid5()`,
`validity_pid5()`, `interval_*()`, `warn_item_order()`) → the standing
message-content candidate row. Any change to what the messages say, to what
matches, or to the padding rule. The three blocked PID-5 rename/label
remainders (validity-scale labels, `_se` labels, the `version` argument) →
their own candidate row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: `rename_hitopsr_items(method = "text")` raises its unmatched
      item-text report through `warn_unmatched_items()`, so the warning carries
      class `hitop_unmatched_items`; a test catches it by class.
- [x] AC2: Each of the eight nothing-matched paths raises class
      `hitop_no_columns_matched` — `rename_pid5_items(method = "number")`,
      `rename_hitopsr_items(method = "original")`, and each of `label_pid5()`,
      `label_hitopsr()` and `label_hitopbr()` under both `target = "items"` and
      `target = "scales"` — with one test per path catching it by class.
- [x] AC3: Both completeness reports — `rename_pid5_items()`'s and
      `rename_hitopsr_items()`'s "only N of M items renamed" — raise class
      `hitop_incomplete_rename`, caught by class in a test for each helper.
- [x] AC4: `rename_pid5_items(method = "number")` given a column whose digits
      exceed R's integer range raises no warning outside this package's
      `hitop_*` classes, and still names that column in its
      `hitop_unmatched_items` report; one test asserts both.
- [x] AC5: `label_pid5()`, `label_hitopsr()` and `label_hitopbr()` each report a
      `<prefix>000` column and a past-integer-range column as out of range: a
      test per helper asserts `hitop_unpadded_items` is raised, and asserts
      `unpadded_item_cols()` places both columns in its `out_of_range` group.
- [x] AC6: The `@return` section of each of the five family functions names
      every condition class that function raises; `NEWS.md` carries a bullet
      for the two new classes and the sibling's adopted one; `DECISIONS.md`
      carries the entry naming them and superseding D-057's completeness
      clause.
- [x] AC7: `Rscript -e 'devtools::document()'` leaves no diff and
      `Rscript -e 'devtools::test()'` is clean (the profile's `verify` slot).

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1, T5
- AC2 → T2, T5, T6
- AC3 → T3, T5
- AC4 → T4, T5
- AC5 → T5
- AC6 → T7
- AC7 → T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits) -->

- [x] T1: In `R/rename_hitopsr_items.R:89-101`, replace the inline
      unmatched-item-text warning with `warn_unmatched_items(item_text[missing_idx],
      "item text")` (`R/util.R:675`); check the helper's brace escaping and
      pluralization against what the inline block produced.
- [x] T2: Add `class = "hitop_no_columns_matched"` at the eight nothing-matched
      sites: `rename_pid5_items.R:106`, `rename_hitopsr_items.R:51`,
      `label_pid5.R:86` and `:128`, `label_hitopsr.R:53` and `:78`,
      `label_hitopbr.R:54` and `:78`.
- [x] T3: Add `class = "hitop_incomplete_rename"` at the two completeness sites:
      `rename_pid5_items.R:164`, `rename_hitopsr_items.R:112`.
- [x] T4: Route `rename_pid5_items()`'s number matching through
      `item_col_numbers()` (`R/util.R:592`), or wrap its `as.integer()`
      (`rename_pid5_items.R:114`) the way that helper does, so no base-R
      coercion warning escapes; the past-range column still reports unmatched.
- [x] T5: Tests, one per path, asserting class not prose: the eight
      nothing-matched paths, the two completeness paths, the HiTOP text report;
      the past-integer-range rename path (written and seen red before T4, both
      on the stray warning and on the column still being named); and per
      `label_*()` helper the `<prefix>000` and past-range out-of-range
      classification. Convert the prose assertions this duplicates
      (`test-rename_hitopsr_items.R:50` and `:102`, the `"No columns matched"`
      matches in `test-label_pid5.R` and `test-label_scales.R`).
- [x] T6: Guard test scanning the five family files for `cli::cli_warn(` call
      sites and failing on any passing no `class =`; assert the scan's domain
      is non-empty, and plant a classless call in both call shapes present
      (single-line and multi-line) to see it red.
- [x] T7: `@return` for the five functions names each class raised; `NEWS.md`
      bullet; append the `DECISIONS.md` entry; `devtools::document()`, then
      `devtools::test()` and `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-02: created by /milestone-plan.
- 2026-09-02: criteria audit ran in full mode ([O] fresh reader, user-facing tier), returning six findings: a source-scan criterion bound two test-harness properties and its grep could not enumerate the base-R warning the probe found (dropped to T6); the unmatched-text criterion asserted message prose the package promises nothing about (narrowed to class); the edge-path criterion bundled an instrument clause and an internal-helper lock (split into AC4/AC5, clauses moved to T5); "every function raising a new class" quantified over an unenumerated domain (replaced by the five named functions in AC6); the completeness class contradicted D-057 (posed at the gate, signed off); AC2's eight paths passed all six questions.
- 2026-09-02: plan gate chose classing all eight nothing-matched reports family-wide over classing only the two rename helpers, because the label helpers' tests would otherwise keep asserting prose the package promises nothing about; falsified by a caller needing to tell a rename's report from a label's, which one shared class cannot do.
- 2026-09-02: plan gate chose one shared class name `hitop_no_columns_matched` over per-helper names, because the condition is the same event at every site and D-034(c) names conditions rather than functions; falsified by the same evidence class as above.
- 2026-09-02: plan chose adding `class =` at each call site over a shared `warn_no_columns_matched()` helper, because the eight messages differ by prefix, by items-vs-scales and by legacy-name wording, so a helper would carry the differences as arguments and centralize nothing; falsified by the messages converging to one wording.
- 2026-09-02: plan gate chose folding the stray base-R coercion warning into this milestone over a separate hotfix, because it sits in the code T2/T4 already rewrite and two review cycles over one file buy nothing; falsified by a user reporting the stray warning before this milestone merges.
- 2026-09-02: T1 done — `rename_hitopsr_items(method = "text")` raises its unmatched item-text report through `warn_unmatched_items()`; the helper's header and pluralization reproduce the inline block's wording, and its brace escaping now leaves a literal `{x}` in an item text intact. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T2 done — all eight nothing-matched reports raise `hitop_no_columns_matched`, at the two rename sites and at both targets of each `label_*()` helper. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T3 done — both completeness reports raise `hitop_incomplete_rename`. Suite clean (17186 pass, 0 fail).
- 2026-09-02: T4 done — `rename_pid5_items(method = "number")` reads its item numbers through `item_col_numbers()`. The escaping warning was first identified as a base-R `simpleWarning`, "NAs introduced by coercion to integer range", raised alongside the two `hitop_*` reports; the AC4 test was written and seen red on it before the fix.
- 2026-09-02: T5 done — new `tests/testthat/test-warning-classes.R` pins the eight nothing-matched paths, both completeness paths, the HiTOP item-text report, the past-integer-range rename path and the two out-of-range shapes per `label_*()` helper, all by class; it builds on the existing `collect_warnings()`/`frame_of_cols()` helpers. Ten prose assertions converted to class assertions across `test-rename_hitopsr_items.R`, `test-label_pid5.R` and `test-label_scales.R`.
- 2026-09-02: T6 done — the guard parses the five family files rather than grepping them, so a multi-line call is read whole and `class =` is a named argument; it pins the domain at the 10 inline sites (11 less the one T1 moved into `warn_unmatched_items()`) and is seen red against planted classless calls in both shapes. A silence control asserts a fully matching `label_hitopbr()` call raises nothing and that an unraised class is not reported.
- 2026-09-02: T4, T5 and T6 share one commit and one suite run — the tests T5 owns are the evidence T4's fix is checked by, and the guard T6 owns lives in the file T5 creates. Suite clean (17217 pass, 0 fail).
- 2026-09-02: T7 done — the `@return` of each of the five functions names every class it raises; the "under either method" clause in both rename helpers was checked by running each helper's text method and reading back `hitop_incomplete_rename`. `NEWS.md` gained a bullet under Improvements and fixes. `DECISIONS.md` already carried D-059, appended at plan time. `devtools::document()` regenerated the five Rd files; `R CMD check` 0 errors, 0 warnings, 0 notes (14m 44s), its test stage OK.

- 2026-09-02: review opened — branch pushed, draft PR #92 opened; verification in progress (suite still running).
- 2026-09-02: review evidence recorded — all seven criteria pass on fresh evidence; cairn_validate exit 0; suite 17217 pass / 0 fail; document() no diff; pkgdown clean. Three review lenses returned nine findings, all from the diff-bug lens, each reproduced or refuted here; none is a return-floor finding.
- 2026-09-02: consistency gate green — devtools::check() 0 errors, 0 warnings, 0 notes; pkgdown check_pkgdown() no problems; cairn_validate exit 0 with 24 pre-existing advisories.
- 2026-09-02: gate-directed fixes committed — the guard walks the five functions' bodies instead of reading `R/*.R`, so it no longer skips under `R CMD check`; its domain covers `warning()`/`rlang::warn()` and it requires a literal `hitop_` class; NEWS names the brace-escaping behavior change and a test pins it. Targeted suite 42 pass / 0 skip; seen red against a planted bare `warning()` and a planted `class = NULL`. Full suite and `check()` re-running at commit time.
- 2026-09-02: fixes re-verified — suite 17228 pass / 0 fail / 9 skip with the guard's skip gone; devtools::check() 0/0/0 with its test stage OK, so the guard runs under R CMD check.
- 2026-09-02: step-7 approval: PR #92 approved for merge.
- 2026-09-02: CI watch hit the harness ceiling with checks still pending (2 of 8 green: line endings, pkgdown); watcher stopped, no merge made. Approval marker `cairn/.merge-approved` written for PR #92 and standing; re-enter with /milestone-review M085.
- 2026-09-02: resume — PR #92 OPEN and mergeable, branch head `76e25dd3` pushed, default branch unmoved since the cut, all seven criteria ticked against recorded evidence; re-entering at step 7. All 8 CI checks green (line endings, pkgdown, test-coverage, macos release, ubuntu devel/release/oldrel-1, windows release). step-7 approval re-posed and granted: PR #92 approved for merge.
- 2026-09-02: PR #92 marked ready; the resume's docs-only commit re-triggered all eight checks, and the second CI watch again hit the harness ceiling with six of eight pending (line endings and pkgdown green). Watcher stopped, no merge made. Approval marker `cairn/.merge-approved` stands for PR #92; re-enter with /milestone-review M085 once the checks finish, and re-enter without appending a work-log line so the merge does not restart CI a third time.

## Decisions
<!-- owner: implement / review · append-only; milestone-local -->

## Review
<!-- owner: review · exclusive -->

Verification ran 2026-09-02 against branch head `0fff7898`, PR #92. Every
criterion below was executed fresh in this session: a script loading the branch
with `pkgload::load_all()` called each path and read back the classes raised
(`scratchpad/ac-evidence.R`), separately from the suite.

### Acceptance criteria

- **AC1** — pass. `rename_hitopsr_items(method = "text")` on a two-column frame
  whose `item_text` matches no item raised exactly one warning, of class
  `hitop_unmatched_items`. `R/rename_hitopsr_items.R:95` calls
  `warn_unmatched_items()`; the inline block is gone from the diff.
- **AC2** — pass. All eight nothing-matched paths raised
  `hitop_no_columns_matched` and nothing else:
  `rename_pid5_items(method = "number")`,
  `rename_hitopsr_items(method = "original")`, and `label_pid5()`,
  `label_hitopsr()`, `label_hitopbr()` under each of `target = "items"` and
  `target = "scales"`. One test per path in `tests/testthat/test-warning-classes.R`.
- **AC3** — pass. `rename_pid5_items(version = "FULL")` on two PID-5 columns and
  `rename_hitopsr_items(method = "original")` on three legacy columns each
  raised `hitop_incomplete_rename`; one class test per helper.
- **AC4** — pass. `rename_pid5_items(c("pid_1", "pid_99999999999"), version = "FULL")`
  raised two warnings, `hitop_unmatched_items` and `hitop_incomplete_rename`,
  both `hitop_*` and no base-R coercion warning among them; the
  `hitop_unmatched_items` message names `pid_99999999999`.
- **AC5** — pass. For each of `pid5_`, `hsr_` and `hbr_`, a frame of
  `<prefix>000` and `<prefix>99999999999` raised `hitop_unpadded_items`, and
  `unpadded_item_cols()` put both columns in `out_of_range` with `mispadded`
  empty.
- **AC6** — pass. The `@return` of each of the five functions names exactly the
  classes that function raises, checked against every warning site in each file:
  three each for the two rename helpers, two each for the three label helpers.
  `NEWS.md` carries the bullet naming both new classes and the adopted one;
  `DECISIONS.md` carries D-059, which supersedes D-057's classless-completeness
  clause.
- **AC7** — pass. `devtools::document()` exited 0 and left `git status` empty;
  `devtools::test()` reported `[ FAIL 0 | WARN 0 | SKIP 9 | PASS 17217 ]`.

### Consistency gate

`cairn_validate.py` exit 0, all checks PASS; 24 advisory warnings, all
pre-existing (23 dangling legacy `D-0NN` tokens, one references-staleness note
on `schmukle2026.md`). No `DESIGN.md` principle changed, so `cairn_impact.py`
did not apply. Toolchain slot: `document()` no diff; `pkgdown::check_pkgdown()`
"No problems found"; `NEWS.md` entry present; README untouched and in sync; no
new top-level files; `devtools::check()` 0 errors, 0 warnings, 0 notes (16m 56s).

### Independent review

Three fresh-context lenses, distinct evidence bases. The blame-history lens and
the prior-review lens each reported no findings; the prior-review lens found
M077 and M079 the only archived reviews touching these files and confirmed the
current diff regresses neither, and its GitHub inline-comment probe returned
empty. The diff-bug lens reported nine ranked findings; each was reproduced or
refuted in this session before triage.

### Findings and triage

Nine findings from the diff-bug lens, in the lens's own severity order. Each
verdict below is this session's own reproduction, not the lens's account.

1. **CONFIRMED — the class guard never runs in CI.** `family_files()`
   (`test-warning-classes.R:190`) reads `R/` through
   `testthat::test_path("..", "..")`, which under `R CMD check` resolves to
   `<pkg>.Rcheck`, where no source `R/` exists, so `skip_if()` fires. CI is
   `r-lib/actions/check-r-package` (`.github/workflows/R-CMD-check.yaml:47`),
   so the guard runs only under a local `devtools::test()`. The file comment at
   `:188` names the skip; nothing records that it leaves CI uncovered.
2. **CONFIRMED — the guard checks that `class` is present, not that it is
   usable.** Reproduced: over a file holding `cli::cli_warn("x", class = NULL)`
   and `cli::cli_warn("y", class = if (flag) "hitop_z")`, `classless()` returned
   `character(0)`. On the `FALSE` branch cli raises an unclassed warning.
3. **CONFIRMED — the guard's domain is `cli_warn` alone.** Same probe: a
   `warning()` and an `rlang::warn()` in the scanned file were invisible both to
   `classless()` and to the pinned count of 10. This is the shape of the base-R
   coercion leak T4 removed.
4. **CONFIRMED, pre-existing — `hitop_no_columns_matched` covers only one of
   each rename helper's two methods.** Under `method = "text"` with every entry
   unmatched, `locs` empties, `n_matched` is 0, and the `n_matched > 0` guard
   (`rename_hitopsr_items.R:110`, unchanged by this diff) suppresses the
   completeness report too, so the only warning is `hitop_unmatched_items`. The
   `@return` prose scopes the class correctly; what is at issue is the class's
   reach, and both the criterion and D-059 enumerate the eight paths deliberately.
5. **CONFIRMED — the NEWS sentence "The messages themselves are unchanged" is
   false for one input, and the change it hides is an unpinned bug fix.**
   Reproduced by running the pre-branch inline block verbatim:
   `item_text = "How often do you feel {sad}?"` **errored** with "Could not
   evaluate cli `{}` expression: `sad`". `warn_unmatched_items()` escapes braces,
   so that input now warns correctly. No test pins it.
6. **CONFIRMED, intentional — the AC5 block asserts `unpadded_item_cols()`'s
   return shape**, an unexported helper. AC5 asks for exactly this, so it is a
   plan-level choice, not a coding defect.
7. **CONFIRMED, minor — `expect_length(calls, 10)` is an equality, not a
   floor**, so a properly classed addition to a family file goes red with a
   count message rather than a class message.
8. **CONFIRMED, minor — two converted assertions dropped their `info =`.**
   `test-label_pid5.R:229` and `test-label_scales.R:385` sit in loops;
   `expect_s3_class()` takes no `info`, so a failure no longer names the
   iteration (the standing testthat-traps lesson).
9. **CONFIRMED, minor — `caught$warnings[[1]]` is unguarded** at
   `test-rename_hitopsr_items.R:48` and `:102`; if the warning stops being
   raised the test errors on a subscript rather than failing on the class. The
   sibling conversions in `test-label_scales.R` kept their length guards.

**Return floor:** no finding demonstrates an acceptance criterion failing —
all seven pass on fresh evidence — so none is a floor return.

**Triage at the 2026-09-02 gate.** Jeff chose to fix the guard and the
changelog on this branch, and approved the merge.

- Findings 1, 2 and 3 — **fixed now.** The guard no longer reads `R/*.R` off
  disk: it walks `body()` of the five exported functions, so it runs under
  `R CMD check` as well as `devtools::test()` and cannot skip. Its domain is
  widened from `cli::cli_warn` to `warning()`, `base::warning()`, `rlang::warn()`
  and `warn()` — the shape T4's base-R leak had. A call counts as guarded only
  when `class` is a character literal whose every element starts with `hitop_`,
  so `class = NULL`, `class = if (flag) "hitop_x"` and a foreign class are all
  reported. The pinned count of 10 became a floor (`expect_gte`), so a properly
  classed addition goes red on the class assertion rather than on arithmetic —
  which also settles finding 7. Seen red both ways: a planted
  `warning("planted: a bare base-R warning")` in `R/label_hitopbr.R` failed the
  guard, and so did replacing that file's `class = "hitop_no_columns_matched"`
  with `class = NULL`; the file was restored from a backup and `git diff` on it
  is empty.
- Finding 5 — **fixed now.** The NEWS sentence "The messages themselves are
  unchanged" now reads that the wording is unchanged with one behavioral
  exception, and names it: the HiTOP-SR report escapes braces, so an
  `item_text` containing `{...}` warns instead of failing with a cli evaluation
  error. A test in `test-warning-classes.R` pins it — the call raises no error,
  raises `hitop_unmatched_items`, and the literal `{sad}` survives into the
  message.
- Finding 4 — **follow-up.** Pre-existing and deliberately out of the
  criterion's eight paths; recorded as a candidate row at hygiene.
- Finding 6 — **rejected.** AC5 asks for exactly this assertion; an intentional
  change the plan called for.
- Findings 8 and 9 — **follow-up.** Test-reach only, no wrong verdict today;
  recorded as a candidate row at hygiene.

**Re-verified after the fixes.** `devtools::test()`
`[ FAIL 0 | WARN 0 | SKIP 9 | PASS 17228 ]` — eleven more assertions than
before and the guard's own skip gone, no "R/ not available" among the nine
remaining skips. `devtools::check()` 0 errors, 0 warnings, 0 notes (16m 12s),
its test stage OK, so the guard now runs under `R CMD check` — the gap
finding 1 named. AC1-AC7 are unaffected by the fixes: the only source file
touched was restored and shows no diff.
