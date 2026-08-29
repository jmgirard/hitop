# M061: The reliability family invents scale display names instead of reading them

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2, GP3, GP4
- **Branch/PR:** `m061-scale-name-consistency` / https://github.com/jmgirard/hitop/pull/68

## Goal

`reliability_pid5()`, `reliability_hitopsr()` and `reliability_hitopbr()` return each
scale's canonical display name read from the keying tables, not a name rebuilt from
the camelCase stem.

## Scope

Surface tier: **user-facing** — the returned display-name column is a public return
value three exported functions hand to callers.

**In:** `R/reliability_engine.R:47` builds the returned name as
`snakecase::to_title_case(names(items_scales))`, the only `snakecase::` call in `R/`.
It diverges from the canonical spelling in nine names: `Distress-Dysphoria`,
`Non-persistence`, `Non-planfulness`, `Non-suicidal Self-injury`,
`Sex-Related Substance Use`, `Well-being` (HiTOP-SR); `p-Factor` (HiTOP-BR);
`Unusual Beliefs & Experiences` (PID-5 FULL and SF); `Negative affectivity` (PID-5 BF).
The engine takes the names from its caller instead. Folded in at the plan gate: the
returned column is renamed `scale` → `Scale` to match `available_scales()`, and the
`available_scales()$nItems` double/integer divergence from `hitop_module()$nItems`
(existing candidate row, retired by this milestone) is closed.

**Out:** any change to a keying table's stored spelling — the tables are already
mutually consistent and their names are IP1 content (D-041, D-042). The `nItems`
double stored in the `*_scales` datasets themselves (M037's lesson) stays double →
candidate row. `pid_norms$scale` keeps its lowercase name; it holds camelCase stems,
not display names → not a divergence. Adding a `camelCase` column to the reliability
return → candidate row. Whether `p-Factor` should be printed `p-factor` is IP1 content
with its own open candidate row (added 2026-08-27, lineage M059); AC2 asserts the
spelling the table ships today and is unaffected by how that row settles.

## Acceptance criteria

- [x] AC1 For each of these calls the returned display-name column equals, elementwise
      and in returned row order, the canonical column of the table its scales came
      from: `reliability_pid5(version = "FULL")` and `(version = "SF")` against
      `pid_scales[[version]]$Facet`; `reliability_pid5(version = "BF")` against
      `pid_scales[["BF"]]$Domain`; `reliability_hitopsr()` against
      `hitopsr_scales$Scale`; `reliability_hitopbr()` against `hitopbr_scales$Scale`;
      and, for each of the 74 modules formed by the sliding window of three
      consecutive rows of `hitopsr_scales` — a procedure that puts every one of the 76
      names through the module path — `reliability_hitopsr(module = m)` against
      `m$scales`. Asserted by a test making all 79 calls.
- [x] AC2 The nine divergent names are returned in canonical spelling, asserted by
      literal old→new string pair in a test, one pair per name listed in Scope.
- [x] AC3 Every one of the 76 values `reliability_hitopsr()` returns in its display-name
      column is accepted verbatim by `hitop_module("hitopsr", scales = )` and comes back
      identical in that module's `$scales`, and equals the same-row
      `available_scales("hitopsr")$Scale` entry. Asserted by a test making one
      `hitop_module()` call per value.
- [x] AC4 The three `reliability_*()` exports return the display name in a column named
      `Scale`; `?reliability_pid5`, `?reliability_hitopsr` and `?reliability_hitopbr`
      each name `Scale` and not `scale` in their `@return` section.
- [x] AC5 `available_scales("hitopsr")$nItems`, `hitop_module("hitopsr", scales = )$nItems`
      and the `nItems` column of all three `reliability_*()` exports are integer,
      asserted by `expect_type(x, "integer")` on one call to each of those five.
- [x] AC6 No number moves: `data-raw/verify_m061_characterization.R` records value and
      signalled conditions together for every cell of the argument matrix (instrument ×
      version × `alpha` on/off × `omega` on/off × two `srange` values × full-instrument
      and module), runs the branch and a `git archive` of the merge base in separate R
      subprocesses, and reports zero differing cells outside the renamed display-name
      column. The script aborts rather than passing if its enumeration comes back empty
      or any enumerated call has no probe.
- [x] AC7 That harness is proven able to report a difference by three planted defects
      differing in form, not only in location — a changed returned number, an added
      condition that leaves every returned value identical, and a deleted probe — each
      run one at a time, each exiting non-zero, with a clean control run before and
      after each.
- [x] AC8 `snakecase` is not a declared dependency of the package:
      `grep -rn snakecase R/ NAMESPACE` returns no hits, and
      `grep -n snakecase DESCRIPTION` returns exactly one line, belonging to the
      `Config/Needs/data-raw` field and to no other field — in particular not to
      `Imports:`, `Suggests:`, `Depends:`, `LinkingTo:` or `Enhances:`. Stated once
      more independently of that text read: `packageDescription("hitop")$Imports`,
      `$Suggests` and `$Depends` each name no `snakecase`. Each of
      `data-raw/pid_info.R`, `data-raw/hitopsr_info.R` and `data-raw/hitopbr_info.R` —
      the three scripts that call `snakecase::to_any_case()` — carries a header note
      saying it needs `snakecase` installed and that it is not a package dependency.
- [x] AC9 The profile verify slot is clean: `Rscript -e 'devtools::test()'` passes and
      `Rscript -e 'devtools::check()'` reports 0 errors, 0 warnings, 0 notes.

## Coverage

- AC1 → T2, T6
- AC2 → T2, T6
- AC3 → T2, T6
- AC4 → T3, T6
- AC5 → T4, T6
- AC6 → T1, T7
- AC7 → T1
- AC8 → T5, T7
- AC9 → T7

## Tasks

- [x] T1 Write `data-raw/verify_m061_characterization.R` on the M060 pattern
      (`data-raw/verify_m060_characterization.R`): value-and-condition recording, base
      ref via `git archive` into a temp dir, two subprocesses, abort on an empty
      enumeration or a missing probe. Cover the full argument matrix named in AC6.
      Prove it fails on the three planted defects of AC7, with clean controls.
- [x] T2 Give `reliability_engine()` a `scale_names` argument replacing the
      `snakecase::to_title_case()` derivation at `R/reliability_engine.R:47`. Suppliers:
      `reliability_pid5()` → `pid_scales[[version]]$Facet` (FULL/SF) or `$Domain` (BF);
      `reliability_hitopbr()` → `hitopbr_scales$Scale`; `reliability_hitopsr()` →
      `hitopsr_scales$Scale`, or the module's own `$scales` on the module path
      (`R/module.R:241` names `itemNumbers` from `camelCase` today).
- [x] T3 Rename the returned column `scale` → `Scale` in the engine and update the
      `@return` line of all three exports (`R/reliability_hitopsr.R:36`,
      `R/reliability_hitopbr.R:27`, `R/reliability_pid5.R:34`); sweep `tests/`,
      `vignettes/` and `README.Rmd` for readers of the old name.
- [x] T4 Coerce `nItems` to integer in `available_scales()` (`R/available_scales.R:50`,
      copied from the dataset's double).
- [x] T5 Drop `snakecase` from `DESCRIPTION` Imports; note in the `data-raw/` scripts
      that still call `to_any_case()` that they need it installed, and declare the
      need as `Config/Needs/data-raw`.
- [x] T6 Tests: AC1's 79-call elementwise comparison, AC2's nine literal pairs, AC3's
      76-value round-trip, AC5's five type assertions. Rewrite
      `tests/testthat/test-reliability.R:119`
      (`expect_equal(rel$scale, snakecase::to_title_case(names(scales)))`) — it derives
      its expectation by re-applying the transformation under test, which IP2 forbids —
      to compare against the canonical dataset column.
- [x] T7 Run the harness against the merge base and record its output;
      `devtools::document()`, `devtools::test()`, `devtools::check()`.
- [x] T8 `NEWS.md`: the nine old→new name pairs and the `scale` → `Scale` rename, as a
      migration note. Two `cairn/DECISIONS.md` entries — the emitted-name alignment plus
      the column rename (GP2; D-018's one-release, no-dual-column migration pattern),
      and removing `snakecase` from Imports (GP4 routes dependency changes through the
      gate).
- [x] T9 (discovered at the AC8 amendment audit) Correct `cairn/DESIGN.md`'s two
      current-knowledge mentions of `{snakecase}` as a generator-family Import — lines
      70 and 106 — which this milestone makes false. The embedded historical decision
      log's mention stays verbatim (IP4).

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier), fresh-context [O]
  reader. Returned findings on 5 of 6 drafted criteria. Fixed at the gate: AC1's
  `setdiff` membership test (passes on a swapped or zero-length column) → elementwise
  equality in table order, and its one-exemplar module clause → the 74-module sliding
  window checked against each module's own scales; AC3's identity claim gained the
  planted-defect negative control now split out as AC7; AC4's "no function derives a
  display name", unenumerable by the `grep -rn snakecase::` it named, → narrowed to the
  dependency claim now in AC8; AC5 (test-harness property) and AC6 (NEWS wording), both
  instrument-bound, → tasks T6 and T8. The auditor's cross-cutting gap — a
  `reliability_*()` value that `hitop_module(scales = )` would reject — became AC3.
  Two findings went to the question gate as the identity-proof-breadth and dependency-
  disposition questions.
- 2026-08-28: plan gate chose reading the canonical name from the keying table over
  keeping a derived name and making the derivation lossless (a `to_title_case()`
  exception map, or renaming the tables' stored names so the derivation round-trips),
  because D-041 already fixed the invariant that one printed name drives the stems, the
  scored columns and the joins alike, and an exception map is the special case D-041
  deleted from `data-raw/hitopsr_info.R`. Falsified by a scale whose display name is
  genuinely unavailable at the engine's caller.
- 2026-08-28: plan gate chose the full D-011-style argument matrix over a six-call spot
  comparison for AC6, Jeff's call, because `reliability_engine()` is shared by every
  reliability call and the tier is user-facing. Falsified by the matrix costing more
  wall-clock than the milestone's remaining work.
- 2026-08-28: plan gate chose removing `snakecase` from `DESCRIPTION` outright over
  moving it to Suggests, Jeff's call over the recommendation, because no package code
  will use it and only maintainer-run `data-raw/` scripts do. Falsified by a
  `data-raw/` regeneration failing for a contributor who reads DESCRIPTION as the
  dependency list.

- 2026-08-28: implementation gate chose running both sides of the characterization
  harness cold on every run over reusing the merge-base recording, Jeff's call on
  the recommendation, because a reused recording is a place the evidence could
  silently go stale; the full matrix costs ~18 min per run and ~2 h across AC7's
  seven runs. Falsified by the cold runs blocking the milestone's other work.
- 2026-08-28: implementation gate chose recording the `snakecase` requirement of the
  five `data-raw/` scripts both as a script header note and as a
  `Config/Needs/data-raw` line in DESCRIPTION, Jeff's call.
- 2026-08-28: T1 done. Harness runs 48 cells per side. Control run clean (0 differ).
  Three planted defects each reported and each exited 1, on its own dimension:
  a shifted alpha on exactly the 24 `alpha = TRUE` cells and no condition change;
  a planted `warning()` on all 48 cells' conditions and no value change; a deleted
  HiTOP-BR path as "No probe for: reliability_hitopbr". Four controls, all clean.

- 2026-08-28: T2, T3, T4 and T6 done and verified together in one run, since the
  AC1-AC5 tests fail until all three code changes land. `reliability_engine()`
  takes a `scale_names` argument; the four suppliers are `pid_scales[[version]]`'s
  `Facet` (FULL/SF) or `Domain` (BF), `hitopbr_scales$Scale`, `hitopsr_scales$Scale`,
  and `module_engine_inputs()`'s new `display_col`. Returned column renamed
  `scale` -> `Scale` in the engine, the three `@return` sections, and seven reader
  sites across `test-reliability.R` and `test-deprecated.R`. `available_scales()`
  coerces `nItems` to integer. `devtools::test()`: FAIL 0, PASS 15500, SKIP 4.
- 2026-08-28: correction to the gate line above: three `data-raw/` scripts call
  `snakecase::to_any_case()` and carry the header note, not five —
  `pid_info.R`, `hitopsr_info.R`, `hitopbr_info.R`. `verify_hitopsr_rename.R` and
  `test-scale-name-hitopsr.R` mention the package in prose only.
- 2026-08-28: T5 done. `snakecase` removed from Imports; `Config/Needs/data-raw:
  snakecase` added; header note in the three calling scripts. This made AC8
  unsatisfiable as written (its DESCRIPTION grep now returns the Config line), so
  AC8 was amended at a mini gate.
- 2026-08-28: AC8 amended. The criteria audit ran in FULL mode (user-facing tier) on
  the amended wording, fresh-context [O] reader that did not author it. It returned
  OK on satisfiability, deliverable-vs-instrument and proportionality, and two
  findings: the draft pinned one literal DESCRIPTION line, which an indented DCF
  continuation would break, and its greps omitted `NAMESPACE`, a declaration site
  outside `R/`. It also noted the gate decision's header-note half was unasserted.
  Jeff took the auditor's replacement wording in full at the mini gate, which fixes
  both findings and adds the header-note clause; that clause widens AC8. The audit's
  two other observations became the "five scripts" correction above and task T9.
- 2026-08-28: T8 done. `NEWS.md` gains the nine old->new name pairs, the
  `scale` -> `Scale` migration note, the integer `nItems` change and the dependency
  removal. D-046 (emitted-name alignment + column rename) and D-047 (`snakecase`
  leaves Imports) appended to `cairn/DECISIONS.md`.
- 2026-08-28: T9 done. `cairn/DESIGN.md` lines 70 and 106 corrected in place and
  marked; the embedded historical decision log's mention left verbatim (IP4).
- 2026-08-28: T7 done. Characterization harness run against the merge base:
  48 cells compared, 0 differ outside the display-name column, 40 carry a
  display-name change (the 8 that do not are the module cells, whose three scales
  were never among the nine). `devtools::document()` leaves no diff;
  `devtools::test()` FAIL 0, WARN 0, SKIP 4, PASS 15500; `devtools::check()`
  0 errors, 0 warnings, 0 notes. All nine tasks checked; status set to review.

- 2026-08-28: review opened. Branch pushed; draft PR #68 opened for CI. AC6, AC7
  and AC9 evidence runs in flight (check, then seven harness runs); AC1-AC5 and AC8
  evidence recorded in the Review section.

## Decisions

- **M061-D1: the module path reads its display names from `hitopsr_scales`, not
  from the module object.** T2 permitted either. AC1 compares the module path's
  returned column against `hitop_module()`'s own `$scales`, so taking the names
  from `module$scales` would make that comparison an identity between an output
  and its own input, testing nothing (IP2). Reading them from the keying table
  instead leaves `hitop_module()` and `reliability_hitopsr()` as two independent
  readers of the same column, which is what AC1 then checks.

## Review

Reviewed 2026-08-28 on `m061-scale-name-consistency` at `e4f559e`, against
`origin/main` at `a322585d`. The branch was already current with the default
branch (`git log HEAD..origin/main` empty), so nothing was merged in.
PR: https://github.com/jmgirard/hitop/pull/68

### Acceptance-criterion evidence

- **AC1 — verified.** Re-derived independently of the milestone's own tests, in a
  scratch script that reads each expectation from the keying table rather than
  from the test file. All 79 calls compared elementwise in returned row order:
  `reliability_pid5()` FULL and SF against `pid_scales[[version]]$Facet`, BF
  against `pid_scales[["BF"]]$Domain`, `reliability_hitopsr()` against
  `hitopsr_scales$Scale`, `reliability_hitopbr()` against `hitopbr_scales$Scale`,
  and each of the 74 sliding-window three-scale modules against its own
  `$scales`. `identical()` on every one; 5 + 74 = 79 confirmed by assertion.
  `test-reliability-scale-names.R` and `test-reliability.R` together: FAIL 0,
  ERROR 0, WARN 0, SKIP 0, PASS 312.
- **AC2 — verified.** Each of the nine old spellings absent from the emitted
  column and each canonical spelling present, checked as literal pairs against
  live output: `Distress-Dysphoria`, `Non-persistence`, `Non-planfulness`,
  `Non-suicidal Self-injury`, `Sex-Related Substance Use`, `Well-being`,
  `p-Factor`, `Unusual Beliefs & Experiences`, `Negative affectivity`.
- **AC3 — verified.** All 76 `reliability_hitopsr()` display names fed back to
  `hitop_module("hitopsr", scales = )` one at a time; every call accepted and
  returned the value unchanged in `$scales`, and the returned column is
  `identical()` to `available_scales("hitopsr")$Scale` row for row. Separately
  confirmed that `hitop_module()` rejects all six old HiTOP-SR spellings, the
  claim `NEWS.md` makes.
- **AC4 — verified.** All three exports return `Scale` and no `scale`; the
  `\value` section of `man/reliability_pid5.Rd`, `man/reliability_hitopsr.Rd`
  and `man/reliability_hitopbr.Rd` each names `Scale`, and `grep '`scale`'` over
  the three pages returns nothing.
- **AC5 — verified.** `expect_type(x, "integer")` equivalent run on all five:
  `available_scales("hitopsr")$nItems`, `hitop_module()$nItems`, and the `nItems`
  column of each of the three `reliability_*()` exports.
- **AC6 — verified.** `Rscript data-raw/verify_m061_characterization.R` re-run at
  review against merge base `a322585d`, both sides recomputed cold in separate
  subprocesses: **48 cells compared, 0 differ outside the display-name column,
  40 carry a display-name change**, exit 0. The 8 cells with no name change are
  the module cells, whose three scales (`Agoraphobia`, `Appetite Loss`,
  `Binge Eating`) are none of the nine. The enumeration guards are exercised
  rather than assumed: the empty-enumeration and cell-count guards run on every
  pass, and the no-probe guard is fired by AC7's third planted defect below.
- **AC7 — verified.** Seven harness runs re-executed at review, one planted
  defect at a time, each with a clean control before and after it (four controls,
  each 48 cells / 0 differ / exit 0). Each defect fired on its own dimension and
  each exited 1:
  - *a changed returned number* (`out$alpha + 1e-6` in `reliability_engine()`) —
    24 differing cells, exactly the 24 `alpha = TRUE` cells, all reported as
    "returned value changed"; conditions changed on 0 cells, verdicts on 0.
  - *an added condition leaving every value identical* (a `warning()` before the
    engine's return) — all 48 cells reported as "conditions changed", value
    changes on 0 cells. The recorded signatures are non-empty on both sides
    (39 -> 40 conditions on the first cell, real lavaan convergence and
    reverse-scoring warnings), so the dimension compares content rather than two
    empty lists.
  - *a deleted probe* (the HiTOP-BR path removed from `build_paths()`) —
    `Error: No probe for: reliability_hitopbr`, aborting rather than silently
    comparing the 40 remaining cells.
  The working tree was byte-restored after each run (`git status` clean at the
  end), so no planted defect reached the branch.
- **AC8 — verified.** `grep -rn snakecase R/ NAMESPACE` exits 1 with no hits.
  `grep -n snakecase DESCRIPTION` returns exactly one line, `43:Config/Needs/data-raw:
  snakecase`; the file's `Imports:`, `Suggests:` and `Depends:` fields were read
  in full and name no `snakecase`, and no `LinkingTo:` or `Enhances:` field
  exists. Each of `data-raw/pid_info.R`, `data-raw/hitopsr_info.R` and
  `data-raw/hitopbr_info.R` carries the header note.
- **AC9 — verified.** `Rscript -e 'devtools::test()'`: FAIL 0, ERROR 0, WARN 0,
  SKIP 4, PASS 15500. `Rscript -e 'devtools::check()'`: **0 errors, 0 warnings,
  0 notes** (`Status: OK`, hitop 0.2.0, 4m58s; `Running 'testthat.R' [215s]` OK).

### Consistency gate

- `cairn_validate.py` exits 0 — every check PASS, including `coverage complete`,
  `scaffold present` and `binding criteria`. 87 advisory warnings, all
  pre-existing shapes (`sizing` on 9 criteria, `work-log format` on the
  milestone's wrapped lines, `dangling id tokens` on the legacy D-001..D-012
  citations, `references staleness` on `schmukle2026.md`).
- `cairn_impact.py --changed`: GP4's line changed, 16 references listed. The
  edit removes a false example ({snakecase} as a generator Import) and leaves
  the principle's stance untouched; every citing D-entry cites the posture, not
  the example, and D-047 is this milestone's reconciliation. No divergence left
  open.
- Toolchain slot (`r-package`): `devtools::document()` leaves no diff;
  `pkgdown::check_pkgdown()` "No problems found"; `devtools::build_readme()`
  leaves `README.md` unchanged; `NEWS.md` carries the user-visible entry with no
  milestone number in it; the branch adds no top-level file, so no
  `.Rbuildignore` entry is owed; `check()` is clean as recorded under AC9.
- CI on PR #68: green on all eight jobs (line endings, pkgdown, test-coverage,
  macOS release, Windows release, Ubuntu release/devel/oldrel-1).

### Independent fresh-context review

Surface tier is user-facing and the diff touches executable surface, so the full
three-lens fan-out ran, each lens on a distinct evidence base, none having seen
the implementation.

- **[S] blame-history** — no findings. It read `git log`/`git blame` on every
  modified line and reports the column rename takes D-018's recorded migration
  path (as D-041's rename did), the table-read replaces exactly the lossy
  derivation D-041's fix flagged, and M061-D1's module-path choice preserves the
  two-independent-readers property.
- **[S] prior-PR-comments** — no findings. Its existence probe found no inline
  review comments on any PR in the repo, so the thread walk was skipped; against
  the archived `## Review` sections touching these files (M037, M041) it found no
  point the diff reintroduces or contradicts.
- **[O] diff-bug** — 8 findings, ranked below. It could not re-verify AC6 itself:
  its own background harness rerun was killed by the environment twice before the
  merge-base side finished. That claim is covered by this session's own run.

Each [O] finding was re-verified against the implementation rather than against
the reviewer's account of it; all 8 hold. Finding 9 is this session's own.

**Return floor: not met.** None of the nine demonstrates an acceptance criterion
failing inside the domain of the procedure it names. Findings 3 and 4 bear on
AC6's harness, but AC6's abort promise is over the export enumeration the script
performs, and a deleted *path* leaves every enumerated export still probed; AC6's
diff promise is over cells differing outside the display-name column, which is
what the run reports. Status stays `review`; all nine go to triage at the gate.

#### Findings, ranked, with dispositions

1. `R/reliability_engine.R:19-20` — **the documented length guard does not
   exist.** The roxygen says a `scale_names` length disagreeing with
   `items_scales` "is caught by `data.frame()` below, which is why no separate
   guard is added". `data.frame()` recycles any vector whose length divides the
   other's: `data.frame(Scale = c("a","b"), nItems = 1:4)` returns 4 rows named
   `a,b,a,b` with no error (re-verified in a vanilla R session; a
   non-divisor length does abort). All four current suppliers are correct, so
   nothing is broken today — the defect is a false claim in a docstring, and a
   guard declined on it. Failure: a future caller hands 5 domain names to the 25
   facet scales and the engine silently returns 25 rows with 5 names cycling,
   the wrong-name class this milestone exists to remove. **Disposition: fix now.**
2. `cairn/DESIGN.md:44` — **a current-knowledge line the diff falsifies was
   missed.** The Function families bullet still says the reliability family
   "returns a per-scale tibble (`scale`, `nItems`, `alpha`, `omega`)". T9
   corrected lines 70 and 106 only; line 196 is the embedded historical log and
   correctly left verbatim. **Disposition: fix now** (corrected in place, marked,
   per the current-knowledge correction rule).
3. `data-raw/verify_m061_characterization.R:246-258` — **the missing-probe guard
   is per-export, not per-path.** `expected` is recomputed from the same
   `build_paths()` a deletion mutates, and the check is
   `setdiff(exports, probed)` at export granularity. Deleting the
   `hitopsr_module` block — the only cell exercising the new module-path name
   supplier — or `pid5_SF` leaves both exports still probed, `expected` shrinks
   to match, and the run reports "40 cells compared; 0 differ" and exits 0.
   Guard reach, not a wrong answer on any current input. **Disposition:
   follow-up candidate row.**
4. `data-raw/verify_m061_characterization.R:342-347, 384-397` — **losing the
   display-name column entirely passes.** `without_names()` drops the column when
   present, so a branch returning none reduces to the same
   `(nItems, alpha, omega)` as the base side and `identical()` succeeds; the
   shape change lands in `renamed_cells`, which is printed but never added to
   `differ`. AC4's test catches this case, so it is guard reach.
   **Disposition: follow-up candidate row.**
5. `R/module.R:184` and `R/score_hitopsr.R:81` — **comments stale after the
   fourth return element.** Both still describe `module_engine_inputs()` as
   returning three values; it now returns four. No behavioral effect — every
   consumer extracts by name. **Disposition: fix now.**
6. `tests/testthat/test-available_scales.R:14` — **the function's own
   view-of-the-table test is blind to T4's type change.**
   `expect_equal(out$nItems, hitopsr_scales$nItems)` treats integer and double as
   equal under edition 3, so the coercion rests on the single new AC5
   `expect_type`; lines 51-52's `as.integer(out$nItems[[i]])` is now a no-op.
   **Disposition: fix now.**
7. `NEWS.md` — **the `nItems` note omits the half that can break code.** It says
   the column is now integer and matches the module and reliability tables, but
   not that `hitopsr_scales$nItems` deliberately stays double (Scope; candidate
   row), so `identical(available_scales()$nItems, hitopsr_scales$nItems)` flips
   from TRUE to FALSE with nothing in NEWS pointing at it. Confirmed:
   `typeof(hitopsr_scales$nItems)` is `double`. Nothing NEWS says is false; it is
   incomplete on a user-facing surface. **Disposition: fix now.**
8. `data-raw/verify_m061_characterization.R:300-308` — **`origin/HEAD` assumed
   present and `git archive`'s exit status unchecked.** In a clone with no
   `origin/HEAD`, `base_sha` is empty, no tarball is written, and the failure
   surfaces as an `untar()` error rather than a diagnosis. Loud, not silent.
   **Disposition: follow-up candidate row** (same row as 3 and 4).
9. `cairn/DECISIONS.md:27` and `:62` — **D-046 and D-047 both date the M061 plan
   gate `2026-08-27`.** The milestone file records its creation and every gate on
   `2026-08-28`, and all three branch commits are dated `2026-08-28`; no M061
   activity happened on the 27th. Both entries are on this branch and unmerged,
   so they are not yet history and can be corrected in place rather than
   superseded. **Disposition: fix now.**

#### Triage outcome (2026-08-28 gate)

Jeff selected "fix six, then merge". Findings 1, 2, 5, 6, 7 and 9 were fixed on
the branch before the approval marker was written; findings 3, 4 and 8 — all
harness guard-reach, none a wrong answer on any current input — were filed as a
follow-up candidate row. Nothing was rejected. That row is filed at the
post-merge hygiene pass rather than here: ROADMAP.md sits at its 59-line cap,
and search-first puts these three with the existing maintainer-run
`data-raw/` verification-tooling row, whose extension needs the disposition
question the records-hygiene rules put to the maintainer.

- **1 fixed.** `reliability_engine()` gains an explicit `cli_assert()` that
  `length(scale_names) == length(items_scales)`, and the roxygen now says what is
  true: `data.frame()` would recycle a divisor length rather than abort. A test
  in `test-reliability-scale-names.R` fires the guard on a divisor length (4
  names for 8 scales) and on a non-divisor length, asserts the message rather
  than bare failure, and keeps a passing control. Proven able to fail: with the
  `cli_assert()` block deleted the file goes red, and green again once restored.
- **2 fixed.** `cairn/DESIGN.md:44` corrected in place and marked
  (`corrected M061`).
- **5 fixed.** `R/module.R:184` now names the fourth element; `R/score_hitopsr.R:81`
  no longer says "three".
- **6 fixed.** `tests/testthat/test-available_scales.R` compares with
  `expect_identical(out$nItems, as.integer(hitopsr_scales$nItems))` and says why.
- **7 fixed.** `NEWS.md` now states that the shipped `*_scales` datasets keep a
  double `nItems`, so `identical()` against `available_scales()$nItems` flips to
  `FALSE` while `==` and joins are unaffected.
- **9 fixed.** D-046 and D-047 now date the plan gate `2026-08-28`. Both entries
  are unmerged, so this is a correction before the record becomes history, not an
  edit to history.
