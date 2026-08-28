# M061: The reliability family invents scale display names instead of reading them

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2, GP2, GP3, GP4
- **Branch/PR:** `m061-scale-name-consistency`

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

- [ ] AC1 For each of these calls the returned display-name column equals, elementwise
      and in returned row order, the canonical column of the table its scales came
      from: `reliability_pid5(version = "FULL")` and `(version = "SF")` against
      `pid_scales[[version]]$Facet`; `reliability_pid5(version = "BF")` against
      `pid_scales[["BF"]]$Domain`; `reliability_hitopsr()` against
      `hitopsr_scales$Scale`; `reliability_hitopbr()` against `hitopbr_scales$Scale`;
      and, for each of the 74 modules formed by the sliding window of three
      consecutive rows of `hitopsr_scales` — a procedure that puts every one of the 76
      names through the module path — `reliability_hitopsr(module = m)` against
      `m$scales`. Asserted by a test making all 79 calls.
- [ ] AC2 The nine divergent names are returned in canonical spelling, asserted by
      literal old→new string pair in a test, one pair per name listed in Scope.
- [ ] AC3 Every one of the 76 values `reliability_hitopsr()` returns in its display-name
      column is accepted verbatim by `hitop_module("hitopsr", scales = )` and comes back
      identical in that module's `$scales`, and equals the same-row
      `available_scales("hitopsr")$Scale` entry. Asserted by a test making one
      `hitop_module()` call per value.
- [ ] AC4 The three `reliability_*()` exports return the display name in a column named
      `Scale`; `?reliability_pid5`, `?reliability_hitopsr` and `?reliability_hitopbr`
      each name `Scale` and not `scale` in their `@return` section.
- [ ] AC5 `available_scales("hitopsr")$nItems`, `hitop_module("hitopsr", scales = )$nItems`
      and the `nItems` column of all three `reliability_*()` exports are integer,
      asserted by `expect_type(x, "integer")` on one call to each of those five.
- [ ] AC6 No number moves: `data-raw/verify_m061_characterization.R` records value and
      signalled conditions together for every cell of the argument matrix (instrument ×
      version × `alpha` on/off × `omega` on/off × two `srange` values × full-instrument
      and module), runs the branch and a `git archive` of the merge base in separate R
      subprocesses, and reports zero differing cells outside the renamed display-name
      column. The script aborts rather than passing if its enumeration comes back empty
      or any enumerated call has no probe.
- [ ] AC7 That harness is proven able to report a difference by three planted defects
      differing in form, not only in location — a changed returned number, an added
      condition that leaves every returned value identical, and a deleted probe — each
      run one at a time, each exiting non-zero, with a clean control run before and
      after each.
- [ ] AC8 `snakecase` appears in neither `DESCRIPTION` nor any file under `R/`, verified
      by `grep -n snakecase DESCRIPTION` and `grep -rn snakecase R/` each returning no
      hits.
- [ ] AC9 The profile verify slot is clean: `Rscript -e 'devtools::test()'` passes and
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
- [ ] T2 Give `reliability_engine()` a `scale_names` argument replacing the
      `snakecase::to_title_case()` derivation at `R/reliability_engine.R:47`. Suppliers:
      `reliability_pid5()` → `pid_scales[[version]]$Facet` (FULL/SF) or `$Domain` (BF);
      `reliability_hitopbr()` → `hitopbr_scales$Scale`; `reliability_hitopsr()` →
      `hitopsr_scales$Scale`, or the module's own `$scales` on the module path
      (`R/module.R:241` names `itemNumbers` from `camelCase` today).
- [ ] T3 Rename the returned column `scale` → `Scale` in the engine and update the
      `@return` line of all three exports (`R/reliability_hitopsr.R:36`,
      `R/reliability_hitopbr.R:27`, `R/reliability_pid5.R:34`); sweep `tests/`,
      `vignettes/` and `README.Rmd` for readers of the old name.
- [ ] T4 Coerce `nItems` to integer in `available_scales()` (`R/available_scales.R:50`,
      copied from the dataset's double).
- [ ] T5 Drop `snakecase` from `DESCRIPTION` Imports; note in the `data-raw/` scripts
      that still call `to_any_case()` that they need it installed.
- [ ] T6 Tests: AC1's 79-call elementwise comparison, AC2's nine literal pairs, AC3's
      76-value round-trip, AC5's five type assertions. Rewrite
      `tests/testthat/test-reliability.R:119`
      (`expect_equal(rel$scale, snakecase::to_title_case(names(scales)))`) — it derives
      its expectation by re-applying the transformation under test, which IP2 forbids —
      to compare against the canonical dataset column.
- [ ] T7 Run the harness against the merge base and record its output;
      `devtools::document()`, `devtools::test()`, `devtools::check()`.
- [ ] T8 `NEWS.md`: the nine old→new name pairs and the `scale` → `Scale` rename, as a
      migration note. Two `cairn/DECISIONS.md` entries — the emitted-name alignment plus
      the column rename (GP2; D-018's one-release, no-dual-column migration pattern),
      and removing `snakecase` from Imports (GP4 routes dependency changes through the
      gate).

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

## Decisions

- **M061-D1: the module path reads its display names from `hitopsr_scales`, not
  from the module object.** T2 permitted either. AC1 compares the module path's
  returned column against `hitop_module()`'s own `$scales`, so taking the names
  from `module$scales` would make that comparison an identity between an output
  and its own input, testing nothing (IP2). Reading them from the keying table
  instead leaves `hitop_module()` and `reliability_hitopsr()` as two independent
  readers of the same column, which is what AC1 then checks.

## Review
