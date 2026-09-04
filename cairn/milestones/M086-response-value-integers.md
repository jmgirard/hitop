# M086: Every response value the package ships is an integer

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m086-response-value-integers` / https://github.com/jmgirard/hitop/pull/94

## Goal

Every response value the package ships — the 550 answer columns of the three `ku_*`
datasets, `hitophsum_choices$Value`, and the response options in the internal
instruction objects — stores whole numbers as integers, so the package's real and
simulated example data agree on the type of the same kind of value, with no shipped
value, distributed artifact, or scored result moved.

Surface tier: user-facing — the `ku_*` datasets and `hitophsum_choices` are documented
exports users read, and the instruction values reach every distributed artifact.

## Scope

**In:** Retype to integer the response columns of `ku_hitopsr` (405), `ku_hitopbr` (45)
and `ku_pid5sf` (100), `hitophsum_choices$Value`, and `options$value` in
`hitopsr_instructions` and `hitopbr_instructions` (`R/sysdata.rda`). All four objects
are rebuilt by re-running their `data-raw/` scripts against their real sources, with
`readr` column types named where a reader is involved; Jeff supplies the two
`Y:/VIDAS/Study1/` files for the run. Extend the M081 sweep in
`test-item-number-type.R` to prove the retype landed, with a response-column plant per
dataset. Extend `test-item-number-merge-base.R` to prove nothing else moved, and add a
`R/sysdata.rda` loader to `helper-merge-base.R`. NEWS entry and a `DECISIONS.md` entry
recording the pre-1.0 waiver of the deprecation cycle, on the D-056 precedent.

**Out:** Any change to the five `sim_*` datasets → they are already integer.
Converting the two drive-sourced datasets in place from what is committed → rejected at
the plan gate in favour of a real re-run; falls back to a candidate row only if the
sources prove unreachable. DOCX response-option comparison → the existing
`test-generate_docx.R` oracle reads `*_instructions$options`, so it would move with the
change and prove nothing; the flat-text formats carry the check. Any change to
`pid_norms`, `pid_instructions$options$value`, or the keying tables → already integer
under D-056.

## Acceptance criteria

- [x] AC1: No dataset the package ships stores a whole-valued number as a bare double:
      the sweep in `tests/testthat/test-item-number-type.R`, walking every dataset
      `data(package = "hitop")` lists into list-columns and nested frames, reports an
      empty set of paths, and a test asserts the sweep's domain is non-empty and
      contains the eight example datasets and `hitophsum_choices`.
- [x] AC2: The sweep is shown able to catch a response column stored as a double: for
      each of `ku_hitopsr`, `ku_hitopbr`, `ku_pid5sf` and `hitophsum_choices`, a planted
      copy with one response column coerced back to double yields exactly that column's
      path, paired against the shipped object yielding nothing at that path.
- [x] AC3: Nothing but the type moved: for each of `ku_hitopsr`, `ku_hitopbr`,
      `ku_pid5sf` and `hitophsum_choices`, applying only the response-column retype —
      and, where the object carries one, the matching `readr` `spec` collector retype —
      to the object at the branch's merge base yields an object `identical()` to the
      committed one.
- [x] AC4: `hitopsr_instructions$options$value` and `hitopbr_instructions$options$value`
      in `R/sysdata.rda` are integer, `pid_instructions$options$value` stays integer, and
      each of the four internal instruction objects is otherwise `identical()` to the
      merge base's.
- [x] AC5: The retype moves no distributed artifact the package builds as flat text:
      for each latest `hitop_artifacts` row per file whose `format` is `qualtrics` or
      `redcap` and whose file `data-raw/artifacts.R` builds from a `generate_*`
      function, a fresh build from the retyped internal data reproduces the committed
      artifact's flat text byte for byte — the `.txt` files whole, the REDCap
      archive's data dictionary read out of the zip. `hitophsum_qualtrics.qsf` is
      outside that check because the package does not build it — it is a Qualtrics
      API export produced by `devel/qualtrics_hitophsum.R` — and the DOCX rows stay
      out of scope; the manifest md5 lock already in `test-artifacts.R` holds the
      committed bytes of all of them. The comparison is a no-regression lock over
      content, not a probe of the type axis: the generators render a response value
      through `as.character()`, so a build from doubled internal data emits the same
      bytes.
- [x] AC6: No scored value moves: for each of the three `ku_*` datasets, every
      `score_hitopsr()`, `score_hitopbr()`, `score_pid5()`, `validity_pid5()`,
      `reliability_hitopsr()`, `reliability_hitopbr()`, `reliability_pid5()`,
      `interval_hitopsr()`, `interval_hitopbr()` and `norm_pid5()` call the dataset
      admits, run with `append = FALSE`, returns an object `identical()` to the same call
      against the merge-base copy of that dataset.
- [x] AC7: Every object whose source is committed to the repo is reproduced by re-running
      its `data-raw/` script: `ku_pid5sf` from `data-raw/ku_pid5sf.csv`,
      `hitophsum_choices` from `data-raw/hitophsum_choices.csv`, and the four internal
      instruction objects from `data-raw/sysdata.R`, each saved-and-reloaded object
      `identical()` to the committed one.
- [x] AC8: `devtools::test()`, `devtools::document()` with no diff, and
      `devtools::check()` at 0 errors / 0 warnings / 0 notes.

## Coverage

- AC1 → T3, T5
- AC2 → T5
- AC3 → T6
- AC4 → T4, T6
- AC5 → T7
- AC6 → T7
- AC7 → T2, T3, T4
- AC8 → T8

## Tasks

- [x] T1: Get the two `Y:/VIDAS/Study1/` source files (`study1_items.csv`,
      `qualtrics_2026-02-26.csv`) reachable from the checkout; log the date and the path
      used. The milestone is `blocked` until they are.
- [x] T2: Add `readr` column types to the two response-column reads in
      `data-raw/ku_data.R` (`:13` and `:69`) — response columns as `col_integer()`, the
      rest unchanged; `:4` reads an item-text lookup and needs none — and re-run the
      script against the real sources, regenerating `ku_hitopsr`, `ku_hitopbr` and
      `ku_pid5sf`. `ku_hitopbr` is derived from `ku_hitopsr`
      (`data-raw/ku_data.R:47-66`), so it inherits the type; check that it did.
- [x] T3: Add `col_integer()` for `Value` to `data-raw/hitophsum_info.R:11` and re-run,
      regenerating `hitophsum_choices`.
- [x] T4: Change `value = c(1, 2, 3, 4)` to `1:4` in the two HiTOP blocks of
      `data-raw/sysdata.R` (`:27-42`) and re-run, regenerating `R/sysdata.rda`.
- [x] T5: Extend `tests/testthat/test-item-number-type.R`: empty the expected set, add
      the non-empty-domain assertion, and add the four response-column plants (AC2),
      each paired against the shipped object. Rename the file's header comment to the
      wider promise it now carries.
- [x] T6: Extend `tests/testthat/test-item-number-merge-base.R` to the four retyped
      datasets, and add a `merge_base_sysdata()` loader to
      `tests/testthat/helper-merge-base.R` — `merge_base_object()` reads only
      `data/<name>.rda` (`helper-merge-base.R:60`), and `R/sysdata.rda` holds all four
      objects in one file.
- [x] T7: Add the artifact and scoring no-move tests (AC5, AC6), building the Qualtrics
      and REDCap exports from the retyped internal data and comparing flat text against
      the committed artifacts, and running each scoring entry point against both the
      committed and merge-base copies of each `ku_*` dataset. The artifact test names
      `hitophsum_qualtrics.qsf` as its single exemption and fails if the exempt set
      grows.
- [x] T8: `NEWS.md` entry; `DECISIONS.md` entry recording the retype and the pre-1.0
      waiver on the D-056 precedent; run the profile's verify and check slots.

## Work log

- 2026-09-02: created by /milestone-plan; absorbs the `ku_*` response-column candidate row (lineage: M081 plan gate and review finding 8).
- 2026-09-02: criteria audit ran in full mode ([O] fresh reader, user-facing tier) and
  returned nine findings. Six fixed before writing: AC5's `append = TRUE` vignette calls
  could never be `identical()` after a retype; AC6's re-run clause was defeated by
  readr's `problems` external pointer; AC6's claim about two HiTOP `col_integer()` reads
  was factually wrong (one reads an item-text lookup, `ku_hitopbr` has no reader); AC4
  quantified over 39 manifest rows where 24 files exist and mandated a disproportionate
  DOCX-parse sweep; AC1 carried a clause binding the test file's hand-authored expected
  list; AC1 named no probe for its new domain. One became a task (T6: `merge_base_object()`
  cannot reach `R/sysdata.rda`). Two went to the gate.
- 2026-09-02: plan gate chose retyping `hitophsum_choices$Value` over exempting it,
  because the audit showed the exemption contradicted the milestone's own promise and
  left one kind of response value stored two ways; falsified by a citable HiTOP-HSUM
  source requiring a non-integer response value.
- 2026-09-02: plan gate chose rebuilding all four objects from their real sources over
  an in-place conversion of the two drive-sourced datasets, because Jeff can supply the
  `Y:` files temporarily and a real re-run keeps `data-raw/ku_data.R` reproducing what is
  committed; falsified by the sources proving unreachable, which sends the two datasets
  to an in-place conversion recorded as a candidate row.
- 2026-09-02: plan gate chose an outright retype over a deprecation cycle, on the D-056
  precedent and the pre-1.0 waiver; falsified by a user depending on the double type,
  which no scored result or artifact byte exposes.
- 2026-09-03: T1 — the two `Y:/VIDAS/Study1/` sources are readable at `/Volumes/general/DATASETS/VIDAS/Study1/` (read-only mount); `data-raw/ku_data.R` keeps its `Y:` path and was run this session against the mounted equivalent.
- 2026-09-03: T2 — `data-raw/ku_data.R` would not re-run: `d9acc49c` put trailing periods on seven `hitopsr_items$Text` entries the export's item lookup lacks, so the text join matched 398 of 405 and the final `select()` errored. The join now matches on the text with any terminal period removed and is declared one-to-one; 405 unique keys on each side.
- 2026-09-03: T2 stopped before regenerating any dataset. The re-run showed the shipped `ku_hitopsr`'s 405 item columns are a permutation of the correct item mapping: every rebuilt column matches exactly one committed column and only 2 of 405 are in place. Three checks agree — the committed `ku_hitopbr` cannot be derived from the committed `ku_hitopsr` (0 of 45 columns) though the script derives it from exactly that; the committed `ku_hitopbr` is reproduced column for column by the rebuild (45 of 45); and scale alpha runs 0.441 median (min -0.08) on the committed `ku_hitopsr` against 0.793 (min 0.351) rebuilt, beside 0.806 on the committed `ku_hitopbr`. `ku_hitopbr` and `ku_pid5sf` rebuild identical to what is committed apart from the type.
- 2026-09-03: T2 left unticked and the working tree's regenerated `data/*.rda` reverted; the mismapping is out of this milestone's scope and AC3 forbids moving a value, so the routing goes to the user at a gate.
- 2026-09-03: blocked on the `ku_hitopsr` mismapping. Jeff's 2026-09-03 gate: the responses were collected before the package formalized item numbering and were renumbered by hand on the assumption that the export's `hitopNNN` was item NNN; the correction goes through `/hotfix` first, and this milestone resumes on the corrected dataset so its no-value-moved criterion holds. The item-text join fix already on this branch is carried into the hotfix.
- 2026-09-03: blocker cleared — the `ku_hitopsr` mismapping shipped as the hotfix in PR #93 (`b1b523d1`), which also carries this branch's item-text join fix; `main` merged into the branch, so the merge base AC3/AC6 read against is the corrected dataset. Status back to `in-progress`; T1's sources re-checked readable at `/Volumes/general/DATASETS/VIDAS/Study1/`.
- 2026-09-03: T2 — `data-raw/ku_data.R` reads the export's response columns with `readr::col_integer()` (the column set read off the file's own header by the same `^hitop` rule the `select()` uses) and `data-raw/ku_pid5sf.csv` with the 100 `pid5sf_NNN` collectors; re-run against the mounted sources. `ku_hitopsr` 405, `ku_hitopbr` 45 (inherited, checked) and `ku_pid5sf` 100 response columns are integer, and each is `identical()` to the merge base after applying only the response-column and `spec` collector retype.
- 2026-09-03: minor amendment — T2, T3 and T4 each leave `test-item-number-type.R`'s hand-authored expected set stale by construction (it still names the response columns as known doubles), so the suite carries that one failure until T5 empties the set; the profile's verify slot is run clean at T5, not at each of the three.
- 2026-09-03: T3 — `data-raw/hitophsum_info.R` reads `Value` with `readr::col_integer()`; re-run. `hitophsum_choices$Value` is integer and the object is `identical()` to the merge base after applying only that column's and its `spec` collector's retype; `hitophsum_items.rda` re-saved unchanged.
- 2026-09-03: T4 — the two HiTOP blocks of `data-raw/sysdata.R` write `value = 1:4`; re-run. All three instruction objects carrying options now hold an integer `value` (`pid_instructions` already did), and each of the four is `identical()` to the merge base's after applying only that retype.
- 2026-09-03: T5 — the sweep's expected set is now empty; the domain test additionally names the three `ku_*`, the five `sim_*` and `hitophsum_choices`; four response-column plants added (`ku_hitopsr$hsr_001`, `ku_hitopbr$hbr_01`, `ku_pid5sf$pid5sf_001`, `hitophsum_choices$Value`), each asserting the planted copy yields exactly that path and the shipped object yields nothing there. File header rewritten to the whole-number promise. Suite 0 failures / 17,244 passes.
- 2026-09-03: T6 — `merge_base_sysdata()` added to `helper-merge-base.R` (reads `R/sysdata.rda` whole, since it holds all four instruction objects). `test-item-number-merge-base.R` gains the four response objects — retype-only reproduces the committed object for each — a pin that `expect_equal()` passes where `identical()` fails on `ku_hitopsr$hsr_001`, the four instruction objects' retype-only comparison with a guard that the retype was not a no-op, and a type assertion on the three shipped `options$value`. The new blocks run rather than skip; the M081 blocks skip as designed. Suite 0 failures / 17,362 passes.
- 2026-09-03: substantive amendment to AC5, accepted at the mini gate. As planned it required a fresh build of every `qualtrics`/`redcap` manifest row, but `hitophsum_qualtrics.qsf` has no builder in the package — it is the Qualtrics API export `data-raw/artifacts.R` deliberately does not rebuild. AC5 now promises the flat-text rebuild only over the rows `data-raw/artifacts.R` builds, states the md5 lock as what holds the rest, and records that the rebuild is a no-regression lock over content rather than a probe of the type axis; the exemption-set assertion moved to T7.
- 2026-09-03: the amended AC5 wording went to a fresh-context [O] reader (full mode, user-facing tier) before it was written. Five findings: the headline quantified over all 24 artifacts where 11 are rebuild-checked; the exemption-registry clause bound the test harness rather than the package; "a `generate_*` function in the package builds" named no procedure deciding membership; "held by the md5 lock alone" overstated the lock. Four fixed in the wording above. The fifth asked for a planted-defect probe on the type axis; it cannot exist — a build with `hitopsr_instructions$options$value` coerced to double reproduces both `hitopsr_qualtrics.txt` and the `hitopsr_redcap.zip` dictionary byte for byte (run this session), because the generators render the value through `as.character()`. That fact is stated in AC5 and in the test file rather than probed.
- 2026-09-03: T7 — `tests/testthat/test-response-value-no-move.R` added. Artifact half: the eleven `qualtrics`/`redcap` files `data-raw/artifacts.R` builds each rebuild to the committed flat text byte for byte (`.txt` whole, the zip's `instrument.csv` read out), with a test asserting `hitophsum_qualtrics.qsf` is the only manifest row without a builder here. Scoring half: one call per entry point each `ku_*` dataset admits, `append = FALSE` where the argument exists, each `identical()` between the committed dataset and the merge-base copy, guarded against a merge base that already stores integers, with the covered call set asserted rather than derived. Suite 0 failures / 17,402 passes.
- 2026-09-03: T8 — NEWS.md gains a Breaking changes entry for the response-value retype; `DECISIONS.md` gains D-060. `devtools::document()` no diff, `devtools::test()` 0 failures, `devtools::check()` 0 errors / 0 warnings / 0 notes, line-ending check clean. Status to `review`.
- 2026-09-03: review — PR #94 opened as a draft; all eight criteria executed with fresh evidence and ticked; `cairn_validate` exit 0 and the r-package consistency-gate clean.
- 2026-09-03: review — three fresh-context reviewers; the two Sonnet lenses returned no findings, the [O] diff-bug lens returned eleven. Five fixed on the branch (all test-only, in the two files this milestone added or extended), one rejected as the accepted AC5 amendment, three sent to a follow-up candidate row, one half-correct and surfaced at the gate. None met the return floor.
- 2026-09-03: review — merge gate: Jeff directed two further fixes beyond the five taken at triage — the same loop-abandoning-skip repair on the sibling M081 item-number block, and `hitophsum_choices`'s `@format` row count (42 stated, 185 actual). Both applied; suite, `document()` and `check()` re-run clean.
- 2026-09-03: step-7 approval: PR #94 approved for merge.
- 2026-09-03: CI wait hit the harness ceiling with 6 of 8 checks pending (line endings and pkgdown passed); watcher stopped, merge not attempted. Resume with /milestone-review M086.
- 2026-09-03: resume — PR #94 read OPEN with every criterion ticked against recorded evidence and the step-7 approval on record, so review re-entered at route (c): step 1 re-run (`origin/main` at `b1b523d1`, branch 0 behind — no re-verification needed), branch pushed, PR conversation read empty (no reviews, no comments, no unresolved threads), step-7 chip re-posed.
- 2026-09-03: step-7 approval: PR #94 approved for merge (re-posed at resume).

## Decisions

## Review

Reviewed 2026-09-03 on `m086-response-value-integers` at `6bd80fb6`, against
`origin/main` at `b1b523d1` (branch 11 ahead, 0 behind — no merge needed).
PR: https://github.com/jmgirard/hitop/pull/94.

### Acceptance-criterion evidence

- AC1 — `test-item-number-type.R`, 21 passes / 0 failures / 0 skips.
  "no shipped dataset stores a whole number as a bare double" reports an empty
  path set; "the sweep runs over the shipped datasets, keying and response data
  included" asserts the domain non-empty and names the eight example datasets
  and `hitophsum_choices`.
- AC2 — same file: four response-column plants
  (`ku_hitopsr$hsr_001`, `ku_hitopbr$hbr_01`, `ku_pid5sf$pid5sf_001`,
  `hitophsum_choices$Value`), 2 passes each — the planted copy yields exactly
  that path, the shipped object yields nothing there.
- AC3 — `test-item-number-merge-base.R`, "retyping the response columns moved
  nothing else" 105 passes over the four objects, plus "the response retype is
  the change, not an equality that hides it" (4 passes) guarding against a
  merge base that already stored integers. The two skips in this file are the
  M081 item-number blocks, skipping as designed.
- AC4 — same file: "the shipped instruction option values are integers" (4
  passes) and "retyping the instruction option values moved nothing else" (5
  passes) over the four `R/sysdata.rda` objects, read through the new
  `merge_base_sysdata()` helper.
- AC5 — `test-response-value-no-move.R`, "a fresh build reproduces every
  committed flat-text artifact" 24 passes over the eleven built
  `qualtrics`/`redcap` files, plus "every flat-text manifest row but the
  API-built QSF has a builder here" (2 passes) locking the exemption set.
  `test-artifacts.R`'s md5 manifest lock ran clean at 122 passes.
- AC6 — same file: "no scored value moved when the response columns became
  integers", 10 passes — one call per entry point each `ku_*` dataset admits
  (3 + 3 + 4), each `identical()` against the merge-base copy — plus "the
  scoring comparison covers every entry point each dataset admits" (4 passes).
- AC7 — run this session from `scratchpad/ac7.R`: `ku_pid5sf` re-read from
  `data-raw/ku_pid5sf.csv`, `hitophsum_choices` and `hitophsum_items` from their
  committed CSVs, and the four instruction objects from `data-raw/sysdata.R`,
  each saved and reloaded — `identical()` TRUE for all seven, and
  `hitophsum_choices$Value` reads back as `integer`.
- AC8 — `devtools::test()` 0 failures / 0 errors / 9 skips / 17,403 passes over
  647 files; `devtools::document()` left the tree clean;
  `devtools::check()` 0 errors / 0 warnings / 0 notes, re-run after the fix-now
  test edits and again after the two gate-directed fixes below (all three runs
  0/0/0).

### Consistency gate

- `cairn_validate.py` exit 0, all checks passed; 42 advisories (work-log
  wrapping, dangling pre-migration D-ids, a references-staleness note, and a
  sizing tripwire on this milestone's 8 criteria) — none a gate failure.
- No `DESIGN.md` principle changed, so `cairn_impact.py` was not run.
- Profile `consistency-gate`: `document()` no diff; `NAMESPACE`/`man/`/`data/`
  regenerate from roxygen and `data-raw/`; README.md newer than README.Rmd and
  in sync; `pkgdown::check_pkgdown()` "No problems found"; NEWS.md carries a
  Breaking changes entry for the retype; no new top-level files, so no
  `.Rbuildignore` change; line-ending policy check passed.

### Independent review

Three fresh-context reviewers, distinct evidence bases (user-facing tier).

- [S] blame-history: no findings. Confirmed the PR #93 item-text join fix
  survives untouched, the M081 test files were extended rather than weakened,
  and D-060 matches the change.
- [S] prior-review record: no findings. The one prior-review record on this
  subject (LESSONS M081, on asymmetric coercion inside `identical()` and on
  guards that probe only a top-level column) is not re-violated. The GitHub
  inline-comment probe returned `[]`, so the PR-thread walk was skipped.
- [O] diff-bug: eleven findings, ranked. Triage below; nothing met the return
  floor — no finding demonstrated an acceptance criterion failing, and none was
  a defect in what the package does for its users.

Finding log (rank order, with disposition):

1. `skip_if()` called inside a `for` loop ends the whole `test_that()` block,
   not the iteration, so a per-object skip would abandon every later object
   silently (`test-item-number-merge-base.R:130`,
   `test-response-value-no-move.R:164`). **Fixed now.** Verified independently:
   a three-iteration loop skipping at the second records 1 pass where 4 are
   expected. Both blocks now read every merge-base object first and decide the
   skip once over all of them, and each asserts by name that every object moved.
   Planted a non-moving object: all three are still compared and the guard
   fails, where the old shape skipped whole. No live coverage was lost — all
   four objects were double at the merge base, so the loops ran whole this run.
2. `ku_pid5sf`'s id column is `response_id`, not `participant`/`biosex`, so the
   scoring test's vacuity guard left a character column in the set and could
   never fire for that dataset (`test-response-value-no-move.R:168`).
   **Fixed now** — the response columns are named from the instrument's naming
   rule (`pid5sf_%03d`), the same set the scoring calls use.
3. One global `changed` flag covered four instruction objects, so one of the two
   retyped objects failing to move would be masked by the other
   (`test-item-number-merge-base.R:181-192`). **Fixed now** — a per-object list
   asserted with `expect_setequal()` against the two objects that must move.
4. AC5's artifact half cannot fail on the type axis, and duplicates
   `test-artifacts.R`'s md5 lock. **Rejected** — this is the amendment accepted
   at the T7 mini gate and stated in AC5's own text; it is an intentional
   no-regression content lock, not a probe of the type axis.
5. `grep("^hitop", ...)` is case-sensitive where `dplyr::starts_with("hitop")`
   is not, so the comment in `data-raw/ku_data.R:39-47` overstates the
   equivalence. **Follow-up** — no effect on the current run (405 integer
   `hsr_*` columns, values 1-4).
6. `col_integer()` coerces an unparseable value to `NA` and records it in
   `problems()`, which `data-raw/ku_data.R` never asserts empty.
   **Follow-up** — a data-raw robustness gap, not a defect in what shipped.
7. `ku_pid5sf`'s collectors are hardcoded as `pid5sf_%03d` where the HiTOP-SR
   read derives its set from the file header; `readr` only warns on a collector
   matching no column. **Follow-up**, same row as 5 and 6.
8. IP1 names response options as instrument content wherever they live,
   including `R/sysdata.rda`, but the header lists only GP2/GP3.
   **Surfaced at the gate** — the change is storage type, not content (the
   values stay 1-4), on the D-028 precedent that a non-content change to
   instrument material is not an IP1 content change; Jeff's plan-gate sign-off
   is logged. The finding's second half — that the milestone's `## Decisions`
   section is empty while D-060 exists — is incorrect: D-060 is cross-cutting
   and belongs in `DECISIONS.md`.
9. `expect_null(hitophsum_instructions$options)` relies on `$` partial matching.
   **Fixed now** — `expect_false("options" %in% names(...))`.
10. A redundant `expect_setequal()` pair in the exemption-set test.
    **Fixed now** — the subsumed first assertion removed (a one-line tidy taken
    while the file was open).
11. `hitophsum_choices`'s roxygen `@format` says "42 rows and 3 columns"; the
    object has 185 rows (`R/data.R:350`). **Fixed at Jeff's direction** — see
    below.

Fix-now work is test-only, in the two files this milestone added or extended;
no package code changed. Both files re-run clean (`test-item-number-merge-base.R`
106 + 4 + 4 + 5 passes, 2 designed M081 skips; `test-response-value-no-move.R`
1 + 24 + 4 + 11 passes, 0 skips).

### Gate-directed fixes

At the merge gate Jeff directed two further fixes beyond the five taken above.

- The sibling M081 item-number block carried the same loop-abandoning
  `skip_if()` shape (`test-item-number-merge-base.R:72-96`) and is repaired the
  same way, so the file no longer holds two contradictory idioms. It still
  reports one clean skip on this branch — all eight item-number objects are
  already integer at the merge base — now for the aggregate reason rather than
  by abandoning at the first object. Discrimination shown by planting M081's own
  merge base (`5636f24e^`): all eight objects are reachable there and all eight
  moved, so the repaired loop runs whole where the old shape would have stopped
  at the first.
- `hitophsum_choices`'s `@format` said 42 rows against an object of 185
  (`R/data.R:351`); corrected, `document()` re-run, and a NEWS entry added under
  Documentation and website. The other twenty shipped datasets' `@format` row
  counts were read off their objects in the same pass and all match.

After both: `devtools::test()` 0 failures / 0 errors / 9 skips / 17,403 passes,
`devtools::document()` no diff, `devtools::check()` 0 errors / 0 warnings /
0 notes.
