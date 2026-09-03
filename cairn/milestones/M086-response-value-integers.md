# M086: Every response value the package ships is an integer

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m086-response-value-integers`

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

- [ ] AC1: No dataset the package ships stores a whole-valued number as a bare double:
      the sweep in `tests/testthat/test-item-number-type.R`, walking every dataset
      `data(package = "hitop")` lists into list-columns and nested frames, reports an
      empty set of paths, and a test asserts the sweep's domain is non-empty and
      contains the eight example datasets and `hitophsum_choices`.
- [ ] AC2: The sweep is shown able to catch a response column stored as a double: for
      each of `ku_hitopsr`, `ku_hitopbr`, `ku_pid5sf` and `hitophsum_choices`, a planted
      copy with one response column coerced back to double yields exactly that column's
      path, paired against the shipped object yielding nothing at that path.
- [ ] AC3: Nothing but the type moved: for each of `ku_hitopsr`, `ku_hitopbr`,
      `ku_pid5sf` and `hitophsum_choices`, applying only the response-column retype —
      and, where the object carries one, the matching `readr` `spec` collector retype —
      to the object at the branch's merge base yields an object `identical()` to the
      committed one.
- [ ] AC4: `hitopsr_instructions$options$value` and `hitopbr_instructions$options$value`
      in `R/sysdata.rda` are integer, `pid_instructions$options$value` stays integer, and
      each of the four internal instruction objects is otherwise `identical()` to the
      merge base's.
- [ ] AC5: The retype moves no distributed artifact: every file named by the latest
      `hitop_artifacts` row per file still matches its manifest md5, and for each row
      whose `format` is `qualtrics` or `redcap`, a fresh build from the retyped internal
      data reproduces the committed artifact's flat text byte for byte — the `.txt` and
      `.qsf` files whole, the REDCap archive's data dictionary read out of the zip.
- [ ] AC6: No scored value moves: for each of the three `ku_*` datasets, every
      `score_hitopsr()`, `score_hitopbr()`, `score_pid5()`, `validity_pid5()`,
      `reliability_hitopsr()`, `reliability_hitopbr()`, `reliability_pid5()`,
      `interval_hitopsr()`, `interval_hitopbr()` and `norm_pid5()` call the dataset
      admits, run with `append = FALSE`, returns an object `identical()` to the same call
      against the merge-base copy of that dataset.
- [ ] AC7: Every object whose source is committed to the repo is reproduced by re-running
      its `data-raw/` script: `ku_pid5sf` from `data-raw/ku_pid5sf.csv`,
      `hitophsum_choices` from `data-raw/hitophsum_choices.csv`, and the four internal
      instruction objects from `data-raw/sysdata.R`, each saved-and-reloaded object
      `identical()` to the committed one.
- [ ] AC8: `devtools::test()`, `devtools::document()` with no diff, and
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
- [ ] T2: Add `readr` column types to the two response-column reads in
      `data-raw/ku_data.R` (`:13` and `:69`) — response columns as `col_integer()`, the
      rest unchanged; `:4` reads an item-text lookup and needs none — and re-run the
      script against the real sources, regenerating `ku_hitopsr`, `ku_hitopbr` and
      `ku_pid5sf`. `ku_hitopbr` is derived from `ku_hitopsr`
      (`data-raw/ku_data.R:47-66`), so it inherits the type; check that it did.
- [ ] T3: Add `col_integer()` for `Value` to `data-raw/hitophsum_info.R:11` and re-run,
      regenerating `hitophsum_choices`.
- [ ] T4: Change `value = c(1, 2, 3, 4)` to `1:4` in the two HiTOP blocks of
      `data-raw/sysdata.R` (`:27-42`) and re-run, regenerating `R/sysdata.rda`.
- [ ] T5: Extend `tests/testthat/test-item-number-type.R`: empty the expected set, add
      the non-empty-domain assertion, and add the four response-column plants (AC2),
      each paired against the shipped object. Rename the file's header comment to the
      wider promise it now carries.
- [ ] T6: Extend `tests/testthat/test-item-number-merge-base.R` to the four retyped
      datasets, and add a `merge_base_sysdata()` loader to
      `tests/testthat/helper-merge-base.R` — `merge_base_object()` reads only
      `data/<name>.rda` (`helper-merge-base.R:60`), and `R/sysdata.rda` holds all four
      objects in one file.
- [ ] T7: Add the artifact and scoring no-move tests (AC5, AC6), building the Qualtrics
      and REDCap exports from the retyped internal data and comparing flat text against
      the committed artifacts, and running each scoring entry point against both the
      committed and merge-base copies of each `ku_*` dataset.
- [ ] T8: `NEWS.md` entry; `DECISIONS.md` entry recording the retype and the pre-1.0
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

## Decisions

## Review
