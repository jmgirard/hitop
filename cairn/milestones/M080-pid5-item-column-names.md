# M080: The shipped PID-5 datasets name item columns as the online exports do

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m080-pid5-item-column-names`

## Goal

The four shipped PID-5 datasets name their item columns the way this package's
own REDCap export writes them, so data fielded through that export scores
without renaming and every PID-5 example uses one selection idiom.

## Scope

Surface tier: **user-facing** — the item-column names are documented dataset
columns and appear in every PID-5 vignette. This completes for the PID-5 what
M077 settled for the HiTOP instruments; the pattern and Jeff's pre-1.0
deprecation waiver are recorded as a `DECISIONS.md` entry in the plan commit.

**In:** renaming the item columns of `sim_pid5`, `sim_pid5sf`, `sim_pid5bf` and
`ku_pid5sf` to `pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100` and
`pid5bf_01`..`pid5bf_25`; the code that regenerates them (`data-raw/sim_pid.R`,
the `data-raw/ku_pid5sf.csv` header read by `data-raw/ku_data.R`); a repo-wide
sweep of the old `pid_<n>` spelling across vignettes, articles, roxygen
examples, tests and `data-raw/`; renaming the generic item-order fixtures in
`tests/testthat/test-item-guards.R` to a non-instrument stem; the dataset
roxygen, the PID vignettes' prose, and a NEWS Breaking-changes entry.

**Out:** `rename_pid5_items()` and `label_pid5()`, the PID-5 counterparts of the
HiTOP rename and label helpers — new exported behavior with its own design
questions → candidate row filed in the plan commit. The scoring functions'
`prefix = "pid_"` default for *output* columns is unchanged (D-026), so items
and scores deliberately carry different stems on this instrument. The HiTOP
datasets are already done (M077). The Qualtrics export's uppercase variant
(`PID5_001`) is documented here, not changed.

## Acceptance criteria

- [ ] AC1 `names(sim_pid5)` is `pid5_001`..`pid5_220`, `names(sim_pid5sf)` and
      the item columns of `ku_pid5sf` are `pid5sf_001`..`pid5sf_100`, and
      `names(sim_pid5bf)` is `pid5bf_01`..`pid5bf_25`, each in ascending item
      order and each compared against a vector built from the matching
      `pid_items` version column, never read off the dataset under test.
- [ ] AC2 Each of the four objects carries the same columns, in the same order,
      as the object at the commit hardcoded in
      `data-raw/rename_pid_item_columns.R`: `identical(unname(lapply(new,
      identity)), unname(lapply(old, identity)))` holds for all four. Every
      attribute other than `names` is `identical()` to the pinned object's,
      except `ku_pid5sf`'s readr `spec`, whose recorded column names are renamed
      with the columns; that check also shows the three `sim_*` objects carry no
      attribute recording item names. Four plants, each applied to the renamed
      object immediately before the assertion it targets, are shown red: a
      changed cell, a dropped column and a reordered pair of columns against the
      column comparison, on `sim_pid5bf` and on `ku_pid5sf`; an un-renamed
      `spec` against the attribute check. And `ku_pid5sf` as the script writes
      it carries the same names, `spec` and columns as the object
      `readr::read_csv()` builds from the renamed `data-raw/ku_pid5sf.csv` —
      whole-object `identical()` is unreachable across a save and load, a readr
      tibble's `problems` attribute being an external pointer.
- [ ] AC3 Re-running the generating code reproduces the shipped names:
      `data-raw/sim_pid.R` executed in full, and the two PID-5 statements of
      `data-raw/ku_data.R` evaluated on their own (its earlier lines read a KU
      network drive), each yield names equal to `item_names()` applied to the
      matching `pid_items` version column.
- [ ] AC4 No PID-5 item column is named by the old pattern in any tracked file.
      Two procedures enumerate the domain: `git grep -nE 'pid_[0-9]' -- .
      ':!cairn/'` returns nothing, and a second pass that `load()`s every
      `data/*.rda` and `R/sysdata.rda` and lists the members of each
      `inst/extdata/` archive finds no `pid_<n>` name. Both are shown returning
      hits at the pre-rename commit, so neither passes over an empty domain.
- [ ] AC5 `NEWS.md` carries a Breaking-changes entry naming the four datasets,
      the old and new spelling for each of the three forms, the Qualtrics
      uppercase variant, and how a caller migrates an `items =` selection;
      `R/data.R`'s four dataset `\item{}` lines and the regenerated `man/`
      pages state the new names.
- [ ] AC6 `devtools::document()` leaves no diff, `devtools::test()` passes, and
      `R CMD check` reports 0 errors and 0 warnings with no NOTE that is not
      also present at the merge base.

## Coverage

- AC1 → T1, T2, T3
- AC2 → T1
- AC3 → T2
- AC4 → T4, T5, T7
- AC5 → T6, T7
- AC6 → T6, T7

## Tasks

- [x] T1 Write `data-raw/rename_pid_item_columns.R` on the shape of
      `data-raw/rename_item_columns.R`: load each of the four objects from the
      pinned pre-rename commit, rename item columns from the trailing integer,
      assert whole-object identity, save with `usethis::use_data()`. Run the
      planted one-cell perturbation and record that the assertion fails.
- [x] T2 Rename the item columns in the `data-raw/ku_pid5sf.csv` header
      (line 1) and the `colnames()` line in `data-raw/sim_pid.R:8` so the next
      run builds the same names; verify each against `item_names()` on
      `pid_items`.
- [x] T3 Extend `tests/testthat/test-data-item-names.R` with the four PID-5
      datasets, expectations built from `pid_items`.
- [x] T4 Run both AC4 procedures at the pre-rename commit, record their hits,
      and move every PID-5 item selection they name to the new names —
      including `data-raw/characterize_calc_se.R:49-58`, `R/score_pid5.R:120`,
      the three PID scoring vignettes and the test files.
- [x] T5 Rename the generic item-order fixtures in
      `tests/testthat/test-item-guards.R` (lines 13, 20, 24, 28, 116) to a
      non-instrument stem, so no `pid_<digit>` name survives anywhere.
- [x] T6 Update `R/data.R`'s four dataset `\item{}` lines and the PID
      vignettes' prose naming the columns; run `devtools::document()`.
- [x] T7 Write the NEWS Breaking-changes entry; run `devtools::test()` and
      `R CMD check`; re-run both AC4 procedures green.

## Work log

- 2026-09-02: created by /milestone-plan; promotes the PID-5 half of the M044 naming candidate row that M077 left standing (D-052).
- 2026-09-02: criteria audit ran in full mode (user-facing tier; [O] fresh-context reader) — findings on AC2 (2), AC3 (3), AC4 (5), AC5, AC6 and T4; ten fixed in the draft before the gate, the test-fixture disposition posed as a gate question.
- 2026-09-02: plan gate chose the export's per-form stems over one shared `pid5_` stem and over padding the scoring stem `pid_`, because the three forms number their items differently and a shared stem would give one name to different items; falsified by users fielding more than one PID-5 form into a single table and finding three stems harder to work with than one.
- 2026-09-02: plan gate chose a one-step rename over a one-release dual-column deprecation, Jeff waiving the cycle as pre-1.0 (the D-052 precedent); falsified by a downstream package pinned to the old column names.
- 2026-09-02: plan gate chose renaming the generic item-order fixtures to a neutral stem over allowlisting them in AC4, because an empty exemption list cannot drift; falsified by the neutral stem colliding with a real column name in a later test.
- 2026-09-02: plan gate chose leaving the PID-5 rename and label helpers out over shipping them here, because they are new exported behavior with their own design questions; falsified by a user reporting they cannot label PID-5 data at all.
- 2026-09-02: branch `m080-pid5-item-column-names` cut from `main` at d3ac6695; that commit is the pre-rename pin `data-raw/rename_pid_item_columns.R` loads from.
- 2026-09-02: minor amendment — T1, T2, T4 and T5 are executed and committed as one migration step, because renaming the datasets apart from their call sites leaves the suite red between commits and no per-task check could run clean. T3, T6 and T7 keep their own commits.
- 2026-09-02: implement gate chose keeping AC4's wording and recording a supplementary search for *built* old names (`paste0("pid_", 1:220)`, `sprintf("pid_%d", 1:100)`) as extra evidence, over amending the criterion — AC4's first sentence already binds the whole domain and every such site moves either way; Jeff left the call to the session.
- 2026-09-02: implement gate chose `q_` as the neutral stem for the generic item-order probes, over `item_` (collides with the package's own vocabulary) and `v_` (says nothing).
- 2026-09-02: checkpoint — the migration, the new `test-data-item-names.R` cases, the dataset roxygen, the regenerated `man/` pages and the NEWS entry are on the branch; `devtools::test()` was still running when this commit was made, so no task box is ticked yet.
- 2026-09-02: defect found verifying AC3 — `ku_pid5sf` is a readr tibble, and the rename script left its `spec` attribute recording the 100 old names `pid_1`..`pid_100`, invisible to a `names()`-only scan and disagreeing with what re-reading the renamed CSV builds. The script now renames those alongside the columns.
- 2026-09-02: substantive amendment to AC2, adopted at a mini gate — the criterion's `as.list()` comparison cannot tell a moved value from that `spec` rename, so it now compares the columns alone, binds every other attribute to the pinned object's, and names four plants instead of one. Amended wording audited in full mode by a fresh-context [O] reader that did not author it; six findings, all folded in before the text was written.
- 2026-09-02: `data-raw/check_pid_item_names.R` commits AC4's second procedure — it walks attributes as well as names, and was shown returning 100 hits against the pre-fix object and 0 across 24 object files and 198 archive members after.
- 2026-09-02: T1-T7 done. `devtools::test()`: FAIL 0, WARN 0, SKIP 7, PASS 16426 (the 7 skips are the pre-existing merge-base and keying skips). `devtools::document()` leaves no diff. `R CMD check`: Status OK, 0 errors, 0 warnings, 0 notes.
- 2026-09-02: both AC4 procedures run at d3ac6695 and now — the text search over tracked files, 18 hits then, none now; the binary sweep, 445 old names across the four objects then, 0 over 24 object files and 198 archive members now. The supplementary search for built old names went from 28 hits to 2, both in the NEWS migration instructions that AC5 asks for.

## Decisions

## Review
