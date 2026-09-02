# M080: The shipped PID-5 datasets name item columns as the online exports do

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Resolves:** —
- **Branch/PR:** `m080-pid5-item-column-names` / [#86](https://github.com/jmgirard/hitop/pull/86)

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

- [x] AC1 `names(sim_pid5)` is `pid5_001`..`pid5_220`, `names(sim_pid5sf)` and
      the item columns of `ku_pid5sf` are `pid5sf_001`..`pid5sf_100`, and
      `names(sim_pid5bf)` is `pid5bf_01`..`pid5bf_25`, each in ascending item
      order and each compared against a vector built from the matching
      `pid_items` version column, never read off the dataset under test.
- [x] AC2 Each of the four objects carries the same columns, in the same order,
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
- [x] AC3 Re-running the generating code reproduces the shipped names:
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
- [x] AC6 `devtools::document()` leaves no diff, `devtools::test()` passes, and
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
- 2026-09-02: review opened; draft PR #86 pushed, CI running.
- 2026-09-02: review gate failed, status back to in-progress. AC4: `git grep -nE 'pid_[0-9]' -- . ':!cairn/'` returns one hit, `data-raw/check_pid_item_names.R:10`, where the criterion requires none. AC5: the NEWS entry gives the old spelling for the full and short forms but not the brief form. AC1, AC2, AC3, AC6 and the consistency gate pass on fresh evidence; five review findings logged, F2 (a lost order-guard probe) and F3 (an unguarded `spec` rename) proposed as fix-now.

## Decisions

## Review

Reviewed 2026-09-02 against PR #86. Evidence below is fresh, gathered this
session by command; AC4 and AC5 fail as written, so the milestone returns to
`in-progress`.

### Acceptance criteria

- **AC1 — pass.** Expectations built from `pid_items` in a session that never
  reads the names off the datasets: `sprintf("pid5_%03d", sort(pid_items$FULL))`
  (220 names, `pid5_001`..`pid5_220`), `sprintf("pid5sf_%03d", sort(pid_items$SF))`
  (100, `pid5sf_001`..`pid5sf_100`), `sprintf("pid5bf_%02d", sort(pid_items$BF))`
  (25, `pid5bf_01`..`pid5bf_25`). `identical()` holds against `names(sim_pid5)`,
  `names(sim_pid5sf)`, `names(ku_pid5sf)[-1]` and `names(sim_pid5bf)`;
  `ku_pid5sf`'s first column is `response_id`. `sort(pid_items$FULL)` is
  `seq_len(220)`, so the order is ascending item order.
- **AC2 — pass.** Against the objects at the pinned commit `d3ac6695`, the
  column comparison `identical(unname(lapply(new, identity)), unname(lapply(old,
  identity)))` holds for all four. Attributes other than `names` are `identical()`
  for the three `sim_*` objects, whose only attributes are `class`, `row.names`
  and `names` — none recording item names. `ku_pid5sf` matches on every attribute
  but `names` and `spec`; its `spec$cols` names now equal its column names, where
  the pinned object's second `spec` entry was `pid_1`. Four plants, each applied
  to the renamed object immediately before its assertion, all red: a changed cell
  and a dropped column on `sim_pid5bf`, a reordered pair of columns on
  `ku_pid5sf` (all three FALSE against the column comparison), and the pinned
  `spec` restored on `ku_pid5sf` (FALSE against the attribute check). Re-reading
  the renamed `data-raw/ku_pid5sf.csv` with `readr::read_csv()` reproduces the
  shipped object's names, columns and `spec` identically.
- **AC3 — pass.** `data-raw/sim_pid.R` sourced in full in a clean environment
  yields names equal to `item_names()` on the matching `pid_items` column for all
  three `sim_*` objects, and the objects themselves are `identical()` to the
  shipped ones. The two PID-5 statements of `data-raw/ku_data.R` (lines 72-73),
  evaluated on their own, yield `ku_pid5sf` item names equal to
  `item_names("pid5sf_", sort(pid_items$SF))`. (`usethis::use_data()` rewrote
  `data/` during the run; the four files were restored with `git checkout` and
  the tree is clean. Only the container bytes differed — the loaded objects
  compared identical.)
- **AC4 — FAIL.** The second procedure passes: `data-raw/check_pid_item_names.R`
  reports 0 hits over 24 object files and 198 archive members, and the same
  collector run against the four pinned objects finds 445 old names, so the
  domain is not empty. The first procedure does not.
  `git grep -nE 'pid_[0-9]' -- . ':!cairn/'` returns one hit:
  `data-raw/check_pid_item_names.R:10`, a comment spelling `pid_1`..`pid_100`
  while describing the defect found mid-implementation. The criterion requires
  it to return nothing. No PID-5 item column carries the old name — the headline
  promise holds — but the procedure the criterion names is not empty. Run across
  the branch: `35f8a6d3` clean, `badd41a5` one hit, `d7614828` one hit; the hit
  entered with the sweep script, one commit before the work-log line claiming
  "none now".
- **AC5 — FAIL.** The `NEWS.md` Breaking-changes entry names all four datasets,
  the new spelling for each of the three forms, the Qualtrics uppercase variant
  (`PID5_001`, `PID5SF_001`, `PID5BF_01` — verified against real exports from the
  three generators), and the migration idiom. It gives the old spelling for two
  forms only — `paste0("pid_", 1:220)` and `sprintf("pid_%d", 1:100)`, then "and
  the like"; the brief form's old spelling (`pid_1` to `pid_25`) appears nowhere,
  where the criterion asks for the old and new spelling for each of the three
  forms. `R/data.R`'s four `\item{}` lines and the four regenerated `man/` pages
  state the new names.
- **AC6 — pass.** `devtools::document()` leaves no diff (tree clean afterwards).
  `devtools::test()` exits 0 with no failures and 7 skips, all pre-existing
  merge-base and keying skips (`test-column-shape.R`, `test-keying.R`,
  `test-scale-name-hitopsr.R`); none is PID-5 related. `R CMD check`: Status OK,
  0 errors, 0 warnings, 0 notes, so no NOTE is added over the merge base.

### Consistency gate

`cairn_validate.py` exit 0, all checks PASS; 24 advisories, all pre-existing
(dangling D-ids inherited from the legacy migration, one references-staleness
note on a page this milestone does not touch). The `release window` advisory did
not fire. No `DESIGN.md` principle changed, so `cairn_impact.py` was skipped.
Toolchain gate (`r-package` profile): `document()` no diff; generated files not
hand-edited; `README.Rmd` untouched by the branch; `pkgdown::check_pkgdown()`
reports no problems; `NEWS.md` carries the user-visible entry; `data-raw/` is
already in `.Rbuildignore`; `check()` clean. Draft PR #86 CI was re-triggered by
the review commit and is pending — not waited on, as the milestone returns.

### Independent review

Three fresh-context lenses, none having authored the work.

- **[S] blame-history — no findings.** Verified that every modified line changes
  only the string literal building column names, leaving the indices, values and
  assertions each line was introduced for intact.
- **[S] prior-review record — no regressions.** The GitHub inline-comment probe
  returned empty, so the archived `## Review` sections were the evidence base.
  M077's two findings (migration examples covering only some old spellings; docs
  mis-crediting the Qualtrics export with the lowercase pattern) are not
  repeated. Raised F5 below as a shared observation.
- **[O] diff-bug — five findings, no correctness bug in shipped code.**

Findings, as reported and ranked, with disposition:

1. **F1 — AC4's archive half lists member names, never member contents.** The six
   `*_redcap.zip` files each hold one member, `instrument.csv`, whose field names
   are never read; `git grep` cannot see inside the zip either, so the one binary
   artifact recording item variable names is scanned by neither procedure. Of the
   198 members counted, 192 are `.docx` internal part names and 6 are those
   `instrument.csv` entries; the `.txt`/`.qsf` files hit the `tryCatch` and
   contribute 0 silently (they are reached by the text search, so no gap there).
   Confirmed at review by listing every archive. Not a live defect: the three
   PID-5 `instrument.csv` files were unzipped and read at review and carry
   `pid5_001`..`pid5_220`, `pid5sf_001`..`pid5sf_100` and `pid5bf_01`..`pid5bf_25`
   with no old-pattern hit. The criterion says "lists the members", which the
   script does, so this is criterion strength, not an AC failure. Disposition:
   maintainer's call at the next gate.
2. **F2 — the fixture rename removed the suite's only unpadded ascending probe.**
   `test-item-guards.R` previously asserted `expect_no_warning(score_pid5(bf,
   items = paste0("pid_", 1:25)))`; every probe now in the file is either
   zero-padded or stops at 9. Reproduced independently at review: a mutant of
   `warn_item_order()` comparing trailing digits lexicographically instead of
   `as.integer()` survives `pid5bf_%02d` 1:25, `pid5_%03d` 1:220, `hsr_%03d` 1:10
   and `q_1..q_3`, and is killed only by an unpadded run past item 9. A user with
   unpadded columns would then get a spurious ascending-order warning with the
   suite green. Disposition: fix-now candidate in the return.
3. **F3 — the rename script's own assertions do not cover the `spec` rename.**
   `strip()` in `data-raw/rename_pid_item_columns.R` drops `spec` from the
   attribute comparison, and `names(spec$cols)[is_item] <- renamed` assumes
   `spec$cols` matches `names(old)` in order and length. If that ever failed, the
   new names would land on the wrong `spec` slots and all four `stopifnot()`s
   would still pass. One added assertion closes it. Disposition: fix-now
   candidate in the return.
4. **F4 — nothing guards `ku_pid5sf`'s `spec` in CI.** The new tests check
   `names()` only, and `check_pid_item_names.R` is run by no workflow
   (`R-CMD-check.yaml` runs only `check_line_endings.R`). A regeneration from a
   stale CSV would leave `names()` right and the suite green — the same defect
   class the work log records finding by hand. Disposition: fix-now candidate or
   follow-up row.
5. **F5 — `data-raw/sim_pid.R` reimplements the padding rule.** `sprintf("%s%0*d",
   prefix, nchar(as.character(n_items)), 1:n_items)` pads to the item *count*
   where `item_names()` pads to the largest item *number*; the two agree only
   because count equals max number on all three forms. Raised by both the [O] and
   the prior-review lens. Disposition: style/DRY, follow-up.

### Outcome

Two criteria fail as written, so the milestone returns to `in-progress` rather
than reaching the merge gate. Both failures are small and local: one comment
line spelling a literal the criterion's own search forbids, and one missing old
spelling in the NEWS entry. F2 and F3 are worth folding into the same return.
