# M080: The shipped PID-5 datasets name item columns as the online exports do

- **Status:** review
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
- [x] AC4 No PID-5 item column is named by the old pattern in any tracked file.
      Two procedures enumerate the domain: `git grep -nE 'pid_[0-9]' -- .
      ':!cairn/'` returns nothing, and a second pass that `load()`s every
      `data/*.rda` and `R/sysdata.rda` and lists the members of each
      `inst/extdata/` archive finds no `pid_<n>` name. Both are shown returning
      hits at the pre-rename commit, so neither passes over an empty domain.
- [x] AC5 `NEWS.md` carries a Breaking-changes entry naming the four datasets,
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
- AC4 → T4, T5, T7, T8, T9
- AC5 → T6, T7, T8
- AC6 → T6, T7, T9

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
- [x] T8 Close the two criteria the review gate failed: reword
      `data-raw/check_pid_item_names.R:10` so AC4's text search is empty, and
      give the NEWS entry the brief form's old spelling alongside the other two.
- [x] T9 Fold in the review findings taken at the return gate: restore an
      unpadded ascending order probe past item 9 (F2), assert the readr `spec`
      rename lands on slots matching the object's own names (F3), read archive
      member contents as well as member names in the sweep script (F1), and
      assert the shipped `spec` names against the columns in the suite (F4).

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
- 2026-09-02: return gate — Jeff chose reading archive member contents (F1) and adding the read-spec guard test (F4) now, both without touching a criterion, and filing the generator's duplicated padding rule (F5) as a candidate row.
- 2026-09-02: minor amendment — T8 and T9 appended for the return's repair and the accepted findings; Coverage extended for AC4, AC5 and AC6. No criterion text changed.
- 2026-09-02: T8 — the sweep script's comment no longer spells an old name, so `git grep -nE 'pid_[0-9]' -- . ':!cairn/'` exits 1 with no output; the NEWS entry now gives all three forms' old spelling, each paired with its replacement.
- 2026-09-02: T9 — the order guard regains an unpadded ascending probe (helper level and end to end); a mutant comparing trailing digits as strings survives every padded probe and is killed by it. The rename script asserts `spec$cols` names equal the object's names before and after the rename; a planted permutation is red on `ku_pid5sf`. The sweep script now reads all 198 archive member bodies as raw bytes; searching it for the *new* pattern reports `inst/extdata/pid5_redcap.zip::instrument.csv`, so the content half is shown non-empty. A new test asserts the shipped `spec` names against `ku_pid5sf`'s columns and is red on a planted stale spec.
- 2026-09-02: candidate row filed for F5 by absorbing it into the standing M079 export-padding row; `ROADMAP.md` is at 59 lines and within 200 bytes of its budget, so the next hygiene pass has compression to do.
- 2026-09-02: return closed, status review. `devtools::test()`: FAIL 0, WARN 0, SKIP 7, PASS 16431 (the same 7 pre-existing skips). `devtools::document()` leaves no diff. `R CMD check`: Status OK, 0 errors, 0 warnings, 0 notes. AC4's text search exits 1 with no output; the sweep script reports 0 hits over 24 object files and 198 archive members with all 198 bodies read.
- 2026-09-02: second review round — all six criteria re-executed on fresh evidence and pass; consistency gate green (`cairn_validate` exit 0, `document()` no diff, `check()` 0/0/0, `pkgdown` clean). Three lenses: blame-history and prior-review no findings, [O] diff-bug eight, none an AC failure or a user-facing defect.

## Decisions

## Review

Second review round, 2026-09-02, against PR #86. The first round failed AC4 and
AC5 and returned the milestone; every criterion below is re-executed with fresh
evidence gathered this session by command against the branch tip.

### Acceptance criteria

- **AC1 — pass.** Expectations built from `pid_items` in a session that never
  reads names off the datasets: `sprintf("pid5_%03d", sort(pid_items$FULL))`
  (220, `pid5_001`..`pid5_220`), `sprintf("pid5sf_%03d", sort(pid_items$SF))`
  (100, `pid5sf_001`..`pid5sf_100`), `sprintf("pid5bf_%02d", sort(pid_items$BF))`
  (25, `pid5bf_01`..`pid5bf_25`). `identical()` holds against `names(sim_pid5)`,
  `names(sim_pid5sf)`, `names(sim_pid5bf)` and `names(ku_pid5sf)[-1]`;
  `ku_pid5sf`'s first column is `response_id`. `sort(pid_items[[v]])` equals
  `seq_len(n)` for all three versions, so the order is ascending item order.
- **AC2 — pass.** Against the objects at the pinned commit `d3ac6695`, the column
  comparison `identical(unname(lapply(new, identity)), unname(lapply(old,
  identity)))` holds for all four. The three `sim_*` objects carry only `class`,
  `row.names` and `names` — none recording item names — and are `identical()` to
  the pinned objects once `names` is dropped. `ku_pid5sf` matches on every
  attribute but `names` and `spec`; its `spec$cols` names now equal its own
  column names, where the pinned object's second `spec` entry was `pid_1`. Four
  plants, each applied to the renamed object immediately before its assertion,
  all red: a changed cell and a dropped column on `sim_pid5bf` and a reordered
  pair of columns on `ku_pid5sf` against the column comparison, and the pinned
  `spec` restored on `ku_pid5sf` against the attribute check. Re-reading the
  renamed `data-raw/ku_pid5sf.csv` with `readr::read_csv()` reproduces the
  shipped object's names, columns and `spec` identically.
- **AC3 — pass.** `data-raw/sim_pid.R` sourced in full in a clean environment
  yields names equal to `item_names()` on the matching `pid_items` column for all
  three `sim_*` objects, and the objects themselves are `identical()` to the
  shipped ones. The two PID-5 statements of `data-raw/ku_data.R` (lines 72-73),
  evaluated alone, yield `ku_pid5sf` item names equal to `item_names("pid5sf_",
  sort(pid_items$SF))`. The full source rewrote the three `sim_*` files under
  `data/`; they were restored with `git checkout` and the tree is clean. The
  re-read `ku_pid5sf` differs from the shipped object on `problems` alone, an
  external pointer — names, columns, `row.names`, `class` and `spec` are all
  `identical()`.
- **AC4 — pass.** `git grep -nE 'pid_[0-9]' -- . ':!cairn/'` exits 1 with no
  output at the branch tip, and returns 18 hits across 11 files at `d3ac6695`.
  `data-raw/check_pid_item_names.R` reports 0 hits over 24 object files and 198
  archive members across 24 archives, all 198 member bodies read; the same script
  run against the pre-rename tree (extracted with `git archive d3ac6695`) reports
  445 hits and exits non-zero. Neither procedure passes over an empty domain. The
  archive-content half is separately shown non-empty: with the pattern swapped to
  the new spelling it reports `inst/extdata/pid5_redcap.zip::instrument.csv`.
- **AC5 — pass.** The `NEWS.md` Breaking-changes entry names all four datasets,
  gives the old and new spelling for each of the three forms — `paste0("pid_",
  1:220)` to `sprintf("pid5_%03d", 1:220)`, `paste0("pid_", 1:100)` to
  `sprintf("pid5sf_%03d", 1:100)`, `paste0("pid_", 1:25)` to
  `sprintf("pid5bf_%02d", 1:25)` — names the Qualtrics uppercase variant, and
  states the migration idiom for an `items =` selection. The uppercase claim was
  checked against real exports: the three Qualtrics generators emit 220, 100 and
  25 ids, `PID5_001`..`PID5_220`, `PID5SF_001`..`PID5SF_100` and
  `PID5BF_01`..`PID5BF_25`. `R/data.R`'s four `\item{}` lines and the four
  regenerated `man/` pages state the new names; no old-pattern name survives in
  `R/` or `man/`.
- **AC6 — pass.** `devtools::document()` exits 0 and leaves the tree clean.
  `devtools::test()`: FAIL 0, WARN 0, SKIP 7, PASS 16431; the 7 skips are the
  pre-existing merge-base and keying skips (`test-column-shape.R` x3,
  `test-keying.R`, `test-scale-name-hitopsr.R` x3), none PID-5 related.
  `devtools::check()`: Status OK, 0 errors, 0 warnings, 0 notes, so no NOTE is
  added over the merge base.

### Consistency gate

`cairn_validate.py` exit 0, every check PASS; two advisories, both pre-existing
(23 dangling D-id tokens inherited from the legacy migration, one
references-staleness note on a page this milestone does not touch). The `release
window` advisory did not fire. No `DESIGN.md` principle changed, so
`cairn_impact.py` was skipped. Toolchain gate (`r-package` profile):
`document()` no diff; `NAMESPACE`, `man/` and `data/*.rda` regenerate rather than
hand-edited; `README.Rmd`/`README.md` untouched by the branch;
`pkgdown::check_pkgdown()` reports no problems; `NEWS.md` carries the
user-visible entry; both new top-level files sit under `data-raw/`, already in
`.Rbuildignore`; `check()` clean. Branch is in sync with `main`, which has not
moved since the cut.

### Independent review

Three fresh-context lenses, none having authored the work.

- **[S] blame-history — no findings.** Ran `git blame`/`git log -L` on the
  modified lines of every touched file: each changes only the string literal or
  format spec that builds column names, leaving indices, values and assertions
  intact, and nothing undoes a past commit's intent or contradicts a D-entry.
  Confirmed `prefix = "pid_"` for score output is untouched (D-026).
- **[S] prior-review record — no regressions.** The GitHub inline-comment probe
  (`gh api repos/jmgirard/hitop/pulls/comments?per_page=1`) returned empty, so
  the archived `## Review` sections were the evidence base. M077's two findings
  on this same class of work — migration examples covering only some old
  spellings, and docs mis-crediting the Qualtrics export with the lowercase
  pattern — are not repeated here.
- **[O] diff-bug — eight findings, no correctness defect in shipped code or
  data.** Independently reproduced the AC4 procedures, the pinned-object column
  comparison, the `spec` guard, and the order-guard mutant kill.

Findings, as reported and ranked, with disposition:

1. **F1 — `sim_pid()`'s new comment states a rule its code does not implement**
   (`data-raw/sim_pid.R:3-14`). The comment says names are "zero-padded to the
   width of that form's last item", but the code pads to
   `nchar(as.character(n_items))`, the item *count*. These coincide on all three
   PID-5 forms and diverge for any instrument whose largest item number exceeds
   its count. The underlying code duplication is F5 of the first round, filed as
   a candidate row — but the comment is new text in this diff asserting the
   `item_names()` semantics the line does not have. Confirmed at review against
   `git diff main..HEAD`.
2. **F2 — AC4's procedures cannot see old names that are built rather than
   spelled.** `paste0("pid_", 1:100)` matches neither the criterion's regex
   (which needs a literal digit after the underscore) nor the binary sweep (which
   reads shipped objects only). Confirmed nothing is wrong today: the only
   built-form occurrences are the three intentional ones in the NEWS migration
   text. The supplementary search the work log records was a one-off, not a
   committed procedure, so the gap reopens on the next edit.
3. **F3 — re-running the rename script would silently revert the four datasets
   to the pinned commit** (`data-raw/rename_pid_item_columns.R:14-86`). It loads
   each object from `d3ac6695`, renames, and `use_data(overwrite = TRUE)` with no
   already-applied guard, so any future change to these datasets would be
   discarded, values and all, by an innocent re-run. Same shape as M077's
   `rename_item_columns.R`, so it is precedent-consistent; the risk compounds as
   such scripts accumulate.
4. **F4 — the sweep script counts non-zip files as scanned archives**
   (`data-raw/check_pid_item_names.R:70-87`). Confirmed at review: of the 24
   files in `inst/extdata`, 6 are `.txt`/`.qsf` and are not archives — they hit
   the `tryCatch` and contribute 0 members — yet the summary prints "24
   archives", implying they were searched. They are in fact covered by the text
   search, so joint coverage is complete; the report line overstates what this
   half did.
5. **F5 — `strip()` exempts `spec` for all four objects, not just the readr
   one** (`data-raw/rename_pid_item_columns.R:63-67`). AC2 exempts `spec` only
   for `ku_pid5sf`. If a `sim_*` object ever carried one, the attribute-equality
   assertion would skip it silently. Unreachable today — the three sims carry
   only `class`, `row.names` and `names`.
6. **F6 — dataset expectations hardcode the pad width instead of deriving it
   from `item_names()`** (`tests/testthat/test-data-item-names.R:24,30,36` use
   literal `%03d`/`%02d`). The datasets and the exports are each pinned to
   literals separately, so nothing asserts the invariant the milestone exists
   for: that the shipped names equal what the export generator writes. A change
   to `item_names()`'s padding rule would move the exports and leave both suites
   green.
7. **F7 — nothing guards `data-raw/ku_pid5sf.csv`'s header.** The new `spec`
   test catches a regeneration from a stale CSV, but not a CSV header that drifts
   from the shipped object without a regeneration, and
   `check_pid_item_names.R` is run by no workflow (`R-CMD-check.yaml` runs only
   `check_line_endings.R`). Residue of the first round's F4.
8. **F8 — dead aliases left in the guard tests**
   (`tests/testthat/test-item-guards.R:47-50,79-80,125-126,143`). `bf <-
   sim_pid5bf` and the three like it are now pure renames of package objects;
   they were meaningful when they carried a `setNames()` call.

### Return floor

No finding demonstrates an acceptance criterion failing, and none is a
load-bearing defect in what the package does for its users: the rename is
value-preserving and complete, and both procedures AC4 names are green over a
domain shown non-empty. The milestone reaches the merge gate; dispositions
below were taken there.
