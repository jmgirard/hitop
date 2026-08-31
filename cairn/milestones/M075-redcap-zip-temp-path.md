# M075: One REDCap archive writer, using a per-call temporary directory it always cleans up, against a stated {zip} floor

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Branch/PR:** `m075-redcap-zip-temp-path` / https://github.com/jmgirard/hitop/pull/81

## Goal

Both REDCap archive-writing blocks become one internal function that builds its
data-dictionary CSV inside a directory created for that call and removes it
whether or not the archive write succeeds, and `DESCRIPTION` states the `{zip}`
version floor the `mode = "cherry-pick"` call needs.

## Scope

Surface tier: **user-facing** — the deliverable changes what two exported
generators do on disk and what the package requires to install.

**In:** the two duplicated export blocks at `R/generate_redcap.R:364-377` (inside
the shared helper `build_redcap_zip()`) and `:664-677` (inside
`generate_redcap_hitophsum()`), merged into one internal function; a per-call
temporary directory whose single member stays named `instrument.csv`; cleanup
registered before anything is written into that directory; a `zip` version floor
in `DESCRIPTION` sourced from the CRAN release record; tests for each; a `NEWS.md`
entry and a `DECISIONS.md` entry recording the floor (GP4's route for a
dependency re-pin, approved at the 2026-08-31 plan gate).

**Out:** rebuilding the six committed `inst/extdata/*_redcap.zip` artifacts and
their `pkgdown/assets/downloads/` copies — deferred at the 2026-08-31 plan gate,
because the fix changes only where the scratch CSV lives and not one archived
byte, so a rebuild would churn the checksum manifest and record a revision that
is not one; the next run of `data-raw/artifacts.R` picks up the merged writer's
output normally. → the deferral is recorded in this milestone's `DECISIONS.md`
entry. Naming or format validation of what REDCap accepts → the standing
candidate row on generator naming rules. Any change to the archive's contents,
entry name, or the dictionaries themselves → not in this milestone.

## Acceptance criteria

- [x] AC1: A test captures the file path handed to `zip::zip()` on two
      successive `generate_redcap_hitopsr()` calls and on one
      `generate_redcap_hitophsum()` call in a single session, and asserts that
      all three paths differ, that each basename is `instrument.csv`, and that
      each archive written carries exactly one entry named `instrument.csv`
      holding that call's own data dictionary. The path assertions run red
      against HEAD, where both call sites use
      `file.path(tempdir(), "instrument.csv")`. (The claim is about the two
      generators exercised; AC4 carries it to the other four.)
- [x] AC2: For each of two injected failure mechanisms — an error carrying a
      class the test itself defines, raised from a mocked `zip::zip()`, and a
      destination path inside a directory that does not exist — a test asserts
      that the injected condition propagates out of `generate_redcap_hitopsr()`
      and out of `generate_redcap_hitophsum()` rather than being swallowed, and
      that listing `tempdir()` before and after the call shows no entry left
      behind. The test runs red against cleanup that happens only after a
      successful archive write.
- [x] AC3: `DESCRIPTION`'s `Imports` entry for `zip` names a version floor equal
      to the earliest `zip` release whose `zip()` accepts
      `mode = "cherry-pick"`, that version taken from the CRAN release record
      for the package.
- [x] AC4: Both generators' archive writing goes through one internal function
      that creates the temporary directory, writes the CSV into it, and calls
      `zip::zip()`; a search for the literal `zip::zip(` across `R/` returns
      exactly one call site as its evidence.
- [x] AC5: The profile's `verify` slot is clean — `devtools::document()` run
      after any roxygen change, and `devtools::test()` passing.

## Coverage

- AC1 → T1, T2
- AC2 → T2, T3
- AC3 → T4
- AC4 → T2
- AC5 → T2, T4, T5

## Tasks

- [x] T1: Write the path-capture test for AC1 in
      `tests/testthat/test-generate_redcap.R` — mock `zip::zip()` to record its
      `files` argument, then let the real call run — covering two successive
      `generate_redcap_hitopsr()` calls and one
      `generate_redcap_hitophsum()` call. Run it on HEAD and record it red.
- [x] T2: Merge `R/generate_redcap.R:364-377` and `:664-677` into one internal
      function taking the data dictionary and the destination path. It creates a
      per-call temporary directory, **registers that directory's removal before
      writing anything into it**, writes `instrument.csv` inside it, and calls
      `zip::zip(mode = "cherry-pick")`. `build_redcap_zip()` and
      `generate_redcap_hitophsum()` both call it. Confirm T1's test now green
      and the existing entry-name tests at `test-generate_redcap.R:345-346` and
      `:358` still pass.
- [x] T3: Write the AC2 failure-path test for both injected mechanisms. Run it
      against a copy of the pre-T2 cleanup ordering and record it red, then
      green against T2's code.
- [x] T4: Identify the earliest `zip` release accepting `mode = "cherry-pick"`
      from the CRAN release record, record the source and the check in the work
      log, and set the `DESCRIPTION` floor to it.
- [x] T5: `NEWS.md` entry for the temp-path fix and the new floor; append the
      `DECISIONS.md` entry recording the dependency floor (extending the entry
      that adopted `{zip}`) and naming the deferred artifact rebuild.

## Work log

- 2026-08-31: created by /milestone-plan.
- 2026-08-31: plan-gate criteria audit ran in **full** mode (user-facing tier), fresh-context [O] reader, two passes. Pass 1 returned six findings across three criteria (AC1 unsatisfiable — `write.csv()` truncates the planted file, so the probe was green on HEAD; AC1 bound the archive not the path; AC2 named a path unobservable after the fix; AC2 pinned a `{zip}`-owned condition class; AC2 verified one failure form; AC3 checked for a `mode` formal, a proxy for accepting `"cherry-pick"`). Pass 2 over the revised wording returned five (AC1's cross-generator pair also satisfied by a per-instrument directory; AC1 over-quantified six exported generators against two exercised; AC2's two probes shared one location; AC4's grep count passed a partial factoring; AC4's second half bound test-harness state). All eleven had one clear answer and were fixed before writing; AC3 passed all six in pass 2.
- 2026-08-31: plan gate chose merging both archive-writing blocks into one internal function over fixing each copy in place (Jeff's call), because one writing path removes the class of divergence rather than one instance of it; falsified by the merge forcing a signature or behavior difference between the two generators that the shared function cannot carry.
- 2026-08-31: plan gate chose a per-call temporary *directory* holding a file named `instrument.csv` over a `tempfile()`-named CSV, because `zip::zip(mode = "cherry-pick")` stores a member by its basename and REDCap's instrument format requires that name, pinned by `test-generate_redcap.R:345-346`; falsified by REDCap accepting an archive whose member carries any other name.
- 2026-08-31: plan gate chose declaring the `{zip}` floor in `DESCRIPTION` alone over also guarding at the call site, because R enforces an `Imports` version requirement when the namespace loads, where the Suggests floor of the earlier ggplot2 decision was not enforced; falsified by an install of the package succeeding against a `zip` below the declared floor.
- 2026-08-31: implement gate — the merged writer absorbs the whole duplicated block (relative-to-absolute destination path, temporary CSV, archive write, success message), not only the two lines the plan names (Jeff's call).
- 2026-08-31: implement gate — `{zip}` floor set to 2.1.0 (Jeff's call). Source: CRAN's release record for `zip` — the NEWS entry for 2.1.0 is the first to state that "zip functions now have a `mode` argument"; no earlier release mentions `mode`. The r-lib/zip source at tag `v2.1.0` defines `zip(mode = c("mirror", "cherry-pick"))`, so `"cherry-pick"` is accepted from that release; `zip_2.1.0.tar.gz` is on CRAN dated 2020-08-10.
- 2026-08-31: T1 — path-capture test added at `test-generate_redcap.R:376`; red on HEAD with exactly one failure, `unique(captured)` length 1 against the expected 3 (all three exports wrote to the same `file.path(tempdir(), "instrument.csv")`). The capture itself recorded 3 calls and every other assertion in the file passed, so the mock reaches `zip::zip()` and the red is the shared-path defect, not a broken probe.
- 2026-08-31: T2 — both blocks now call `write_redcap_zip()` (`R/generate_redcap.R:369`), which makes the destination absolute, creates `tempfile("hitop-redcap-")`, registers `unlink(recursive = TRUE)` on exit before `dir.create()`, writes `instrument.csv` into it, archives it and reports success. `grep -n "zip::zip("` over `R/` returns one line. T1's test green; the entry-name tests still pass; `devtools::test()` FAIL 0 WARN 0 SKIP 4 PASS 16217.
- 2026-08-31: T3 — two failure-path tests added (`test-generate_redcap.R:406`, `:439`), each running both generators. Against a copy of the pre-T2 ordering (scratch CSV at `file.path(tempdir(), "instrument.csv")`, removed only after `zip::zip()` returns) both went red, each on a leftover `instrument.csv`; the injected-condition and error-message assertions passed in that run, so the red is the cleanup ordering. The before/after diff alone was not discriminating — the first test's leak was already in `before` for the second, so each test also asserts by name that neither `instrument.csv` nor a `hitop-redcap-*` directory is left in `tempdir()`. Green against T2's code; `devtools::test()` FAIL 0 WARN 0 SKIP 4 PASS 16228.
- 2026-08-31: T4 — `DESCRIPTION` Imports now reads `zip (>= 2.1.0)`. The check: CRAN's release notes for `zip` first mention the `mode` argument under 2.1.0 and no earlier release mentions it; the r-lib/zip source at tag `v2.1.0` defines `mode = c("mirror", "cherry-pick")`; `zip_2.1.0.tar.gz` is on CRAN dated 2020-08-10. Installed here: 3.0.2. `devtools::test()` FAIL 0 WARN 0 SKIP 4 PASS 16228.
- 2026-08-31: T5 — `NEWS.md` gains an "Improvements and fixes" section under the development version (the scratch-directory fix and the new floor); `cairn/DECISIONS.md` gains D-050, extending D-035 with the `zip (>= 2.1.0)` floor and naming the deferred artifact rebuild. `devtools::document()` no diff; `devtools::test()` FAIL 0 WARN 0 SKIP 4 PASS 16228.
- 2026-08-31: all tasks done, status set to review.
- 2026-08-31: plan gate deferred rebuilding the six committed REDCap artifacts (Jeff's call); falsified by a rebuilt archive differing in any byte from its committed copy.
- 2026-08-31: review — PR #81 opened as a draft; `main` had not moved, so no merge was needed. All five criteria met with fresh evidence, red claims re-run against a scratch clone of `main`. Consistency gate all green (`cairn_validate` exit 0; `check()` 0/0/0). Three-lens fan-out: two lenses clean, [O] returned eight findings, none an acceptance-criterion failure.

## Decisions

- 2026-08-31: the `{zip}` floor and the deferred artifact rebuild are recorded in `cairn/DECISIONS.md` as D-050; both are cross-cutting, so neither is restated here.

## Review

Reviewed 2026-08-31 on `m075-redcap-zip-temp-path` at 22f8bb59, PR #81.
Red-claim evidence for AC1 and AC2 comes from a scratch clone of `main`
(`git clone -b main` into the session scratchpad) carrying only this branch's
`tests/testthat/test-generate_redcap.R`, so the working tree was never moved.

- AC1 — met. `test-generate_redcap.R:370` runs two `generate_redcap_hitopsr()`
  calls and one `generate_redcap_hitophsum()` in one session, capturing the
  `files` argument of a mocked `zip::zip()` that forwards to the real one:
  3 captures, 3 distinct paths, every basename `instrument.csv`, and each
  archive holding exactly one `instrument.csv` whose `Form Name` is that call's
  own. Green on the branch (`devtools::test(filter = "generate_redcap")`
  FAIL 0 PASS 88). Red against `main` in the scratch clone: `unique(captured)`
  length 1 against the expected 3 at `:390`, both call sites there writing
  `file.path(tempdir(), "instrument.csv")`.
- AC2 — met. Two failure-path tests (`:406` injected classed error from a
  mocked `zip::zip()`; `:437` destination inside a directory that does not
  exist), each exercising both generators. On the branch the injected condition
  propagates by class from both, the unwritable-destination error names the
  destination path, and nothing is left in `tempdir()`. Red against `main` in
  the scratch clone: leftover `instrument.csv` at `:427`, `:433` and `:462`
  (the second test's before/after diff is blind there because the first test's
  leak is already in its `before` — the by-name assertions are what discriminate).
- AC3 — met. `DESCRIPTION` Imports reads `zip (>= 2.1.0)`. Checked fresh against
  CRAN's release record for `zip`: in `cran.r-project.org/web/packages/zip/news/`
  the only release note introducing the `mode` argument sits under 2.1.0, and no
  earlier release (2.0.4 back to 1.0.0) mentions `mode`; the r-lib/zip source at
  tag `v2.1.0` defines `zip(mode = c("mirror", "cherry-pick"))`; CRAN's archive
  index lists `zip_2.1.0.tar.gz` dated 2020-08-10.
- AC4 — met. `grep -rn 'zip::zip(' R/` returns exactly one line,
  `R/generate_redcap.R:387`, inside `write_redcap_zip()` (defined at `:369`,
  creating the scratch directory, registering its removal, writing the CSV into
  it). Its two callers are `build_redcap_zip()` at `:358` and
  `generate_redcap_hitophsum()` at `:672`.
- AC5 — met. `devtools::document()` produced no diff (`git status` after it
  showed only this milestone file). `devtools::test()`: FAIL 0 WARN 0 SKIP 4
  PASS 16228.

**Consistency gate.** `cairn_validate.py` exit 0, every check PASS; 22 advisories
(21 dangling `D-0NN` tokens in pre-migration prose, 1 references-staleness) all
pre-existing and unchanged by this branch. No `DESIGN.md` principle changed, so
`cairn_impact.py` did not run. Profile (`r-package`) `consistency-gate` slot:
`devtools::document()` no diff; `NAMESPACE`, `man/` and `data/*.rda` untouched;
`README.Rmd` untouched so no re-knit owed; `pkgdown::check_pkgdown()` "No
problems found"; `NEWS.md` carries the user-visible entry with no milestone
number; no new top-level file, so no `.Rbuildignore` entry owed;
`devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes (its "detritus in
the temp directory" check also OK). `data-raw/check_line_endings.R` passed.

**Independent review.** Full three-lens fan-out (user-facing tier, executable
surface). [S] blame-history: no findings — the D-035 `mode = "cherry-pick"`
comment and rationale are relocated intact, and the M025/M058 `on.exit` lesson
is about a top-level script scope, not this call frame. [S] prior-PR-comments:
no prior-review evidence on either surface — no archived `## Review` section
touches these files, and the repo's GitHub inline-comment probe returned empty.
[O] diff-bug returned eight, ranked; dispositions:

1. Follow-up. The success-path cleanup has no assertion of its own: a mutant
   cleaning up only on the error path is caught, but only because the AC1 test's
   leaked directory is still in `tempdir()` when the later failure tests run
   their by-name assertions. Detection depends on file order. (AC1 and AC2 both
   hold as written; this is reach beyond them.)
2. Follow-up. `grepl("^/|^[A-Za-z]:", file)` treats `~/out/x.zip` as relative and
   prepends `getwd()`, so a tilde destination errors; Windows UNC paths fall the
   same way. Pre-existing in both original copies, but M075 made this the one
   destination-path routine in the package.
3. Follow-up. The `zip (>= 2.1.0)` floor is sourced, not exercised — the suite
   only ever runs against the installed 3.0.2 — and `zip` 2.3.0's notes mention
   better handling of absolute paths containing `:`, which this writer passes.
   AC3 is met as written; the floor is the earliest release accepting the mode.
4. Rejected. `dir.create()`'s return unchecked: the path is a fresh `tempfile()`
   name, so failure means an unwritable `tempdir()`; the next line still errors
   and the registered cleanup still runs.
5. Rejected. The `setdiff(list.files(tempdir()), before)` assertions are the
   before/after listing AC2 names in so many words; the by-name assertions were
   added beside them precisely because the diff goes blind after an earlier leak.
6. Rejected. Verified against the shipped text: `NEWS.md` says two exports "used
   one file" and that a failed export "left the file behind", not that a second
   call overwrote a file in use. Both claims hold.
7. No action — milestone bookkeeping this review performs.
8. No action — `DESIGN.md:186` quotes D-010, an append-only decision entry.

None of the eight demonstrates an acceptance criterion failing, and none is a
load-bearing defect in what the generators do; status stays `review`.
