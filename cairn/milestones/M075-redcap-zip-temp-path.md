# M075: One REDCap archive writer, using a per-call temporary directory it always cleans up, against a stated {zip} floor

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP4
- **Branch/PR:** `m075-redcap-zip-temp-path`

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

- [ ] AC1: A test captures the file path handed to `zip::zip()` on two
      successive `generate_redcap_hitopsr()` calls and on one
      `generate_redcap_hitophsum()` call in a single session, and asserts that
      all three paths differ, that each basename is `instrument.csv`, and that
      each archive written carries exactly one entry named `instrument.csv`
      holding that call's own data dictionary. The path assertions run red
      against HEAD, where both call sites use
      `file.path(tempdir(), "instrument.csv")`. (The claim is about the two
      generators exercised; AC4 carries it to the other four.)
- [ ] AC2: For each of two injected failure mechanisms — an error carrying a
      class the test itself defines, raised from a mocked `zip::zip()`, and a
      destination path inside a directory that does not exist — a test asserts
      that the injected condition propagates out of `generate_redcap_hitopsr()`
      and out of `generate_redcap_hitophsum()` rather than being swallowed, and
      that listing `tempdir()` before and after the call shows no entry left
      behind. The test runs red against cleanup that happens only after a
      successful archive write.
- [ ] AC3: `DESCRIPTION`'s `Imports` entry for `zip` names a version floor equal
      to the earliest `zip` release whose `zip()` accepts
      `mode = "cherry-pick"`, that version taken from the CRAN release record
      for the package.
- [ ] AC4: Both generators' archive writing goes through one internal function
      that creates the temporary directory, writes the CSV into it, and calls
      `zip::zip()`; a search for the literal `zip::zip(` across `R/` returns
      exactly one call site as its evidence.
- [ ] AC5: The profile's `verify` slot is clean — `devtools::document()` run
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
- [ ] T3: Write the AC2 failure-path test for both injected mechanisms. Run it
      against a copy of the pre-T2 cleanup ordering and record it red, then
      green against T2's code.
- [ ] T4: Identify the earliest `zip` release accepting `mode = "cherry-pick"`
      from the CRAN release record, record the source and the check in the work
      log, and set the `DESCRIPTION` floor to it.
- [ ] T5: `NEWS.md` entry for the temp-path fix and the new floor; append the
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
- 2026-08-31: plan gate deferred rebuilding the six committed REDCap artifacts (Jeff's call); falsified by a rebuilt archive differing in any byte from its committed copy.

## Decisions

## Review
