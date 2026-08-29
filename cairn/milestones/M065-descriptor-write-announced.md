# M065: The generators announce the descriptor they write

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m065-descriptor-write-announced`

## Goal

Say on the console that a descriptor sidecar was written, so a caller reading the
generator's output can answer whether it was saved and where.

## Scope

Surface tier: **user-facing** — the console output of three exported functions.

**In:** a `cli::cli_alert_success()` naming the descriptor's path, emitted by
`generate_docx_hitopsr()`, `generate_qualtrics_hitopsr()` and
`generate_redcap_hitopsr()` after each one's existing alert naming the
instrument file, and only where the build succeeded; the tests for it; the NEWS
entry GP2 owes for a console-output change.

**Out:** the descriptor's JSON format, its fields and its version string — all
untouched. The HiTOP-HSUM generators, which take no `descriptor`. Any change to
when the sidecar is written or rolled back; this milestone only reports what
already happens.

## Acceptance criteria

- [ ] AC1 Each of the three generators, called with `descriptor = <path>`, emits
      a `cli::cli_alert_success()` naming that path with `{.file }`, after the
      alert naming the instrument file; called without `descriptor`, none of them
      emits such a message. Verified by a test whose domain is the three
      generator names crossed with descriptor and no-descriptor, asserting on the
      captured message text and its order relative to the instrument alert.
- [ ] AC2 The message never announces a descriptor that is not on disk when the
      call returns: for each generator crossed with each of the three failure
      forms the code can take — the descriptor path refused before any write, the
      descriptor and instrument paths colliding, and the instrument write failing
      after the sidecar was written and rolled back — the test asserts no
      descriptor success message was emitted and no descriptor file remains.
- [ ] AC3 `NEWS.md` records the new console output, and the entry names a
      behavior the AC1 test would fail without.
- [ ] AC4 `devtools::document()` leaves no diff, `devtools::test()` is clean, and
      `devtools::check()` reports no error, warning or note that the merge-base
      baseline recorded in T1 does not already carry.

## Coverage

- AC1 → T2, T3
- AC2 → T2, T3
- AC3 → T4
- AC4 → T1, T5

## Tasks

- [x] T1 Record the merge-base `devtools::check()` baseline into this file, with
      the Imports the local run needs installed, so AC4 compares against a known
      state rather than assuming 0/0/0.
- [x] T2 Write the AC1 and AC2 tests first and see them red: the message
      assertions against the current silence, and the three failure forms against
      the existing rollback (`R/generate_docx.R:330-348`,
      `R/generate_qualtrics.R:112-125`, `R/generate_redcap.R:112-125`).
- [x] T3 Emit the alert at each of the three call sites after the build is marked
      complete and after the instrument file's own alert, never in
      `write_descriptor_sidecar()` (`R/module_file.R:505`), which runs before the
      instrument file exists and whose write is rolled back on failure.
- [x] T4 Add the NEWS entry.
- [x] T5 Run `devtools::document()`, `devtools::test()` and `devtools::check()`
      and compare against T1's baseline.

## Work log

- 2026-08-28: created by /milestone-plan.
- 2026-08-28: criteria audit ran in FULL mode (user-facing tier); returned 2 findings on this milestone — a rollback probe standing one failure form in for a family of three, and a promise of a note-free `check()` bound to the harness rather than the deliverable and unverified against this repo's actual baseline — both fixed before the criteria were written.
- 2026-08-28: plan chose the alert at the three call sites over one alert inside the shared sidecar writer (rejected: the writer runs before the instrument file is built and its file is removed when the build fails, so an alert there would announce a descriptor that does not survive the call); falsified by the sidecar write moving to after the instrument build, which would make the shared writer the right home.
- 2026-08-29: /milestone-implement started on branch `m065-descriptor-write-announced`; gate settled the message wording as "Module descriptor successfully written to <path>".
- 2026-08-29: T1 merge-base `devtools::check()` baseline at `d927234` on R 4.6.1 with officer, flextable, snakecase, zip, jsonlite and lavaan installed: Status OK, 0 errors, 0 warnings, 0 notes (5m35s).
- 2026-08-29: T2 added three tests to `tests/testthat/test-generator-descriptor.R` — the announcement and its order after the instrument message, the no-`descriptor` silent control, and the three failure forms with a per-case condition check plus a writable-target control on the rollback form; seen red (2 failures on the announcement, 3 on the rollback control) before T3.
- 2026-08-29: T3 emitted `cli::cli_alert_success("Module descriptor successfully written to {.file {descriptor}}")` after `built <- TRUE` in each of the three generators; the descriptor test file is green (190 passing) and `devtools::test()` is clean (15,567 passing, 4 skipped).
- 2026-08-29: T4 extended the existing unreleased `descriptor` bullet in `NEWS.md` rather than adding a second one for the same feature; the sentence landed in the T2/T3 commit, which staged the whole tree.
- 2026-08-29: T5 `devtools::document()` left no diff; `devtools::test()` clean (15,567 passing, 4 skipped); branch `devtools::check()` Status OK, 0 errors, 0 warnings, 0 notes — the same figures as T1's `d927234` baseline.
- 2026-08-29: all tasks checked; status set to review.

## Decisions

## Review
