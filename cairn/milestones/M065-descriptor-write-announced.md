# M065: The generators announce the descriptor they write

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP2, GP3
- **Branch/PR:** `m065-descriptor-write-announced` / https://github.com/jmgirard/hitop/pull/72

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

- [x] AC1 Each of the three generators, called with `descriptor = <path>`, emits
      a `cli::cli_alert_success()` naming that path with `{.file }`, after the
      alert naming the instrument file; called without `descriptor`, none of them
      emits such a message. Verified by a test whose domain is the three
      generator names crossed with descriptor and no-descriptor, asserting on the
      captured message text and its order relative to the instrument alert.
- [x] AC2 The message never announces a descriptor that is not on disk when the
      call returns: for each generator crossed with each of the three failure
      forms the code can take — the descriptor path refused before any write, the
      descriptor and instrument paths colliding, and the instrument write failing
      after the sidecar was written and rolled back — the test asserts no
      descriptor success message was emitted and no descriptor file remains.
- [x] AC3 `NEWS.md` records the new console output, and the entry names a
      behavior the AC1 test would fail without.
- [x] AC4 `devtools::document()` leaves no diff, `devtools::test()` is clean, and
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
- 2026-08-29: /milestone-review opened draft PR #72; AC1/AC2/AC3 verified with fresh evidence including a feature-reverted discrimination run; consistency gate clean; AC4 and the diff-bug lens still in flight at this checkpoint.
- 2026-08-29: AC4 verified fresh — check() Status OK 0/0/0 in 5m52.8s, matching T1's baseline; all four criteria ticked. Three lenses reported: no correctness bug in R/, twelve findings all in tests/docs/tracking, none meeting the return floor.

## Decisions

## Review

Reviewed 2026-08-29 on branch `m065-descriptor-write-announced` at `b0ae919`,
merge base `d927234` (`origin/main` unmoved since the branch was cut, so no
merge was needed). PR https://github.com/jmgirard/hitop/pull/72.

### Acceptance-criterion evidence

- **AC1** — `testthat::test_local(filter = "generator-descriptor")` green over
  the whole file. The announcement test loops the three generator names, finds
  exactly one message carrying the descriptor's own path in each, matches
  "descriptor" in it, and asserts its index is greater than the index of the
  message carrying the instrument path; the no-`descriptor` companion asserts
  the instrument message is still present and no message mentions a descriptor.
  Discrimination shown fresh in a throwaway clone with only the three
  `R/generate_*.R` hunks reverted to `origin/main`: the announcement test goes
  red (`Expected said to have length 1. Actual length: 0.`) while the silent
  control stays green.
- **AC2** — same run. The failure-form test crosses the three generators with
  the three failure forms; each case names the failure it must be (rlang error
  quoting the descriptor path; rlang error saying "different"; a builder error
  that must *not* quote the descriptor path) before asserting no descriptor
  message and no descriptor file. The rollback form's writable-target control
  asserts the same descriptor path is written and announced when the builder
  can open its target — and that control goes red in the feature-reverted
  clone (three failures, one per generator), so the case reaches the sidecar
  rather than passing by never getting there.
- **AC3** — `NEWS.md` extends the unreleased `descriptor` bullet with: the
  console names the descriptor it saved, after the message naming the form
  itself, and a call passing no `descriptor` says nothing about one. Both
  clauses are what the AC1 test asserts (path present + index ordering; the
  silent control), and both were observed failing in the reverted clone.
- **AC4** — `devtools::document()` left the working tree clean apart from this
  milestone file (no `man/`, `NAMESPACE` or `DESCRIPTION` diff).
  `devtools::check()` on the branch tree: **Status OK, 0 errors, 0 warnings,
  0 notes**, 5m52.8s, its `testthat.R` run green in 260s — the same figures as
  T1's `d927234` merge-base baseline (Status OK, 0/0/0, 5m35s), so the branch
  adds no error, warning or note the baseline does not already carry. The
  targeted `test_local(filter = "generator-descriptor")` run was green over the
  whole file with all three generators exercised. The check ran against the
  tree at `b0ae919`; the commits after it touch only `cairn/`, which
  `.Rbuildignore`'s `^cairn$` entry keeps out of the built package.

### Consistency gate

`cairn_validate.py` exit 0, all 16 checks PASS (21 advisories, all pre-existing:
20 dangling legacy D-id tokens and one references-staleness note on
`schmukle2026.md`, none touched by this diff). No `DESIGN.md` principle changed,
so `cairn_impact.py` was not run. Toolchain slot: `document()` no diff; no
generated file hand-edited; `README.Rmd`/`README.md` untouched and last written
by the same commit; `pkgdown::check_pkgdown()` reports no problems; `NEWS.md`
carries the entry with no milestone number in it; no new top-level file, so no
`.Rbuildignore` entry owed; `check()` clean. Line-ending policy check passed.

### Independent review

Executable surface touched, user-facing tier, so the full three-lens fan-out ran
in fresh context.

Three fresh-context lenses ran. The blame-history lens reported no conflict: the
alert sits after `built <- TRUE`, outside the rollback window M055 built, and
`write_descriptor_sidecar()` is untouched. The prior-review lens reported no
prior-review evidence on GitHub (`pulls/comments` empty) and no regression
against the archived Review sections of M054/M055/M056 or `LESSONS.md`. The
diff-bug lens found no correctness bug in `R/` and returned twelve findings,
all in the tests, docs or tracking, ranked below with their disposition.

1. The AC1 loop aborts on the first generator when the feature is absent:
   `msgs[[said]]` with `said` empty throws, so the red-phase run covered only
   `generate_docx_hitopsr`. Confirmed against
   `tests/testthat/test-generator-descriptor.R:586-588`.
2. The same abort-the-loop fragility in AC2's rollback `verify`:
   `conditionMessage(err)` with `err` NULL would take the remaining
   generator x case combinations with it.
3. AC1 asserts message text and order but not the `cli_alert_success` prefix or
   the `{.file }` styling the criterion names, so demoting the call to
   `cli_alert_info()` would leave the suite green.
4. `CLI_PLAIN` pins `cli.width` but not `cli.condition_width`; AC2's first case
   greps the descriptor path out of an abort message, which a long `TMPDIR` on
   another runner could wrap.
5. The nine-line emit block is triplicated verbatim across the three
   generators.
6. `capture_generator_failure()` muffles every warning unconditionally, not
   only the builder's connection warnings its comment names.
7. The `@param descriptor` roxygen narrates the write-and-rollback behavior but
   not the new console message.
8. AC2's refused-path and colliding-path forms abort before any write, so they
   hold regardless of this milestone's change; only the rollback form and its
   writable-target control discriminate.
9. The milestone file was uncommitted when the lens ran. Resolved at `0dd4923`.
10. Scope cites GP2 for the NEWS entry, but GP2 is "Scored output never changes
    silently" (`cairn/DESIGN.md:104`) and this changes no scored value.
11. The new alert prints into the suite's console output from the pre-existing
    tests that pass `descriptor` without capturing messages.
12. The NEWS bullet now reads write -> rollback -> console -> `itemOrder`,
    interleaving two topics.

None of the twelve demonstrates an acceptance criterion failing, so none meets
the return floor; dispositions are taken at the merge gate.
