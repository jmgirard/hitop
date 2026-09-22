<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section. -->
# M107: A module descriptor reads back as the format documents it

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP3
- **Resolves:** —
- **Surface tier:** user-facing — `write_module()` and `read_module()` are exported functions
- **Branch/PR:** `m107-module-descriptor-roundtrip`

## Goal

A module descriptor that `write_module()` writes reads back through `read_module()` as the format documents it, on every platform.

## Scope

**In:** four gaps in `R/module_file.R`. The writer uses a text connection, so Windows writes CRLF (`R/module_file.R:176`). The reader accepts JSON strings, booleans and fractions where the format documents item numbers (`read_module_numbers()`, `R/module_file.R:430`). The writer checks the class but not that `items` and `nItems` agree with the module's `scales`. The help pages do not say that a `hitop_subset` or a module with double items reads back as a `hitop_module` with integer items. D-066 records the gate's choices.

**Out:** recording the export's column names in the descriptor, so that `score_hitopsr()` needs no `items` typed out. That adds a field under D-039(d) and stays a candidate row. The permutation check that `layout_items()` duplicates stays in the M091 loose-ends candidate row.

## Acceptance criteria

- [ ] AC1: On every platform, a descriptor that `write_module()` writes ends each line with LF (0x0A) and holds no CR (0x0D) byte.
- [x] AC2: `read_module()` refuses an `items`, `nItems` or `itemOrder` field when any value in it is a JSON string, a JSON boolean, a JSON `null` inside an array, or a JSON number whose parsed value is not whole. It raises `hitop_module_file_items_mismatch` for `items` and `nItems`, and `hitop_module_file_bad_item_order` for `itemOrder`. A field whose whole value is JSON `null` still reads as absent. Whole numbers written as `2.0` or `3e0` are still accepted. Each value kind is tested in each field as the whole field. For the two array fields, each kind is also tested as one element of an otherwise valid array. Both accepted forms are tested.
- [x] AC3: `write_module()` rebuilds the module with `hitop_module()` from its `instrument` and `scales`. It aborts before writing when the module's `items` are not numeric or differ from the rebuild in value or order, or when its `nItems` differs in value. Each such abort is a {cli} error that names the field that differs, never a bare R error. When the rebuild itself fails, it aborts with the rebuild's error as the parent. Either abort leaves the path as it was: no file is created, and an existing file is unchanged. A module whose `items` are doubles equal in value to the rebuild's is still written. Each of these is tested.
- [x] AC4: `?write_module` and `?read_module` state that `read_module()` returns a `hitop_module` with integer items, also for a file written from the deprecated `hitop_subset` class or from a module whose items are doubles. A test asserts that a file written from the result of `hitop_subset()` reads back identical to the build that `hitop_module()` makes of the same instrument and scales. A second test asserts the same for a module whose `items` are doubles.
- [x] AC5: `NEWS.md` has an entry under its development heading for each behavior change in AC1 to AC3.
- [x] AC6: `devtools::document()` leaves no diff. `devtools::test()` reports 0 failures and `devtools::check()` reports 0 errors and 0 warnings. Each runs locally or, where the local toolchain cannot run it, in the `R-CMD-check.yaml` jobs at the PR head.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T5

## Tasks

- [x] T1: Line endings. Write the byte test first in `tests/testthat/test-module_file.R`. It asserts at least one 0x0A and no 0x0D. It cannot go red off Windows (LESSONS, M020 and M086), so the `windows-latest` job of `.github/workflows/R-CMD-check.yaml` at the PR head is its proof. Then write through `base::file(file, open = "wb")` with `enc2utf8()` and `useBytes = TRUE`, as `R/json_export.R:68` does. Open the connection inside the existing `rlang::try_fetch()`, so that the unwritable-path test keeps its message.
- [x] T2: Reader number fields. Write the tests first. For each kind that today's reader accepts, choose a probe it accepts and see it red before the fix: the right item numbers written as strings, fractions such as `12.4` that truncate to the right items, `true` inside an array, and `nItems` as `"3"` for a 3-item module. The other whole-field probes lock a refusal that already holds. Then keep the current parse for `format`, `instrument` and `scales`, and type-check the three number fields on a second parse with `simplifyVector = FALSE`, because the simplified parse turns `[true, 2]` into integers before any check can see it (LESSONS, M054). Update the Errors section of `?read_module`.
- [x] T3: Writer check. Write the tests first. Plant defects in location (an item dropped, added, swapped, substituted, or repeated) and in form (`NA` in `items`, `items` removed, `items` as a list, character `items`, `nItems` removed, `NA`, a fraction or of length 2, an unknown scale name). Assert the {cli} abort and the field it names for each, both with no file at the path and with an existing file that must stay byte-identical. Add the double-items control. Then add the rebuild check to `write_module_impl()` before the write.
- [x] T4: Help pages and lock. Add the AC4 sentence to `?write_module` and `?read_module`. Add the `hitop_subset()` round-trip test, catching `hitop_deprecated_subset` by class, and the double-items round-trip test. Run `devtools::document()`.
- [x] T5: Add the NEWS entries. Run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-09-22: created by /milestone-plan. Absorbs two candidate rows: the `write_module_file()` CRLF row (M086) and the three M054 descriptor gaps. The M054 row's column-names remainder stays a candidate.
- 2026-09-22: criteria audit, full mode, by a fresh [O] reader. It returned 12 findings. 10 were fixed in the draft: instrument wording in AC1 and AC6, the parse-time boolean coercion, probes that pass before the fix, `null` and exponent forms, form-axis plants for AC3, an existing file at the path, the double-items identity exception, and AC4 being a lock. 2 were posed at the gate (write-check strength, old classes).
- 2026-09-22: criteria re-audit after the gate, full mode, by a second fresh [O] reader. It returned 8 findings, all fixed: evidence wording moved from AC1 to T1, AC3 requires a {cli} error and refuses non-numeric `items`, AC4 tests the double-items round trip, AC6's local-or-CI clause covers `test()` too, and T2 and T3 name the probe and parse details.
- 2026-09-22: plan gate chose a full rebuild check in `write_module()` over a count-only check because with that check a wrong or reordered item list still writes a file the reader refuses; falsified by a caller who needs to write a module that does not match its scales.
- 2026-09-22: plan gate chose to document that `hitop_subset` and double-item modules read back as `hitop_module` over refusing `hitop_subset` in `write_module()` because refusing breaks the shim's promise that every module function accepts it; falsified by a caller that relies on the class of the object read back.
- 2026-09-22: plan gate chose to refuse non-number item values at once (pre-1.0 waiver) over a warn-first release because the format never allowed them and files the package wrote never hold them; falsified by a report of a hand-written descriptor with string numbers that worked before.
- 2026-09-22: implement started on branch `m107-module-descriptor-roundtrip`. No question gate: the plan left nothing open.
- 2026-09-22: T1 done. `write_module_impl()` opens `file(open = "wb")` inside `try_fetch()` and writes with `enc2utf8()` and `useBytes = TRUE`. The new byte test passes on macOS, and a planted CRLF file turns its assertion red. Its Windows proof is the `windows-latest` CI job at the PR head.
- 2026-09-22: T2 done. The three number fields come from a second `simplifyVector = FALSE` parse, and `read_module_numbers()` accepts only whole finite JSON numbers. Before the fix, the old reader accepted 8 of 17 probes: `nItems` as a string or fraction, and a string, `true` or fraction element in `items` and `itemOrder`. After the fix it refuses all 17 with the field's class. `devtools::test()`: 0 failed, 13 skipped, 716 tests.
- 2026-09-22: T3 done. `write_module_impl()` rebuilds the module with `hitop_module()` before it opens the path. It refuses `items` or `nItems` that differ from the rebuild, and it writes the rebuild's fields. The tests plant 9 defects in `items` and 5 in `nItems`. Two more plants make the rebuild fail: an unknown scale and the instrument `"pid5"`. All 3 refusal tests were red before the fix, and the double-items control passed. `devtools::test()`: 0 failed, 13 skipped, 720 tests.
- 2026-09-22: T4 done. `?write_module` states the rebuild check and the integer-items read back, and `?read_module`'s Value says the same. Two lock tests pass: the `hitop_subset()` round trip, with `hitop_deprecated_subset` muffled by class, and the double-items round trip. `devtools::document()` rewrote both Rd files.
- 2026-09-22: T5 done. NEWS has two Breaking changes entries (reader refusal, writer check) and one Improvements entry (LF). `devtools::document()` leaves no diff. `devtools::check()`: 0 errors, 0 warnings, 0 notes on macOS.
- 2026-09-22: claim audit: 41 claims read, 4 corrected — NEWS.md, R/module_file.R, man/read_module.Rd, man/write_module.Rd
- 2026-09-22: the claim audit's re-read held 3 of the 4 corrections. The fourth, the NEWS "Before" sentence of the writer entry, took the reader's wording. A run of `main`'s writer and reader showed that string and swapped items both wrote a file that read back.
- 2026-09-22: implement complete, status set to review. After the audit corrections, `devtools::test()`: 0 failed, 13 skipped, 722 tests. The AC1 Windows proof waits for the `windows-latest` job at the PR head.

## Decisions

## Review

Branch synced 2026-09-22: `origin/main` is an ancestor of the branch head, so no merge was needed.

- AC2 evidence (2026-09-22): a fresh probe script wrote 17 refused descriptors. As the whole field, a string, `true` and a fraction were refused in all 3 fields (9 of 9). `items` and `nItems` raised `hitop_module_file_items_mismatch`. `itemOrder` raised `hitop_module_file_bad_item_order`. As one element of a valid array, a string, `true`, `null` and a fraction were refused in `items` and `itemOrder` with the same classes (8 of 8). Whole-field `null` in all 3 fields read as absent. Whole numbers written as `2.0` and `3e0` read. The tests in `test-module_file.R` cover each case and pass.
- AC3 evidence (2026-09-22): the probe planted 4 defects in a two-scale module: items reversed, items as strings, `nItems` plus 1, and an unknown scale. Each raised an `rlang_error`. The message named `items`, `items`, `nItems` and "Cannot rebuild" in turn. The unknown-scale error carried the rebuild's "Unknown scale name" error as its parent. In each case the path stayed absent, and an existing file kept its MD5 sum. A module with double items was written. The branch tests plant 9 `items` defects, 5 `nItems` defects and 2 rebuild failures, each at an absent path and over an existing file. They pass.
- AC4 evidence (2026-09-22): `man/read_module.Rd` Value and `man/write_module.Rd` Details both state the integer-items read back, for `hitop_subset` and for double items. The probe wrote a `hitop_subset()` module and a double-items module. Each read back `identical()` to the `hitop_module()` build of the same instrument and scales. The two lock tests assert the same and pass.
- AC5 evidence (2026-09-22): `NEWS.md` has three new entries under `# hitop (development version)`. The LF entry for AC1 is under Improvements and fixes. The reader refusal for AC2 and the writer check for AC3 are under Breaking changes. No entry names a milestone number.
- AC6 evidence (2026-09-22, macOS, local): `devtools::document()` left `git status` clean. `devtools::test()` reported 0 failed, 0 errors and 13 skipped. `devtools::check()` reported 0 errors, 0 warnings and 0 notes in 6m 2s.
- AC1 evidence, local part (2026-09-22, macOS): the probe's written descriptor held 10 LF bytes and 0 CR bytes, and the byte test passes. The writer opens `file(open = "wb")`, a binary connection. The plan names the `windows-latest` job at the PR head as the Windows proof. AC1 stays unticked until that job is green at step 8.
- Consistency gate (2026-09-22): `cairn_validate.py` exit 0. Its 24 advisory warnings predate this branch. `pkgdown::check_pkgdown()` found no problems. README and DESIGN principles are not in the diff, so the README rebuild and `cairn_impact.py` do not apply. No new top-level files.

Independent review, 2026-09-22 (user-facing tier, three lenses). The prior-review lens and the blame-history lens reported no findings. The diff-bug lens reported 8, ranked most severe first. Dispositions are proposed here and settled at the merge gate.

- F1 (`R/module_file.R:533`): the unreadable-field message says "It must be a JSON array of item numbers." For `nItems`, which is one number, that tells the user the wrong shape. This branch sends more `nItems` values down this path. Proposed: fix now.
- F2 (`tests/testthat/test-module_file.R`, `expect_write_refused`): the rebuild-failure test checks that the parent is an `rlang_error`, not that it is the rebuild's "Unknown scale name" or bad-instrument error. Proposed: fix now.
- F3: the refusal tests check the class `rlang_error` plus a message match. Proposed: reject, because the message match names the field as AC3 asks, and F2's fix covers the parent.
- F4: a module whose `scales` were changed by hand is written with the rebuild's scale names, and NEWS does not say so. Proposed: reject, because a `hitop_module()` build already holds those names and the help says the file holds the rebuild's fields.
- F5: "whole" is judged on the parsed value, so `8.0000000000000001` reads as 8. Proposed: reject, because AC2 says "parsed value".
- F6: the LF test cannot fail off Windows. Proposed: no change, because the `windows-latest` job at step 8 is AC1's proof.
- F7: D-039 carries no pointer to D-066. Proposed: reject, because D-066's heading names the narrowing and decisions are append-only.
- F8: a disk-full error can leave a truncated file. Proposed: reject, because the old writer did the same and every refusal runs before the open.
