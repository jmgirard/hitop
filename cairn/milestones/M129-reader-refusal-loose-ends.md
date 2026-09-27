<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M129: `read_form_responses()` caps its line and row refusals at five, refuses a UTF-16 file once, and keeps the mismatch class when a path holds a brace

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the error messages of the exported reader `read_form_responses()`   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m129-reader-refusal-loose-ends   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

A malformed or mis-encoded hitop-form file stops `read_form_responses()` with a message of bounded length that names only lines the file holds, and a differing file's path never breaks the mismatch refusal.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** the three loose ends of M123's review (F2, F4, F6), absorbed from their candidate row. (1) The byte, whitespace-line, field-count and `instrument`-cell refusals show five lines or rows and count the rest, as the value refusals show five cells (`form_cell_lines()`, `R/read_form_responses.R:197`). (2) A UTF-16 file is refused once as UTF-16, before the byte refusal (`form_file_lines()`, `R/read_form_responses.R:269`), which today names a line past the file's last. (3) The mismatch bullets (`R/read_form_responses.R:144-153`) pass through `form_bullets()`, so a brace in a path no longer replaces `hitop_form_responses_mismatch` with a cli evaluation error. Docs and NEWS for the three.

**Out:** the mismatch refusal's one line per differing file, the `item_order` and date refusals' inline row lists (cli shortens a long vector), and the classes of the reader's other refusals (D-064): no change asked, so no row. A UTF-16 file whose first line is not ASCII and that has no byte-order mark: the page writes UTF-8 only, so no row. The invalid-UTF-8 item value stays in the M110 item-column row.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets. -->

- [x] AC1: `read_form_responses()`'s byte refusal (a NUL byte or a byte sequence that is not UTF-8), whitespace-line refusal, field-count refusal and `instrument`-cell refusal each list at most five lines or rows at fault, the first five in file order, each bullet worded as today, followed, when more are at fault, by one bullet "... and n more lines." or "... and n more rows." ("line" or "row" when n is 1), where n is the number not shown. For each of the four refusals, files with one, five, six and seven faults are refused naming exactly the expected lines or rows, with no count bullet at one and five, "1 more" at six and "2 more" at seven. For the byte refusal, a seven-fault file whose NUL lines and non-UTF-8 lines alternate names the first five by line number, each by its kind, and "... and 2 more lines.".
- [x] AC2: `read_form_responses()` refuses a file as UTF-16, with a message naming the file, saying it is UTF-16 and asking that it be saved as UTF-8, and naming no line, when the file begins with the byte-order mark FF FE or FE FF, or when its first line (the bytes before the first line feed byte, or the whole file when it holds none) is at least two bytes long with a NUL byte at every even offset or at every odd offset, counted from 0. Such a file is refused as UTF-16 even when it also holds a byte sequence that is not UTF-8. Refused this way: two-line ASCII UTF-16LE files and two-line ASCII UTF-16BE files, each with CRLF and with LF endings and each with and without a leading byte-order mark; a two-line ASCII UTF-16LE file with no line feed after its last line; a seven-line ASCII UTF-16LE file; and UTF-16LE files whose line 2 holds U+010A, with and without a leading FF FE. Controls, which keep today's line-naming refusal: a UTF-8 file holding a NUL byte as the first byte of line 3 names line 3; a UTF-8 file whose last byte is a NUL after a final line feed names line 3; a UTF-8 file of a header line and a line holding one NUL byte names line 2; and a UTF-8 file whose header ends in a NUL byte before its line feed names line 1.
- [x] AC3: `read_form_responses()` raises `hitop_form_responses_mismatch`, with each differing file's path shown as written, when a path holds a brace: `a.csv` beside a `b{x}.csv`, a `b{.csv`, a `c}.csv` and a `b{.val x}.csv`, each holding one more item column, each raise the class with that path shown as written; and `a{x}.csv` beside a differing `b.csv` names both paths as written.
- [ ] AC4: `?read_form_responses` Details replaces its sentence on line errors with one saying that the line and row errors name the first five lines or rows at fault and count the rest, as the item-value errors do, and names the UTF-16 refusal; NEWS.md carries an entry for the cap, the UTF-16 refusal and the brace fix; `devtools::document()` leaves no diff.
- [x] AC5: `devtools::test()` passes and `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

## Coverage
<!-- owner: plan · create/amend-via-gate -->

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T1, T2, T3, T4, T5

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive change is amend-via-gate. -->

- [x] T1: Tests first (red), then one helper giving the first five of a list of pre-formatted lines plus the count bullet, its noun ("line", "row", "cell") passed in, returned through `form_bullets()`. `form_cell_lines()` uses it. The byte (L288-300), whitespace (L341-353), field-count (L366-379) and instrument (L464-479) refusals use it. Read bullets from `cnd$body` (M096 lesson); build fault files with the existing `raw_file()`/`bytes_of()` helpers.
- [x] T2: Tests first (red), then the UTF-16 check at the head of `form_file_lines()`, on the raw bytes before the NUL scan and the byte-order-mark strip. Rewrite the UTF-16LE test (`tests/testthat/test-read_form_responses.R:1764`) to the new refusal. Build files with `iconv(..., toRaw = TRUE)` and a prepended mark where the case asks.
- [x] T3: Tests first (red), then the mismatch `lines` (L144-148) through `form_bullets()`. Write the brace-named files in a `withr::local_tempdir()`.
- [x] T4: Revise the Details sentence at `R/read_form_responses.R:72-76` against observed messages (derived-claims rule), add the NEWS entry under "Improvements and fixes", run `devtools::document()`.
- [x] T5: `devtools::test()` and `devtools::check()` clean.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-27: created by /milestone-plan from the M123-review candidate row (F2, F4, F6); probes confirmed a 2-line UTF-16LE file names lines 1 to 3 and a `b{x}.csv` path turns the mismatch class into a cli evaluation error.
- 2026-09-27: criteria audit ran in full mode (fresh Opus reader), twice. Fixed before and after the gate: an alternating NUL/non-UTF-8 byte case, a lone `{` and a cli-markup path in AC3, a test-reading phrase dropped from AC1; on the gate-changed AC2, a one-line-feed UTF-8 false positive (rule moved to a first-line NUL pattern), BE line endings and a markless U+010A case added, a code-order clause restated as behavior.
- 2026-09-27: plan gate chose to cap the field-count refusal with the other three over leaving it uncapped as M121's review (F3) chose, because the cap keeps each shown row's own line and count; falsified by a user needing the sixth ragged row and beyond named.
- 2026-09-27: plan gate chose one whole-file UTF-16 refusal over correcting UTF-16LE line numbers, and over ignoring NUL bytes after the final line feed (which mislabels a UTF-8 file ending in a stray NUL), because it tells the researcher to save as UTF-8; falsified by a UTF-8 file the page or a store writes being refused as UTF-16.
- 2026-09-27: implement started on branch m129-reader-refusal-loose-ends; question gate skipped (nothing open: the UTF-16 message wording is bound by AC2).
- 2026-09-27: T1 done: `form_first_five(lines, noun, total)` caps the byte, whitespace, field-count, instrument and cell refusals; 17 new tests, 9 red before the helper (the 6- and 7-fault cases and the alternating file); suite 0 failed.
- 2026-09-27: T2 done: `form_is_utf16()` runs at the head of `form_file_lines()` and refuses the file once, naming no line. The old UTF-16LE test became 12 refusal tests (all red before the check) and 4 UTF-8 controls (green before and after). Suite 0 failed.
- 2026-09-27: T3 done: the mismatch bullets pass through `form_bullets()`. Of 5 new tests, `b{x}.csv`, `b{.csv` and `b{.val x}.csv` were red before the fix. The `c}.csv` and first-file `a{x}.csv` cases passed before and after. Suite 0 failed.
- 2026-09-27: T4 done: Details adds UTF-16 to the list of refused files and a sentence naming the four capped errors by name, because the date and `item_order` errors list rows inline (cli cuts them at 20, observed). NEWS "Before" claims were read against `main` in a scratch copy. `devtools::document()` rebuilt only `man/read_form_responses.Rd`.
- 2026-09-27: T5 done: `devtools::test()` 0 failed, 20755 passed. `devtools::check()` 0 errors, 0 warnings, 0 notes.
- 2026-09-27: claim audit: 31 claims read, 2 corrected — NEWS.md, tests/testthat/test-read_form_responses.R
- 2026-09-27: delegation: one fresh [O] reader ran the claim audit against HEAD and a scratch copy of `main`. It found no wrong behavior claim and 2 wording fixes (applied in cb6ce73b), and its re-read found both hold.
- 2026-09-27: status set to review.
- 2026-09-27: review pass 1 stopped at step 3. AC4 fails as written. Details keeps its line-error sentence. The new sentence names four errors, not "the line and row errors", because the date and `item_order` row errors name every row. The criterion needs a gated amendment, so status is back to in-progress for that amendment alone. AC1, AC2, AC3 and AC5 passed with fresh evidence.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive. -->

Review pass 1, 2026-09-27, on `m129-reader-refusal-loose-ends` at 82124789. The branch contains `origin/main`, so nothing was merged. Evidence from a fresh probe script written at review (not the branch tests), reading `cnd$body`.

- AC1: pass. The probe wrote whitespace-line, field-count, `instrument`-cell and NUL-byte files with 1, 5, 6 and 7 faults. Each refusal names the first faults in file order, worded as before. There is no count bullet at 1 and 5, "... and 1 more line." or "row." at 6, and "... and 2 more lines." or "rows." at 7. A 7-fault file alternating NUL and 0xFF lines names lines 2 to 6, each by its kind, then "... and 2 more lines.".
- AC2: pass. The message is "'<file>' is a UTF-16 file, not UTF-8." with the bullet "Save the file as UTF-8 and read it again.". It names no line. Eight two-line ASCII files got it: UTF-16LE and UTF-16BE, CRLF and LF, with and without a mark. So did a UTF-16LE file with no final line feed and a seven-line UTF-16LE file. So did UTF-16LE files with U+010A in line 2, with and without FF FE. So did a UTF-16LE header followed by 0xFF 0xFF. The four UTF-8 controls kept the line refusal and named lines 3, 3, 2 and 1.
- AC3: pass. `a.csv` beside `b{x}.csv`, `b{.csv`, `c}.csv` and `b{.val x}.csv`, each with one more item column, raised `hitop_form_responses_mismatch`. The bullet showed each path as written, for example "'b{.val x}.csv' differs from it in count.". `a{x}.csv` beside a differing `b.csv` raised the class and named both paths as written.
- AC4: fail as written. The criterion asks that Details replace its sentence on line errors. The new sentence is to say that "the line and row errors" name the first five and count the rest. The branch keeps that sentence ("An error on a line names the lines at fault, counted from the file's first line.") unchanged. It adds a sentence that names only the four capped errors. A sentence worded as AC4 asks is false for two row errors. A 7-row file with a bad `form_build` date names all seven rows ("Response rows 1, 2, 3, 4, 5, 6, and 7: ..."). The Out scope keeps the date and `item_order` row lists as they are. The branch text names the UTF-16 refusal, NEWS.md carries the entry, and `devtools::document()` left no diff. The criterion is wrong, not the work, so this is an amendment return.
- AC5: pass. `devtools::test()` gave 0 failed, 0 errors, 20755 passed and 15 skipped. `devtools::check()` gave 0 errors, 0 warnings and 0 notes.
- Stopped after step 3. The consistency gate and the independent review did not run on this pass.
