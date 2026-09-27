<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M130: `read_form_responses()` refuses a UTF-16 file after blank lines, names a UTF-32 file as UTF-32, and stops calling `a`,NUL,`b` UTF-16

- **Status:** planned   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the error messages and Details of the exported reader `read_form_responses()`   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** —   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

A hitop-form file saved as UTF-16 or UTF-32 stops `read_form_responses()` with one error that names its encoding and no line, and a UTF-8 file without a NUL byte is never called UTF-16 or UTF-32.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** Replace `form_is_utf16()` (`R/read_form_responses.R:340`) with a helper that returns `"UTF-32"`, `"UTF-16"` or `NA` for a file's raw bytes. It tries UTF-32LE, UTF-32BE, UTF-16LE and UTF-16BE, in that order, and returns the first that matches.
- A file that starts with FF FE 00 00 or 00 00 FE FF is UTF-32. A file that starts with FF FE or FE FF otherwise is UTF-16.
- Without a mark, the helper reads the bytes as whole code units of each encoding (4 bytes or 2), ignoring a trailing partial unit. It splits them into lines on that encoding's line feed unit and drops one trailing CR unit from each line. A line is blank when nothing is left.
- The encoding matches when its first line that is not blank is made only of printable ASCII (U+0020 to U+007E) and tab. It also matches when every line is blank and the file holds at least one line feed unit.

`form_file_lines()` (`:288`) refuses a match once, before the byte check. The error says "{file} is a UTF-32 file, not UTF-8." or "... is a UTF-16 file, not UTF-8." and asks that the file be saved as UTF-8. It stays an unclassed `cli_abort()` (D-064). Update the Details and the UTF-16 sentences of the development-version NEWS bullet M129 added.

**Out:** A UTF-16 or UTF-32 file whose first non-blank line holds a character outside printable ASCII still reaches the byte check. A page-saved file's header is ASCII, so no row is kept, and a user report takes `/hotfix`. Correcting the byte check's line numbers for such a file is also out, because M129's plan gate chose one whole-file refusal. A condition class for these refusals is out, because D-064 keeps them unclassed until a caller needs one.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets.
     Every item opens with its positional label — `ACn:` — the item's
     position counted top-to-bottom, the number Coverage cites; an
     insertion, removal, or reorder renumbers the labels and the Coverage
     lines together. -->

- [ ] AC1: `read_form_responses()` stops on each of 16 UTF-16 files: UTF-16LE and UTF-16BE, LF and CRLF line ends, one and two blank lines before the header, each with and without its byte-order mark. Each stop is one error that carries neither `hitop_form_responses_mismatch` nor `hitop_form_responses_none`, names the file, says "is a UTF-16 file", names no line, and raises no warning. `tests/testthat/test-read_form_responses.R` asserts each file.
- [ ] AC2: `read_form_responses()` stops on each of 16 UTF-32 files: UTF-32LE and UTF-32BE, LF and CRLF line ends, zero and one blank line before the header, each with and without its byte-order mark (FF FE 00 00, 00 00 FE FF). Each stop is one error that carries neither class, names the file, says "is a UTF-32 file", does not say "UTF-16", names no line, and raises no warning. The test file asserts each file.
- [ ] AC3: `read_form_responses()` stops on each of 10 files of blank lines and nothing else, with no mark. Eight hold two blank lines: UTF-16LE, UTF-16BE, UTF-32LE and UTF-32BE, LF and CRLF line ends. Two hold one LF line: UTF-16LE and UTF-16BE. Each stop is the error for the file's encoding, as AC1 and AC2 describe it. The test file asserts each file.
- [ ] AC4: `read_form_responses()` gives each of these files the error listed, and the test file asserts which error:
  - A UTF-8 file whose first line is the bytes `a`, NUL, `b` and whose second line is the header, with LF and with CRLF line ends: the NUL-byte error naming line 1.
  - A UTF-8 file with a blank first line, the header on line 2 and a NUL byte on line 3, with LF and with CRLF line ends: the NUL-byte error naming line 3.
  - A UTF-16LE file of one CR and nothing else (0D 00): the NUL-byte error naming line 1.
  - An empty file, and a file of only the UTF-8 byte-order mark: the no-header-row error.
  - The 1-byte file `a` and the 2-byte file `ab`: the lead-column error.
  - A file of only FF FE, a file of only FE FF, and the 2-byte file 0A 00: the UTF-16 error.
  - A file of only FF FE 00 00, and a file of only 00 00 FE FF: the UTF-32 error.
- [ ] AC5: The 13 UTF-16 files the existing tests read keep the UTF-16 error naming no line. They are the eight-case `utf16_cases` matrix, the file with no final line feed, the seven-line file, the two U+010A files and the café file. The 4 UTF-8 controls keep the NUL-byte error naming lines 3, 3, 2 and 1.
- [ ] AC6: `?read_form_responses` Details and the development-version NEWS bullet each state these facts:
  - A file starting FF FE 00 00 or 00 00 FE FF is taken as UTF-32, and one starting FF FE or FE FF as UTF-16.
  - Empty lines and lines of a lone CR at the top are skipped.
  - In each encoding the lines are split on that encoding's line feed, and the first line that is not blank, its trailing CR dropped, must be printable ASCII or tab.
  - A file of blank lines only in one of those encodings that holds at least one line feed is taken as that encoding.
  - A file taken as UTF-32 is refused as UTF-32, not as UTF-16.

  `grep -n "every other byte" R/read_form_responses.R NEWS.md` returns nothing, and `devtools::document()` leaves no diff.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T2
- AC6 → T3

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. Every item opens with its positional label —
     `Tn:` — the item's position counted top-to-bottom, the number Coverage
     cites; an insertion, removal, or reorder renumbers the labels and the
     Coverage lines together. -->

- [ ] T1: Tests first, in `tests/testthat/test-read_form_responses.R` after the UTF-16 block (`:1804`). Build the AC1, AC2 and AC3 matrices from raw bytes, because `iconv()` writes no mark for the LE and BE targets and `Rscript -e` strips `\r`. Add a UTF-32 refusal helper beside `utf16_refusal()` that asserts neither class and records warnings with `withCallingHandlers`. Add the AC4 files. Run the new tests against the current code: the LF blank-line, UTF-32, blank-only and `a`,NUL,`b` cases must fail, each for its stated reason.
- [ ] T2: Replace `form_is_utf16()` as Scope states, and name the encoding in the `form_file_lines()` error. The full suite passes with the M129 UTF-16 tests and controls unedited. Plant a byte split in place of the unit split, see AC1 go red, and restore it; stage the fix with `git add` first (LESSONS M118).
- [ ] T3: Rewrite the Details UTF-16 sentences (`R/read_form_responses.R:60-62`, `:83`) and the UTF-16 sentences of the NEWS bullet (`NEWS.md` "caps four of its line and row errors") to the AC6 facts, written from the shipped code. Run `devtools::document()` and `devtools::check()`.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates.
     EXEMPT from the 150-line cap (D-046): history under D-045, never edited,
     so the cap must never demand a trim here. Wrapped entries get a WARN. -->

- 2026-09-27: created by /milestone-plan from the `[low]` candidate row "Three edges of `read_form_responses()`'s UTF-16 check" (lineage: M129 review R2, R3, R6).
- 2026-09-27: criteria audit (full mode, fresh [O] reader) returned 7 findings, all fixed before the gate: the scope rule split on unit line feeds and dropped trailing CR, AC1 and AC2 reworded to the reader and to neither class, "the 2-byte file `ab`", FF FE 00 00 added, the existing-file count corrected to 13, AC6 given a fact list and a stale-wording grep. The blank-only file went to the gate.
- 2026-09-27: criteria re-audit after the gate (full mode, second fresh [O] reader) probed the Scope rule on every AC1-AC5 file with the stated outcomes, and returned 6 findings, all fixed: AC3 gains two one-line files, AC4 gains a lone-CR file, 0A 00, 00 00 FE FF and both line ends, AC6's facts name the per-encoding split, tab and the line feed the blank-only branch needs, and the Goal is bounded to UTF-8 files without a NUL byte.
- 2026-09-27: plan gate chose a UTF-32 refusal of its own over one message naming "UTF-16 or UTF-32" because the error then says which encoding to convert from; falsified by a UTF-8 file the UTF-32 rule matches.
- 2026-09-27: plan gate chose to refuse a blank-only UTF-16 or UTF-32 file by its encoding over keeping the NUL-byte error because that error names lines the file does not hold; falsified by a UTF-8 file of blank lines refused as UTF-16 or UTF-32.
- 2026-09-27: plan chose reading whole code units of each encoding over keeping the NUL-every-other-byte rule with blank lines skipped because the byte rule still calls `a`,NUL,`b` and UTF-32 text UTF-16; falsified by a UTF-8 page-saved file the unit rule matches.

## Decisions
<!-- owner: implement / review · append-only; milestone-local; promote
     cross-cutting ones to cairn/DECISIONS.md. -->

## Review
<!-- owner: review · exclusive; evidence per criterion, consistency-gate
     results, review findings + triage. -->
