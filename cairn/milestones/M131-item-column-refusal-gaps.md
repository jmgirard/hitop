<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M131: Scoring refusals show hidden characters, pass blank SPSS codes, and refuse old SPSS codes and invalid UTF-8

- **Status:** review   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
- **Priority:** normal   <!-- owner: plan · create/amend-via-gate; high | normal | low -->
- **Depends on:** —   <!-- owner: plan · create/amend-via-gate; M<xx>, M<yy> or — -->
- **Driving RR:** —   <!-- owner: plan · create/amend-via-gate; RR<NN> whose Binding criteria bind this milestone's ACs (binding-criteria check), or — -->
- **Principles touched:** GP2, GP3   <!-- owner: plan · create/amend-via-gate; comma-separated IPn/GPn ids this milestone touches, or — -->
- **Resolves:** —   <!-- owner: plan · create/amend-via-gate; comma-separated GitHub issues the scope absorbs, each `#N closes` (the PR closes it at merge) or `#N partial` (the remainder gets a candidate row), or — ; skill conduct only — no validate check parses it -->
- **Surface tier:** user-facing — the refusals and results of seven exported scoring functions   <!-- owner: plan · create/amend-via-gate; user-facing | internal — <one-clause reason>; skill conduct only — no validate check parses it -->
- **Branch/PR:** m131-item-column-refusal-gaps   <!-- owner: implement (branch) / review (PR URL) · create -->

## Goal
<!-- owner: plan · create; a wrong goal returns to plan, never edited in place -->

The four item-column inputs M110 left open get the refusal or score D-068 and D-069 intend, and a Latin-1 value stops crashing the check.

## Scope
<!-- owner: plan · create/amend-via-gate -->

**In:** The item-column check in `R/util.R` (`validate_item_columns()` at `:339`, `unparsed_value()` at `:438`, `is_invisible()`, `code_points()`, `declared_missing()` at `:484`). It serves seven functions: the three `score_*()` functions, the three `reliability_*()` functions, and `validity_pid5()`.
- A refused value that mixes visible and invisible characters shows each invisible one as its code point.
- A declared-missing SPSS value that is blank after `trimws()` no longer causes a refusal.
- The pre-2.0 haven class `labelled_spss` gets the declared-missing refusal, with its own tip.
- A character value is converted to UTF-8 from its declared encoding. A value that is still not valid UTF-8 is refused under `hitop_nonnumeric_items`.
- The `items` docs of the seven functions, the development NEWS bullet, and one D-entry that annotates D-068(a) and D-069(b) and (e).

**Out:** Trimming Unicode spaces so `"1 "` scores as 1 (declined at the plan gate, see the work log). A separate class for the encoding refusal (declined at the plan gate). The unclassed score-column refusals of `norm_pid5()`, `plot_pid5()` and `interval_*()` stay in their own candidate row. A column name that is not valid UTF-8 is out, and so is a session in a locale that is not UTF-8. Neither has a known report, so a report takes `/hotfix`.

## Acceptance criteria
<!-- owner: plan · create/amend-via-gate; review reads, never reinterprets.
     Every item opens with its positional label — `ACn:` — the item's
     position counted top-to-bottom, the number Coverage cites; an
     insertion, removal, or reorder renumbers the labels and the Coverage
     lines together. -->

- [x] AC1: The `hitop_nonnumeric_items` message shows some refused values with code points. The rule covers both refusal kinds: a character value that does not parse, and a code that an SPSS column declares missing. The rule is by Unicode category: Z, Cc and Cf, the classes `is_invisible()` uses. A value can hold a character outside those categories and a character in them other than U+0020. Each such character then shows as its code point in angle brackets, so `"1 "` shows as `"1<U+00A0>"`. A value made only of Z, Cc and Cf characters, U+0020 included, still shows as its list of code points. The tests place one character from each category (U+00A0, U+0085, U+200B) at the start, middle and end of a value, in both refusal kinds. A value with an interior U+0020 shows unchanged.
- [x] AC2: A character SPSS column can declare missing a value that is blank after `trimws()`, such as `""`, `" "` or a tab. The rule applies to `haven_labelled_spss` and to the pre-2.0 class of AC3. The column is not refused for holding that value. Each function returns the same result as for the same column with no declaration, where the blank cells score as `NA`. A column that also holds a declared code that is not blank is still refused, and the message names that code. The tests build the current class with `haven::labelled_spss()`.
- [x] AC3: Before version 2.0, haven gave SPSS columns the class `c("labelled_spss", "labelled")`. The source is haven 1.1.2 `R/labelled_spss.R`, which sets `na_values` and `na_range`. A column of that class can hold a value that its `na_values` or `na_range` declares missing. The column is then refused under `hitop_nonnumeric_items`, and the value shows as it does for `haven_labelled_spss`. Its tip does not name `haven::zap_missing()`, because that function leaves such a column unchanged (haven 2.5.5, observed 2026-09-27). The tip says to set the declared values to `NA`. A call shows each class's tip only for a refused column of that class. The same column with no declared code returns the same result as its plain values. The tests use hand-built numeric and character columns of that class.
- [x] AC4: This criterion applies in a UTF-8 session. A character value is first converted to UTF-8 from the encoding that `Encoding()` declares. A value that is still not valid UTF-8 is refused under `hitop_nonnumeric_items`, before any other check on its column. The column can be plain, haven labelled, or SPSS. No base R error occurs. The message names the column and gives a tip that names `iconv()`. It shows each invalid byte in the form that `iconv(sub = "byte")` writes, so `"1\xa0"` shows as `"1<a0>"`. The tests place an invalid byte at the start, middle and end of a value. They also use the truncated sequences `"\xe2\x80"` and `"\xc3"`, a `"bytes"`-marked value, and an SPSS column that declares the invalid value missing. A Latin-1-marked `"1\xa0"` is not refused as invalid UTF-8. It is refused as text and shows as `"1<U+00A0>"`.
- [x] AC5: The seven functions share one check path. One probe per criterion, run through each of the seven functions, shows each behavior in AC1 to AC4 through all of them.
- [x] AC6: The `items` documentation of the seven functions states four things. It names the pre-2.0 `labelled_spss` class and the tip that sets its declared values to `NA`. It says a declared value that is blank is not refused. It names the refusal of text that is not valid UTF-8. The development NEWS bullet "Scoring refuses an item column it cannot read as numbers" states the same changes. A test from AC1 to AC5 backs each such claim.

## Coverage
<!-- owner: plan · create/amend-via-gate; each acceptance criterion → the
     task(s) satisfying it, by positional number (AC/Task counted
     top-to-bottom). Review reads to fence evidence — tracking-rules "AC fencing". -->

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T5
- AC6 → T6, T7

## Tasks
<!-- owner: plan (create) / implement (check-off, minor edits); substantive
     change is amend-via-gate. Every item opens with its positional label —
     `Tn:` — the item's position counted top-to-bottom, the number Coverage
     cites; an insertion, removal, or reorder renumbers the labels and the
     Coverage lines together. -->

- [x] T1: Write the AC1 tests in `tests/testthat/test-nonnumeric-items.R` and see them fail on the current `"1 "` display. Build the characters with `intToUtf8()`. Then add a helper beside `code_points()` that writes each Z, Cc and Cf character other than U+0020 as `<U+XXXX>`. Use it in both message branches of `validate_item_columns()` (`R/util.R:369`).
- [x] T2: Write the AC2 tests and see them fail. Then make `declared_missing()` (`R/util.R:484`) skip a character value that is blank after `trimws()`. Compare each function's result against the same column built with no declaration.
- [x] T3: Write the AC3 tests with hand-built columns of class `c("labelled_spss", "labelled")` and see them fail. Cite haven 1.1.2 `R/labelled_spss.R` in a test comment. Then extend the SPSS check in `unparsed_value()` (`R/util.R:442`) to that class. Give the class its own kind or tip, so the `zap_missing()` tip shows only for `haven_labelled_spss`.
- [x] T4: Write the AC4 tests and see them fail with base R's error. Build bytes with `rawToChar(as.raw(...))`, because a raw byte in `Rscript -e` fails in the parser. Then convert character values with `enc2utf8()` at the top of the character path in `unparsed_value()`. Refuse a value that `validUTF8()` rejects, before the SPSS check. Show the value with `iconv(sub = "byte")`, and add the `iconv()` tip. Make sure that `is_invisible()` and `trimws()` never see an invalid value.
- [x] T5: Write the AC5 test. It loops one probe per criterion over the seven functions, as the existing loop at `test-nonnumeric-items.R:105` does.
- [x] T6: Update the `items` docs of the seven functions (for example `R/score_pid5.R:16` to `:19`) and the development NEWS bullet at `NEWS.md:238`. Run `devtools::document()`.
- [x] T7: Append D-075, which annotates D-068(a) and D-069(b) and (e). It records four choices. They are the blank declared codes, the pre-2.0 class and its tip, the encoding refusal under the same class, and the mixed-value display. Run `devtools::test()` and `devtools::check()`, with 0 errors and 0 warnings.

## Work log
<!-- owner: any skill · append-only; one line per entry; absolute dates. -->

- 2026-09-27: created by /milestone-plan from the candidate row "Four item-column refusal gaps M110 deferred". Its lineage is M110 (AC5 re-audit, review F2, F4, F6) and M109 review G5. All four gaps reproduced on R 4.6.1 with haven 2.5.5.
- 2026-09-27: criteria audit (full mode, [O] fresh reader) returned 14 findings, all fixed before the gate. The fixes: U+000B probe replaced by U+0085, because `as.numeric()` accepts a vertical tab at the ends. Blank exemption extended to the old class. Per-class tips. "No base R error" instead of "no invalid UTF-8 error". Latin-1 control replaced, because it was a no-op on ASCII. More byte probes. AC5 restated as a behavior. AC6 names the docs that become false. The audit also found the Latin-1-marked crash, now in AC4.
- 2026-09-27: plan gate chose showing code points over trimming Unicode spaces so `"1 "` scores as 1. Trimming widens D-068(a)'s rule and scores an all-invisible value as `NA`. Falsified by a user whose export pads numbers with non-breaking spaces and who needs them scored without a step.
- 2026-09-27: plan gate chose the class `hitop_nonnumeric_items` for the encoding refusal over a new public class. D-069(d) made the same choice for the SPSS and 64-bit refusals. Falsified by a caller who must catch an encoding refusal apart from the other refusals.
- 2026-09-27: plan gate chose one milestone over splitting the encoding work into M132. All five changes touch the same two helpers and one test file. Falsified by a review that has to return one gap while the others are ready.
- 2026-09-27: implement started on branch m131-item-column-refusal-gaps. No question gate, because the plan fixed the display form, the class and the tips.
- 2026-09-27: T1 done. `mark_invisible()` in `R/util.R` writes each Z, Cc and Cf character other than U+0020 as `<U+XXXX>` in both message branches. The two new tests failed first on the old display. The M110 test at `test-nonnumeric-items.R:693` pinned the old `"1 "` display, so its two expectations now name the code point. Full suite: 22022 expectations, 0 failed.
- 2026-09-27: T2 done. `declared_missing()` skips a character value that is blank after `trimws()`. The two new tests failed first: the declared column was refused, and the mixed column named `""` rather than `"99"`. Full suite: 22087 expectations, 0 failed.
- 2026-09-27: T3 done. `unparsed_value()` applies the SPSS check to `labelled_spss` too and marks the refusal `old`, which gets its own tip naming `na_values` and `na_range`. The refusal tests failed first because the old-class codes scored. The no-code and blank-code old-class tests are regression guards. Full suite: 22438 expectations, 0 failed.
- 2026-09-27: T4 done. `unparsed_value()` converts character values with `enc2utf8()` and refuses a value `validUTF8()` rejects as kind `encoding`, first among the checks on a column. The message shows it through `iconv(sub = "byte")` and the tip names `iconv()`. Before the fix, the tests failed with base R `simpleError`s. The message was "input string 2 is invalid UTF-8", or "invalid multibyte string at '<a0>'" for the Latin-1 value. Full suite: 23465 expectations, 0 failed.
- 2026-09-27: T5 done. Each T1 to T4 test already loops its probe over `nonnumeric_cases`. So T5 adds a guard, stated apart from the list, that the list names the seven functions. A plant dropping `reliability_hitopbr` turned only that guard red. Full suite: 23466 expectations, 0 failed.
- 2026-09-27: T6 done. `validity_pid5()` inherits the `score_pid5()` passage. The six `items` passages and the NEWS bullet state the encoding refusal, the blank-code exemption, the old class and its tip, and the mixed-value display. `devtools::document()` rewrote seven Rd files. No article names these refusals. Full suite: 23466 expectations, 0 failed.
- 2026-09-27: T7 done. D-075 appended, annotating D-068(a) and D-069(b), (d) and (e). `devtools::check()`: 0 errors, 0 warnings, 0 notes.
- 2026-09-27: claim audit: 37 claims read, 3 corrected — tests/testthat/test-nonnumeric-items.R, R/util.R, NEWS.md
- 2026-09-27: the claim audit's [O] reader re-read the 3 corrections once and found all three true. Full suite: 23466 expectations, 0 failed. Status set to review.
- 2026-09-27: review started. No PR exists, and the branch contains main. AC1 to AC6 verified, and the consistency gate passed. The prior-review and blame-history lenses found nothing. The diff-bug lens is still running (checkpoint).

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive. -->

Evidence gathered 2026-09-27 on branch head c9d5c146, which contains main (cde67a5d). Full suite: 965 tests, 23466 expectations, 0 failed, 0 errors, 0 warnings. The 15 skips are the old merge-base skips. `test-nonnumeric-items.R`: 38 tests, 4024 expectations, 0 failed. A review probe outside the repo ran 28 checks through each of the seven functions. All 196 results were TRUE.

- AC1: the test at `test-nonnumeric-items.R:745` places U+00A0, U+0085 and U+200B at the start, middle and end, in both refusal kinds. It asserts that `<U+XXXX>` shows and the raw value does not. `:776` shows `"1 2"` unchanged. `:689` keeps the all-invisible list of code points. In all seven functions, the probe saw `"1<U+00A0>"` (text), `"<U+200B>1"` (declared missing) and `"1<U+0085>2"`. A declared `"  "` showed U+0020 and U+00A0 as code points.
- AC2: `:806` builds `haven::labelled_spss()` columns that declare `""`, `" "` or a tab, and compares each to the undeclared column. If `""` and `"99"` are both declared, the test at `:825` sees `"99"` named. The two tests compare results, so the probe added a plain-text control. For each blank code, the declared column returned a result, not an error, equal to the plain character column. This held in all seven functions.
- AC3: `:876` refuses hand-built `c("labelled_spss", "labelled")` columns of three kinds. It asserts the column name, the shown code, the old-class tip, and no `zap_missing()` tip. `:898` shows each tip only for its own class. If both classes are refused, it shows both tips. `:929` and `:949` show the no-code and blank-code columns scoring as their plain values. `:847` cites the haven 1.1.2 source. In all seven functions, the probe saw "holds 99, which it declares missing". It also saw the tip "declares to `NA` before scoring" and no `zap_missing`.
- AC4: `:997` refuses a lone invalid byte, a byte at the start, middle and end, and the truncated sequences `\xe2\x80` and `\xc3`. Each goes in a plain, a haven labelled, and an SPSS column that declares the value missing. It asserts the class, the column name, the `iconv(sub = "byte")` form and the `iconv()` tip. `:1025` refuses a `"bytes"`-marked value. `:1038` reads a Latin-1 `"1\xa0"` as text, shown as `"1<U+00A0>"`, with no "UTF-8" in the message. Both skip outside a UTF-8 session, and this session is UTF-8. The probe repeated all 18 byte cases and both marked values in all seven functions. Each was a `hitop_nonnumeric_items` condition, so no base R error occurred.
- AC5: every M131 test from `:745` to `:1038` loops its probe over `nonnumeric_cases`. The guard at `:46` states the seven function names apart from that list and passes. The review probe called each of the seven functions directly, outside that list, and all 196 results were TRUE.
- AC6: the added `items` roxygen lines are byte-identical in the six `R/score_*.R` and `R/reliability_*.R` files (same md5), and `validity_pid5()` inherits them (`man/validity_pid5.Rd` names `labelled_spss`). They name the pre-2.0 `labelled_spss` class and its set-to-`NA` tip, the blank declared value, and the refusal of text that is not valid UTF-8. The NEWS bullet states the same changes and the mixed-value display. The backing tests: blank at `:806`, old class and tip at `:876` and `:898`, UTF-8 at `:997`, Latin-1 at `:1038`, display at `:745`. `devtools::document()` made no diff.

Consistency gate, 2026-09-27:
- `cairn_validate.py`: exit 0, all checks PASS. Two advisory WARNs (dangling D-001 to D-012 tokens, and `schmukle2026.md` provenance) are older than this branch.
- `cairn_impact.py`: skipped. The branch changes no `DESIGN.md` principle.
- `devtools::document()`: no diff. `pkgdown::check_pkgdown()`: no problems. README files untouched.
- NEWS.md: the development bullet states the changes and names no milestone. The branch adds no top-level file.
- `devtools::check()`: 0 errors, 0 warnings, 0 notes (5 min).

Independent review, 2026-09-27. The [S] prior-review lens found no regression of an M109 or M110 review finding, and no PR threads exist. The [S] blame-history lens found no conflict. The one flipped M110 expectation is recorded in D-075(d). The [O] diff-bug lens found no criterion failure and seven findings. The dispositions are proposed and wait for the gate:
- F1 (reproduced): the NEWS bullet and D-075 Context say Latin-1 text "used to stop with a base R error". On main, `"aé"` and `"a<A0>b"` marked Latin-1 were refused normally. Only values such as `"1é"` and `"1\xa0"` crashed. Proposed: fix now. Narrow NEWS, and add one annotating D-entry for the D-075 Context clause.
- F2: outside a UTF-8 session, unmarked invalid bytes are refused as choice text, with no `iconv()` tip. The docs and NEWS state the UTF-8 refusal with no locale scope. Proposed: fix now. Add "in a UTF-8 session" to the docs and NEWS. The Scope already puts other locales out.
- F3 (reproduced): the encoding message does not apply `mark_invisible()`. Bytes `c2 a0 31 a0` show a raw non-breaking space before `1<a0>`. Proposed: fix now, with a test.
- F4: a `"bytes"`-marked value that is valid UTF-8 shows as escapes (`"x\\xc3\\xa9"`). The refusal still happens. Proposed: reject, because readers do not mark valid text as bytes and the refusal is right.
- F5: the two blank-code tests compare two calls. Two calls that fail the same way also pass them. Proposed: fix now. Assert that the declared call returns a result equal to the column with those cells as `NA`.
- F6: an old-class SPSS factor column shows its code unquoted. Proposed: reject, because the input is contrived and the refusal is right.
- F7: `item_values()` does not unclass the old class. It works today because base `[` and `as.numeric()` drop the class. Proposed: reject, because no failure reproduces and the parse path took that class before this branch.
