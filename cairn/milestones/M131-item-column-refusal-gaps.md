<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M131: Scoring refusals show hidden characters, pass blank SPSS codes, and refuse old SPSS codes and invalid UTF-8

- **Status:** in-progress   <!-- owner: transitioning skill · mirror-update; cairn/ROADMAP.md is the authority -->
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

- [ ] AC1: The `hitop_nonnumeric_items` message shows some refused values with code points. The rule covers both refusal kinds: a character value that does not parse, and a code that an SPSS column declares missing. The rule is by Unicode category: Z, Cc and Cf, the classes `is_invisible()` uses. A value can hold a character outside those categories and a character in them other than U+0020. Each such character then shows as its code point in angle brackets, so `"1 "` shows as `"1<U+00A0>"`. A value made only of Z, Cc and Cf characters, U+0020 included, still shows as its list of code points. The tests place one character from each category (U+00A0, U+0085, U+200B) at the start, middle and end of a value, in both refusal kinds. A value with an interior U+0020 shows unchanged.
- [ ] AC2: A character SPSS column can declare missing a value that is blank after `trimws()`, such as `""`, `" "` or a tab. The rule applies to `haven_labelled_spss` and to the pre-2.0 class of AC3. The column is not refused for holding that value. Each function returns the same result as for the same column with no declaration, where the blank cells score as `NA`. A column that also holds a declared code that is not blank is still refused, and the message names that code. The tests build the current class with `haven::labelled_spss()`.
- [ ] AC3: Before version 2.0, haven gave SPSS columns the class `c("labelled_spss", "labelled")`. The source is haven 1.1.2 `R/labelled_spss.R`, which sets `na_values` and `na_range`. A column of that class can hold a value that its `na_values` or `na_range` declares missing. The column is then refused under `hitop_nonnumeric_items`, and the value shows as it does for `haven_labelled_spss`. Its tip does not name `haven::zap_missing()`, because that function leaves such a column unchanged (haven 2.5.5, observed 2026-09-27). The tip says to set the declared values to `NA`. A call shows each class's tip only for a refused column of that class. The same column with no declared code returns the same result as its plain values. The tests use hand-built numeric and character columns of that class.
- [ ] AC4: This criterion applies in a UTF-8 session. A character value is first converted to UTF-8 from the encoding that `Encoding()` declares. A value that is still not valid UTF-8 is refused under `hitop_nonnumeric_items`, before any other check on its column. The column can be plain, haven labelled, or SPSS. No base R error occurs. The message names the column and gives a tip that names `iconv()`. It shows each invalid byte in the form that `iconv(sub = "byte")` writes, so `"1\xa0"` shows as `"1<a0>"`. The tests place an invalid byte at the start, middle and end of a value. They also use the truncated sequences `"\xe2\x80"` and `"\xc3"`, a `"bytes"`-marked value, and an SPSS column that declares the invalid value missing. A Latin-1-marked `"1\xa0"` is not refused as invalid UTF-8. It is refused as text and shows as `"1<U+00A0>"`.
- [ ] AC5: The seven functions share one check path. One probe per criterion, run through each of the seven functions, shows each behavior in AC1 to AC4 through all of them.
- [ ] AC6: The `items` documentation of the seven functions states four things. It names the pre-2.0 `labelled_spss` class and the tip that sets its declared values to `NA`. It says a declared value that is blank is not refused. It names the refusal of text that is not valid UTF-8. The development NEWS bullet "Scoring refuses an item column it cannot read as numbers" states the same changes. A test from AC1 to AC5 backs each such claim.

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
- [ ] T5: Write the AC5 test. It loops one probe per criterion over the seven functions, as the existing loop at `test-nonnumeric-items.R:105` does.
- [ ] T6: Update the `items` docs of the seven functions (for example `R/score_pid5.R:16` to `:19`) and the development NEWS bullet at `NEWS.md:238`. Run `devtools::document()`.
- [ ] T7: Append D-075, which annotates D-068(a) and D-069(b) and (e). It records four choices. They are the blank declared codes, the pre-2.0 class and its tip, the encoding refusal under the same class, and the mixed-value display. Run `devtools::test()` and `devtools::check()`, with 0 errors and 0 warnings.

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
- 2026-09-27: T4 done. `unparsed_value()` converts character values with `enc2utf8()` and refuses a value `validUTF8()` rejects as kind `encoding`, first among the checks on a column. The message shows it through `iconv(sub = "byte")` and the tip names `iconv()`. Before the fix, the tests failed with base R `simpleError`s ("input string 2 is invalid UTF-8", and "invalid multibyte string at '<a0>'" for the Latin-1 value). Full suite: 23465 expectations, 0 failed.

## Decisions
<!-- owner: implement / review · append-only; milestone-local. -->

## Review
<!-- owner: review · exclusive. -->
