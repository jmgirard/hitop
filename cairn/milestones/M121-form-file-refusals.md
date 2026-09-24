# M121: `read_form_responses()` and hitop-form refuse a malformed file by name: a short or long row, an empty file, a value refusal naming no row, an unnumbered or second-stem item column, and a descriptor whose items are not ascending

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — an exported reader's refusals and the public page's descriptor check
- **Branch/PR:** `m121-form-file-refusals` (hitop and hitop-form)

## Goal

A malformed hitop-form file stops at `read_form_responses()` or at the hitop-form page with a message that names the file, the row or column at fault, and the fault, instead of reading as data or as an unrelated fault.

## Scope

**In:** `read_form_response_file()` (`R/read_form_responses.R:213`) gains four refusals, each an unclassed `cli_abort()` naming the file, as M113 and M115 added theirs under D-064. A response row whose field count differs from the header's, fewer or more, is refused naming the row, from a field count taken before `read.csv()` pads or wraps it; the count reads `#` as data and a quoted line break as one record, as `read.csv()` does. A file with no header (zero bytes, blank lines only, a BOM only) is refused instead of base R's "no lines available in input". The whole-number, integer-range, `form_build` and `submitted` refusals name the response rows at fault, as the `item_order` refusal does. Before any value check, an item column whose name is not `^[a-z0-9]+_[0-9]+$` is refused naming the column, and item columns whose stems (the part before the last underscore) differ are refused naming the stems, so the `item_order` check reads unambiguous numbers. In hitop-form, `checkModule()` (`form.js:290`) refuses a descriptor whose `items` are not in ascending order, on the form page and in the link builder, which calls the same function (D-073). Docs: `?read_form_responses`, `?write_module`'s `items` text, the online-collection article's two-forms paragraph, the hitop-form README, and a NEWS entry.

**Out:** `read_module()` is unchanged: it returns the rebuild's ascending items whatever order the file lists (`R/module_file.R:478`), and D-073 keeps its set comparison. Condition classes for the new refusals: D-064 keeps the reader's two public classes, and a class for a further refusal takes a D-entry under D-034(c) when a caller asks to catch one. A descriptor naming a PID-5 instrument: DESIGN Known issue 11. Dropping unknown columns instead of refusing them, and sorting a descriptor's `items` in the page: rejected at the plan gate (work log).

## Acceptance criteria

- [x] AC1: `read_form_responses()` stops on a file holding a response row whose field count differs from the header's, with a message naming the file and the response rows, for a row with fewer fields and for a row with more; shown by a test over a three-row file of each shape whose bad row is the first, and by a test over a file with two bad rows asserting both row numbers. A file whose `participant` cell holds `#`, and a file whose `study` cell is quoted and holds a line break, read as before, shown by a test over each.
- [x] AC2: `read_form_responses()` stops on a zero-byte file, on a file of blank lines only, and on a file holding only a UTF-8 byte-order mark, each with the reader's own message naming the file; shown by a test over each asserting the file name in the message and that the condition is not base R's "no lines available in input".
- [x] AC3: Each of the reader's four value refusals (an item value that is not a whole number, one outside the integer range, a `form_build` that does not parse, a `submitted` that does not parse) names the response rows at fault, counted from the first row after the header; shown by a test per refusal over a three-row file whose bad value is on row 1, and by one test with bad values on rows 1 and 3 asserting both numbers.
- [x] AC4: `read_form_responses()` stops on a file with an item column whose name does not match `^[a-z0-9]+_[0-9]+$` (`foo`, `hitopbr_`, `_01`, `Hitopbr_01`), naming the file and the column, and on a file whose item columns carry more than one stem (`hitopbr_01` beside `pid5bf_01`), naming the file and the stems; shown by a test per shape. A `foo` column holding text is refused for its name, not as a non-whole value, and a two-stem file with an `item_order` cell of `1 1` is refused for the stems, not the cell; shown by a test of each.
- [x] AC5: hitop-form refuses a study link whose descriptor lists `items` in an order that is not ascending, on the form page with a message naming the fault, and the link builder refuses the same pasted descriptor with the same message; shown by a Playwright test of each over two descriptors built from `module-plain.json`, one with `items` reversed and one ascending but for its last two swapped. The existing descriptor tests (an ascending `items`, an `itemOrder` that rearranges them) stay green.
- [x] AC6: `?read_form_responses` describes the four new refusals and says a module descriptor's `items` are ascending; `?write_module`'s `items` text says hitop-form requires them ascending while `read_module()` compares them as a set; the online-collection article's two-forms paragraph says a sheet mixing two instruments' stems is refused and a module's beside the full HiTOP-SR still reads as one; the hitop-form README's descriptor paragraph and its tests table say the page refuses a descriptor whose items are not ascending; NEWS holds an entry for the reader's refusals and the page's check, each behavior it asserts enforced by a test in AC1 to AC5.
- [x] AC7: In hitop, `devtools::document()` leaves no diff and `devtools::test()` is clean; in hitop-form, `npx playwright test` is clean.

## Coverage

- AC1 → T1, T2
- AC2 → T1, T2
- AC3 → T1, T2
- AC4 → T1, T2
- AC5 → T4, T5
- AC6 → T3, T5
- AC7 → T2, T3, T5

## Tasks

- [x] T1: In `tests/testthat/test-read_form_responses.R`, add the tests AC1 to AC4 name, each writing its file in a temp directory as the file's other tests do; run them red against the reader as it stands (the two valid-file tests of AC1 are green already and stay).
- [x] T2: In `read_form_response_file()`: wrap `read.csv()` in `rlang::try_fetch()` and refuse its empty-input error naming the file; count each line's fields with `utils::count.fields(file, sep = ",", quote = "\"", comment.char = "")`, fold an `NA` count (a quoted line break's continuation) into its record, and refuse a record whose count differs from the header's, naming the rows; right after `item_cols` is computed (`R/read_form_responses.R:266`), refuse a name outside `^[a-z0-9]+_[0-9]+$` and a second stem; carry `which()` rows into the four value refusals as the `item_order` refusal does (`R/read_form_responses.R:321`). Tests green.
- [x] T3: Roxygen for `?read_form_responses` (Details, `R/read_form_responses.R:14-70`) and `?write_module` (`R/module_file.R:52-56`), `devtools::document()`, the online-collection article paragraph (`vignettes/articles/online-collection.Rmd:53-57`), a NEWS entry, and a purl-and-run of the article (LESSONS M115).
- [x] T4: In hitop-form `tests/guard.spec.js` (after G6, `form.js` descriptor guards) and `tests/link.spec.js`, add the AC5 tests over the two descriptors; run red.
- [x] T5: In `form.js` `checkModule()`, after the repeat check (`form.js:310`), refuse `items` that are not ascending with `bad('its items are not in ascending order.')`; update the README descriptor paragraph (`README.md:34-45`) and the `guard.spec.js` and `link.spec.js` rows of its tests table; `npx playwright test` green.

## Work log

- 2026-09-24: created by /milestone-plan, absorbing the M116 F1 and the M113/M115 candidate rows.
- 2026-09-24: criteria audit ran in full mode ([O] fresh reader, M121 and M122 together): 13 findings; 8 on this file fixed in the draft (the field count reads `#` and quoted line breaks, bad rows first with a two-row probe, a BOM-only file, the stem regex written into AC4 and the check moved before the value checks, the article paragraph and `?write_module` in AC6, an adjacent-swap probe); 1 posed at the gate (the D-039 annotation, now D-073).
- 2026-09-24: plan gate chose refusing an item column whose name is outside `^[a-z0-9]+_[0-9]+$` over ignoring such columns because the reader already refuses a file that does not look like the page's and a dropped column would lose a mistyped item silently; falsified by a store export whose own columns cannot be dropped before the read.
- 2026-09-24: plan gate chose the page refusing non-ascending `items` (D-073) over sorting them in the page and over refusing them in `read_module()` because the page's descriptor check refuses by name and `read_module()` already returns ascending items from its rebuild; falsified by a descriptor writer other than `write_module()` and the Module Builder that lists items in another order on purpose.
- 2026-09-24: implement started; branch `m121-form-file-refusals` cut from pushed main in hitop and from `origin/main` in hitop-form. No question gate: the plan fixed the checks, their order and their wording sites. The lint hook's hits on the tracking files are cairn's record grammar, left as at M120.
- 2026-09-24: T1 done: 21 refusal tests red (a short row and the four bad column names read silently today; a long row stops with base R's "duplicate 'row.names'"; an empty file with "no lines available in input"; the value refusals name columns, not rows); the `#` cell and the quoted line break tests green as they stand.
- 2026-09-24: T2 done; reader file 83 tests green, suite 817 green. Deviation from the task text: no `rlang::try_fetch()` around `read.csv()`. The field count runs before `read.csv()`, because a long first row stops `read.csv()` with "duplicate 'row.names'" before any later check, and `count.fields()` returns nothing for a zero-byte, blank-lines-only or BOM-only file, so the no-header refusal reads from that and `read.csv()`'s empty-input error is never reached. A file of whitespace-only lines reads as a zero-column frame and is refused by the first-columns check, as before.
- 2026-09-24: T3 done: `?read_form_responses` Details lists the refusals and names the rows; `?write_module` `items` text; the article's two-forms paragraph; NEWS under "Improvements and fixes"; `document()` stable; the online-collection and modules articles purled and run clean; suite 817 green.
- 2026-09-24: T4 done (hitop-form commit on `m121-form-file-refusals`): `NOT_ASCENDING`, `NOT_ASCENDING_MESSAGE` and `notAscendingDescriptor()` in `helpers.mjs`; G13 in `guard.spec.js` and L15 in `link.spec.js`, two tests each; all four red, the page starting the form and the builder printing a link.
- 2026-09-24: T5 done (hitop-form): `checkModule()` refuses `items` with any entry below the one before it, after the repeat check; README's descriptor step and the link and guard rows of its tests table; `npx playwright test` 201 passed, the ascending-items and `itemOrder` descriptor tests among them.
- 2026-09-24: claim audit: 42 claims read, 7 corrected — NEWS.md, R/read_form_responses.R (a comment), tests/testthat/test-read_form_responses.R (a comment), vignettes/articles/online-collection.Rmd, hitop-form README.md, form.js (a comment), tests/guard.spec.js (a comment). The re-read cleared six; the NEWS "before" sentence was then narrowed to the two faults the reader observed on the old reader, without a further pass. The article's two-forms paragraph now carries the random-order exception (a module row's `item_order` lists only the module's items, so the reader refuses the mixed download).
- 2026-09-24: status → review; `document()` no diff, suite 817 green, Playwright 201 passed.
- 2026-09-24: review pass 1: every criterion verified with fresh evidence, gate clean, three lenses run; one test assertion, one comment and one hitop-form README sentence fixed at the gate (Review F2, F5, F10); no return.

## Decisions

## Review

- 2026-09-24 sync: in hitop and in hitop-form the branch holds `origin/main` as an ancestor and no PR exists; nothing to merge in.
- AC1 evidence: the three field-count tests (a short row first, a long row first, rows 1 and 3 both named and row 2 not) and the `#` cell and quoted line-break read tests pass in `tests/testthat/test-read_form_responses.R`; full suite 20099 pass, 0 fail, 0 warn, 15 skips in unrelated files.
- AC2 evidence: the zero-byte, blank-lines-only and BOM-only tests each assert the file name, "no header row" and the absence of "no lines available in input"; pass.
- AC3 evidence: eight tests, each of the four refusals over a bad row 1 (asserting "Response row 1", no "rows") and over rows 1 and 3 (asserting "Response rows 1 and 3"); pass.
- AC4 evidence: the four name tests, the `foo`-text test and the two stem tests pass. Review strengthened the four name tests' column assertion to `Column <name>.` because the hint's `hitopbr_01` holds `hitopbr_` and `_01` as substrings, so two of them could not fail (F2); a probe showed the old assertion passing and the new one failing on a message naming another column; the reader file is green after the change.
- AC5 evidence: `npx playwright test` 201 passed; G13 (items reversed, last two swapped) on the form page asserts the message and no Begin button, L15 the same two in the builder assert the message and an empty link output; the ascending-items and `itemOrder` render tests pass.
- AC6 evidence, read on the branch: `?read_form_responses` Details lists the no-header, field-count, column-name, stem and value refusals with the row numbering and says `write_module()` writes items ascending and the page requires them; `?write_module`'s `items` text names the page's requirement beside the set comparison; the article's two-forms paragraph carries the two-stem refusal and the one-stem read with its random-order exception; the hitop-form README step 4 and its `guard.spec.js` and `link.spec.js` rows say the page and the builder refuse a descriptor whose items are not ascending; NEWS has the entry under "Improvements and fixes", each behavior it asserts matched to a test in AC1 to AC5, its "before" sentence describing the old reader as T1's red run observed it.
- AC7 evidence: `devtools::document()` no diff; `devtools::test()` 0 fail, 0 warn; `npx playwright test` 201 passed.
- Driving RR: none, no projection to record.
- Consistency gate: `cairn_validate.py` all checks passed (24 advisory warnings, dangling id tokens and one references staleness, pre-existing); no principle changed, so `cairn_impact` skipped; `document()` no diff; `README.Rmd` untouched; `pkgdown::check_pkgdown()` no problems; NEWS entry present; no new top-level file; `devtools::check()` 0 errors, 0 warnings, 0 notes.
- Independent review: [O] diff-bug 12 findings, none a criterion failure; [S] blame-history 0 findings; [S] prior-review 0 findings (archives M096 to M120 read; both repos' PR comment probes empty).
- F1 (follow-up, candidate row): a line of spaces or a tab, or a BOM plus whitespace lines, still stops with base R's "first five rows are empty" or "duplicate 'row.names'", not naming the file; outside AC2's three shapes.
- F2 (fixed now): the `hitopbr_` and `_01` name tests matched the hint; assertion narrowed to the column line.
- F3 (rejected): the field-count refusal prints one line per bad row with no cap; the per-row line carries that row's count, which a one-line list would lose, and a store export with hundreds of ragged rows is a store fault.
- F4 (follow-up, candidate row): a non-UTF-8 byte stops the field count early, so the refusal names a wrong count with a warning nothing catches; the M110 row's invalid UTF-8 item is the value-level case.
- F5 (fixed now): the comment before the name check claimed the `item_order` check then reads "unambiguous numbers", but `hitopbr_01` beside `hitopbr_1` passes both checks; reworded to "item numbers of one stem".
- F6 (follow-up, candidate row): a multi-line whitespace-only file is refused with an empty first-columns list.
- F7 (follow-up, candidate row): leading whitespace lines are taken as the header, so the field-count refusal's row numbers point past the real header.
- F8 (rejected): the field-count check runs before the lead-column check, so a ragged non-hitop-form file is refused for its rows, and the note says "The row" beside several rows; either refusal stops the file, and the wording is cosmetic.
- F9 (follow-up, candidate row): the whole-number and integer-range refusals list columns and rows on separate lines, so a multi-row export does not say which cell; AC3 as written is met.
- F10 (fixed now): the hitop-form README file-format paragraph now says the page requires ascending items, as the roxygen does.
- F11 (rejected): no test builds the mixed module-plus-full download the article describes; the existing `item_order` "missing" test and the blank-cell `NA` tests cover the mechanism.
- F12 (follow-up, candidate row): the item-column stem is not checked against the `instrument` cell; out of scope.
