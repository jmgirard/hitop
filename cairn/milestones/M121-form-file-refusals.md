# M121: `read_form_responses()` and hitop-form refuse a malformed file by name: a short or long row, an empty file, a value refusal naming no row, an unnumbered or second-stem item column, and a descriptor whose items are not ascending

- **Status:** in-progress
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

- [ ] AC1: `read_form_responses()` stops on a file holding a response row whose field count differs from the header's, with a message naming the file and the response rows, for a row with fewer fields and for a row with more; shown by a test over a three-row file of each shape whose bad row is the first, and by a test over a file with two bad rows asserting both row numbers. A file whose `participant` cell holds `#`, and a file whose `study` cell is quoted and holds a line break, read as before, shown by a test over each.
- [ ] AC2: `read_form_responses()` stops on a zero-byte file, on a file of blank lines only, and on a file holding only a UTF-8 byte-order mark, each with the reader's own message naming the file; shown by a test over each asserting the file name in the message and that the condition is not base R's "no lines available in input".
- [ ] AC3: Each of the reader's four value refusals (an item value that is not a whole number, one outside the integer range, a `form_build` that does not parse, a `submitted` that does not parse) names the response rows at fault, counted from the first row after the header; shown by a test per refusal over a three-row file whose bad value is on row 1, and by one test with bad values on rows 1 and 3 asserting both numbers.
- [ ] AC4: `read_form_responses()` stops on a file with an item column whose name does not match `^[a-z0-9]+_[0-9]+$` (`foo`, `hitopbr_`, `_01`, `Hitopbr_01`), naming the file and the column, and on a file whose item columns carry more than one stem (`hitopbr_01` beside `pid5bf_01`), naming the file and the stems; shown by a test per shape. A `foo` column holding text is refused for its name, not as a non-whole value, and a two-stem file with an `item_order` cell of `1 1` is refused for the stems, not the cell; shown by a test of each.
- [ ] AC5: hitop-form refuses a study link whose descriptor lists `items` in an order that is not ascending, on the form page with a message naming the fault, and the link builder refuses the same pasted descriptor with the same message; shown by a Playwright test of each over two descriptors built from `module-plain.json`, one with `items` reversed and one ascending but for its last two swapped. The existing descriptor tests (an ascending `items`, an `itemOrder` that rearranges them) stay green.
- [ ] AC6: `?read_form_responses` describes the four new refusals and says a module descriptor's `items` are ascending; `?write_module`'s `items` text says hitop-form requires them ascending while `read_module()` compares them as a set; the online-collection article's two-forms paragraph says a sheet mixing two instruments' stems is refused and a module's beside the full HiTOP-SR still reads as one; the hitop-form README's descriptor paragraph and its tests table say the page refuses a descriptor whose items are not ascending; NEWS holds an entry for the reader's refusals and the page's check, each behavior it asserts enforced by a test in AC1 to AC5.
- [ ] AC7: In hitop, `devtools::document()` leaves no diff and `devtools::test()` is clean; in hitop-form, `npx playwright test` is clean.

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
- [ ] T2: In `read_form_response_file()`: wrap `read.csv()` in `rlang::try_fetch()` and refuse its empty-input error naming the file; count each line's fields with `utils::count.fields(file, sep = ",", quote = "\"", comment.char = "")`, fold an `NA` count (a quoted line break's continuation) into its record, and refuse a record whose count differs from the header's, naming the rows; right after `item_cols` is computed (`R/read_form_responses.R:266`), refuse a name outside `^[a-z0-9]+_[0-9]+$` and a second stem; carry `which()` rows into the four value refusals as the `item_order` refusal does (`R/read_form_responses.R:321`). Tests green.
- [ ] T3: Roxygen for `?read_form_responses` (Details, `R/read_form_responses.R:14-70`) and `?write_module` (`R/module_file.R:52-56`), `devtools::document()`, the online-collection article paragraph (`vignettes/articles/online-collection.Rmd:53-57`), a NEWS entry, and a purl-and-run of the article (LESSONS M115).
- [ ] T4: In hitop-form `tests/guard.spec.js` (after G6, `form.js` descriptor guards) and `tests/link.spec.js`, add the AC5 tests over the two descriptors; run red.
- [ ] T5: In `form.js` `checkModule()`, after the repeat check (`form.js:310`), refuse `items` that are not ascending with `bad('its items are not in ascending order.')`; update the README descriptor paragraph (`README.md:34-45`) and the `guard.spec.js` and `link.spec.js` rows of its tests table; `npx playwright test` green.

## Work log

- 2026-09-24: created by /milestone-plan, absorbing the M116 F1 and the M113/M115 candidate rows.
- 2026-09-24: criteria audit ran in full mode ([O] fresh reader, M121 and M122 together): 13 findings; 8 on this file fixed in the draft (the field count reads `#` and quoted line breaks, bad rows first with a two-row probe, a BOM-only file, the stem regex written into AC4 and the check moved before the value checks, the article paragraph and `?write_module` in AC6, an adjacent-swap probe); 1 posed at the gate (the D-039 annotation, now D-073).
- 2026-09-24: plan gate chose refusing an item column whose name is outside `^[a-z0-9]+_[0-9]+$` over ignoring such columns because the reader already refuses a file that does not look like the page's and a dropped column would lose a mistyped item silently; falsified by a store export whose own columns cannot be dropped before the read.
- 2026-09-24: plan gate chose the page refusing non-ascending `items` (D-073) over sorting them in the page and over refusing them in `read_module()` because the page's descriptor check refuses by name and `read_module()` already returns ascending items from its rebuild; falsified by a descriptor writer other than `write_module()` and the Module Builder that lists items in another order on purpose.
- 2026-09-24: implement started; branch `m121-form-file-refusals` cut from pushed main in hitop and from `origin/main` in hitop-form. No question gate: the plan fixed the checks, their order and their wording sites. The lint hook's hits on the tracking files are cairn's record grammar, left as at M120.
- 2026-09-24: T1 done: 21 refusal tests red (a short row and the four bad column names read silently today; a long row stops with base R's "duplicate 'row.names'"; an empty file with "no lines available in input"; the value refusals name columns, not rows); the `#` cell and the quoted line break tests green as they stand.

## Decisions

## Review
