# M36: Two-line response-option legend on the PID paper forms

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m36-pid-legend-two-lines`

## Goal

The PID paper forms print their response-option legend on two lines of two options each, so no option phrase is broken mid-phrase by column wrapping, while the HiTOP-SR/BR forms keep their single-line legend and no legend wording changes.

## Scope

**In:** a pairs-per-line argument on the shared `make_items_table()` helper (`R/generate_docx.R:236`), used by the three PID DOCX generators (`R/generate_docx.R:997, 1061, 1125`); a one-line fix to the same helper's even-row shading index, which aborts on a one-item table (added at the 2026-07-31 implementation gate); parse-based tests over the generated legend for PID and for SR/BR; regeneration of the six committed PID DOCX artifacts through `data-raw/artifacts.R`'s existing `rebuild_stems`/`rebuild_formats` filters, with their `hitop_artifacts` rows and a NEWS entry.

**Out:** any change to legend *wording*, option values, or labels — IP1 content, signed off as layout-only at the 2026-07-30 plan gate and recorded as a D-entry, which is not a licence for wording drift. The HSUM overview's separately hardcoded response-option sentence (`R/generate_docx.R:919, 936`) — a different code path with a 58-character string that does not wrap; a candidate row if it ever does. Qualtrics and REDCap artifacts — the legend is a DOCX layout concern and their generators never call `make_items_table()`. A repeatable width-budget check on the rendered line — the maintainer chose visual confirmation at the plan gate; a candidate row if the estimate is ever wanted.

## Acceptance criteria

- [ ] AC1 `make_items_table()` takes an argument setting how many `value = label` pairs print per legend header line, defaulting to all pairs on one line derived from `nrow(opts)` rather than a hardcoded 4. Evidence is in two parts, because a permanent test cannot read the pre-milestone bytes (`R CMD check` builds from a tarball with no `.git`, and the merge-base moves once this branch merges): (a) a one-time implement-time comparison of the default legend against `git show <merge-base>:inst/extdata/pid5_US.docx`, recorded verbatim in the Review section; and (b) a permanent test asserting that the default legend matches the legend in the committed `hitopsr_US.docx` and `hitopbr_US.docx` — forms this milestone does not rebuild, so they stay an external oracle rather than the code's own output (IP2).
- [ ] AC2 A freshly generated document from each of `generate_docx_pid5()`, `generate_docx_pid5sf()`, and `generate_docx_pid5bf()` carries, in the items table's own header and not counting the scoring table's header row, exactly two legend lines: the first holding the pairs for values 0 and 1, the second the pairs for values 2 and 3 — verified by parsing `word/document.xml`, never by checksum (LESSONS 2026-07-16, M20).
- [ ] AC3 The pairs recovered from those two lines equal `pid_instructions$options` value-for-value (compared as character) and label-for-label in printed order; the ` • ` separator at the 1|2 boundary is removed by the split and no character is added anywhere.
- [ ] AC4 Freshly generated `generate_docx_hitopsr()` and `generate_docx_hitopbr()` documents each carry exactly one legend header line holding all four pairs.
- [ ] AC5 The six committed PID DOCX (`pid5`, `pid5sf`, `pid5bf` × `_US`/`_A4`) are regenerated through `data-raw/artifacts.R` with `rebuild_stems <- c("pid5","pid5sf","pid5bf")` and `rebuild_formats <- c("docx")`, each gaining one new `hitop_artifacts` row dated the rebuild day with a `changes` note naming the legend split; each committed file parses back to the two header lines of AC2; NEWS.md records the regenerated forms (GP2).
- [ ] AC6 No artifact other than those six changes: every other file in `inst/extdata/` is byte-identical to its pre-milestone committed bytes (compared against `<merge-base>` via `git show`), and `hitop_artifacts` gains no row for any of them.
- [ ] AC7 The rebuilt US and A4 PID forms are opened and visually confirmed to break no option phrase mid-phrase, with a screenshot recorded as review evidence (the maintainer's chosen proof — Word's line breaking is not observable from `document.xml`).
- [ ] AC8 `devtools::test()` and `devtools::check()` clean.
- [ ] AC9 `make_items_table()` builds a one-item table without error, with a regression test that fails against the pre-fix `seq(2, n, by = 2)` shading index (added to scope at the maintainer's direction at the 2026-07-31 implementation gate).

## Coverage

- AC1 → T1
- AC2 → T1, T2
- AC3 → T2
- AC4 → T2
- AC5 → T3, T4
- AC6 → T3
- AC7 → T4
- AC8 → T4
- AC9 → T1

## Tasks

- [x] T1 Add the pairs-per-line argument to `make_items_table()` (`R/generate_docx.R:236-250`), defaulting to `nrow(opts)`; split the legend into that many `add_header_lines()` values. Verify the default against the legend recovered from the pre-milestone committed `pid5_US.docx` (one-time, recorded as evidence) and lock it with a test against the committed SR/BR forms. Fix the even-row shading index in the same helper and add its regression test.
- [x] T2 Pass 2 from the three PID generators (`R/generate_docx.R:997, 1061, 1125`); add parse-based tests over fresh tempfiles asserting two lines with the expected pairs for PID and one line for SR/BR. Anchor the assertions on line structure, not bare substring presence (LESSONS 2026-07-30, M26).
- [x] T3 Regenerate the six PID DOCX via `data-raw/artifacts.R` with the two filters set; confirm exactly six new manifest rows and that no other `inst/extdata/` file moved.
- [x] T4 NEWS entry; open the rebuilt US and A4 forms and capture the visual confirmation; run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-07-31: created by /milestone-plan.
- 2026-07-31: plan-gate criteria audit ([O], fresh context) returned seven findings — five fixed in place before the gate (AC1 under-determined default and self-referential oracle, AC2 header-count scoping, AC3 separator ambiguity, AC5 md5-proves-nothing plus the uncovered committed files, AC6 heading/body mismatch and unpinned ref), two carried to the gate as questions (no criterion tested the wrap goal; no IP1 sign-off was named).
- 2026-07-31: plan gate chose an explicit pairs-per-line argument over a width-driven automatic split because Word's line breaking is not observable from the file format, so an estimator would be an approximation shipped as a rule; falsified by an instrument whose option labels make the correct split depend on rendered width rather than option count.
- 2026-07-31: plan gate chose visual confirmation of the rebuilt forms over a repeatable width-budget test because the estimate approximates Word's real breaking and can pass while the phrase still wraps; falsified by a later wrap regression reaching a committed form unnoticed, which is the case a repeatable check would have caught.
- 2026-07-31: Jeff signed off the six PID DOCX rebuild as layout-only under IP1 at the plan gate (D-028).
- 2026-07-31: branch `m36-pid-legend-two-lines` cut from main at 25a3a1b; status in-progress.
- 2026-07-31: T1 done — `make_items_table()` gained `opts_per_line` (default `nrow(opts)`), the three PID generators pass 2, and the even-row shading index moved to `which()`. One-time AC1(a) check: the default legend is identical to the legend in `git show 25a3a1b:inst/extdata/pid5_US.docx` (one line, all four pairs).
- 2026-07-31: implementation gate amended AC1 — a permanent test cannot read pre-milestone bytes (`R CMD check` builds from a tarball with no `.git`, and the merge-base moves after merge), so evidence split into the one-time comparison above plus a lasting test against the committed SR/BR forms, which this milestone does not rebuild (IP2).
- 2026-07-31: implementation gate added the one-item shading fix to scope as AC9 at Jeff's direction — `seq(2, n, by = 2)` counts backwards at n = 1 and aborts, reachable through a single-item subset form; carried a regression test rather than a candidate row.
- 2026-07-31: six legend tests added to `test-generate_docx.R` plus two parsers in `helper-generators.R`; all six verified red by mutation (reverting `opts_per_line = 2` and the shading index) before being accepted green.
- 2026-07-31: T2 done with T1 (same edit surface); full `devtools::test()` clean at FAIL 0 | WARN 0 | SKIP 1 | PASS 11704.
- 2026-07-31: T3 done — six PID DOCX rebuilt via `data-raw/artifacts.R` (stems pid5/pid5sf/pid5bf, format docx); exactly six files changed md5 (all other `inst/extdata/` files byte-identical) and the manifest gained exactly six rows dated 2026-07-31; all six committed files parse back to the two expected legend lines.
- 2026-07-31: T4 done — NEWS entry added under 0.2.0; `devtools::test()` clean (FAIL 0 | WARN 0 | SKIP 1 | PASS 11704) and `devtools::check()` Status OK (0 errors, 0 warnings, 0 notes, 4m24s).
- 2026-07-31: AC7 visual confirmation is partial — all six rebuilt forms were rendered and inspected page-by-page (macOS QuickLook), showing the two legend lines unbroken on both US and A4; the Word-native open AC7 names was not performed, because Word's AppleScript `save as` errors -1708 on this machine and `osascript` has neither accessibility nor screen-recording grant, so Word could be neither driven nor captured. Left for the maintainer at the review gate rather than silently substituted.

## Decisions

## Review
