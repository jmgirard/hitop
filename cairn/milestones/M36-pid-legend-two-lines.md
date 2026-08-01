# M36: Two-line response-option legend on the PID paper forms

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, GP2
- **Branch/PR:** `m36-pid-legend-two-lines` · https://github.com/jmgirard/hitop/pull/39

## Goal

The PID paper forms print their response-option legend on two lines of two options each, so no option phrase is broken mid-phrase by column wrapping, while the HiTOP-SR/BR forms keep their single-line legend and no legend wording changes.

## Scope

**In:** a pairs-per-line argument on the shared `make_items_table()` helper (`R/generate_docx.R:236`), used by the three PID DOCX generators (`R/generate_docx.R:997, 1061, 1125`); a one-line fix to the same helper's even-row shading index, which aborts on a one-item table (added at the 2026-07-31 implementation gate); parse-based tests over the generated legend for PID and for SR/BR; regeneration of the six committed PID DOCX artifacts through `data-raw/artifacts.R`'s existing `rebuild_stems`/`rebuild_formats` filters, with their `hitop_artifacts` rows and a NEWS entry.

**Out:** any change to legend *wording*, option values, or labels — IP1 content, signed off as layout-only at the 2026-07-30 plan gate and recorded as a D-entry, which is not a licence for wording drift. The HSUM overview's separately hardcoded response-option sentence (`R/generate_docx.R:919, 936`) — a different code path with a 58-character string that does not wrap; a candidate row if it ever does. Qualtrics and REDCap artifacts — the legend is a DOCX layout concern and their generators never call `make_items_table()`. A repeatable width-budget check on the rendered line — the maintainer chose visual confirmation at the plan gate; a candidate row if the estimate is ever wanted.

## Acceptance criteria

- [x] AC1 `make_items_table()` takes an argument setting how many `value = label` pairs print per legend header line, defaulting to all pairs on one line derived from `nrow(opts)` rather than a hardcoded 4. Evidence is in two parts, because a permanent test cannot read the pre-milestone bytes (`R CMD check` builds from a tarball with no `.git`, and the merge-base moves once this branch merges): (a) a one-time implement-time comparison of the default legend against `git show <merge-base>:inst/extdata/pid5_US.docx`, recorded verbatim in the Review section; and (b) a permanent test asserting that the default legend matches the legend in the committed `hitopsr_US.docx` and `hitopbr_US.docx` — forms this milestone does not rebuild, so they stay an external oracle rather than the code's own output (IP2).
- [x] AC2 A freshly generated document from each of `generate_docx_pid5()`, `generate_docx_pid5sf()`, and `generate_docx_pid5bf()` carries, in the items table's own header and not counting the scoring table's header row, exactly two legend lines: the first holding the pairs for values 0 and 1, the second the pairs for values 2 and 3 — verified by parsing `word/document.xml`, never by checksum (LESSONS 2026-07-16, M20).
- [x] AC3 The pairs recovered from those two lines equal `pid_instructions$options` value-for-value (compared as character) and label-for-label in printed order; the ` • ` separator at the 1|2 boundary is removed by the split and no character is added anywhere.
- [x] AC4 Freshly generated `generate_docx_hitopsr()` and `generate_docx_hitopbr()` documents each carry exactly one legend header line holding all four pairs.
- [x] AC5 The six committed PID DOCX (`pid5`, `pid5sf`, `pid5bf` × `_US`/`_A4`) are regenerated through `data-raw/artifacts.R` with `rebuild_stems <- c("pid5","pid5sf","pid5bf")` and `rebuild_formats <- c("docx")`, each gaining one new `hitop_artifacts` row dated the rebuild day with a `changes` note naming the legend split; each committed file parses back to the two header lines of AC2; NEWS.md records the regenerated forms (GP2).
- [x] AC6 No artifact other than those six changes: every other file in `inst/extdata/` is byte-identical to its pre-milestone committed bytes (compared against `<merge-base>` via `git show`), and `hitop_artifacts` gains no row for any of them.
- [ ] AC7 The rebuilt US and A4 PID forms are opened and visually confirmed to break no option phrase mid-phrase, with a screenshot recorded as review evidence (the maintainer's chosen proof — Word's line breaking is not observable from `document.xml`).
- [x] AC8 `devtools::test()` and `devtools::check()` clean.
- [x] AC9 `make_items_table()` builds a one-item table without error, with a regression test that fails against the pre-fix `seq(2, n, by = 2)` shading index (added to scope at the maintainer's direction at the 2026-07-31 implementation gate).

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

- 2026-07-31: review — cairn_validate exit 0; r-package consistency gate clean (document() no diff, README in sync, pkgdown no problems, check() Status OK 0/0/0); eight of nine criteria verified with fresh evidence, AC7 carried to the maintainer as partial. Three-lens fan-out: blame-history and prior-review lenses zero findings, diff-bug lens 14, of which one scored >=80 (F4, 82) and was fixed — the "not a hardcoded four" test used a three-option table that passes against a hardcoded 4, moved to five options and re-verified red by mutation.

## Decisions

## Review

Evidence gathered 2026-07-31 on branch `m36-pid-legend-two-lines` at `efa0759`,
merge-base `25a3a1b`. Every line below was produced by running the command, not
from recall.

- AC1 (a) One-time comparison, recorded verbatim as the criterion requires. The
  legend recovered from `git show 25a3a1b:inst/extdata/pid5_US.docx` is the
  single line
  `0 = Very False or Often False • 1 = Sometimes or Somewhat False • 2 = Sometimes or Somewhat True • 3 = Very True or Often True`,
  which is exactly what `make_items_table()`'s default `opts_per_line = nrow(opts)`
  produces for a four-option set. (b) The signature is
  `make_items_table(items_df, item_col, opts, printable_w, font_size, font_family, opts_per_line = nrow(opts))`
  — derived from `nrow(opts)`, no hardcoded 4. The lasting test is
  "the default keeps one line, matching the committed SR/BR forms" in
  `test-generate_docx.R`; re-run here by hand, a fresh `generate_docx_hitopsr()`
  and `generate_docx_hitopbr()` each yield
  `1 = Not at all • 2 = A little • 3 = Moderately • 4 = A lot`, identical to the
  legend parsed out of the committed `hitopsr_US.docx` and `hitopbr_US.docx`
  (external oracle — forms this milestone does not rebuild, IP2).
- AC2 Fresh tempfile documents from `generate_docx_pid5()`, `generate_docx_pid5sf()`,
  and `generate_docx_pid5bf()`, parsed out of `word/document.xml` and scoped to
  the items table's own header: each carries exactly 2 legend lines, the first
  `0 = … • 1 = …`, the second `2 = … • 3 = …`. Never a checksum comparison.
- AC3 For all three fresh documents the recovered pairs equal
  `pid_instructions$options` value-for-value compared as character (TRUE) and
  label-for-label in printed order (TRUE). The ` • ` at the 1|2 boundary is
  removed by the split and nothing is added anywhere: concatenating the two
  lines and dropping the two remaining separators reproduces the pre-milestone
  one-line legend with its separators dropped, character for character (TRUE).
- AC4 Fresh `generate_docx_hitopsr()` and `generate_docx_hitopbr()` documents
  each carry exactly 1 legend line holding all 4 pairs.
- AC5 The six committed PID DOCX were rebuilt through `data-raw/artifacts.R`
  with `rebuild_stems <- c("pid5","pid5sf","pid5bf")` and
  `rebuild_formats <- c("docx")`; the script's own filter check reported
  `Rebuilt stems: pid5, pid5bf, pid5sf | formats: docx`. `hitop_artifacts` went
  26 → 32 rows, the 6 added all dated 2026-07-31, one per file, each carrying a
  `changes` note naming the split ("Response-option legend split across two
  header lines … legend wording, values, and labels are unchanged"), and each
  row's `md5` matches the file on disk. All six committed files parse back to
  the two header lines of AC2. NEWS.md records the regenerated forms with no
  milestone number (GP2).
- AC6 `git diff --name-only 25a3a1b..HEAD -- inst/extdata/` lists exactly those
  6 files. Each of the other 18 files in `inst/extdata/` was compared byte-wise
  against `git show 25a3a1b:<path>` and all 18 are identical. No manifest row
  was added for any of them (0 added rows outside the six), and every
  pre-existing row survives unaltered.
- AC7 **Partially met — the one gap in this review.** All six rebuilt forms were
  rendered and inspected page by page, and the two legend lines print whole on
  both US and A4 with no option phrase broken; the US and A4 PID-5 renders were
  shown to the maintainer. But the rendering is macOS QuickLook's, not Word's,
  and the criterion names Word because Word's line breaking is the thing at
  issue. Word could not be driven or captured from this session: its AppleScript
  `save as` errors -1708 and `osascript` holds neither accessibility nor
  screen-recording permission. Carried to the maintainer at the merge gate
  rather than counted as met.
- AC8 `devtools::test()` clean at FAIL 0 | WARN 0 | SKIP 1 | PASS 11704.
  `devtools::check()` Status OK — 0 errors, 0 warnings, 0 notes (4m24s).
- AC9 `make_items_table()` builds a one-item table without error, verified for
  both a PID options set (`opts_per_line = 2`) and an SR options set (default);
  both return a `flextable`. The regression test in `test-generate_docx.R` does
  fail against the pre-fix index: at n = 1, `seq(2, n, by = 2)` aborts with
  "wrong sign in 'by' argument" while `which(seq_len(n) %% 2 == 0)` returns
  `integer(0)`. The fix is behavior-preserving where the old form worked —
  checked n = 0..7, the two agree exactly at every n ≥ 2.

**Consistency gate.** `cairn_validate` exit 0, all checks pass; 21 advisories,
of which 20 are the standing pre-migration id references in DESIGN.md/SOURCES.md
and 1 is new — a sizing tripwire on this milestone's 9 acceptance criteria. Not
actioned: the milestone is one helper argument plus a regeneration, and the
criterion count reflects how finely the artifact rebuild was fenced, not a
milestone that should have been two. No DESIGN.md principle changed, so
`cairn_impact` was not run. Toolchain gate (r-package profile): `devtools::document()`
produces no diff; `NAMESPACE`, `man/`, and `data/*.rda` are generated, not
hand-edited; README.md in sync after `devtools::build_readme()`;
`pkgdown::check_pkgdown()` reports no problems; NEWS.md carries the entry; no new
top-level files; `devtools::check()` clean.

**Independent fresh-context review.** Three reviewers with distinct evidence
bases, then a separate Sonnet scorer that did not generate the findings.
- [S] blame-history lens: zero findings. It established that the `seq(2, n, by = 2)`
  index was not a deliberate choice — it entered at `ac83899f` (M20), the same
  commit that used the `which(... %% 2 == 0)` idiom for four other tables in this
  same file, so the fix resolves an inconsistency rather than undoing intent.
- [S] prior-review lens: zero findings. The only archived `## Review` finding
  touching `R/generate_docx.R` is M24's `isTRUE()` guard at line 129, a code path
  this diff does not touch. The GitHub inline-comment probe returned empty, so
  the PR threads were not walked.
- [O] diff-bug lens: 14 findings, scored below. One reached the actioning
  threshold.

**Actioned (score ≥ 80).**
- F4 (82) — the test named "opts_per_line defaults to the option count, not a
  hardcoded four" did not discriminate the mutation it named. It used a
  three-option table, and at three options both `seq(1, 3, by = 3)` and
  `seq(1, 3, by = 4)` yield a single start, so one legend line results either
  way; the test passed against a hardcoded 4, leaving AC1's "derived from
  `nrow(opts)` rather than a hardcoded 4" with no permanent test behind it.
  Confirmed by hand before actioning. **Fixed now:** the table moved to five
  options, where the default prints one line and a hardcoded 4 prints two, plus
  a pair-count assertion; the `opts_per_line = 2` case became 5 = 2+2+1, still
  checking the short final line carries no dangling separator. Re-verified by
  mutation: hardcoding the default to `4L` turns the suite red (FAIL 1), and
  restoring it turns it green (FAIL 0 | PASS 111 in the file, 11705 overall).

**Logged, not actioned (score < 80) — 13 findings, surfaced not dropped.**
- F1 (32) a fractional `opts_per_line` (2.5) silently drops an option from the
  legend; unreachable — the helper is internal and its five call sites pass a
  literal 2 or nothing.
- F2 (28) invalid values abort with base-R messages rather than `cli_assert`;
  same reachability argument.
- F3 (22) a zero-row `opts` would abort in the same `seq()` the AC9 fix was about;
  no shipped instrument has zero options.
- F5 (50) no test at `opts_per_line = 1` or above `nrow(opts)`; both verified
  correct by hand, so a coverage gap, not a defect.
- F6 (45) label-for-label equality is asserted for `pid5` only, not `pid5sf`/`pid5bf`;
  all three pass the identical `pid_instructions$options`.
- F7 (28) the loop-over-generators tests give no per-generator failure attribution.
- F8 (35) one assertion mirrors the production `paste()` shape; its inputs are the
  source dataset rather than the code's output, so it is a mirror, not self-reference.
- F9 (30) the SR/BR oracle test `skip_if`s rather than failing if those committed
  forms ever stop being installed.
- F10 (25) `docx_legend_lines()`'s comment claims a structural guarantee that is
  actually incidental to the `"N.  "` item prefix.
- F11 (24) `docx_legend_pairs()` fails open on malformed input instead of erroring.
- F12 (48) AC9's recorded justification is wrong: a one-item form is *not*
  reachable today — `hitop_subset()` supports only `hitopsr` and its smallest
  scale has 3 items. The fix is still correct and worth keeping; only the stated
  reachability was overclaimed.
- F13 (12) one new code line is 87 characters against `air.toml`'s 80; formatter-catchable.
- F14 (15) milestone bookkeeping the review gate itself closes.
