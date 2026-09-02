# M084: The `label_*()` family's unpadded-item report

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Branch/PR:** `m084-unpadded-item-report` — https://github.com/jmgirard/hitop/pull/91

## Goal

The three `label_*()` helpers report a prefixed item column they could not label
whether or not any other column matched, and describe what they found.

## Scope

Surface tier: **user-facing** — three exported functions' warning behavior and
their documentation.

**In:** `unpadded_item_cols()` and `warn_unpadded_items()` (`R/util.R:590`) and
their three call sites in `R/label_pid5.R`, `R/label_hitopsr.R` and
`R/label_hitopbr.R`; the three defects and the test gap M083 filed (F1, F2, F3,
F9). F1: each helper returns early on "no columns matched" before the report
runs, so a frame whose item columns are *all* mis-padded escapes
`hitop_unpadded_items`. F2: a column padded to the form's width but naming no
item of the form is called unpadded, and the hint hands back the offending name.
F3: the third plural marker keys on `{width}`, so a one-column report reads
"they were not labelled". F9: every existing unpadded test leaves padded
neighbours in the frame, so none can reach F1. Tests, roxygen, `man/` and a
`NEWS.md` bullet.

**Out:** the `rename_*_items()` reports, `rename_hitopsr_items()`'s classless
no-match warning, labelling `validity_pid5()` and `_se` columns, and inferring
`version` — all on the standing "four remainders M082 and M083 left" candidate
row. A new condition class for out-of-range columns → declined at the plan
gate; the same class carries both sentences.

## Acceptance criteria

- [x] AC1: On a frame whose every prefixed item column is mis-padded so that no
      column matches an expected item name — at most five such columns, cli
      truncating longer lists — each of `label_pid5()` (FULL, SF and BF),
      `label_hitopsr()` and `label_hitopbr()` raises its existing no-match
      warning *and* a warning of class `hitop_unpadded_items` naming every
      mis-padded column. The SR probe uses a wider spelling (`hsr_0001`), since
      `hsr_1`..`hsr_405` still matches items 100 and up.
- [x] AC2: A column carrying the prefix and a number padded to the form's width
      but naming no item of the form (`pid5_221`, `pid5sf_101`, `pid5bf_26`,
      `hsr_406`, `hbr_46`) is reported under class `hitop_unpadded_items` in its
      own sentence, which states the form's item-number range, not in the
      not-zero-padded sentence; the report's "expected as" hint never repeats an
      out-of-range column's own name. A frame carrying one out-of-range and one
      mis-padded column reports each in its own sentence, on each instrument.
- [x] AC3: Every plural marker in the report keys on the number of columns the
      sentence carrying it reports: a one-column sentence reads "1 column is …
      was not labelled" and a two-column sentence "2 columns are … were not
      labelled", at both instrument widths the helpers pass (2 and 3).
- [x] AC4: Each of `man/label_pid5.Rd`, `man/label_hitopsr.Rd` and
      `man/label_hitopbr.Rd` states that the report is raised whether or not any
      column matched, and that a padded number outside the form's range is
      reported as out of range; `grep -RF "That warning is raised only when at
      least one column did match" R/ man/` returns nothing.
- [x] AC5: Correctly padded frames stay silent — the shipped `sim_*` and `ku_*`
      item frames, a module subset (`ku_hitopsr[, c("participant", "hsr_001",
      "hsr_002")]`), and a frame with no prefixed column (`data.frame(a = 1)`)
      raise no `hitop_unpadded_items` warning, the last raising only the
      no-match warning.
- [x] AC6: `NEWS.md` carries a bullet for the development version describing the
      changed report.
- [x] AC7: `Rscript -e 'devtools::test()'` is clean, `Rscript -e
      'devtools::document()'` produces no diff, and `Rscript -e
      'devtools::check()'` reports no error, warning or note absent from the
      baseline recorded in this file's work log at implementation start.

## Coverage

- AC1 → T2, T3
- AC2 → T4, T5
- AC3 → T4, T5
- AC4 → T7
- AC5 → T6
- AC6 → T7
- AC7 → T1, T8

## Tasks

- [x] T1: Record the pre-work `devtools::check()` and `devtools::test()` results
      as one work-log line — the baseline AC7 measures against.
- [x] T2: Add the all-mispadded probes: `test-label_pid5.R` for FULL, SF and BF;
      `test-label_scales.R` for SR (wider spelling) and BR. ≤5 columns per
      probe, asserting the class and every named column. Run each against the
      current `R/` and record it red.
- [x] T3: Move `warn_unpadded_items()` ahead of the no-match early return in
      `R/label_pid5.R:83`, `R/label_hitopsr.R:45` and `R/label_hitopbr.R:46` so
      both warnings fire. T2 green; the existing no-match assertions
      (`test-label_pid5.R:185`, `test-label_scales.R:206,220`) still pass. Sweep
      every test that merely *calls* a label helper for a newly raised warning
      (LESSONS M029).
- [x] T4: Split `unpadded_item_cols()` (`R/util.R:590`) into mis-padded and
      out-of-range groups; rewrite `warn_unpadded_items()` (`R/util.R:594`) to
      give each group its own sentence, state the form's item-number range in
      the out-of-range sentence itself (AC2's wording; a separate `i` hint would
      only repeat it), and place `cli::qty()` immediately before each
      plural marker (LESSONS M030). Update the three call sites' arguments.
- [x] T5: Tests for T4 — out-of-range probes on all five forms, a mixed
      out-of-range + mis-padded frame per instrument, and one- versus two-column
      wording at both widths. Show each red by reverting T4's change before
      trusting its green.
- [x] T6: Silence-regression tests for AC5, capturing conditions with
      `withCallingHandlers`, never `expect_no_warning(message = )`, which takes
      `message` as a regexp selector and passes on a raised warning (LESSONS
      M032).
- [x] T7: Update the three helpers' `@param prefix` and `@return` roxygen, run
      `devtools::document()`, add the `NEWS.md` bullet.
- [x] T8: Run `devtools::test()` and `devtools::check()`; compare against T1's
      baseline and record the comparison.

## Work log

- 2026-09-02: created by /milestone-plan, promoting the standing candidate row
  (lineage M077, M083 findings 1, 2, 3, 9).
- 2026-09-02: plan gate chose raising both the no-match and the padding warning
  over replacing the no-match warning because callers catching the existing
  warning keep working and one `suppressWarnings()` still silences both;
  falsified by a report that the paired warnings read as contradictory.
- 2026-09-02: plan gate chose an own-sentence out-of-range report under the
  existing class over a silent skip and over a new condition class because a
  typo'd item number stays visible and no new public name is minted; falsified
  by a caller needing to catch out-of-range columns apart from mis-padded ones.
- 2026-09-02: plan gate chose a baseline-relative check bar over `check()`
  0/0/0 because a pre-existing note would otherwise block work that did not
  cause it; falsified by the baseline hiding a note this milestone introduced.
- 2026-09-02: T1 baseline at the branch point (6f75befc's parent): `devtools::test()` FAIL 0 / WARN 0 / SKIP 9 / PASS 17043; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes.
- 2026-09-02: implementation gate chose reporting a column that is both mis-padded and out of range under out-of-range alone over the padding sentence or both, because the padding hint would otherwise name a column the form does not carry either; and chose the no-match warning ahead of the padding report, matching the order a partly-matching frame already reads in.
- 2026-09-02: T2 added five all-mis-padded probes (`label_pid5()` FULL/SF/BF, `label_hitopsr()` at `hsr_0001`, `label_hitopbr()` at `hbr_1`) plus the `collect_warnings()`/`warning_text()` test helpers; all five red against the current `R/`, each raising 1 warning where 2 are expected.
- 2026-09-02: T3 ran the report after the no-match warning instead of behind an early return in all three helpers; the five T2 probes green, and the full suite FAIL 0 / WARN 0 / SKIP 9 / PASS 17088 with no other test newly raising a warning.
- 2026-09-02: T4 split `unpadded_item_cols()` into `mispadded` and `out_of_range` and gave `warn_unpadded_items()` the signature `(cols, prefix, expected, max_n, instrument)`, folding the split in; the three call sites pass `max_n` instead of a digit width. Minor task edit: T4 now says the range is stated in the out-of-range sentence rather than in a separate `i` hint, which would only repeat it.
- 2026-09-02: T5 added out-of-range, mixed and one-versus-two-column probes on all five forms. Discrimination: planting the pre-split classification (every unmatched column called mis-padded) reddened 20 assertions across both files; planting the missing `cli::qty()` reddened the 6 pluralization assertions. Full suite FAIL 0 / WARN 0 / SKIP 9 / PASS 17159.
- 2026-09-02: T6 added the silence regression over the eight shipped item frames, two module subsets and a frame with no prefixed column, capturing with `withCallingHandlers`; each shipped frame is also shown to label something, so the domain cannot silently empty. Discrimination: reporting every prefixed column regardless of `expected` reddened it. Full suite PASS 17186.
- 2026-09-02: T7 rewrote the three helpers' `@param prefix` and `@return` and added the `NEWS.md` bullet under "Improvements and fixes"; `devtools::document()` rewrote the three `.Rd` files and a second run left no further diff. Each of the three carries "raised whether or not any other column matched" and "reported as out of range", and AC4's grep for the removed sentence returns nothing.
- 2026-09-02: T8 final results against T1's baseline: `devtools::test()` FAIL 0 / WARN 0 / SKIP 9 / PASS 17186 (baseline 17043, +143 from this milestone's tests); `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes, identical to the baseline -- no error, warning or note absent from it. `devtools::document()` produces no diff.
- 2026-09-02: criteria audit ran in full mode ([O] fresh reader, user-facing
  tier); it returned findings on AC1 (unsatisfiable — cli truncates lists past
  five, and `hsr_1`..`hsr_405` does not reach the no-match path), AC2 (no class
  or text pinned; FULL-only probe), AC3 (one of three markers pinned), AC4
  (unbounded negative, vacuous for two of three files), AC6-as-drafted (a
  property of the checker's report, no baseline) and two additions (silence
  regression; the two-report contract). All were disposed at the gate — five
  fixed in the wording above, the two-report contract and the check bar posed
  as questions.

## Decisions
- 2026-09-02: review step 3-4 -- all seven criteria executed with fresh evidence and ticked; `cairn_validate` exit 0 and every r-package toolchain check green; PR #91 open as a draft with CI running. Independent review pending (the diff-bug lens still running; the blame-history and prior-review lenses returned).
- 2026-09-02: review step 5 -- three fresh-context lenses; the prior-review lens found nothing, the blame lens one item, the diff-bug lens ten and no correctness bug. Six triaged fix-now, one to the standing candidate row, two rejected as out of scope, one stale. Dispositions in the Review section.
- 2026-09-02: gate triage -- Jeff chose fixing the six before merging. Applied: the NEWS bullet rewritten to describe the report rather than a regression no release had (and the `label_pid5()` bullet's narrower rule corrected), the padding hint moved after both sentences, D-058 appended annotating D-052's stripping consequence, the contiguous-numbering assumption recorded in `unpadded_item_cols()`'s comment, the test helper `at()` renamed `sentence_pos()`, and the `item_names()` comment block separated from `item_col_numbers()`'s. Suite re-run FAIL 0 / WARN 0 / SKIP 9 / PASS 17186, unchanged; `document()` no diff.
- 2026-09-02: step-7 approval: PR #91 approved for merge (Jeff, at the merge chip, choosing the fix-then-merge option).
- 2026-09-02: resume: PR #91 read OPEN, `origin/main` unmoved at `05b3e485`, branch head already pushed; step 1 needed no merge and step 3's evidence stands. step-7 approval: PR #91 approved for merge (re-posed chip, Jeff choosing merge).
- 2026-09-02: CI watch hit the harness ceiling with four of eight checks pending; the moved task was stopped and the session closed before the merge. Local `check()` on the final tree is 0/0/0 and the suite is green; PR #91 stays open with the approval recorded.

## Review

Evidence gathered 2026-09-02 on `m084-unpadded-item-report` at `b8b1986e`, PR
#91, against `origin/main` at `05b3e485`. The AC1-AC3 and AC5 probes ran from a
scratch script calling the three helpers under `withCallingHandlers`; its
transcript is summarized below, never pasted.

- **AC1 — met.** Each of the five all-mis-padded frames (five prefixed columns,
  none matching) raised exactly two warnings: the existing no-match warning
  first, then one of class `hitop_unpadded_items` naming all five columns.
  `label_pid5()` FULL (`pid5_1`..`pid5_5`), SF (`pid5sf_1`..`5`), BF
  (`pid5bf_1`..`5`), `label_hitopsr()` (`hsr_0001`..`hsr_0005`, the wider
  spelling) and `label_hitopbr()` (`hbr_1`..`hbr_5`). Each report named every
  one of its five columns and the width expected (3, 3, 2, 3, 2 digits).
- **AC2 — met.** `pid5_221`, `pid5sf_101`, `pid5bf_26`, `hsr_406` and `hbr_46`
  each raised `hitop_unpadded_items` reading "its number is outside the range 1
  to {220,100,25,405,45}", carrying no "not zero-padded" clause and no "expected
  as" hint. The five mixed frames (one out-of-range plus one mis-padded per
  instrument) each produced one warning of two sentences, the mis-padded column
  in the padding sentence with its `i` hint and the out-of-range column in the
  range sentence. `pid5_0221` — four digits and past item 220 — reported as out
  of range only, so the hint never handed back a name the form does not carry.
- **AC3 — met.** At width 2 (`label_hitopbr()`) and width 3
  (`label_hitopsr()`), a one-column padding sentence read "1 column is … so it
  was not labelled" and a two-column one "2 columns are … so they were not
  labelled"; the out-of-range sentence likewise read "1 column is … its number
  is … it was not labelled" against "2 columns are … their numbers are … they
  were not labelled". Six of six markers keyed on their own sentence's count.
- **AC4 — met.** `grep -RF "That warning is raised only when at least one column
  did match" R/ man/` returns nothing (exit 1). With newlines normalized, each
  of `man/label_pid5.Rd`, `man/label_hitopsr.Rd` and `man/label_hitopbr.Rd`
  carries "raised whether or not any other column matched" once and "reported as
  out of range" once.
- **AC5 — met.** All eight shipped item frames (`sim_pid5`, `sim_pid5sf`,
  `sim_pid5bf`, `sim_hitopsr`, `sim_hitopbr`, `ku_pid5sf`, `ku_hitopsr`,
  `ku_hitopbr` — the package ships no `ku_pid5`) raised zero warnings and each
  labelled its full item set (220, 100, 25, 405, 45, 100, 405, 45 columns), so
  the silent domain is non-empty. The module subset `ku_hitopsr[, c("participant",
  "hsr_001", "hsr_002")]` raised zero warnings and labelled 2 columns.
  `data.frame(a = 1)` raised the no-match warning alone under each of the three
  helpers, with no `hitop_unpadded_items`.
- **AC6 — met.** `NEWS.md` carries one bullet under the development version's
  "Improvements and fixes" describing the unconditional raise, the two-sentence
  split and the per-sentence pluralization.
- **AC7 — met.** `devtools::test()`: FAIL 0 / WARN 0 / SKIP 9 / PASS 17186,
  against the T1 baseline's FAIL 0 / WARN 0 / SKIP 9 / PASS 17043 (+143).
  `devtools::check()`: Status OK, 0 errors / 0 warnings / 0 notes, duration
  6m 23s — identical to the baseline, so no error, warning or note absent from
  it. `devtools::document()` left the working tree clean apart from this
  milestone file.

### Consistency gate

- `cairn_validate.py`: exit 0, all 16 checks PASS (`coverage complete` and
  `scaffold present` among them). 41 advisories, all pre-existing and none a
  gate failure: `work-log format` on wrapped lines, `dangling id tokens` on the
  legacy D-001..D-012 references, `references staleness` on
  `schmukle2026.md`.
- `cairn_impact.py`: not run — the diff changes no `DESIGN.md` principle
  (GP3 is applied, not amended; `cairn/DESIGN.md` is untouched by the branch).
- r-package profile `consistency-gate` slot: `devtools::document()` no diff;
  no hand-edited generated file (`NAMESPACE`, `data/*.rda` untouched, `man/`
  regenerated); `README.Rmd`/`README.md` untouched by the branch and in sync;
  `pkgdown::check_pkgdown()` "No problems found"; `NEWS.md` carries the bullet;
  the branch adds no top-level file (`NEWS.md` is the only top-level path it
  touches), so no `.Rbuildignore` entry is owed; `devtools::check()` 0/0/0. No
  newly exported object, so no `_pkgdown.yml` row is owed.

### Independent review (three lenses, fresh context)

- **[S] prior-PR-comments:** no findings. It located prior-review evidence on
  the touched files (M077's "half-labelling unpadded data silently" and M083's
  two roxygen fixes) and reports the diff resolves rather than regresses each.
  The GitHub inline-comment probe returned `[]`, so that surface was skipped.
- **[S] blame-history:** one item, matching the diff-bug lens's F3 below; it
  found no change contradicting a decision, resurrecting a fixed bug, or
  undoing deliberate past work, and confirmed the removed early return was
  never itself a recorded decision.
- **[O] diff-bug:** no correctness bug. It independently reproduced AC1-AC6 by
  probe, confirmed the suite at FAIL 0 / PASS 17186, and confirmed the new
  tests discriminate (reverting the split reddens the range assertions,
  dropping `cli::qty()` reddens the pluralization assertions, restoring the
  early return reddens the two-warning probes). Ten ranked findings follow.

**Findings and disposition** (ranked as reported; every one logged):

1. *"The NEWS bullet fixes a defect that never shipped."* `hitop_unpadded_items`
   is itself new in the same unreleased development-version section
   (`NEWS.md:13` and `NEWS.md:128`), so a reader is told about a regression no
   release ever had. — **fix now.**
2. *"The out-of-range sentence renders after the mis-padded sentence's `i` hint,
   and as an unnamed line,"* so the hint sits between the two sentences and
   reads as if it governs both. — **fix now.**
3. *"D-052's standing consequence is now false"* — it says `prefix` is "still
   pasted, never stripped, in these functions," and `item_col_numbers()` strips
   it. Raised independently by the blame-history lens. — **fix now**, as an
   annotating D-entry (`DECISIONS.md` is append-only).
4. *"`in_range` uses `1..max_n` as a proxy for 'names an item of the form'."*
   Exact for all five forms today (each contiguous 1..max, verified), but the
   assumption is unrecorded. — **fix now**, as a comment.
5. *"The class and helper names now cover columns that are not unpadded."* —
   **rejected:** the plan gate declined a second condition class deliberately,
   and the same class carrying both sentences is what the milestone planned.
6. *"Untested edge: the only path where `item_col_numbers()` returns `NA`"* — a
   number too large for an integer, and `pid5_000`. Both behave sensibly on
   probe; neither is pinned. — **follow-up**, absorbed into the standing
   "four remainders M082 and M083 left" candidate row.
7. *"Duplicate column names are reported twice."* — **rejected:** pre-existing
   and unchanged by the diff (the reviewer says so itself).
8. *"`at()` is a very broad name in a shared helper file"* — a generic
   `at(haystack, needle)` in the namespace every test file sees. — **fix now.**
9. *"The `item_names()` comment block is now further orphaned."* — **fix now.**
10. *"AC7 is still unchecked."* — **no longer applicable:** the reviewer read
    the file before this review's step-3 pass ticked AC7 against its evidence
    line; the box is ticked and evidenced above.

### Fix-now work applied at the gate

Findings 1, 2, 3, 4, 8 and 9 were fixed on the branch before the approval
marker; finding 6 goes to the standing candidate row at hygiene, findings 5 and
7 were rejected as recorded above, and finding 10 was stale. The fixes touch
message layout, comments, the changelog and a test-helper name — no change to
which columns are labelled or which are reported, so AC1-AC6 stand as evidenced
(re-probed after the change: the mixed frame still reports each column in its
own sentence, the hint now last and still naming no out-of-range column).
`devtools::test()` re-run: FAIL 0 / WARN 0 / SKIP 9 / PASS 17186, unchanged.
`devtools::document()`: no diff.

### AC7 re-verified on the final tree

After the gate fixes, at `321f0598`: `devtools::test()` FAIL 0 / WARN 0 / SKIP 9
/ PASS 17186; `devtools::check()` Status OK, 0 errors / 0 warnings / 0 notes,
duration 10m 9s — still identical to the T1 baseline; `devtools::document()` no
diff.

### CI wait, 2026-09-02

The foreground `gh pr checks 91 --watch --fail-fast` reached the harness
ceiling and was moved to the background, then stopped with `TaskStop` — no
watcher left armed. Fresh state at that moment: `macos-latest (release)`,
`test-coverage`, `line endings` and `pkgdown` pass; `ubuntu-latest` (devel,
oldrel-1, release) and `windows-latest (release)` pending. The merge did not
run; PR #91 is open, ready for review, with the step-7 approval recorded above.
Resume re-derives the check and merge state from `gh pr checks` / `gh pr view`.

