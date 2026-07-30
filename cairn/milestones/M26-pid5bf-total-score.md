# M26: PID-5-BF total score across scoring, reliability, and the BF paper forms

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP2
- **Branch/PR:** `m26-pid5bf-total-score` / https://github.com/jmgirard/hitop/pull/29

## Goal

Ship the PID-5-BF total score that `pid_norms` already norms, across every surface a
`pid_scales` entry reaches.

## Scope

**In:** the `total` row in `pid_scales[["BF"]]`; the total column in `score_pid5(version =
"BF")`; the resulting sixth row in `reliability_pid5(version = "BF")`; regeneration of
`pid5bf_A4.docx` and `pid5bf_US.docx` with their `hitop_artifacts` rows per D-016; the
source trace in `cairn/SOURCES.md`; roxygen, NEWS, tests.

**Out:** `norm_pid5()` and every conversion behavior → M27. The BF Qualtrics and REDCap
artifacts, whose generators do not read `pid_scales` and must stay byte-identical (AC4).
Facet-level, stratified, and IRF norms → their ROADMAP candidate rows. Any prose about
what a total score means (IP4).

## Acceptance criteria

- [x] **AC1.** The BF total's computation rule — item-level mean over all 25 items versus
      the mean of the five domain means, and its behavior under `missing = "apa"` — is
      traced to a citable published source in `cairn/SOURCES.md` with a page or table
      anchor, beside the existing A–9 (p. 174) norm-table entry, and
      `cairn/references/markon2024.md` records the same anchor. No rule ships unsourced
      (IP2, IP3); the two candidates agree on complete data and diverge only under
      missingness (D-017).
- [x] **AC2.** `score_pid5(version = "BF")` returns `prefix` + `total` alongside its five
      domains, computed by AC1's rule and verified by a hand-computed oracle fixture that
      does not call `score_pid5()` (IP2). FULL and SF scored *values* are unchanged, shown
      by the existing value assertions in `tests/testthat/test-score_pid5.R` passing
      untouched; the BF shape assertions at
      [test-score_pid5.R:216](tests/testthat/test-score_pid5.R:216) are updated to expect
      six columns.
- [x] **AC3.** `pid_scales[["BF"]]` gains a `total` row covering all 25 BF items with no
      reverse marks (matching `SOURCES.md`'s BF note), produced by re-running its
      `data-raw/` script rather than hand-edited, with `pid_items` untouched.
      `reliability_pid5(version = "BF")` consequently returns six rows including `total`,
      and [test-reliability.R:169](tests/testthat/test-reliability.R:169) expects `6L`.
- [x] **AC4.** `pid5bf_A4.docx` and `pid5bf_US.docx` are regenerated so their scoring
      table carries the `total` row; each gains a `hitop_artifacts` row with a new
      `build_date`, new `md5`, and a `changes` entry naming the total, per D-016; the
      checksum-lock test passes against the new rows. `pid5bf_qualtrics.txt` and
      `pid5bf_redcap.zip` are byte-identical to their committed versions, shown by their
      manifest md5s being unchanged.
- [x] **AC5.** NEWS.md records both scored-output changes under GP2 — the new BF total
      column and the BF reliability row count — and `devtools::document()` leaves the tree
      clean while `devtools::check()` reports 0 errors and 0 warnings, with every NOTE
      justified in this file's Review section.

## Coverage

- AC1 → T1
- AC2 → T2, T3
- AC3 → T2, T4
- AC4 → T5
- AC5 → T6

## Tasks

- [x] **T1.** Read `cairn/references/sources/markon2024.epub` for the BF total's
      computation rule and its missing-data handling; record it in `cairn/SOURCES.md` and
      `cairn/references/markon2024.md` with a page or table anchor. If the book states no
      computation rule, stop and return to plan rather than choosing one (IP3).
- [x] **T2.** Add the `total` row to `pid_scales[["BF"]]` via its `data-raw/` script (all
      25 items, no reverse keys); regenerate `data/*.rda`; confirm `pid_items` is untouched.
- [x] **T3.** Implement the total in `score_pid5(version = "BF")` per T1's rule
      ([score_pid5.R:107](R/score_pid5.R:107)); add the hand-computed oracle test; update
      the BF shape assertions.
- [x] **T4.** Update the BF reliability expectation
      ([reliability_pid5.R:60](R/reliability_pid5.R:60) consumers) and confirm alpha/omega
      compute over the 25-item total without error.
- [x] **T5.** Regenerate both BF DOCX artifacts
      ([generate_docx.R:1136](R/generate_docx.R:1136)); add their `hitop_artifacts` rows;
      verify the checksum-lock test and that the BF Qualtrics/REDCap artifacts are untouched.
- [x] **T6.** Roxygen `@details` for the total's rule with its citation, NEWS entries,
      `_pkgdown.yml` check; run `document()` / `test()` / `check()`.

## Work log

- 2026-07-30: created by /milestone-plan.
- 2026-07-30: plan gate chose to add a PID-5-BF total to `score_pid5()` (D-017) over shipping the book's TOT norm rows as unusable data or dropping them, because the book publishes a normed total and a norm with nothing to convert is dead weight; falsified by the book defining the total on a metric `score_pid5()` cannot produce.
- 2026-07-30: plan gate chose the `pid_scales` row (D-019) over a scorer-only special case, accepting the ripple into `reliability_pid5()` and both BF DOCX artifacts, because a scale absent from `pid_scales` needs permanent special-casing in every consumer; falsified by the regenerated paper form proving confusing to administer.
- 2026-07-30: plan split this milestone out of the original M26 (norming, now M27) after the gate answers pushed the combined scope past four split tripwires; the halves ship independently and M27 depends on this one.
- 2026-07-30: T1 done — the book states the rule outright at Ch. 3, p. 23 (item-level mean over all 25 items), settling D-017's open choice; recorded in `SOURCES.md` and `references/markon2024.md`. No IP3 stop.
- 2026-07-30: implementation gate chose independent APA proration for the total (it can compute when all five domains are NA) over NA-ing it with the domains, because GP1 gives the book's stated rule the default and the alternative needs a special case; falsified by users reading a total-with-blank-domains profile as a bug.
- 2026-07-30: implementation gate chose to print all 25 item numbers in the DOCX total row over a prose "All 25 items" cell, because a uniform comma-joined cell is verifiable against the keying table by the same oracle the other rows use (IP2); falsified by the printed cell overflowing its column on either page size.
- 2026-07-30: T2-T4 done — the `pid_scales` row makes the total fall out of the existing engine with no new code path in `score_engine()`; six shape assertions across four test files updated (two more than the plan anticipated: `test-interface.R:32`/`:75` and `test-keying.R:248`), no FULL/SF value assertion touched; suite 9947 pass / 0 fail.
- 2026-07-30: T5 done — both BF DOCX regenerated (Total row lands in the previously blank padding cell, layout stays 3x2) with two new manifest rows; BF Qualtrics/REDCap left byte-identical.
- 2026-07-30: minor plan amendment — `data-raw/artifacts.R` gained `rebuild_stems`/`rebuild_formats` filters (default NULL = old behavior). Sourcing it wholesale rebuilds all 19 artifacts, and since DOCX footers stamp `Sys.Date()` and REDCap zips embed mtimes (LESSONS M20), that churns every checksum and would append 19 manifest rows for a BF-only change — and would break AC4's byte-identical requirement. First run with stems alone still churned `pid5bf_redcap.zip`; its unzipped content was verified identical to HEAD and the file restored.
- 2026-07-30: supersedes the proration premise stated at the gate above — the bound is at most 3 of 5 domains NA with a computed total (verified against `apa_mean()`), not all five, which is impossible since blanking 5 domains needs 10 missing and the total drops at 7; the chosen behavior is unchanged and `SOURCES.md` is corrected in place.

- 2026-07-30: T6 done — `@details` section for the total with its p. 23 citation, `pid_scales`/`reliability_pid5()` docs updated for the sixth BF row, NEWS entry (incl. the stale "normed here but not yet scored" clause corrected in the same unreleased section), and the BF vignette's "yields the 5 domain scores only" prose fixed. `_pkgdown.yml` needs no change: M26 exports no new function. document() clean, check() 0/0/0.

- 2026-07-30: maintainer confirmed the regenerated BF forms look right; raised the response-option legend wrapping mid-phrase, verified pre-existing (byte-identical legend on main) and PID-only, captured as a ROADMAP candidate with the chosen two-per-line break rather than widened into this milestone's scope.

## Decisions

## Review

Verified 2026-07-30 on `m26-pid5bf-total-score` @ PR #29, all evidence re-run at review.

**AC1 — sourced computation rule.** The book's sentence is present verbatim (whitespace-
normalized comparison against the extracted `markon2024.epub` Chapter 3 text) in both
`cairn/SOURCES.md` (new keying-table row + "Note on the BF total score") and
`cairn/references/markon2024.md`. Page anchor p. 23 taken from the EPUB's own `page23`
pagebreak span, not inferred. The book states no missing-data rule; recorded as such.

**AC2 — scored output.** `score_pid5(version = "BF")` returns 6 columns with `pid_total`
last; it equals an independent `rowMeans()` recompute over the 25 items. Hand-computed
oracle in `test-score_pid5.R` asserts literal expected values (0, 2, 1.16), the APA
proration boundary (6 unanswered prorates, 7 drops), and the total-computes-while-a-
domain-drops case; none of its expectations call `score_pid5()` (IP2). The only lines
removed from that file are three shape assertions — no FULL/SF value assertion changed.

**AC3 — keying table and reliability.** `pid_scales[["BF"]]` has 6 rows; the `total` row
carries `nItems = 25`, item numbers exactly `1:25` ascending, and no reverse-keyed item.
Generated by re-running `data-raw/pid_info.R`. `pid_items` is byte-identical to `main`
(the file is absent from the diff entirely). `reliability_pid5(version = "BF")` returns 6
rows including `Total` at 25 items.

**AC4 — artifacts.** Both BF DOCX changed vs `main` and carry new `hitop_artifacts` rows
dated 2026-07-30 with a `changes` note naming the total; all four BF manifest md5s match
the files on disk. `pid5bf_qualtrics.txt` and `pid5bf_redcap.zip` are byte-identical to
`main` and keep their 2026-07-16 rows. The `Total` row occupies the scoring table's
previously blank padding cell, so the layout stays 3x2; maintainer visually confirmed
both forms.

**AC5 — NEWS and check.** NEWS records both breaking output changes (total column,
reliability row count) with no milestone numbers in user-facing text. `devtools::check()`
re-run at review: **0 errors, 0 warnings, 0 notes**. `document()` produces no diff.

**Consistency gate.** `cairn_validate` exit 0, all checks pass (24 dangling-id advisories,
all pre-existing legacy D-001-D-012 / M13-M16 references). Profile `consistency-gate`
slot: `document()` no-diff clean, `pkgdown::check_pkgdown()` "No problems found", NEWS
entry present, README.Rmd untouched, check clean. No principle changed, so `cairn_impact`
does not apply.

