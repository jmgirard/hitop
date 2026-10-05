# M164: README capability table

- **Status:** review
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** —
- **Resolves:** —
- **Surface tier:** user-facing — README.Rmd renders the GitHub front page and the pkgdown home page
- **Branch/PR:** m164-readme-capability-table

## Goal

Replace the README's "Development Progress" checklist with a table of what the package does for each instrument, kept true by a test.

## Scope

**In:** Delete the checklist section of `README.Rmd`. Add a `## What the package covers` section with one table. Below the table, add a sentence that points to `?validity_pid5` and `?norm_pid5`. Tidy the Key Features list. Fix its tab indent, drop "Comprehensive", name no instruments, and point the scoring bullet to the table. Add `tests/testthat/test-readme-capabilities.R`. Regenerate `README.md`. Name the table in the keep-in-step line of CLAUDE.md.

**Column rules** (the test encodes these, and AC3 cites them):
- Row to version: PID-5 `FULL`, PID-5-SF `SF`, PID-5-BF `BF`, PID5BF+M `BFPM`, PID-5-IRF `IRF`, PID-5-FFBF `FFBF`, PID-5 Child `FULL`, PID-5-BF Child `BF` (`?score_pid5`, D-091). The three HiTOP rows take no version.
- Items: the row count of `hitopsr_items`, `hitopbr_items`, `hitophsum_items` or `pid_ffbf_items`. For the other PID-5 rows, the count of non-missing entries in the `pid_items` column that the row's version names.
- Scoring and Reliability: a mark when the `score_*()` or `reliability_*()` function of the instrument exists and returns. The test calls it on a one-row data frame of in-range responses, with the row's version. When no such function exists, the cell is blank. If the call fails for any other reason, the test fails.
- Tutorial: a link to each top-level file under `vignettes/` that holds a scoring call for the row. For a PID-5 row, that call is `version = "<row version>"`. For a HiTOP row, it is `score_hitopsr(` or `score_hitopbr(`. When no file holds one, the cell is blank.
- Forms: the formats among Word (`docx_us` or `docx_a4`), Qualtrics, REDCap and JSON that `hitop_artifacts` holds for the instrument name of the row. When it holds none, the cell is blank.

**Out:**
- Validity and Norms columns. The child and forensic rows need footnoted judgments that the rules cannot state. The sentence under the table points to the help pages instead.
- New homes for the unchecked checklist items. Each already has one. HiTOP-HSUM scoring is in the "Instruments awaiting materials" row. HiTOP norms are in the "HiTOP-SR/BR norms" row, and HiTOP visualization in the "Hierarchical PID-5 profile display" row. Individual reports, CRAN and the paper are in the "Clinical reporting & release" row. The PID-5-FFBF forms are M163.
- The PID-5-FFBF Forms cell. It stays blank until M163 merges. The `hitop_artifacts` rows that M163 adds then fail the new test until its branch fills the cell.
- A NEWS entry. The README is not package behavior.

## Acceptance criteria

- [x] AC1: The checklist is gone. `awk '/^```/{f=!f; next} !f && /^#{1,6} /' README.Rmd`, which skips ```-delimited code chunks, prints exactly four lines, in this order: one whose text begins `# hitop`, then `### Key Features`, `## Installation` and `## What the package covers`. `grep -nE '[-*+] \[[ xX]\]|Phase [0-9]|waiting for|\bM[0-9]{2,}\b' README.Rmd README.md` prints nothing.
- [x] AC2: The `## What the package covers` section holds one Markdown table with the columns Instrument, Items, Scoring, Reliability, Tutorial and Forms. The table has one row for each of these instruments: HiTOP-SR, HiTOP-BR, HiTOP-HSUM, PID-5, PID-5-SF, PID-5-BF, PID5BF+M, PID-5-IRF, PID-5 Child, PID-5-BF Child and PID-5-FFBF. A sentence below the table points to `?validity_pid5` and `?norm_pid5`. README.md carries the same table and sentence.
- [ ] AC3: Each cell of that table follows its column rule in Scope, in both directions. A mark or entry that the rule denies fails, and a blank that the rule fills fails. `tests/testthat/test-readme-capabilities.R` shows this under `devtools::test()`. If README.Rmd is absent, as under R CMD check, the test skips.
- [x] AC4: Key Features is tidied. `grep -c "$(printf '\t')" README.Rmd` prints 0. The list keeps three bullets: scoring and data tools, downloads, and metadata. No bullet names an instrument, and the scoring bullet points to the table. "Comprehensive" does not occur in README.Rmd.
- [x] AC5: Two line ranges of README.Rmd match none of the D-083 (b) patterns. The ranges are `### Key Features` to `## Installation`, and `## What the package covers` to the end of the file. The check uses R `grepl(pattern, line, ignore.case = TRUE, perl = TRUE)` for each regular expression, and `fixed = TRUE` for `?c=` and `?z=`.
- [x] AC6: README.md is the output of `devtools::build_readme()` on README.Rmd. After a fresh run on a machine with every Imports package installed, `git diff --exit-code HEAD -- README.Rmd README.md` exits 0. `Rscript -e 'pkgdown::build_home(preview = FALSE)'` exits 0.
- [x] AC7: The keep-in-step line of CLAUDE.md names the README capability table beside `tests/`, NEWS.md and the reference index.

## Coverage

- AC1 → T2, T3
- AC2 → T2
- AC3 → T1, T2, T3
- AC4 → T2, T3
- AC5 → T3
- AC6 → T3
- AC7 → T3

## Tasks

- [x] T1: Write `tests/testthat/test-readme-capabilities.R` first. It finds README.Rmd from the test directory. If the file is absent, the test skips. It parses the table under `## What the package covers` and checks every cell against the column rules in Scope. The row-to-version map is written in the test. Make sure that it fails on the current README because no table exists, not because of an error.
- [x] T2: Edit README.Rmd. Delete the "Development Progress" section. Add the new section, the table and the help-page sentence, and derive each cell from the package under `devtools::load_all()`. Tidy Key Features. Run `Rscript -e 'devtools::build_readme()'`, then make sure that the T1 test passes.
- [x] T3: Prove that the test can fail. In a scratch copy, plant one wrong cell in each column in each direction. Make sure that the test goes red and names that cell, then restore. Add the table to the keep-in-step line of CLAUDE.md. Run the AC1, AC4 and AC5 checks, the AC6 rebuild with its diff and `build_home()`, and the full `devtools::test()`.

## Work log

- 2026-10-04: created by /milestone-plan.
- 2026-10-04: question set: what replaces the checklist. Answer: a capability table (the recommended links section was declined).
- 2026-10-04: question set: tidy Key Features too. Answer: yes.
- 2026-10-04: criteria audit (full mode, fresh Opus reader) on the links-section draft returned 6 findings, all taken. They banned milestone numbers and phase notes, added a retired-term check, dropped the live-site curl, avoided a self-link, diffed against HEAD and named the machine.
- 2026-10-04: criteria re-audit (full mode, fresh Opus reader) on the table draft returned 6 findings, all taken toward the narrower promise. Column rules and the row-to-version map moved into Scope. Validity and Norms columns were dropped. Tutorial and Forms cells are checked in both directions. Key Features names no instrument. AC5 names its tool and ranges, and AC1 pins the heading list.
- 2026-10-04: plan gate chose a capability table over a links section (the user's answer). Falsified by readers who find the table stale, or by table edits for reasons other than a real capability change.
- 2026-10-04: plan chose a test that checks each cell against the package over a hand-checked static table. A new instrument then fails the test until the README follows. Falsified by test failures on changes that leave every capability the same.
- 2026-10-04: plan chose to drop Validity and Norms columns over footnoted cells, because the child and forensic rows need judgments the rules cannot state. Falsified by a reader who asks which forms have validity scales or norms.
- 2026-10-04: collision sweep found no README work in the ROADMAP, archive or DECISIONS beyond D-083, which governs the new prose (AC5). M163 does not touch the README. Open issue #87 (hosting data or links) does not overlap, and no PRs are open.
- 2026-10-04: no NEWS entry, because the README is not package behavior.
- 2026-10-04: implement started on branch m164-readme-capability-table. The five untracked `devel/hitopdat_*` files are unrelated and stay unstaged.
- 2026-10-04: T1 done. The new test fails all 5 cases on the old README with "no table under `## What the package covers`", not an error. A mark reads "Yes", and a Tutorial cell links the site's article URL. A one-row call works for every scoring and reliability function (checked under load_all).
- 2026-10-04: T2 done. Cells were derived with the test's own helpers under load_all, and the test passes 58 checks. Key Features bullets are plain, with no bold lead-ins. The instrument full names and the child age range moved from Key Features to a sentence at the top of the new section. Forms lists formats in the order Word, Qualtrics, REDCap, JSON, and the test compares them as a set.
- 2026-10-04: T3 planted defects: 14 plants (Items up and down, a denied and a missed mark in Scoring and Reliability, a denied, missed and wrong Tutorial link, a denied and missed Forms format, a format on a blank cell, a dropped and a renamed row). Each turned the test red and named the planted cell. The README was restored, and the test passed 58 checks again. Script kept in the session scratchpad only.
- 2026-10-04: T3 checks: the AC1 grep for checklist text prints nothing. AC4 tab count 0 and "Comprehensive" count 0. AC5 found 0 hits over 7 + 21 lines, and every pattern hit its positive control. AC6: build_readme then `git diff --exit-code HEAD` exit 0, and build_home exit 0. Full devtools::test(): FAIL 0, SKIP 15, PASS 28259. CLAUDE.md keep-in-step line now names the table.
- 2026-10-04: AC1 as planned fails: `grep -nE '^#{1,6} ' README.Rmd` also prints line 37, `# install.packages("pak")`, an R comment inside the Installation code block that predates M164. The deliverable is unchanged, so the repair is to the wording.
- 2026-10-04: re-audit: AC1 (full) — met as written. "Four headings" also covers underlined and HTML headings that the awk cannot see. Wording narrowed to "four `#` headings".
- 2026-10-04: re-audit: AC1 (full) — met as written. "Outside fenced code blocks" also covers `~~~`, nested and indented fences that the awk does not see. The reader suggested making the awk command the definition. This is the second re-audit line on AC1, so the wording choice goes to the user.
- 2026-10-04: substantive amendment: AC1's heading check now uses the awk command that skips code chunks, and the command defines the four headings (the user chose this wording at a stop over the plain-words version). The README is unchanged.
- 2026-10-04: claim audit: 24 claims read, 2 corrected — README.Rmd, README.md, CLAUDE.md, tests/testthat/test-readme-capabilities.R
- 2026-10-04: the 2 corrections: Key Features bullet 1 no longer claims data-cleaning tools for every marked instrument, and the Tutorial sentence says "shows the scoring call" in place of "covers". The same reader re-read both, and both hold. The audit also found that DESCRIPTION and `?hitophsum_items` call HSUM a "Measure", outside this diff. That finding went to the "User-facing names" candidate row. Two other ROADMAP rows were trimmed to keep the file under 24,000 bytes.
- 2026-10-04: after the corrections, README.md was rebuilt, the capability test passes 58 checks, and the tab and retired-term checks stay clean. Status set to review.
## Decisions

## Review

Evidence gathered 2026-10-04 on branch head a8cd60e0, which contains origin/main 48889a03.

- AC1: the awk command printed exactly 4 lines, in order: `# hitop <a href=…`, `### Key Features`, `## Installation`, `## What the package covers`. The checklist grep over README.Rmd and README.md printed nothing (exit 1).
- AC2: the section holds one table of 13 lines (a header, a separator and 11 rows). The header in both files reads `| Instrument | Items | Scoring | Reliability | Tutorial | Forms |`. README.Rmd:61 and README.md:71-72 hold the `?validity_pid5` and `?norm_pid5` sentence. The README.md table equals the README.Rmd table line for line, apart from pandoc's separator dashes (diff exit 0). The capability test's first case confirms the 11 row names as a set, with no duplicates.
- AC3: `devtools::test(filter = "readme-capabilities")` gave FAIL 0, SKIP 0, PASS 58. A fresh run of the 14-plant script turned the test red for every plant. That covers both directions in Items, Scoring, Reliability, Tutorial and Forms, plus a dropped and a renamed row, and each failure named the planted cell. The README was restored, and the test passed 58 checks again. The skip under R CMD check is shown by the check run in the gate below.
- AC4: tab count 0 and "Comprehensive" count 0 in README.Rmd. Key Features has 3 bullets: scoring and reliability for the marked instruments plus rename and label helpers, then downloads, then data tables. No bullet names an instrument, and bullet 1 points to "the table below".
- AC5: R `grepl(perl = TRUE, ignore.case = TRUE)` over 7 Key Features lines and 21 new-section lines found 0 hits for all 10 regular expressions, and `fixed = TRUE` found 0 hits for `?c=` and `?z=`. Every pattern hit its positive control, and the lookbehind stayed silent on "study link builder".
- AC6: `devtools::build_readme()` exit 0, then `git diff --exit-code HEAD -- README.Rmd README.md` exit 0. `pkgdown::build_home(preview = FALSE)` exit 0. Machine: local macOS with every Imports package installed.
- AC7: CLAUDE.md:30 names "the README capability table ("What the package covers", checked by `test-readme-capabilities.R`)" beside `tests/`, NEWS.md and the reference index.
