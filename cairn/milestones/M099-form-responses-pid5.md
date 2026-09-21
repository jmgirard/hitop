# M099: The package's reader scores the three PID-5 forms saved by hitop-form

- **Status:** review
- **Priority:** normal
- **Depends on:** M098
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** user-facing — tested behavior of an exported function and a vignette section
- **Branch/PR:** `m099-form-responses-pid5`

## Goal

Prove that a PID-5 file saved by hitop-form round-trips through `read_form_responses()` and `score_pid5()` for each of the three versions. Document the hand-off in the PID-5 vignettes.

## Scope

**In:** the three hitop-form PID-5 fixtures copied into the package with provenance. Three read-and-score tests with a recomputed-mean oracle. One section in the full-form scoring vignette and one pointer sentence in each of the SF and BF vignettes (Jeff, 2026-09-20 plan gate). NEWS.

**Out:** any change to `read_form_responses()`. None is expected, and a defect found here goes through the amendment protocol. The page → M098. The exports → M097. A shared article on online collection for every instrument → not planned. The HiTOP-SR section stays in `modules-hitopsr.Rmd`.

## Acceptance criteria

- [x] AC1: For each of `FULL`, `SF` and `BF`, `read_form_responses()` on its fixture returns one row. The item columns are named `pid5_001` to `pid5_220`, `pid5sf_001` to `pid5sf_100` or `pid5bf_01` to `pid5bf_25`. They are of type integer. They hold the value 0 where the file holds `0`. `score_pid5(data, items = <those columns>, version = <v>, append = FALSE)` runs with its defaults `srange = c(0, 3)` and `missing = "apa"`. It returns every scale mean equal to a mean the test recomputes. The recomputation reverses a file value where `pid_items$Reverse` is true for the row whose version column holds that item's number. It averages by `pid_scales[[v]]$itemNumbers`. It uses `pid_domains` for the FULL and SF domain means. The BF total is a row of `pid_scales$BF`.
- [x] AC2: `vignettes/pid5_scoring.Rmd` gains a section on collecting PID-5 responses with hitop-form: the study link, the saved file, `read_form_responses()` and `score_pid5()`. The SF and BF vignettes each point to it in one sentence. The three vignettes render.
- [x] AC3: NEWS names the PID-5 support in the hitop-form hand-off. `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4

## Tasks

- [x] T1: Copy `responses-pid5.csv`, `responses-pid5sf.csv` and `responses-pid5bf.csv` from hitop-form at its M098 merge commit into `tests/testthat/fixtures/`. Record repository, commit and generator in `tests/testthat/fixtures/README.md` as the HiTOP rows do.
- [x] T2: Add the three round-trip tests to `tests/testthat/test-read_form_responses.R` on the pattern at `:374-407`. Each carries the recomputed-mean oracle of AC1 and the 0-value assertion.
- [x] T3: Write the vignette section and the two pointer sentences. Render the three vignettes.
- [x] T4: NEWS entry. Run `devtools::check()`.

## Work log

- 2026-09-20: created by /milestone-plan; part one of the online-form candidate row (lineage M096).
- 2026-09-20: criteria audit ran in full mode by a fresh [O] reader; findings on this file repaired before the gate: the fixture-provenance criterion demoted to T1, the oracle's join between `Reverse` and the version's number column and the scoring defaults named in AC1.
- 2026-09-20: plan gate chose one section in the full-form vignette with pointers from the SF and BF vignettes over repeating it in all three or a new shared article because one copy stays in step; falsified by readers of the SF or BF vignette reporting the pointer as insufficient.
- 2026-09-21: implement started on `m099-form-responses-pid5`. No question gate: the plan left nothing open.
- 2026-09-21: T1 done. The three PID-5 fixtures copied from hitop-form `bfb1d9c` with CRLF converted to LF, and their row added to the fixtures README.
- 2026-09-21: T2 done. Three PID-5 round-trip tests added. A plant that drops the reversal changes 8 of 25 FULL facets. No SF or BF item is reversed in `pid_items`, so the reversal is exercised on FULL only. Full suite: 0 fail, 17541 pass, 13 skip.
- 2026-09-21: T3 done. `pid5_scoring.Rmd` gains "Collecting Responses Online with hitop-form", which reads and scores the full-form fixture. The SF and BF vignettes point to it in one sentence each. All three render with `rmarkdown::render()`.
- 2026-09-21: T4 done. NEWS extends the `read_form_responses()` entry with the PID-5 hand-off. `devtools::check()`: 0 errors, 0 warnings, 0 notes, vignettes rebuilt.
- 2026-09-21: claim audit: 26 claims read, 0 corrected — NEWS.md, tests/testthat/fixtures/README.md, tests/testthat/test-read_form_responses.R, vignettes/pid5_scoring.Rmd, vignettes/pid5sf_scoring.Rmd, vignettes/pid5bf_scoring.Rmd
- 2026-09-21: the claim reader noted that the `read_form_responses()` help page names only HiTOP column examples and omits `score_pid5()` from its see-also. Left unchanged because Scope Out excludes changes to that function. Raised for review.
- 2026-09-21: status set to review.

## Decisions

## Review

Evidence, 2026-09-21, on `m099-form-responses-pid5` at `5ca1d2c2` (level with `origin/main`, no merge needed).

- AC1: `devtools::test(filter = "read_form_responses")`: 113 expectations, 0 failed, 0 errors. The three PID-5 tests (FULL, SF, BF) pass 8 expectations each. They cover one row, the named integer item columns, every file `0` read as `0L`, and `score_pid5()` equal to the recomputed means. The recomputation includes the FULL and SF domains from `pid_domains` and the BF total from `pid_scales$BF`. A hand re-run showed that `pid_items` reverses 16 FULL items and no SF or BF item. Without the reversal, 8 of 25 FULL facet means differ from `score_pid5()`. The FULL file holds 55 zeros.
- AC2: `vignettes/pid5_scoring.Rmd:196` opens "Collecting Responses Online with hitop-form". The section covers the study link from the link builder, the saved file and its columns, a `read_form_responses()` chunk and a `score_pid5()` chunk. `pid5sf_scoring.Rmd:30` and `pid5bf_scoring.Rmd:32` each point to that section in one sentence. All three are package vignettes. `devtools::check()` rebuilt the vignette outputs with status OK. Both hitop-form links returned HTTP 200.
- AC3: `NEWS.md:18-20` extends the `read_form_responses()` entry. It names the PID-5, PID-5-SF and PID-5-BF files, the value 0 kept as 0, and `score_pid5()` with the matching `version`. It holds no milestone number. `devtools::check()` returned 0 errors, 0 warnings and 0 notes in 4m 7s, with tests OK.
- Consistency gate: `cairn_validate.py` exited 0 with 24 advisories, all from before this branch. `devtools::document()` left no diff. `pkgdown::check_pkgdown()` found no problems. The branch does not touch `README.Rmd` or `README.md`. The branch adds no new top-level file and changes no DESIGN principle, so `cairn_impact` does not apply.
