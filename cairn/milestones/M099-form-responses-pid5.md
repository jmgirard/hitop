# M099: The package's reader scores the three PID-5 forms saved by hitop-form

- **Status:** planned
- **Priority:** normal
- **Depends on:** M098
- **Driving RR:** —
- **Principles touched:** IP2
- **Resolves:** —
- **Surface tier:** user-facing — tested behavior of an exported function and a vignette section
- **Branch/PR:** —

## Goal

Prove that a PID-5 file saved by hitop-form round-trips through `read_form_responses()` and `score_pid5()` for each of the three versions. Document the hand-off in the PID-5 vignettes.

## Scope

**In:** the three hitop-form PID-5 fixtures copied into the package with provenance. Three read-and-score tests with a recomputed-mean oracle. One section in the full-form scoring vignette and one pointer sentence in each of the SF and BF vignettes (Jeff, 2026-09-20 plan gate). NEWS.

**Out:** any change to `read_form_responses()`. None is expected, and a defect found here goes through the amendment protocol. The page → M098. The exports → M097. A shared article on online collection for every instrument → not planned. The HiTOP-SR section stays in `modules-hitopsr.Rmd`.

## Acceptance criteria

- [ ] AC1: For each of `FULL`, `SF` and `BF`, `read_form_responses()` on its fixture returns one row. The item columns are named `pid5_001` to `pid5_220`, `pid5sf_001` to `pid5sf_100` or `pid5bf_01` to `pid5bf_25`. They are of type integer. They hold the value 0 where the file holds `0`. `score_pid5(data, items = <those columns>, version = <v>, append = FALSE)` runs with its defaults `srange = c(0, 3)` and `missing = "apa"`. It returns every scale mean equal to a mean the test recomputes. The recomputation reverses a file value where `pid_items$Reverse` is true for the row whose version column holds that item's number. It averages by `pid_scales[[v]]$itemNumbers`. It uses `pid_domains` for the FULL and SF domain means. The BF total is a row of `pid_scales$BF`.
- [ ] AC2: `vignettes/pid5_scoring.Rmd` gains a section on collecting PID-5 responses with hitop-form: the study link, the saved file, `read_form_responses()` and `score_pid5()`. The SF and BF vignettes each point to it in one sentence. The three vignettes render.
- [ ] AC3: NEWS names the PID-5 support in the hitop-form hand-off. `devtools::check()` reports 0 errors, 0 warnings and 0 notes.

## Coverage

- AC1 → T1, T2
- AC2 → T3
- AC3 → T4

## Tasks

- [ ] T1: Copy `responses-pid5.csv`, `responses-pid5sf.csv` and `responses-pid5bf.csv` from hitop-form at its M098 merge commit into `tests/testthat/fixtures/`. Record repository, commit and generator in `tests/testthat/fixtures/README.md` as the HiTOP rows do.
- [ ] T2: Add the three round-trip tests to `tests/testthat/test-read_form_responses.R` on the pattern at `:374-407`. Each carries the recomputed-mean oracle of AC1 and the 0-value assertion.
- [ ] T3: Write the vignette section and the two pointer sentences. Render the three vignettes.
- [ ] T4: NEWS entry. Run `devtools::check()`.

## Work log

- 2026-09-20: created by /milestone-plan; part one of the online-form candidate row (lineage M096).
- 2026-09-20: criteria audit ran in full mode by a fresh [O] reader; findings on this file repaired before the gate: the fixture-provenance criterion demoted to T1, the oracle's join between `Reverse` and the version's number column and the scoring defaults named in AC1.
- 2026-09-20: plan gate chose one section in the full-form vignette with pointers from the SF and BF vignettes over repeating it in all three or a new shared article because one copy stays in step; falsified by readers of the SF or BF vignette reporting the pointer as insufficient.

## Decisions

## Review
