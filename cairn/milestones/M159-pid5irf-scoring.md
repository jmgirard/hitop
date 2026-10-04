# M159: PID-5 informant form (IRF) keying and scoring

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** M157
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP2
- **Resolves:** —
- **Surface tier:** user-facing — a new `version` value on exported scoring functions and new keying data
- **Branch/PR:** m159-pid5irf-scoring

## Goal

Researchers can score the 218-item PID-5 Informant Form (Markon et al., 2013) with `score_pid5()` and `reliability_pid5()`.

## Scope

**In:**
- The IRF keying and item text enter the package through `data-raw/`. The source is the APA scoring key on the shelf (`cairn/references/sources/apa2013pid5irf.pdf`), with its reverse list, facet table and domain table. The criteria audit reported that the key's Step 1 lists 16 reverse items, while its facet table marks 14 with R. Items 98 and 176 carry no R, and their wording is not reversed. T2 settles which list governs.
- The informant instructions enter `R/sysdata.rda`.
- `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` accept the new version.
- A references page, a SOURCES.md row, tests, NEWS and a vignette section.

**Out:**
- Word, Qualtrics and REDCap forms go to M160.
- IRF norms (markon2024 A–10 to A–12) stay a candidate row, promoted after this milestone.
- `validity_pid5()` keeps its versions. No published validity scale for the IRF is on the shelf.
- `norm_pid5()`, `plot_pid5()` and the online tools stay on the downstream candidate row.

## Acceptance criteria

- [ ] AC1: The package holds the 218 IRF items in APA order. Each item has its text, its facet as the key's facet table prints it, and its reverse flag from the list T2's gate chose. A keying test compares the reverse list and every facet's item list with a transcription typed into the test file, not derived from the package tables. It also checks the 25 facets and the 5 domains against the key's facet and domain tables. The references page records the T1 script that checks all 218 texts against the shelf PDF, its run date and its result.
- [ ] AC2: `score_pid5(version = <IRF string>)` returns the 25 facet and 5 domain columns that the FULL version returns, named the same way. Its values equal hand-computed values under the APA rules the key prints (reverse, prorate, average). The test fixture has at least 5 respondents. One respondent has a facet just under the 25% missing limit, one has a facet over it, and one has a domain that goes `NA`. Items 98 and 176 take values other than 1.5 there. The expected values are typed into the test.
- [ ] AC3: The existing versions do not change. M157's `data-raw/` characterization script, extended with M157's version, runs at the merge base and at the branch head. It shows `identical()` output over the same functions and argument grid as M157 AC3. The BF+M input is built from `sim_pid5` columns at the BF+M item numbers.
- [ ] AC4: `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` each have a test on the IRF version. `label_pid5()` labels with the informant item text.
- [ ] AC5: The help pages and `vignettes/pid5_scoring.Rmd` name the IRF and cite Markon et al. (2013) and the APA key. NEWS.md has an entry. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T4
- AC4 → T3, T4
- AC5 → T5

## Tasks

- [ ] T1: Write the references page for the APA IRF key and add the SOURCES.md row. Transcribe the 218 items and check the transcription against the PDF text by a script run on the shelf copy.
- [ ] T2: Pre-implementation gate. Put three questions to Jeff:
  - Which reverse list governs, Step 1's 16 items or the facet table's 14? This is keying content and needs Jeff's sign-off. Record it as an open question in SOURCES.md. (RB tripwire: ip-touching)
  - Does the IRF go in a column of `pid_items` or in a separate table? The audit reported that the IRF lacks self-report items 96 and 177, so the row alignment shifts before item 96. A column holds informant text beside the self-report text. (RB tripwire: irreversible-api)
  - What are the version string and the item-name stem for `rename_pid5_items()` and the M160 exports?

  Then build the data in `data-raw/` and write the keying test (AC1). Keying content needs Jeff's sign-off before merge.
- [ ] T3: Thread the version through the four functions and add the instructions to `R/sysdata.rda`.
- [ ] T4: Write the fixture tests (AC2, AC4). Extend M157's characterization script and run it at the merge base and at the head (AC3).
- [ ] T5: Update the help pages, vignette and NEWS. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157, M158, M160 and M161. It depends on M157 so that the two version additions do not collide in the same functions.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 2 judgment findings, all applied. The key's two reverse lists disagree (16 in Step 1, 14 marked R), so AC1 takes the list the T2 gate chooses. AC2 probes the 25% boundary. AC3 reuses M157's characterization script. The T2 gate adds the item-name stem.
- 2026-10-03: implement started on branch `m159-pid5irf-scoring`, cut from main at `d46368c2` after M158 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit.

## Decisions

## Review
