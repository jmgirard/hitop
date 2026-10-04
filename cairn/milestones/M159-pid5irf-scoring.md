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

- [x] T1: Write the references page for the APA IRF key and add the SOURCES.md row. Transcribe the 218 items and check the transcription against the PDF text by a script run on the shelf copy.
- [x] T2: Pre-implementation gate. Put three questions to Jeff:
  - Which reverse list governs, Step 1's 16 items or the facet table's 14? This is keying content and needs Jeff's sign-off. Record it as an open question in SOURCES.md. (RB tripwire: ip-touching)
  - Does the IRF go in a column of `pid_items` or in a separate table? The audit reported that the IRF lacks self-report items 96 and 177, so the row alignment shifts before item 96. A column holds informant text beside the self-report text. (RB tripwire: irreversible-api)
  - What are the version string and the item-name stem for `rename_pid5_items()` and the M160 exports?

  Then build the data in `data-raw/` and write the keying test (AC1). Keying content needs Jeff's sign-off before merge.
- [x] T3: Thread the version through the four functions and add the instructions to `R/sysdata.rda`.
- [ ] T4: Write the fixture tests (AC2, AC4). Extend M157's characterization script and run it at the merge base and at the head (AC3).
- [ ] T5: Update the help pages, vignette and NEWS. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M157, M158, M160 and M161. It depends on M157 so that the two version additions do not collide in the same functions.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 2 judgment findings, all applied. The key's two reverse lists disagree (16 in Step 1, 14 marked R), so AC1 takes the list the T2 gate chooses. AC2 probes the 25% boundary. AC3 reuses M157's characterization script. The T2 gate adds the item-name stem.
- 2026-10-03: implement started on branch `m159-pid5irf-scoring`, cut from main at `d46368c2` after M158 merged. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit.
- 2026-10-03: T1 done. `data-raw/pid_irf_items.csv` holds the 218 items (IRF number, mapped self-report number, facet, text), built from `pdftotext -layout`. `data-raw/check_pid_irf_text.R` reads the shelf PDF with `pdftotext -raw` and passes on all 218 texts, 25 facets and the mapping. A planted wrong word and facet gave 4 failures. New references page `apa2013pid5irf.md`, two SOURCES.md rows and OQ-4.
- 2026-10-03: implement choice: the IRF text drops the leading ellipsis and the final period and uses ASCII quotes, as `pid_items.csv` does for the self-report text. The check normalizes the PDF the same way. The IRF has no counterpart to self-report items 96 and 177, both reverse-keyed. Mapped across, the self-report reverse flags give the Facet Table's 14 R marks, not Step 1's 16.
- 2026-10-03: T2 gate: Jeff chose the Facet Table's 14 reverse items, two new `pid_items` columns (`IRF`, `TextIRF`), and `version = "IRF"` with `pid5irf_001` names. The tripwire escalation was offered and not taken. This is keying sign-off for the reverse list. Recorded as D-089, and OQ-4 is resolved.
- 2026-10-03: T2 done. `data-raw/pid_info.R` adds `IRF` after `BFPM` and `TextIRF` after `Text`, and builds `pid_scales$IRF` (25 facets in FULL order). Domains reuse `pid_domains`. Five keying tests type the key's reverse list, Facet Table, Domain Table and four text anchors. Planting Step 1's list and a moved Withdrawal item failed two of them. `test-column-shape.R`'s hand list of `pid_scales` elements gained `IRF`. Full suite: 0 failures.
- 2026-10-03: T3 done. `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` take `version = "IRF"` (218 items, `pid_domains` map, `pid5irf_` stem, label "PID-5-IRF"). The text paths read `TextIRF`. New internal `pid_irf_instructions` (start, continue, prompt, stem, the self-report options); the other sysdata objects are identical. The check script now also matches start, continue and prompt to the PDF, and a planted prompt edit failed it. Full suite: 26635 passes, 0 failures.
- 2026-10-03: implement choice: `pid_irf_instructions` keeps the form's later-page text (`continue`), its rating prompt and its "He or she…" stem beside `start`, so M160's Word form can print them. The response options are a copy of `pid_instructions$options`, as the form prints the same labels.
- 2026-10-03: T4 stop (ip-touching tripwire, emerged mid-work): the IRF key (p. 9) says to "round up to the nearest whole number" after proration. Both child keys on the shelf and the self-report key quoted in SOURCES.md say "round to the nearest whole number", and `apa_mean()` rounds half up for every version. AC2's fixture cannot be typed until the rule is settled. Jeff chose escalation via `/milestone-brief` over round-up (recommended), round-to-nearest and stopping.

## Decisions

## Review
