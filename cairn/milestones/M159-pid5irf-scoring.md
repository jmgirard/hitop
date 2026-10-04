# M159: PID-5 informant form (IRF) keying and scoring

- **Status:** review
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

- [x] AC1: The package holds the 218 IRF items in APA order. Each item has its text, its facet as the key's facet table prints it, and its reverse flag from the list T2's gate chose. A keying test compares the reverse list and every facet's item list with a transcription typed into the test file, not derived from the package tables. It also checks the 25 facets and the 5 domains against the key's facet and domain tables. The references page records the T1 script that checks all 218 texts against the shelf PDF, its run date and its result.
- [x] AC2: With `append = FALSE` and `calc_se = FALSE`, `score_pid5(version = "IRF")` returns the 30 columns that the FULL version returns under the same `prefix`, with the same names in the same order. On a test fixture of at least 5 respondents, with `missing = "apa"`, `srange = c(0, 3)` and `calc_se = FALSE`, every one of the 30 columns for every respondent equals, within `testthat::expect_equal()`'s default tolerance, a value hand-computed from the APA key under D-009's rule, read half-up as SOURCES.md's "Note on FULL/SF domain scoring" records, which D-090 applies to the IRF. A facet's items are those the key's Facet Table lists. Each of the Facet Table's 14 R items is reversed as 3 minus the answer (D-089(c)). A facet with more than 25% of its items unanswered is `NA`. Otherwise the prorated raw is the partial sum times the item count over the items answered, rounded to the nearest whole number with halves up, and the facet is that rounded raw over the item count. A domain is the mean of its 3 primary facets in the key's Domain Table, and is `NA` if any of the 3 is `NA`. The key's printed "round up" is not applied. The fixture includes: (i) a facet with exactly 25% of its items unanswered, which is scored; (ii) a facet with the fewest unanswered items that exceeds 25% of its items, which is `NA`; (iii) a domain that is `NA` while at least one of its 3 facets is scored; (iv) a prorated facet whose prorated raw has a fractional part strictly between 0 and one half, whose expected value differs from the value a ceiling rule gives; (v) a prorated facet whose prorated raw is an exact half with an even whole part, whose expected value differs from the value base `round()` gives; (vi) a respondent who answers items 98 and 176 with whole numbers from 0 to 3, with their Unusual Beliefs & Experiences and Depressivity facets scored, so the expected values differ from those Step 1's 16-item reverse list gives. The expected values are typed into the test, with the arithmetic in comments.
- [x] AC3: The existing versions do not change. M157's `data-raw/` characterization script, extended with M157's version, runs at the merge base and at the branch head. It shows `identical()` output over the same functions and argument grid as M157 AC3. The BF+M input is built from `sim_pid5` columns at the BF+M item numbers.
- [x] AC4: `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` each have a test on the IRF version. `label_pid5()` labels with the informant item text.
- [x] AC5: The help pages and `vignettes/pid5_scoring.Rmd` name the IRF and cite Markon et al. (2013) and the APA key. NEWS.md has an entry. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

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
- [x] T4: Write the fixture tests (AC2, AC4). Extend M157's characterization script and run it at the merge base and at the head (AC3). In the AC2 fixture, vary answers within facets, so a wrong item list or IRF numbering shows, and include one scored domain with a prorated primary facet.
- [x] T5: Update the help pages, vignette and NEWS. The `missing` help text, the IRF help section and NEWS state that the key's printed "round up" is read as the nearest-whole-number rule (D-090), and the `apa_mean()` comment points to D-090. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

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
- 2026-10-03: blocked on RB07 (`cairn/reviews/RB07-irf-proration-rounding.md`, advisory, no binding criteria). The brief commit sits on the milestone branch, not on main, because this milestone's tracking lives on the branch.
- 2026-10-03: RR07 ingested (Fable, advisory). Verdict: keep round-half-up for the IRF, no code change (D-090). Spot check: the Level 2 Anxiety key's worked example (23.33 to 23) and its sha256 match the RR. Triage: recs 1, 3, 4 (SOURCES.md and the references page now), 5 and 6 applied; rec 4's help and NEWS text and rec 7 (`apa_mean()` comment) scheduled in T5; rec 2 (AC2 wording) goes through the amendment protocol; recs 8 to 10 rejected as the RR recommends (no ceiling, no version-specific rule or warning, no change to other versions). Finding 1 of the first audit showed D-009's own text writes base `round()`, so D-090 cites the SOURCES.md half-up note instead.
- 2026-10-03: re-audit: AC2 (full) — RR07's wording: 6 clear fixes (D-009 citation, unscoped value claim, the 98/176 clause did not bind, 25% probe, domain-NA probe, column order and `append`) and 5 judgment findings. Judgments decided toward the narrower promise: fixture probes stay in AC2, "more than 25%" kept, other missing modes and the two shorter facets left to the shared engine and AC1; the "round up" statement in docs went to T5, not AC5.
- 2026-10-03: re-audit: AC2 (full) — fixed wording: 5 clear fixes (`calc_se` and `prefix` scope, oracle defined by `apa_mean()` against IP2, the over-25% probe must sit at the fewest items past the limit, tolerance, one fixture reference) and 3 judgment findings (docs statement, already in T5; varied answers and a prorated primary facet, added to T4; fixture size, kept). Second re-audit line on AC2, so the final wording goes to Jeff.
- 2026-10-03: status back to `in-progress` after RR07; RB07 and RR07 moved to `cairn/reviews/archive/`.
- 2026-10-03: substantive amendment: AC2 — replaced with the twice-audited wording that names D-090's round-to-nearest rule in place of "the APA rules the key prints", fixes `append`, `calc_se`, `prefix`, `missing` and the tolerance, and lists fixture cases (i) to (vi). Jeff adopted it at the stop over RR07's shorter text and stopping.
- 2026-10-03: T4 done. `fx_pid5irf()` (5 rows, answers varying within facets) and 150 expected values typed in `test-score_pid5.R` with their arithmetic. The values came from an independent Python oracle over the key's typed tables, and the package matched all 150 before the test was written. Fixture cases (i) to (vi) are in the helper header. Planted ceiling, base `round()`, a `>=` cutoff and Step 1's reverse list each failed the value test. AC4 tests for `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()`; a planted self-report label failed the label test. `characterize_bfpm.R` gained a BFPM pairing built from `sim_pid5`; merge base `d46368c2` and head give 100 of 100 identical calls (20 BFPM). Full suite: 26695 passes, 0 failures.
- 2026-10-03: T5 done. `?score_pid5` gains the IRF in its description, a "PID-5 Informant Form" details section (facets, mapping, the 14 reverse items, the "round up" reading), the D-090 sentence in `missing`, both Markon et al. (2013) references (DOI checked on Crossref) and an example. `?reliability_pid5`, `?rename_pid5_items` and `?label_pid5` name the IRF and cite both sources. New vignette section "The PID-5 Informant Form". NEWS entry; the BF+M entry lost its column counts, which the IRF entry now states (18 and 5). `apa_mean()` comment points to D-090. `check_pkgdown()` no problems; `devtools::check()` 0 errors, 0 warnings, 0 notes.
- 2026-10-03: claim audit corrections applied (checkpoint; the reader's re-read of the corrected claims is pending): vignette norms and informant sentences, test-comment page citations and normalization source, `check_pid_irf_text.R` header, PASS line and section 4 (now compares the final period; a planted "!" failed it), the `?score_pid5` "matters only" sentence, `TextIRF` help, and four stale lines. Full suite: 26695 passes, 0 failures.
- 2026-10-03: claim audit: 300 claims read, 13 corrected — vignettes/pid5_scoring.Rmd, tests/testthat/test-keying.R, tests/testthat/test-label_pid5.R, tests/testthat/test-rename_pid5_items.R, data-raw/check_pid_irf_text.R, data-raw/characterize_bfpm.R, R/score_pid5.R, R/data.R, R/rename_pid5_items.R
- 2026-10-03: the same reader re-read all 13 corrected claims once and found each accurate. It confirmed that the check script now fails when a stored instruction's final "." is changed. The item-98 quote on the references page was fixed too.
- 2026-10-03: implement done. Status set to `review`.

## Decisions

- 2026-10-03 (T4, from RR07): the IRF prorates with the nearest-whole-number rule, halves up, through the unchanged `apa_mean()`. The key's printed "round up" is read as a wording lapse. Cross-cutting, so recorded as D-090.

## Review

- Evidence AC1 (2026-10-03, head `d0a140ed`): `pid_items` has 218 rows with `IRF` numbered 1 to 218 in row order, each with `TextIRF` text, 25 facets in all. `test-keying.R` 0 failures (1 skip, OQ-1); its 4 IRF tests (69 expectations) compare the 14-item reverse list, all 25 facet item lists and the 5 domain triplets with transcriptions typed from the key, plus 4 text anchors. `data-raw/check_pid_irf_text.R` rerun on the shelf PDF prints PASS for all 218 texts, 25 facets, the mapping and the instructions. The references page's provenance line names that script, its run date (2026-10-03) and its result.
- Evidence AC2 (2026-10-03, head `d0a140ed`): `test-score_pid5.R` 0 failures; its 4 IRF tests (40 expectations) pass. With `append = FALSE` and `calc_se = FALSE`, IRF output names are identical to FULL's under the default and an `x_` prefix. On `fx_pid5irf()` (5 rows), all 30 columns for every row equal the 150 values typed with their arithmetic, under `missing = "apa"`, `srange = c(0, 3)`, `calc_se = FALSE`, by `expect_equal()`. The fixture cases, rerun: (i) Submissiveness R3 has 1 of 4 missing and scores 2; (ii) Anhedonia R3 has 3 of 8 missing (the fewest past 25%) and is `NA`; (iii) Detachment R3 is `NA` while Withdrawal scores 1.3; (iv) Callousness R4 prorates 15.27 to 15/14 = 1.071, against 1.143 under a ceiling; (v) Distractibility R4 prorates 4.5 to 5/9 = 0.556, against 0.444 under base `round()`; (vi) items 98 and 176 are answered 2 and 0 in R1, where Unusual Beliefs & Experiences (1.625) and Depressivity (1.857) differ from Step 1's list values (1.5 and 29/14), asserted in the test.
- Evidence AC3 (2026-10-03): `data-raw/characterize_bfpm.R` (M157's script, extended with a BFPM pairing built from `sim_pid5` at the BF+M item numbers) ran against the merge base `d46368c2` in a temporary worktree and against the branch head. Both runs give 100 entries with the same names, and all 100 are `identical()`: 20 FULL, 40 SF (`sim_pid5sf` and `ku_pid5sf`), 20 BF and 20 BFPM. The grid is M157's: `score_pid5()` over missing × `calc_se` × `append` (60), `reliability_pid5()` over alpha × omega (20), `rename_pid5_items()` by number and text (10), `label_pid5()` items and scales (10).
- Evidence AC4 (2026-10-03): `test-reliability.R`, `test-rename_pid5_items.R` and `test-label_pid5.R` each 0 failures. Their IRF tests: `reliability_pid5()` returns FULL's 25 scale rows with facet sizes typed from the key and alphas over typed items for 3 facets (6 expectations); `rename_pid5_items()` renames 218 numbered columns to `pid5irf_001` to `pid5irf_218` and matches informant text, while self-report text stays unmatched (6); `label_pid5()` labels items with typed informant wording and scales with FULL's names (8). Rerun: `pid5irf_098` is labelled "sometimes hears things that aren't really there" and `pid5irf_001` "doesn't get as much pleasure out of things as others seem to", the informant text.
- Evidence AC5 (2026-10-03, head `d0a140ed`): `?score_pid5`, `?reliability_pid5`, `?rename_pid5_items` and `?label_pid5` name the IRF, and each cites both Markon et al. (2013, *Assessment* 20(3), 370-383) and the APA's *PID-5-IRF—Adult* key (read in the `.Rd` files). `vignettes/pid5_scoring.Rmd` has the section "The PID-5 Informant Form", citing Markon et al. (2013) and the APA informant key. NEWS.md has the "`score_pid5()` scores the PID-5 Informant Form" entry. `pkgdown::check_pkgdown()` finds no problems. `devtools::check()` gives 0 errors, 0 warnings and 0 notes.
- Consistency gate (2026-10-03): `cairn_validate.py` passes (advisories only: 33 dangling ids, all legacy D-001 to D-012 citations; 1 references staleness). `devtools::document()` makes no diff. `check_pkgdown()` no problems. NEWS.md has the entry. No new top-level files. No DESIGN principle changed, so `cairn_impact` is skipped. README.Rmd did not list the PID-5-IRF, so the gate added it (features line; data, scoring, reliability and tutorial rows ticked; export row unticked until M160) and rebuilt README.md. DESIGN.md's goal and scoring lines and CLAUDE.md's instrument line now name the IRF.
- spawned: diff-bug, blame-history, prior-review
- diff-bug #1: informant scores have FULL's column names and no version record, so `norm_pid5()`/`plot_pid5()` accept them as "FULL" against self-report norms — fix now (docs: `?score_pid5`, `?norm_pid5`, `?plot_pid5`, vignette and NEWS warn), fixed ba61b770; the runtime guard is follow-up, row "PID-5 norms not yet shipped".
- diff-bug #2: `write_instrument_json()` and the generators read `pid_items$Text`, a trap for M160 — `R/json_export.R` comment fixed now, fixed ba61b770; the routing is follow-up, same row.
- diff-bug #3: `?pid_domains` and DESIGN.md called the domain map FULL/SF only — fix now, fixed ba61b770.
- diff-bug #4: `?pid_items` had one `@source`, the IRF key, crediting it for the whole table — fix now (citation moved into the `TextIRF` item), fixed ba61b770.
- diff-bug #5: `validity_pid5()`/`norm_pid5()`/`plot_pid5()` version docs did not mention the IRF, and a refusal is a bare `match.arg()` error — docs fixed now, fixed ba61b770; the cli message is follow-up (pre-existing for BFPM), same row.
- diff-bug #6: `R/score_engine.R` comments listed FULL/SF/BFPM as the domain-map versions — fix now, fixed ba61b770.
- diff-bug #7: `fx_pid5irf()` answers are all `i %% 4`, so swapping a facet item for a congruent one leaves the 150 values unchanged — fix now (independent recomputation from the typed key on random data with NAs, all three missing modes; a planted item swap fails it), fixed ba61b770.
- diff-bug #8: label/rename full sweeps skip the IRF and its tests checked 4 and 2 texts — fix now (all 218 labels and a 218-text reverse-order rename), fixed ba61b770.
- diff-bug #9: `method = "text"` for the IRF needs text without stem, ellipsis and final period, and the help did not say so — fix now (help states it), fixed ba61b770.
- diff-bug #10: the check script compared response labels with a typed vector and skipped the stem — fix now (labels and stem matched against the PDF; planted reversed labels and a changed stem fail), fixed ba61b770.
- diff-bug #11: `pid_items`' readr `spec` lists 15 columns — reject, planned: the `data-raw/pid_info.R` comment records the spec as what was read from `pid_items.csv`.
- diff-bug #12: inserting `IRF` shifts later column positions and NEWS did not say so — fix now (NEWS sentence), fixed ba61b770.
- diff-bug #13: "halves up" vs `round_half_up()`'s half-away-from-zero for negative `srange` — follow-up (pre-existing wording), row "PID-5 norms not yet shipped".
- diff-bug #14: `TextIRF` help writes "He or she..." where `pid_irf_instructions$stem` stores "…" — reject, style.
- diff-bug #15: `test-json-export.R:9` comment counts three forms — follow-up (pre-existing), same row.
- blame-history #1: the IRF departs from the printed "round up" with no runtime signal and no separate sign-off line — fix now: the merge question asks for that sign-off in so many words, and the approval line records it.
- blame-history #2: no test that validity/norm/plot refuse "IRF" — fix now, fixed ba61b770.
- blame-history #3: IRF reverse keying rides on the shared `Reverse` column, guarded only by the keying test — reject, planned: D-089(b) and its Consequences accept the coupling, and `test-keying.R` fails on any change.
- blame-history #4: DESIGN.md data-model lines (items columns, `pid_domains`, internal data, form-variant recipe, wrapper sentence) — fix now, fixed ba61b770.
- blame-history #5: stale comments in `R/json_export.R`, `data-raw/json_export.R`, `test-plot_pid5.R` — fix now, fixed ba61b770.
- blame-history #6: the BF+M NEWS entry lost its column counts — reject, false harm: both entries are unreleased and the BF+M entry still names its added column; the IRF entry gives the final counts.
- blame-history #7: SOURCES.md marks the IRF reverse row verified though it overrides Step 1 — reject, planned: maintainer sign-off (D-089) with OQ-4 kept, marked resolved, with its evidence.
- blame-history #8: `characterize_bfpm.R` keeps its name, does not run the IRF, and a header line is long — reject, planned (AC3 names M157's script and is a no-move check) and style.
- blame-history #9: legacy `pid_` columns can be renamed as IRF items — reject, false: the reviewer found it consistent with the explicit-version rule and nothing new.
- prior-review #1: no IRF refusal test (M157 blame-history #3) — fix now, same as blame-history #2, fixed ba61b770.
- prior-review #2: `R/json_export.R` "four columns" comment (M157 diff-bug #11) — fix now, fixed ba61b770.
- prior-review #3: `R/score_engine.R` domain-map comments (M157 diff-bug #11) — fix now, same as diff-bug #6, fixed ba61b770.
- prior-review #4: `?pid_domains` and test comments said FULL/SF — fix now, fixed ba61b770.
- prior-review #5: DESIGN.md data descriptions — fix now, same as blame-history #4, fixed ba61b770.
- prior-review #6: no IRF standard-error test though the help names IRF (M157 diff-bug #4) — fix now (facet and domain SE test), fixed ba61b770.
- prior-review #7: IRF tested under `"apa"` only (M157 criteria audit) — fix now (the recomputation test runs all three modes), fixed ba61b770.
- prior-review #8: few IRF items and facets checked (M157 diff-bug #5, #7) — fix now (all 218 texts, all 25 alphas), fixed ba61b770.
- prior-review #9: no IRF unpadded/out-of-range label test (M157 prior-review #2) — fix now, fixed ba61b770.
- Fix verification (2026-10-03, head `ba61b770`): `devtools::test()` 27042 passes, 0 failures; `check_pid_irf_text.R` PASS; `devtools::check()` 0 errors, 0 warnings, 0 notes; `check_pkgdown()` no problems; `cairn_validate.py` passes after the deferred findings were folded into the "PID-5 norms not yet shipped" row (ROADMAP at its 60-line cap).
