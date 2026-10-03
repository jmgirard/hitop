# M157: PID5BF+M keying and scoring

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP1, IP2, IP3, GP1, GP2
- **Resolves:** —
- **Surface tier:** user-facing — a new `version` value on exported scoring functions and new keying data
- **Branch/PR:** m157-pid5bfpm-scoring

## Goal

Researchers can score the 36-item PID5BF+M (Bach et al., 2020) with `score_pid5()` and `reliability_pid5()`.

## Scope

**In:**
- The BF+M keying enters `pid_items` and `pid_scales` through `data-raw/pid_info.R`. That is a 36-position map onto PID-5 item numbers, plus 18 facets with 2 items each and 6 domains: Negative Affectivity, Detachment, Antagonism, Disinhibition, Anankastia and Psychoticism.
- The keying source is the FU Berlin key sheet on the shelf (`cairn/references/sources/fuberlin_pid5bfpm_de.pdf`, p. 2). Its table maps each BF+M item to its PID-5 item number and gives the scoring rule.
- `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()` accept the new version.
- A references page, a SOURCES.md row, tests, NEWS, and a scoring vignette section.

**Out:**
- Word, Qualtrics and REDCap forms go to M158.
- The 34-item PID5BF+ waits on Kerber et al. (2022) Figure 1 or its supplement (candidate row).
- BF+M norms (Rek et al., 2022) get a candidate row.
- `validity_pid5()`, `norm_pid5()` and `plot_pid5()` keep their current versions. Each refuses the new version through its existing `match.arg()`. The plot gets a candidate row, and no validity scale exists for this form.
- Module Builder, hitop-form and the Study Link Builder get a candidate row.

## Acceptance criteria

- [ ] AC1: `pid_items` gains a BF+M column. Its 36 non-missing values sit on exactly the PID-5 rows that the key sheet (p. 2) lists, each at its BF+M position. `pid_scales` gains a BF+M entry whose 18 facets list exactly the BF+M item pairs the key sheet gives. The package also holds the BF+M facet-to-domain map, 6 domains with 3 facets each. A keying test compares all three with a transcription of the key-sheet table typed into the test file, not derived from `pid_items`.
- [ ] AC2: `score_pid5(version = <BF+M string>)` returns 18 facet and 6 domain columns. Its values equal hand-computed values under the scoring metric and missing-data rule the pre-implementation gate settles. The test fixture has at least 5 respondents. It covers each `missing` mode the gate keeps for the BF+M, with a missing item under each mode. The expected values are typed into the test, not computed by package code. (RB tripwire: irreversible-api)
- [ ] AC3: The existing versions do not change. A `data-raw/` characterization script in the `characterize_calc_se.R` pattern runs at the merge base and at the branch head. It shows `identical()` output for `score_pid5()` over version (FULL, SF, BF) × `missing` × `calc_se` × `append` on `sim_pid5`, `sim_pid5sf`, `ku_pid5sf` and `sim_pid5bf`. It does the same for `reliability_pid5()` over version × `alpha` and `omega`, for `rename_pid5_items()` over version × its matching methods, and for `label_pid5()` over version.
- [ ] AC4: `reliability_pid5()` returns one row per BF+M facet and domain. `rename_pid5_items()` maps the 36 BF+M items, and `label_pid5()` labels them. Each has a test on the new version.
- [ ] AC5: The `score_pid5()` help page and `vignettes/pid5_scoring.Rmd` name the form and cite Bach et al. (2020). They state the scoring metric and missing-data rule. They also say that no BF+M item is reverse-keyed. NEWS.md has an entry. `devtools::check()` reports 0 errors and 0 warnings, and `pkgdown::check_pkgdown()` passes.

## Coverage

- AC1 → T1, T2
- AC2 → T3, T4
- AC3 → T4
- AC4 → T3, T4
- AC5 → T5

## Tasks

- [x] T1: Write the references page for the key sheet: extracted table, scoring rule, provenance. If Jeff uploads Bach et al. (2020), check the key against it and record the result. Add the SOURCES.md row.
- [x] T2: Add the BF+M column and the `pid_scales` entry in `data-raw/pid_info.R`, and regenerate the data. Build both from a key CSV with its own facet and domain labels, in the key sheet's domain-grouped order. Add `pid_bfpm_domains` (D-088(c)) and its docs. Write the keying test (AC1). Keying content needs Jeff's sign-off before merge.
- [ ] T3: Pre-implementation gate (settled by RR06 and D-088), then code. The gate's questions (RB tripwire: irreversible-api):
  - The version string, and the item-name stem for `rename_pid5_items()` and the M158 exports.
  - The output column names, including `anankastia`.
  - Where the facet-to-domain map lives, since `pid_domains` holds only the 5 FULL and SF domains.
  - The scoring metric. The key sheet sums each facet's 2 items and averages the 3 facet sums for a domain. The package reports item means elsewhere.
  - The missing-data rule. The key sheet states no proration rule.

  Then thread the version through `score_pid5()`, `reliability_pid5()`, `rename_pid5_items()` and `label_pid5()`. If `score_engine()` cannot express the chosen metric, teach it the metric. `reliability_pid5()` adds the 6 domain rows from `pid_bfpm_domains`. `label_pid5(target = "scales")` reads that table for BFPM. The reliability engine reports omega as `NA` for a scale with fewer than 3 items (D-088(f)), with a test that BFPM reliability emits no warning.
- [ ] T4: Write the hand-computed fixture tests (AC2, AC4). Write the characterization script (AC3) and run it at the merge base before the version code lands.
- [ ] T5: Update the help page, the vignette section and NEWS. Run `devtools::document()`, `devtools::test()`, `devtools::check()` and `pkgdown::check_pkgdown()`.

## Work log

- 2026-10-03: created by /milestone-plan, together with M158–M161. Sources come from a web survey (shelf files dated 2026-10-03). Johannes Zimmermann's original request is not on file yet.
- 2026-10-03: plan chose a new `version` value on `score_pid5()` over a separate `score_pid5bfpm()`. CLAUDE.md sets one function per task with a version argument. Falsified by a BF+M rule that the shared engine cannot express without per-version branches outside `score_pid5()`.
- 2026-10-03: plan chose the FU Berlin key sheet as the keying source over waiting for Bach et al. (2020). The sheet maps every item to a PID-5 number and states the rule, and it is on the shelf. Falsified by a disagreement between the sheet and the paper.
- 2026-10-03: criteria audit (full mode, fresh Opus reader) returned 3 clear fixes and 2 judgment findings, all applied. AC1 now binds the facet-to-domain map. AC2 covers each kept `missing` mode. AC3 names a `data-raw/` characterization script, because `test-score_pid5.R` has no argument matrix. The T3 gate adds the item-name stem, and `label_pid5()` joins the scope.
- 2026-10-03: implement started on branch `m157-pid5bfpm-scoring`. The untracked `devel/hitopdat_*` files predate the branch and stay out of every commit.
- 2026-10-03: T1 done. Pages `references/fuberlin2020pid5bfpm.md` and `references/bach2020.md`, and a `BFPM` row in SOURCES.md. Bach et al. (2020) was already on the shelf as `bach2020a.pdf`. Its p. 181 confirms the 6 anankastia PID-5 items (123, 176, 140, 220, 34, 115) and the domain rule. Its Appendix A, the full key, is not on the shelf. All 36 key-sheet PID-5 numbers match the German item at their BF+M position in pair order.
- 2026-10-03: minor amendment: T2 now runs after the T3 gate, because the `pid_items` column name is the version string that the gate settles.
- 2026-10-03: T3 gate posed with the agent's five recommendations. Jeff chose escalation. Blocked on RB06 (`cairn/reviews/archive/RB06-pid5bfpm-api.md`). The brief commit lands on the milestone branch, not main, because a status edit on main conflicts with the branch's ROADMAP row.
- 2026-10-03: RR06 ingested (Fable subagent). It accepts the five gate recommendations and adds a domain-grouped row order, omega `NA` under 3 items, and no total score. Promoted as D-088. Triage is in Decisions. Minor amendment: T2 and T3 name the new work. Status back to in-progress.
- 2026-10-03: T2 done. `data-raw/pid_bfpm_key.csv` feeds `pid_items$BFPM` (after `BF`), `pid_scales$BFPM` and the new `pid_bfpm_domains`. The regenerated FULL, SF and BF elements and `pid_domains` are `identical()` to the old files, and every old `pid_items` column is unchanged. AC1's keying test failed on two planted key errors (a swapped pair, a facet moved to another domain) and passes on the real key.
- 2026-10-03: T2 choice: `tibble::add_column()` keeps readr's class and `spec` on `pid_items`. The spec stays the record of `pid_items.csv` and does not list BFPM.
- 2026-10-03: T2 found that `test-plot_pid5.R` built its cases from `names(pid_scales)`, which RR06 missed. The cases now come from `plot_pid5()`'s own `version` choices.

## Decisions

- 2026-10-03 (RR06, triage): recommendations 1 to 6 and 8 applied as D-088 (a) to (e) and (g). Recommendation 7 applied as D-088(f), with the omega guard in the reliability engine rather than in `calc_omega()`. `calc_omega()` is exported, and a guard there changes its result for a direct 2-item call. No shipped scale has fewer than 3 items, so the engine guard changes no existing output. Recommendation 9 (the rejected alternatives) is recorded in D-088. Recommendation 10: the `calc_se` note on 1-item facets goes into the help text in T5, and the scoring-table note goes to M158's work log.

## Review
