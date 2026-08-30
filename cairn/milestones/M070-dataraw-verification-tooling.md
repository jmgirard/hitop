<!-- Section ownership + write-modes: see tracking-rules.md "Milestone-file
     section ownership". A phase skill never rewrites another phase's section.
     Per-section owners are tagged below. The one size check that can fail is
     cairn_validate's <150 over the plan-owned body. -->
# M070: `data-raw/` verification tooling is reduced to what still re-runs, with its live guard defects repaired

- **Status:** in-progress
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** IP2
- **Branch/PR:** `m070-dataraw-verification-tooling`

## Goal

`data-raw/` keeps only verification scripts a maintainer can still re-run, with the two defects repaired in the guards those scripts share.

## Scope

Internal tier: every script here is `.Rbuildignore`d maintainer tooling, so no external consumer of the package relies on it.

**In:** deleting the three spent one-shot verifiers, each written as evidence for a milestone that shipped and each diffing against a merge base that has since moved — `verify_hitopsr_rename.R` (already recorded as reporting any later branch's own changes as unexpected), `verify_m060_characterization.R` and `verify_m061_characterization.R`. Repairing the stray-text guard in the shared Table 1 extractor, whose `grepl()` arguments are reversed at `data-raw/hitopsr_table1.R:289` so a single letter of lost text passes as watermark. Repairing `data-raw/mutate_norms_book_check.R`, which reports a crashed run and a genuine miss identically. Re-running the three durable verifiers the guard repair touches.

**Out:** the six latent hardening items M059's review filed against the shared extractor and `verify_hitopsr_names.R` — watermark stripping adjacent to digits, the `\bScales\b` section test, the Superspectra-last partition, the empty prose oracle, the glued-fragment control, `rendering_pattern()`'s rewrite order — none a wrong answer on any current input → the narrowed candidate row T5 writes. The four M059 items on the *test suite* rather than these scripts → the same row: AC6's Word sweep floors at two files, so under `R CMD check` it inspects the installed pair and not the staged copies; the AC2 self-check plants a flipped `Reverse` and asserts the difference directly instead of exercising the comparison loop it validates, so it would pass with that loop deleted; AC5's added-row search reduces `==` with `&`, which would index phantom `NA` rows if any manifest cell were ever `NA`; and AC1's probe injects NAs only inside the five scale columns, so it cannot separate the two `missing` settings. `verify_hitopbr_devstats.R`'s aggregate Range check, compensated by its own comparison 2 → the same row. Committing the norms CSVs or the book extraction as CI fixtures, which needs its own decision about a ~42 KB second copy of `pid_norms` → the same row. `characterize_calc_se.R`'s inert condition comparison → the M068/M069 test-reach row, where it already sits.

## Acceptance criteria

- [ ] AC1 `data-raw/verify_hitopsr_rename.R`, `data-raw/verify_m060_characterization.R` and `data-raw/verify_m061_characterization.R` are absent from the working tree, and `git grep -n -F` over the tracked tree for each of the three basenames returns hits only under `cairn/milestones/archive/`.
- [ ] AC2 The stray-text guard in `data-raw/hitopsr_table1.R` classifies tokens through a named predicate whose accepted set is the watermark fragment vocabulary the script's own stripping step names; evaluated over the token vector `c("Fo", "rP", "ee", "rR", "ev", "iew", "ForPeer", "eview", "e", "o", "r", "w", "Anhedonia")` that predicate returns `TRUE` for the first eight and `FALSE` for the last five.
- [ ] AC3 With that repair in place, `data-raw/verify_hitopsr_names.R`, `data-raw/verify_hitopsr_devstats.R` and `data-raw/verify_hitopbr_devstats.R` each exit 0 against the shelf PDF and report no stray text.
- [ ] AC4 For every mutation in `data-raw/norms_mutations.R`, `data-raw/mutate_norms_book_check.R` reports exactly one of three verdicts — caught, not caught, or errored — and a sweep in which any run errors exits non-zero naming that mutation as errored rather than as a miss.
- [ ] AC5 `Rscript -e 'devtools::test()'` clean and `Rscript -e 'devtools::check()'` 0 errors / 0 warnings, NOTEs justified.

## Coverage

- AC1 → T1
- AC2 → T2
- AC3 → T3
- AC4 → T4
- AC5 → T6

## Tasks

- [x] T1 Delete the three spent one-shot verifiers; sweep the tracked tree with `git grep -n -F` for each basename and confirm the only survivors are the archive summaries, which are history and stay untouched.
- [ ] T2 Replace the reversed `grepl()` at `data-raw/hitopsr_table1.R:289` with a named predicate whose accepted set is the fragment vocabulary the stripping step already lists; drive it over AC2's token vector and record both verdict rows in the work log.
- [ ] T3 Run the three durable Table 1 verifiers against the shelf PDF; record exit status, stray-text report and the label and cell counts each prints.
- [ ] T4 Give `data-raw/mutate_norms_book_check.R` a three-way verdict separating a non-zero exit carrying no comparison counts from one that carries them; prove it able to fail by planting a mutation that aborts the book extraction, confirming the sweep exits non-zero naming it errored, then removing the plant. Run the sweep against a `git archive` export of HEAD, not the checkout (the M034/M035 lesson: the harness swaps `data/pid_norms.rda` in place).
- [ ] T5 The narrowed candidate row landed with this plan and points at this file's Out scope for the twelve items' detail. Before archiving, move that detail back into the row (the archive summary is too short to hold it) and confirm the row names no script this milestone deleted.
- [ ] T6 Profile verify slot: `devtools::test()`, then `devtools::check()` at the gate.

## Work log

- 2026-08-30: created by /milestone-plan.
- 2026-08-30: plan gate chose deleting the three spent one-shot verifiers and repairing only the two live guard defects over also hardening the six latent extractor items, because each latent is recorded as producing no wrong answer on any current input and hardening widens what the checker promises; falsified by a source revision or a table regeneration in which one of those six admits or drops a real cell.
- 2026-08-30: plan gate chose repairing the stray-text guard against the stripping step's own fragment vocabulary over swapping `grepl()`'s arguments literally as M041's finding describes, because the literal swap makes the guard demand the whole watermark phrase and would reject the genuine fragments the page produces; falsified by AC3's runs stopping on stray text the loose guard accepted. A length floor was also weighed and rejected as a number with no source.
- 2026-08-30: T1 — the three spent one-shot verifiers are deleted; `git grep -n -F` over the tracked tree for each basename hits only `cairn/milestones/archive/M058-nssi-scale-name.md` and `cairn/milestones/archive/M060-append-collision-empty-selection.md` (M061's summary names none) plus this milestone's own Scope and AC1 lines, which name what was deleted.
- 2026-08-30: plan gate chose leaving the twelve unfixed items on one narrowed candidate row over planning a second milestone for the six extractor items now; falsified by two or more of them being reached by one source revision, which would make them a bounded scope rather than a watch list.

## Decisions

<!-- owner: implement / review · append-only; milestone-local -->

## Review

<!-- owner: review · exclusive -->
