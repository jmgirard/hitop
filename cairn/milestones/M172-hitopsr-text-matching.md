# M172: HiTOP-SR item-text matching and duplicate refusal

- **Status:** planned
- **Priority:** normal
- **Depends on:** —
- **Driving RR:** —
- **Principles touched:** GP3
- **Resolves:** —
- **Surface tier:** user-facing — changes what an exported function accepts and refuses
- **Branch/PR:** —

## Goal

`rename_hitopsr_items(method = "text")` matches item text by the rules `rename_pid5_items()` uses, and it refuses two columns that match one item.

## Scope

**In:**
- The text method matches through `normalize_item_text()` (R/util.R:187), in place of `trimws()` alone.
- Two columns that match one item stop the call with the class `hitop_duplicate_item_match`, as in `rename_pid5_items()` (D-094(c)).
- The duplicate refusal moves into one internal helper that both rename functions call.
- Help text, two NEWS entries, and a decision entry that extends D-094(c) to the HiTOP-SR.

**Out:**
- Ignoring letter case. The plan gate kept the PID-5 rules (2026-10-07). A change would cover both functions in a later milestone.
- Classes for the numeric refusals in `norm_pid5()`, `plot_pid5()` and `interval_*()`. They stay in part (a) of the "Helpers behind newer conventions" candidate row.
- A text-rename function for the HiTOP-BR. None exists, and nobody asked for one.
- `method = "original"`. Its legacy names are unique, so it cannot match one item twice.

## Acceptance criteria

- [ ] AC1: `rename_hitopsr_items(method = "text")` compares `item_text` with `hitopsr_items$Text` after both pass through `normalize_item_text()`. A test in `tests/testthat/test-rename_hitopsr_items.R` renames one text per difference to the expected `hsr_` name. The differences are `‘` and `’` in place of an apostrophe, a leading `...`, a leading `…` and a trailing `…`. They also include no final period, and a no-break space at the start and at the end. Each per-difference expectation fails on the base commit.
- [ ] AC2: A test gives all 405 texts in `hitopsr_items$Text` as they are. It asserts that 405 columns become `hsr_001` to `hsr_405` in item order, with no warning.
- [ ] AC3: Under `method = "text"`, two or more columns that match one item stop the call with an error of class `hitop_duplicate_item_match`. A test has two duplicated items. One is a repeated text. The other is two texts that differ only in what `normalize_item_text()` removes. A third case puts an unmatched `item_text` entry between two duplicate columns. Each case asserts the class, and that the message names each duplicated item number with its own columns. Each case fails on the base commit.
- [ ] AC4: `tests/testthat/test-rename_pid5_items.R`, `test-pid5-version.R` and `test-score_pid5ffbf.R` pass with no edit to their expectations. For one FFBF duplicate call, the error message is identical to the base commit's message.
- [ ] AC5: `?rename_hitopsr_items` states the matching rules of AC1 and the error of AC3. NEWS.md has one entry under "Breaking changes" for the AC3 refusal and one under "Improvements and fixes" for the AC1 matching.
- [ ] AC6: `devtools::test()` reports 0 failures, and `devtools::check()` reports 0 errors and 0 warnings.

## Coverage

- AC1 → T1, T2
- AC2 → T1
- AC3 → T1, T2
- AC4 → T2
- AC5 → T3
- AC6 → T4

## Tasks

- [ ] T1: Write the AC1, AC2 and AC3 tests in `tests/testthat/test-rename_hitopsr_items.R` first. Type the expected names as literals, not from `hitopsr_items`. Run the AC1 and AC3 expectations on the base, and log which fail.
- [ ] T2: Move the duplicate refusal out of `rename_pid5_items()` (R/rename_pid5_items.R, the `dup_n` block) into an internal helper in R/util.R that takes the form label. Call it from both rename functions. Switch the `rename_hitopsr_items()` text match to `normalize_item_text()`. Capture one FFBF duplicate message on the base before the move, and compare it after.
- [ ] T3: Update the roxygen for `method` and `@return`, then run `devtools::document()`. Write the two NEWS entries. Append the decision entry that extends D-094(c) to the HiTOP-SR. Narrow the candidate row "Helpers behind newer conventions" to part (a).
- [ ] T4: Run `devtools::test()` and `devtools::check()`.

## Work log

- 2026-10-07: created by /milestone-plan. Promotes part (b) of the candidate row "Helpers behind newer conventions" (lineage: M109, M170 review). The collision sweep found no other overlap.
- 2026-10-07: question set: what to plan? Answer: HiTOP-SR text matching, over starting M171 or the open-data idea of issue #87.
- 2026-10-07: question set: should issue #87 get a roadmap row? Answer: yes, as a candidate row in this plan commit. Nothing posted on GitHub.
- 2026-10-07: plan gate chose an error now (Jeff, pre-1.0 deprecation waiver) over a warning first and an error later, because `rename_pid5_items()` already errors and the two functions then agree. Falsified by user reports of scripts that relied on two columns sharing one name.
- 2026-10-07: plan gate chose the PID-5 matching rules (Jeff) over also ignoring letter case, so one helper serves both functions. Falsified by user reports of HiTOP-SR or PID-5 texts that differ only in letter case.
- 2026-10-07: plan chose one shared duplicate helper over a copy in each function, so the two messages cannot drift. Falsified by a need for the HiTOP-SR message to differ from the PID-5 message beyond its label.
- 2026-10-07: criteria audit (fresh Opus reader, full mode) returned 6 findings, all applied. AC1 drops curly double quotes, because no HiTOP-SR item holds a double quote (16 hold apostrophes). AC1 now requires each case to fail on the base, and it adds `‘` and a trailing `…`. AC3 adds an unmatched text between two duplicates and names items by item number. AC4 narrows "keeps its behavior" to the three named test files plus one identical FFBF message.
