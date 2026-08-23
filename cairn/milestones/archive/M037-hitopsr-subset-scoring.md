# M037: Score HiTOP-SR subset-collected data

**Status:** done (2026-08-01, PR #40 https://github.com/jmgirard/hitop/pull/40)

**Goal:** Score, and estimate reliability for, data collected with a
`hitop_subset()`-generated HiTOP-SR short form from its item columns alone.

**Outcome:** `score_hitopsr()` and `reliability_hitopsr()` take a final
`subset = NULL` argument accepting the `hitop_subset` descriptor the
`generate_*_hitopsr()` family already consumes. In `R/subset.R`,
`subset_engine_inputs()` remaps it into the engines' `n_items`/`reverse_items`/
`items_scales` as positions within the supplied columns
(`match(itemNumbers, subset$items)`), reads the reverse key from `hitopsr_items`
not the descriptor, and aborts when `nItems` and `items` disagree. Subset
scores, `_se`, and alpha equal the full run; `devel/regression_probe_m37.R`
compares 43 cells of values and conditions against a `git archive` base export.

**Decisions:** none promoted. Four gate choices sit in the work log: descriptor
as the only subset input; reliability carried in-milestone; positions rather
than `NA`-padding; the instrument check confined to the scoring path.

**Review:** two of three lenses zero findings; the diff-bug lens returned 14,
scored by a fourth agent. F2 (82) actioned; merge withheld once until the
maintainer's required F11 (78) and F4 (75) shipped as T8/T9, all criteria
re-verified. F3 and F9 refuted. No lessons retired.
