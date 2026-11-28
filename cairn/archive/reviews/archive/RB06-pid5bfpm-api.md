# RB06: PID5BF+M version string, output names, domain map, scoring metric and missing-data rule (M157)

- **Date:** 2026-10-03
- **Output required:** write findings to `cairn/reviews/RR06-pid5bfpm-api.md`
- **Binding criteria:** not requested

You are performing an independent expert review. This brief is fully
self-contained. Do not assume any conversation context. Read only what this
brief directs you to read, answer the numbered questions, and write your
findings to the output path above using the same numbering.

## Background

`hitop` is an R package (version 0.2.0, pre-1.0, GPL-3) that scores, screens and
distributes questionnaires of the HiTOP Society. Its PID-5 support covers three
forms of the Personality Inventory for DSM-5. Each exported function takes a
`version` argument: `"FULL"` (220 items, 25 facets plus 5 domains), `"SF"` (100
items, 25 four-item facets plus 5 domains), and `"BF"` (25 items, 5 five-item
domains plus a total). Scores are item means on the 0 to 3 response scale.

Milestone M157 adds a fourth form, the 36-item **PID5BF+M** (Bach et al., 2020).
It has 18 two-item facets and 6 domains of 3 facets each: the 5 DSM-5 domains
plus Anankastia. Every BF+M item is a PID-5 item, so the keying is a map from the
36 BF+M positions onto PID-5 item numbers. The keying source is a German key
sheet from FU Berlin, with the paper as a partial cross-check. Both are
summarized in the two reference pages listed under Materials. That keying is
settled and is not under review.

The open questions fix public API names and the scoring scale. Once released,
these are hard to change: users write the names into their code, and the scores
enter their analyses. The milestone plan therefore marks this gate as an
irreversible API decision that needs an independent review.

The implementing session's recommendation, which you may accept, amend or
reject:

- R1. `version = "BFPM"`. The new `pid_items` column and `pid_scales` element
  are also named `BFPM`. Item columns that `rename_pid5_items()` and
  `label_pid5()` build, and the later Qualtrics and REDCap forms write, are
  `pid5bfpm_01` to `pid5bfpm_36`.
- R2. Output columns are `prefix` plus a camelCase scale name. The 15 facets the
  form shares with the PID-5 keep their PID-5 names, for example
  `pid_emotionalLability` and `pid_unusualBeliefsExperiences`. The three
  Anankastia facets are `pid_perfectionism`, `pid_rigidity` and
  `pid_orderliness`. The domains are `pid_negativeAffectivity`,
  `pid_detachment`, `pid_antagonism`, `pid_disinhibition`, `pid_anankastia` and
  `pid_psychoticism`. Facets come first, then domains, as for FULL and SF.
- R3. `pid_scales$BFPM` holds the 18 facet rows in the same 5-column shape as
  `pid_scales$FULL`. A new exported 6-row tibble, `pid_bfpm_domains`, holds the
  facet-to-domain map with the same 4 columns as `pid_domains` (`Domain`,
  `camelCase`, `primaryFacets`, `facetStems`). `pid_domains` stays as it is,
  because the FULL and SF paths read all of its rows.
- R4. Scores are item means on 0 to 3. A facet is the mean of its 2 items. A
  domain is the mean of its 3 facet scores, through the same `domain_map` path
  that FULL and SF use. The key sheet sums the 2 items of each facet and
  averages the 3 facet sums, which gives exactly twice these values on complete
  data. The help page would state the factor of 2.
- R5. All three existing `missing` modes stay, with their current meaning, and
  `"apa"` stays the default. Under `"apa"` the 25% rule blanks a 2-item facet
  when one item is missing (50% is over 25%). A domain is `NA` when any of its
  facets is `NA`. So `"apa"` gives the same output as `"complete"` for this form.
  Under `"available"` a facet is its one answered item, and a domain is the mean
  of its facets that are not `NA`.

## Materials

Read these files. Line numbers are as of commit `defa25bd` on branch
`m157-pid5bfpm-scoring`.

- `cairn/references/fuberlin2020pid5bfpm.md`: the key sheet's table and scoring
  rule. Note that the sheet sums within facets and states no missing-data rule.
- `cairn/references/bach2020.md`: the paper's domain rule ("average scores for
  each domain's 3 primary facets", p. 181) and its facet names.
- `R/score_pid5.R` (all, 201 lines): the exported function, its `missing`
  documentation and the per-version setup at lines 158-200.
- `R/score_engine.R` (all, 172 lines): the shared engine and its `domain_map`
  path at lines 115-127 and its domain standard errors at lines 146-154.
- `R/util.R` lines 718-728: `apa_mean()`, the 25% rule with proration.
- `R/reliability_pid5.R`, `R/rename_pid5_items.R`, `R/label_pid5.R`: the other
  three functions that take the new version.
- `R/data.R` lines 1-61: the documented shapes of `pid_items`, `pid_scales` and
  `pid_domains`.
- `R/plot_pid5.R` lines 220-275 and `R/norm_engine.R` lines 120-135: other
  readers of `pid_scales` and `pid_domains`. They do not take the new version in
  M157, but they show how the tables are consumed.
- `data-raw/pid_info.R` (all, 146 lines): how the tables are built.
- `cairn/DESIGN.md` lines 87-110: the design principles IP1 to IP4 and GP1 to
  GP4.
- `cairn/DECISIONS.md`: read only these entries, found by their `### D-0NN`
  headings: D-052 and D-055 (item-name stems), D-017, D-019 and D-021 (the BF
  total, a past case of adding a scale to `pid_scales`), D-034 (public
  contracts), D-046 and D-053 (the reliability output's `Scale` and `camelCase`
  columns).
- `cairn/milestones/M157-pid5bfpm-scoring.md`: the milestone's scope and
  acceptance criteria. AC2 needs hand-computed expected values under the
  metric and missing-data rule this review settles.
- `cairn/milestones/M158-pid5bfpm-forms.md` lines 14-30 and
  `cairn/milestones/M161-pid5-child-forms.md`: later milestones that build on
  these names. M158's printed scoring table states the metric chosen here.

You may run R. For example, `Rscript -e 'devtools::load_all(); str(pid_scales)'`
shows the current tables.

## Questions

1. Version string and item stem (R1). Is `"BFPM"` the right `version` value and
   `pid_items` column name? Consider `"BF+M"` (the printed form name, but a
   non-syntactic column name), `"BFM"`, and accepting more than one spelling.
   Consider the exact-match behavior of `match.arg()` with `"BF"` already a
   choice, and how the name reads beside FULL, SF and BF. Is `pid5bfpm_` the
   right item stem under D-055's one-stem-per-form rule?
2. Output column names (R2). Should the 15 shared facets reuse the PID-5 column
   names, even though each holds only 2 of the PID-5 facet's items? Consider a
   user who scores FULL and BFPM data into one table, and the SF precedent,
   where 4-item facets already share FULL's names. Should the Psychoticism facet
   keep the PID-5 name `unusualBeliefsExperiences`, or take the paper's
   "Unusual beliefs"? Are `perfectionism`, `rigidity`, `orderliness` and
   `anankastia` the right stems?
3. Where the facet-to-domain map lives (R3). Compare R3's new `pid_bfpm_domains`
   table with at least these choices: (a) extra columns on `pid_scales$BFPM`,
   (b) domain rows inside `pid_scales$BFPM`, as the BF element carries its
   `Total` row, (c) an internal object in `R/sysdata.rda`, and (d) widening
   `pid_domains` with a version column. Judge each against the existing readers
   of these tables, `reliability_pid5()`'s need for domain rows (AC4), and the
   M158 printed scoring table.
4. Scoring metric (R4). GP1 says a published algorithm is the default. The key
   sheet prints sums within facets, and the paper says domains use "average
   scores" of facets. Does R4's item-mean scale honor GP1, or does GP1 require
   the sheet's sums, or an option for them? Weigh consistency with the package's
   other PID-5 outputs, the paper's wording, and how later BF+M norms would be
   applied. Say whether a domain should be the mean of its 3 facet means or the
   mean of its 6 items. The two agree on complete data and differ under
   `missing = "available"`.
5. Missing-data rule (R5). The key sheet gives no rule. GP1 says that where no
   published rule exists, the default is chosen on its merits, and the HiTOP-SR
   and HiTOP-BR chose `"available"` that way. Should the BF+M keep `"apa"` as
   its default, where one missing item blanks its facet and domain? Or should it
   take a different default, which would make the default of `missing` depend on
   `version`? Should `"apa"` be refused or warned about for this form? Under
   `"available"`, is a domain from 1 or 2 of its 3 facets acceptable, or should
   it need all 3?
6. Anything the five recommendations miss that would be costly to change after
   release. Examples: the order of output columns, the `Scale` names that
   `reliability_pid5()` returns for the new facets and domains, and whether a
   BF+M total score should exist.

## Constraints

- The keying (which PID-5 item sits at each BF+M position, the facet pairs, the
  domain membership, no reverse keying) is settled from the key sheet under IP1.
  Do not relitigate it. Report any doubt about it under "Beyond the brief".
- The plan chose a new `version` value on `score_pid5()` and its siblings over a
  separate `score_pid5bfpm()` function. CLAUDE.md sets one function per task
  with a `version` argument. Flag disagreement explicitly rather than working
  around it.
- No new package dependencies.
- The existing FULL, SF and BF outputs must not change (GP2). Any design that
  changes them is out of scope.
- `validity_pid5()`, `norm_pid5()` and `plot_pid5()` do not take the new version
  in M157.
- D-055 (one item-name stem per form) and D-034 (condition classes and
  documented shapes are public contracts) stand.

## Output format

In `RR06-pid5bfpm-api.md`: answer each question by number with your reasoning
and evidence. List any additional findings separately under "Beyond the brief".
End with concrete recommendations, each marked apply, consider or
reject-with-reason. Your report is advisory: emit a `## Binding criteria`
section ONLY if this brief's header slot says `requested`.
