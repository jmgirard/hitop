# RR06: PID5BF+M version string, output names, domain map, scoring metric and missing-data rule (M157)

- **Date:** 2026-10-03
- **Brief:** `cairn/reviews/RB06-pid5bfpm-api.md`
- **Reviewed at:** commit `defa25bd` on `m157-pid5bfpm-scoring` (the brief's line numbers)
- **Status:** advisory (binding criteria not requested)

Method. I read every file the brief lists. I extracted the eight D-entries by
heading. I ran R against the loaded package to check five things. They are the
exact-match precedence of `match.arg()`, the `snakecase` stems, and the PID-5
facet and reverse status of the 36 keyed items. They are also `apa_mean()` on
a 2-item scale and `calc_omega()` on a 2-item scale. Where a finding rests on one of
those runs, I say "verified".

Summary. R1, R2, R4 and R5 hold. R3 holds with one addition: the domain rows
that `reliability_pid5()` needs are built from the map at call time, not
stored. The recommendations miss two things that are costly to change after
release. The first is the row order of `pid_scales$BFPM`. It fixes the output
column order, the reliability row order and the M158 table at once. The second
is omega from `reliability_pid5()` on 2-item facets. That model is not
identified, and the current code returns an arbitrary number plus a lavaan
warning per facet.

## 1. Version string and item stem (R1): accept `"BFPM"` and `pid5bfpm_`

`"BFPM"` is the right value. The facts that decide it:

- `match.arg()` gives an exact match precedence over a partial one. Verified:
  `match.arg("BF", c("FULL","SF","BF","BFPM"))` returns `"BF"`, and `"BFP"`
  returns `"BFPM"`. So adding `"BFPM"` beside `"BF"` changes nothing for a
  caller who writes `"BF"`. `"B"` alone is already an error today, so no
  caller depends on it. The same holds for `"BFM"`. But `"BFM"` loses the
  "+" and reads as "BF, modified". A reader can give that reading to a future
  PID5BF+ too (34 items, no modification). `"BFPM"` keeps the "P" for "+". It
  leaves `"BFP"` free for the 34-item form, which is a candidate row.
- `"BF+M"` as a `version` value is defensible on its own. As the `pid_items`
  column name it is non-syntactic. Every reader then writes
  `pid_items[["BF+M"]]` or backticks. `data-raw/pid_info.R` and
  `rename_pid5_items()` index `pid_items[[version]]` by the version string.
  One string for version and column is what lets four functions share one
  lookup with no `switch` (`R/rename_pid5_items.R:80`, `R/label_pid5.R:80`,
  `R/score_pid5.R:174`). Reject `"BF+M"`.
- More than one spelling (`"BF+M"` normalized to `"BFPM"`) is a second promise
  to document on four help pages and in M158's two exporters. D-052 rejected
  lenient matching for the same reason ("one pattern is one promise"). The
  `toupper()` already there covers case. Reject.
- Beside `FULL`, `SF` and `BF`, the string `BFPM` reads as the fourth form's
  suffix, as `SF` and `BF` are the suffixes of "PID-5-SF" and "PID-5-BF". The
  help page must say once that `"BFPM"` is the PID5BF+M. Then a reader who
  knows only the printed name finds it.

Item stem. D-055's pattern is `pid5` + lowercase version + `_`, padded to the
width of the form's largest number: `pid5_001`, `pid5sf_001`, `pid5bf_01`. The
rule applied to `BFPM` with 36 items gives `pid5bfpm_01` to `pid5bfpm_36`.
`item_names(prefix, n, max_n = 36)` already produces that for free. The
Qualtrics uppercase `PID5BFPM_01` follows M158 AC2's existing convention.
Accept. The stem is the only place a user sees "bfpm" in lowercase. The
`rename_pid5_items()` and `label_pid5()` help text lists three stems
(`R/rename_pid5_items.R:7-8`, `R/label_pid5.R:18-19`). It must list four.

## 2. Output column names (R2): accept, with the printed names fixed too

Shared facets reuse the PID-5 stems. The SF precedent is exact:
`pid_anxiousness` already means 9 items under FULL and 4 under SF. `pid_norms`
is keyed by `version`, so a score is never interpreted without its form. A
user who scores FULL and BFPM into one table hits the `hitop_append_collision`
error. That user hits the same error today with FULL and SF, and the same
answer applies: one `prefix` per form. New stems for the 15 shared facets
(`pid_bfpmAnxiousness` or similar) buy nothing that the `prefix` argument does
not. They also split one concept across two spellings. D-041 and D-046 spent
two milestones removing that. Accept.

Verified in R: each of the 30 shared items sits in the PID-5 facet that the
key sheet names it under. Items 62 and 122 are both Emotional Lability, and so
on through 44 and 77 under Perceptual Dysregulation. None of the 36 is
reverse-keyed in `pid_items$Reverse`. So the shared names are not only
convenient, they are true. Each BF+M facet is a 2-item subset of the PID-5
facet of the same name.

Psychoticism facet. Keep `unusualBeliefsExperiences` and the printed name
"Unusual Beliefs & Experiences". The two items (194, 209) are PID-5 Unusual
Beliefs & Experiences items, so the shared-name rule applies. The paper's
Table 2 shortens the label to "Unusual beliefs" as it shortens "Negative
Affectivity" to sentence case. That is a typesetting choice, not a renaming.
The invariant that D-041 and D-046 fixed matters more: one printed name drives
the stem, the scored column and the `Scale` that the reliability family
returns. If the stem stays `unusualBeliefsExperiences`, the printed `Facet`
must be "Unusual Beliefs & Experiences". Otherwise `pid_scales$BFPM$Facet` and
`camelCase` disagree on one row. The open question in `bach2020.md` closes
with that reason.

Anankastia stems. `perfectionism`, `rigidity`, `orderliness` and `anankastia`
are the `snakecase` lower-camel of the paper's Table 2 names (verified). That
is how every other stem in `pid_info.R` is built. The help page must state two
facts so nobody is surprised. First, all six anankastia items are PID-5 Rigid
Perfectionism items. Verified: 123, 176, 140, 220, 34 and 115 all carry
`Facet == "Rigid Perfectionism"` and `Domain == NA` in `pid_items`. Second,
`pid_perfectionism` and `pid_rigidity` are not sub-scores of
`pid_rigidPerfectionism` in any package sense, and no FULL or SF domain
contains them. Nothing collides: `rigidPerfectionism` keeps its stem on FULL
and SF and never appears on BFPM.

Domain printed names. Use the five `pid_domains$Domain` spellings as they are
("Negative affectivity", sentence case) plus "Anankastia". Then the shared
domains carry one printed name across `pid_domains`, `pid_scales$BF$Domain`
and the new table.

## 3. Where the facet-to-domain map lives (R3): accept the exported table; build domain rows at call time

R3 is the right shape. Judged against the readers:

- (a) extra columns on `pid_scales$BFPM` (a `Domain` per facet row). One
  table, no new export, and `split(camelCase, Domain)` gives the map. But it
  puts the BF+M map in a different place from the FULL/SF map. Every reader
  that handles both forms then branches on where to look rather than on which
  table. Those readers are `label_pid5(target = "scales")`, M158's table, and a
  later `plot_pid5()` that takes the version. It also widens one element of a
  list whose documented shape is "5 columns" (`R/data.R:28-29`), a D-034
  contract. Runner-up. Reject.
- (b) domain rows inside `pid_scales$BFPM`, as BF carries `Total`. This is
  the one to be careful about. It looks like it gives `reliability_pid5()`
  (AC4) and M158's table their domain rows for free. The cost is in scoring.
  `score_pid5()` scores every `items_scales` row from items. A 6-item domain
  row is then an item mean over 6 items, not the mean of 3 facet means. Under
  `"apa"` it survives 1 missing item (1/6 is under 25%, prorated) while its
  facet is `NA`. That is the D-021 situation again, but this time against the
  paper's stated rule (domain = average of the 3 facet scores). To avoid it,
  `score_pid5()` must filter domain rows out of `items_scales` and feed them
  to `domain_map` instead. That is a per-version special case, which the plan
  rejected for `score_pid5bfpm()`. Reject.
- (c) an internal object in `R/sysdata.rda`. Works for every reader. But a
  user who prints `pid_domains` to see how FULL domains are built cannot do
  the same for the BF+M. AC1's keying test then asserts on an unexported
  object. GP3 and IP2 both prefer the map in daylight. Reject.
- (d) widen `pid_domains` with a `version` column. Every existing reader
  takes all rows: `R/score_pid5.R:178`, `R/label_pid5.R:121-122` and
  `R/plot_pid5.R:511,525,544-548`. `tests/testthat/test-keying.R:242` pins
  `nrow(pid_domains)` to 5. All of them then need a filter. That is exactly
  the FULL/SF path the constraints say not to touch. Reject.

So: `pid_bfpm_domains`, 6 rows, the four `pid_domains` columns, exported and
documented beside `pid_domains`. Two additions:

1. `reliability_pid5(version = "BFPM")` builds its 6 domain entries at call
   time. It concatenates the item lists of each domain's three facets, read
   from `pid_scales$BFPM$itemNumbers` through `pid_bfpm_domains$facetStems`.
   That gives 24 `items_scales` entries: 18 facet rows, then 6 domain rows.
   `Scale` comes from `$Facet` then `$Domain`. `camelCase` comes from the two
   tables' stems. `nItems` is 2 then 6. The engine needs no change. This is
   the first version for which `reliability_pid5()` returns domain rows on a
   faceted form. FULL and SF return facets only
   (`R/reliability_pid5.R:7-8`). AC4 asks for it and it is sound. The help
   page's "facet level for FULL/SF" sentence must state the exception.
2. `label_pid5(target = "scales")` at `R/label_pid5.R:120-123` appends
   `pid_domains` for every version except BF. For BFPM it must read
   `pid_bfpm_domains`. Otherwise `pid_anankastia` goes unlabelled and the
   five other domains are labelled by accident.

M158's printed table. `make_scoring_table()` (`R/generate_docx.R:501`) sorts
rows alphabetically by `Scale` and prints one item list per row. It cannot
show domain grouping. It cannot state a facet-mean-then-domain-mean rule by
itself. M158 has to pass it 18 facet rows and add a second table or a sentence
for the domains. That is the same work under every option here. R3 does not
make it harder.

Name. `pid_bfpm_domains` follows the package's instrument-then-noun pattern.
If the 34-item PID5BF+ ever lands, it gets `pid_bfp_domains` on the same
pattern. A list of tables like `pid_scales` is the general form. But turning
`pid_domains` into a list breaks its readers. Per-form tables are the
consistent choice now.

## 4. Scoring metric (R4): item means honor GP1; domain = mean of 3 facet means

Does GP1 require the sheet's sums? No. The two published statements do not
conflict with each other or with item means. The sheet sums 2 items and
averages 3 facet sums. The paper averages 3 facet "scores". On complete data,
both equal the item-mean scale multiplied by 2 at every level. A metric that
is a fixed multiple of the published one carries the same information and the
same ordering. That is what GP1 protects. What GP1 forbids is a different
rule, and there is none here. Against sums: the package has three PID-5 forms
on item means. `pid_norms` and `norm_engine.R` treat every PID-5 facet and
domain as `"mean"` (`R/norm_engine.R:115`). The APA key that the package
already follows reports facet "average scores". A fourth form on sums is the
odd one out inside the package. It also reads as a different scale from the
FULL facet of the same name. Accept item means.

Loudness. GP1 says deviations are loud. The help page must give the factor of
2, as the brief says. It must say that the key sheet's facet sum is
`2 * pid_<facet>` and its domain average is `2 * pid_<domain>` on complete
data. The scoring vignette must show the multiplication once. M158's printed
instruction line must state the item-mean rule as the other PID-5 forms' lines
do ("Average the responses...", `R/generate_docx.R:1259`). Then the package's
paper form and its scorer agree.

An option for sums: reject. No other instrument in the package has a metric
argument, and `x * 2` is the whole of it. If BF+M norms (Rek et al., 2022,
candidate row) turn out to be on sums, `norm_engine.R` already has a `"sum"`
metric partition. A `pid_norms` row on sums is then converted at lookup, not
by a change to the scorer.

Domain rule. Mean of the 3 facet means, through `domain_map`. That is what
the paper says (p. 181). It is what FULL and SF do. It is what the
standard-error and `"available"` paths already implement. The mean of 6 items
differs only under `"available"` with a missing item. There it weights the
half-answered facet by its one item rather than as one of three facets. The
paper's rule weights facets equally. The two also differ in `calc_se` (domain
SE from 3 facet scores versus from 6 items). Choose facet means and say so.

## 5. Missing-data rule (R5): keep `"apa"` as the default; no warning; domain from available facets

Keep `"apa"` as the default for this form. No published rule exists, so these
are the merits:

- The APA rule is the nearest published rule. The 36 items are PID-5 items,
  and the rule was written for PID-5 scales. Applied as written to a 2-item
  scale, it blanks the facet at one missing item. Verified: `apa_mean()` on a
  2-item matrix returns `NA` for one `NA`, and the mean otherwise. The rule's
  purpose points the same way. It refuses a score from too little of the
  scale. A 1-item facet is exactly the case the 25% threshold exists to
  refuse.
- A version-dependent default means `missing = NULL` resolved inside the
  function. That is a signature change on an exported function whose
  FULL/SF/BF behavior must not move. AC3 still holds on values, but every
  help page then describes two defaults. The HiTOP-SR and HiTOP-BR chose
  `"available"` as whole-function defaults, not per-version ones. There is no
  precedent for the latter.
- `"apa"` and `"complete"` coincide on this form. The help page states that in
  one sentence: with 2 items per facet, the 25% rule reduces to "any missing
  item blanks the facet, and a blank facet blanks its domain". That sentence
  is also what AC2's expected values are computed from.

Warn or refuse `"apa"` for this form: reject both. A warning on the default
fires on every ordinary call. D-052 rejected that for the same reason. A
refusal makes the default an error on one version. The name `"apa"` describes
the rule, which is applied unchanged. The documentation, not a condition,
carries the fact that it reduces to `"complete"` here.

Under `"available"`, a domain from 1 or 2 of its 3 facets is acceptable. The
engine already does this for FULL and SF (`R/score_engine.R:116-120`). There a
domain can be built from one facet of up to 9 items. To require all 3 for
BFPM only is a per-version branch in the one path the plan wanted shared. The
user who chooses `"available"` has chosen lenient scoring. The help page must
say that under it a BFPM facet can be one item and a domain can be one facet.

## 6. What the recommendations miss

**6a. Row order of `pid_scales$BFPM` (apply, before T2).** This is the most
costly omission. `pid_info.R` builds FULL and SF by `tidyr::nest(.by = Facet)`.
That orders facets by first appearance in item order (anhedonia first). The
same build for BFPM gives emotionalLability, manipulativeness,
irresponsibility, withdrawal, unusualBeliefsExperiences, perfectionism, and so
on. That interleaves the domains. That order becomes the `score_pid5()`
column order, the `reliability_pid5()` row order and `label_pid5()`'s match
order the day the table ships. Build the BFPM element in key-sheet order
instead: the 18 facets grouped by domain, with the domains in the sheet's
order (Negative Affectivity, Detachment, Antagonism, Disinhibition,
Anankastia, Psychoticism). That is also the order of Bach et al. Table 2.
`pid_bfpm_domains` takes the same six-domain order. The 6 domain columns then
follow it, and Anankastia sits fifth, before Psychoticism, as both sources
print it. One more fact: the BFPM facet labels cannot come from
`pid_items$Facet` at all. The six anankastia items carry "Rigid
Perfectionism" there. The element must be built from the key CSV's own facet
and domain columns.

**6b. Omega on 2-item facets (apply).** A one-factor CFA with two indicators
is not identified. `lavaan` reports df = -1 and six parameters for three
moments. Verified on simulated data: `calc_omega()` returns 0.63 for two items
generated symmetrically, with loadings 0.86 and 0.64. Any other pair of
loadings fits equally well. lavaan emits a "model is not identified" warning.
`reliability_engine()` catches errors only (`tryCatch(error = ...)`), not
warnings. So `reliability_pid5(version = "BFPM")` as currently wired prints 18
warnings and returns 18 arbitrary omegas beside 6 sound ones. Fix this before
release, because a number corrected later is a GP2 change. Set omega to `NA`
for any scale with fewer than 3 items. The guard goes in `calc_omega()`
(`R/reliability.R:147-151`, beside the existing `k > 1`) or in the engine.
Document that omega needs 3 items. Alpha on 2 items is defined and stays.

**6c. `Scale` and `camelCase` for the reliability rows (apply).** As in Q3:
18 facet rows with `Scale = pid_scales$BFPM$Facet`, then 6 domain rows with
`Scale = pid_bfpm_domains$Domain`. Stems come from the same rows. `nItems` is
2 and 6. No row is derived, which is D-046's rule.

**6d. No BF+M total (apply).** Neither source defines one. The BF total exists
because Markon et al. define it (D-017). IP3 bars a scale with no key. The
help page's sentence "The FULL and SF versions have no total score"
(`R/score_pid5.R:95-96`) becomes "FULL, SF and BFPM".

**6e. Documented shapes that change (apply, NEWS).** `pid_items` goes from 15
to 16 columns. `pid_scales` goes from length 3 to 4 (`R/data.R:5,28`).
`tests/testthat/test-column-shape.R:33-39` pins the set of `nItems` tables
and needs `pid_scales$BFPM`. The `Facet`/`Domain` entry of `pid_items` must
say that those columns name the PID-5 facet. It must also say that the BF+M
regroups six Rigid Perfectionism items into three facets held only in
`pid_scales$BFPM`. D-034 makes these documented shapes contracts. So they are
NEWS lines even though no value moves.

**6f. `calc_se` on BFPM (consider).** Deprecated, but it runs. A 2-item facet
SE is `sd / sqrt(2)`. With one item answered under `"available"`, the SE is
`NA` (sd of one value) while the score is not. `mask_se_na` does not mask
that. This is harmless and already true for other forms. If anyone asks, one
clause in the help page covers it.

## Beyond the brief

- Keying: no doubt found. The 36 items are distinct PID-5 items. Each is in
  the facet the sheet names. None is reverse-keyed. The paper's six anankastia
  numbers match the sheet (verified against `pid_items`).
- `make_scoring_table()` sorts by `Scale` alphabetically. M158's AC1 says "the
  parsed scoring table equals M157's BF+M facets, item pairs and domain map".
  The existing table alone cannot meet that. M158 must plan the domain
  presentation now rather than discover it at its gate.
- `plot_pid5()` and `norm_pid5()` are out of scope.
  `tests/testthat/test-norms.R:36` reads `pid_scales[[version]]` for named
  versions only, and `norm_mean_scales` is a fixed vector. Neither breaks on a
  fourth element. Nothing else iterates over `names(pid_scales)`.

## Recommendations

1. **Apply.** `version = "BFPM"`, `pid_items$BFPM`, `pid_scales$BFPM`, stem
   `pid5bfpm_` (Q1). No alternate spellings.
2. **Apply.** Shared facets keep PID-5 stems and printed names, including
   `unusualBeliefsExperiences` / "Unusual Beliefs & Experiences". Anankastia
   facets are `perfectionism`, `rigidity` and `orderliness`. The domain is
   `anankastia`. Domain printed names are as in `pid_domains`, plus
   "Anankastia" (Q2).
3. **Apply.** Exported `pid_bfpm_domains` with the four `pid_domains`
   columns. `reliability_pid5()` builds its 6 domain item lists from it at
   call time. `label_pid5(target = "scales")` reads it for BFPM (Q3).
4. **Apply.** Item means. Domain = mean of 3 facet means via `domain_map`.
   The help page and vignette state the factor of 2 against the key sheet.
   M158's printed line says "average" (Q4).
5. **Apply.** `"apa"` stays the default and is documented as reducing to
   `"complete"` on this form. `"available"` keeps its FULL/SF domain behavior
   (Q5).
6. **Apply.** Build `pid_scales$BFPM` and `pid_bfpm_domains` in key-sheet,
   domain-grouped order, from the key CSV's own facet labels (6a).
7. **Apply.** Omega returns `NA` for scales with fewer than 3 items. Document
   it. Add a test that `reliability_pid5(version = "BFPM")` emits no warning
   (6b).
8. **Apply.** No BF+M total (6d). Update the documented shapes and NEWS (6e).
9. **Reject-with-reason.** A version-dependent default for `missing`. A
   warning or refusal on `"apa"` for BFPM. A sum-metric option. Domain rows
   stored in `pid_scales$BFPM`. A wider `pid_domains`. Each is a second rule
   or a second place for one concept. The reasons are under Q3 and Q5.
10. **Consider.** A one-line `calc_se` note for 2-item facets (6f). A note in
    M158's plan about the alphabetical scoring table (Beyond the brief).
