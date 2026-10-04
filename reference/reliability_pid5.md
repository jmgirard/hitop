# Estimate PID-5 scale reliability

Compute per-scale internal-consistency reliability — Cronbach's alpha
and McDonald's omega — for the Personality Inventory for DSM-5: full
version (PID-5, 220 items), short form (PID-5-SF, 100 items), brief form
(PID-5-BF, 25 items), modified brief form (PID5BF+M, 36 items), or
Informant Form (PID-5-IRF, 218 items; Markon et al., 2013). Reliability
is estimated on the reverse-keyed item responses, at the facet level for
FULL/SF/IRF and the domain level for BF (the same scales
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
outputs, before FULL/SF/IRF domain aggregation). The IRF facets are the
full form's 25, numbered by informant item and reverse-keyed as the APA
IRF key's Facet Table marks (see
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)).
The BF version also returns a `Total` row covering all 25 items; note
that this scale spans five heterogeneous domains, so its internal
consistency is not comparable to a domain's and is reported without
further interpretation.

## Usage

``` r
reliability_pid5(
  data,
  items,
  version = c("FULL", "SF", "BF", "BFPM", "IRF"),
  srange = c(0, 3),
  alpha = TRUE,
  omega = TRUE
)
```

## Arguments

- data:

  A data frame containing (at least) all the PID items (numerically
  scored and in order).

- items:

  A vector of column names (as strings) or numbers (as integers)
  corresponding to the PID items in order. Items must be supplied in
  instrument order; duplicated entries are an error. Each column must be
  numeric or logical, or character holding only numbers (blank cells and
  `NA` values count as missing, but the text `"NA"` is refused). A haven
  labelled column is read as its plain values. Any other column, such as
  the choice text of an online export or a factor, is an error of class
  `hitop_nonnumeric_items`. So is a 64-bit integer (`integer64`) column,
  and, in a UTF-8 session, a column holding text that is not valid UTF-8
  (text marked Latin-1 is read as its text); convert such text with
  [`iconv()`](https://rdrr.io/r/base/iconv.html). So is an SPSS column
  (`haven_labelled_spss`) that holds a value it declares missing; turn
  those codes into `NA` with
  [`haven::zap_missing()`](https://haven.tidyverse.org/reference/zap_missing.html)
  first. A declared value that is blank is not refused, since blank
  cells count as missing. The SPSS class of haven before 2.0
  (`labelled_spss`) is refused the same way, but
  [`haven::zap_missing()`](https://haven.tidyverse.org/reference/zap_missing.html)
  leaves it unchanged, so set the values its `na_values` or `na_range`
  attribute declares to `NA` first.

- version:

  A string indicating the version of the PID to score: "FULL", "SF",
  "BF", "BFPM" (the 36-item PID5BF+M), or "IRF" (the 218-item Informant
  Form). Will be automatically capitalized. (default = `"FULL"`)

- srange:

  An optional numeric vector specifying the minimum and maximum values
  of the items, used for reverse-coding. (default = `c(0, 3)`)

- alpha:

  Optional logical; if `TRUE`, include a column of Cronbach's alpha per
  scale. (default = `TRUE`)

- omega:

  Optional logical; if `TRUE`, include a column of McDonald's omega
  (total) per scale, estimated via a one-factor CFA (requires the lavaan
  package). (default = `TRUE`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per scale and columns `Scale` (the scale's canonical display name,
as the instrument's keying table spells it), `camelCase` (the stem that
names the scale's column in the matching `score_*()` output, read from
the same keying-table row), `nItems` (integer), and (when requested)
`alpha` and `omega`.

## Details

The BFPM version returns both levels: its 18 two-item facets, then its 6
domains, each estimated over the 6 items of its 3 facets (the map is in
[pid_bfpm_domains](https://jmgirard.github.io/hitop/reference/pid_bfpm_domains.md)).
Omega is `NA` for every facet, because a one-factor model of 2 items is
not identified; alpha is reported for all 24 rows.

Alpha is computed by
[`calc_alpha()`](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
(covariance-based, pairwise deletion) and omega by
[`calc_omega()`](https://jmgirard.github.io/hitop/reference/calc_omega.md)
(one-factor lavaan CFA, FIML). A scale whose estimate cannot be computed
(e.g. too few items or, for omega, a non-converging CFA or an
uninstalled lavaan) is returned as `NA` rather than aborting the call.
Omega needs at least 3 items: for a scale with fewer, no model is fitted
and omega is `NA`.

## References

Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F. (2013). The
development and psychometric properties of an informant-report form of
the Personality Inventory for DSM-5 (PID-5). *Assessment, 20*(3),
370-383.
[doi:10.1177/1073191113486513](https://doi.org/10.1177/1073191113486513)
The scoring key for `version = "IRF"` is the APA's *The Personality
Inventory for DSM-5—Informant Form (PID-5-IRF)—Adult* (2013).

## Examples

``` r
# Facet-level reliability for the full PID-5 (alpha only)
reliability_pid5(sim_pid5, items = 1:220, version = "FULL", omega = FALSE)
#> # A tibble: 25 × 4
#>    Scale                  camelCase             nItems   alpha
#>    <chr>                  <chr>                  <int>   <dbl>
#>  1 Anhedonia              anhedonia                  8 -0.211 
#>  2 Suspiciousness         suspiciousness             7 -0.211 
#>  3 Risk Taking            riskTaking                14 -0.0128
#>  4 Impulsivity            impulsivity                6  0.141 
#>  5 Eccentricity           eccentricity              13  0.0554
#>  6 Distractibility        distractibility            9 -0.154 
#>  7 Restricted Affectivity restrictedAffectivity      7 -0.144 
#>  8 Submissiveness         submissiveness             4 -0.194 
#>  9 Withdrawal             withdrawal                10 -0.0290
#> 10 Callousness            callousness               14  0.0147
#> # ℹ 15 more rows
```
