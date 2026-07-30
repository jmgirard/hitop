# Describe a Subset of an Instrument's Scales

Builds a validated description of a subset of an instrument's scales,
for use with the `generate_*` family. Supplying the result as the
`subset` argument of a generator produces a shortened instrument
containing only the items belonging to the chosen scales, **keeping each
item's original number** so that data collected with the shortened form
can still be scored against the full instrument's key.

## Usage

``` r
hitop_subset(instrument = "hitopsr", scales)
```

## Arguments

- instrument:

  A string naming the instrument to subset. Currently only `"hitopsr"`
  is supported. (default = `"hitopsr"`)

- scales:

  A character vector of scale names to keep. Names may be given either
  as they are printed on the instrument (`"Antisocial Behavior"`) or as
  the camelCase stems used in scored output (`"antisocialBehavior"`), in
  any mixture and ignoring case. Duplicates are dropped.

## Value

An object of class `hitop_subset`: a list with the resolved
`instrument`, the canonical display `scales` and their `camelCase`
stems, the `items` kept (original instrument numbering, ascending), the
parallel `reverse` keying flags, and `nItems`.

## See also

[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
[`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md),
and
[`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md),
each of which takes a `subset` argument.

## Examples

``` r
# Describe a two-scale subset of the HiTOP-SR
s <- hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
s
#> <hitop_subset> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss

# The item numbers are the original HiTOP-SR numbers, not 1..8
s$items
#> [1]  66 109 118 144 202 260 291 389
```
