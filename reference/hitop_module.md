# Describe a Module of an Instrument's Scales

Builds a validated description of a **module**: a chosen set of an
instrument's scales, administered and scored on its own. Supplying the
result as the `module` argument of a generator produces an instrument
containing only the items belonging to those scales, **keeping each
item's original number**, so that data collected with the module can
still be scored against the full instrument's key. Supplying it again to
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
or
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
scores the collected columns.

Use
[`available_scales()`](https://jmgirard.github.io/hitop/reference/available_scales.md)
to see which scales an instrument offers.

## Usage

``` r
hitop_module(instrument = "hitopsr", scales, call = rlang::current_env())
```

## Arguments

- instrument:

  A string naming the instrument to build a module from. Currently only
  `"hitopsr"` is supported. (default = `"hitopsr"`)

- scales:

  A character vector of scale names to keep. Names may be given either
  as they are printed on the instrument (`"Antisocial Behavior"`) or as
  the camelCase stems used in scored output (`"antisocialBehavior"`), in
  any mixture and ignoring case. Duplicates are dropped.

- call:

  Internal. The environment blamed by any error this raises. A default
  argument is evaluated in this function's own frame, so a direct call
  blames `hitop_module()`; the deprecated
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md)
  passes its own frame instead, so a bad argument there names the
  function the user actually wrote. (default = this function's frame)

## Value

An object of class `hitop_module`: a list with the resolved
`instrument`, the canonical display `scales` and their `camelCase`
stems, the `items` kept (original instrument numbering, ascending), the
parallel `reverse` keying flags, and `nItems`.

## See also

[`available_scales()`](https://jmgirard.github.io/hitop/reference/available_scales.md)
for the scale names this accepts;
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
[`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md),
and
[`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md),
each of which takes a `module` argument;
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
and
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md)
for scoring the result.

## Examples

``` r
# Describe a two-scale module of the HiTOP-SR
m <- hitop_module("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
m
#> <hitop_module> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss

# The item numbers are the original HiTOP-SR numbers, not 1..8
m$items
#> [1]  66 109 118 144 202 260 291 389

# Select the collected item columns by NAME, never by position: `m$items`
# holds item numbers, which are column positions only in a frame that is
# exactly the 405 items in order. `ku_hitopsr` leads with `participant` and
# `biosex`, so `ku_hitopsr[m$items]` would quietly return the wrong columns.
collected <- ku_hitopsr[sprintf("hsr%03d", m$items)]
ncol(collected) == m$nItems
#> [1] TRUE
```
