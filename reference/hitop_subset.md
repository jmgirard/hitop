# Deprecated: Describe a Subset of an Instrument's Scales

`hitop_subset()` was renamed to
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
in hitop 0.2.0, when a chosen set of an instrument's scales became a
*module* throughout this package. It is kept so existing scripts keep
running: it warns, then returns the same descriptor
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
returns, carrying the legacy `hitop_subset` class. Every function that
accepts a module also accepts that legacy object.

The `subset` argument of
[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
[`reliability_hitopsr()`](https://jmgirard.github.io/hitop/reference/reliability_hitopsr.md),
[`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
[`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md),
and
[`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md)
was renamed to `module` at the same time, and is deprecated in the same
way.

## Usage

``` r
hitop_subset(instrument = "hitopsr", scales)
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

## Value

An object of class `hitop_subset`, identical to what
[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
returns apart from its class attribute.

## See also

[`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md),
which replaces this.

## Examples

``` r
# Deprecated; use hitop_module() instead
m <- suppressWarnings(
  hitop_subset("hitopsr", scales = c("Agoraphobia", "Appetite Loss"))
)
m
#> <hitop_subset> hitopsr: 8 items from 2 scales
#> * Agoraphobia
#> * Appetite Loss
```
