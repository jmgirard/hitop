# Label HiTOP-SR Columns with Semantic Descriptions

Add literal item text or clean scale names as attributes to data frame
columns, making them readable by data viewers and reporting packages.

## Usage

``` r
label_hitopsr(data, target = c("items", "scales"), prefix = "hsr_")
```

## Arguments

- data:

  A data frame containing HiTOP-SR items or scales.

- target:

  A string specifying what to label: `"items"` to label raw item columns
  with questionnaire text, or `"scales"` to label computed scale
  columns.

- prefix:

  A string specifying the prefix used on the column names. Item columns
  are expected as the prefix followed by the item number zero-padded to
  three digits (`hsr_001` to `hsr_405` under the default, the pattern
  the shipped datasets and the package's REDCap export use; the
  Qualtrics export writes `HSR_001`, matched by `prefix = "HSR_"`).
  Columns that carry the prefix and a number without that padding are
  not labelled, and a warning of class `hitop_unpadded_items` names
  them; scale columns as the prefix followed by the scale's `camelCase`
  name, which is what
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  writes under its own default `prefix`. (default = `"hsr_"`)

## Value

A data frame with labeled columns.

## Examples

``` r
# Attach item text as a `label` attribute to the raw item columns
labeled <- label_hitopsr(sim_hitopsr, target = "items")
attr(labeled$hsr_001, "label")
#> [1] "I preferred to stay home than to go to a party."
```
