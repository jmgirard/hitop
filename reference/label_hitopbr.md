# Label HiTOP-BR Columns with Semantic Descriptions

Add literal item text or clean scale names as attributes to data frame
columns for the HiTOP Brief Report (HiTOP-BR), making them readable by
data viewers and reporting packages.

## Usage

``` r
label_hitopbr(data, target = c("items", "scales"), prefix = "hbr_")
```

## Arguments

- data:

  A data frame containing HiTOP-BR items or scales.

- target:

  A string specifying what to label: `"items"` to label raw item columns
  with questionnaire text, or `"scales"` to label computed scale
  columns.

- prefix:

  A string specifying the prefix used on the column names. Item columns
  are expected as the prefix followed by the item number zero-padded to
  two digits (`hbr_01` to `hbr_45` under the default, the pattern the
  shipped datasets and the package's REDCap export use; the Qualtrics
  export writes `HBR_01`, matched by `prefix = "HBR_"`). Columns that
  carry the prefix and a number without that padding are not labelled,
  and a warning of class `hitop_unpadded_items` names them; scale
  columns as the prefix followed by the scale's `camelCase` name, which
  is what
  [`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
  writes under its own default `prefix`. (default = `"hbr_"`)

## Value

A data frame with labeled columns.

## Examples

``` r
# Attach item text as a `label` attribute to the raw item columns
labeled <- label_hitopbr(sim_hitopbr, target = "items")
attr(labeled$hbr_01, "label")
#> [1] "I found it easy to deceive others."
```
