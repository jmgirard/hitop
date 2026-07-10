# Label HiTOP-SR Columns with Semantic Descriptions

Add literal item text or clean scale names as attributes to data frame
columns, making them readable by data viewers and reporting packages.

## Usage

``` r
label_hitopsr(data, target = c("items", "scales"), prefix = "HSR_")
```

## Arguments

- data:

  A data frame containing HiTOP-SR items or scales.

- target:

  A string specifying what to label: `"items"` to label raw item columns
  with questionnaire text, or `"scales"` to label computed scale
  columns.

- prefix:

  An optional string specifying the prefix used on the column names.
  (default = `"HSR_"`)

## Value

A data frame with labeled columns.

## Examples

``` r
# Attach item text as a `label` attribute to the raw item columns
labeled <- label_hitopsr(sim_hitopsr, target = "items", prefix = "hsr_")
attr(labeled$hsr_1, "label")
#> [1] "I preferred to stay home than to go to a party."
```
