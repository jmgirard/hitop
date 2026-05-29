# Label HiTOP-BR Columns with Semantic Descriptions

Add literal item text or clean scale names as attributes to data frame
columns for the HiTOP Brief Report (HiTOP-BR), making them readable by
data viewers and reporting packages.

## Usage

``` r
label_hitopbr(data, target = c("items", "scales"), prefix = "HBR_")
```

## Arguments

- data:

  A data frame containing HiTOP-BR items or scales.

- target:

  A string specifying what to label: `"items"` to label raw item columns
  with questionnaire text, or `"scales"` to label computed scale
  columns.

- prefix:

  An optional string specifying the prefix used on the column names.
  (default = `"HBR_"`)

## Value

A data frame with labeled columns.
