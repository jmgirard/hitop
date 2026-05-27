# Rename Columns to Standard HiTOP-SR Item Names

Rename data frame columns to standard HiTOP-SR item names based on
either legacy ("Original") variable names or item text matching.

## Usage

``` r
rename_hitopsr_items(
  data,
  method = c("original", "text"),
  item_cols = NULL,
  item_text = NULL,
  prefix = "HSR_"
)
```

## Arguments

- data:

  A data frame containing the HiTOP-SR items.

- method:

  A string specifying the matching method: `"original"` to match against
  the legacy item pool names in `hitopsr_items$Original`, or `"text"` to
  match against the literal item prompt text. (default = `"original"`)

- item_cols:

  An optional character vector of current column names to be renamed.
  Required if `method = "text"`.

- item_text:

  An optional character vector of item texts corresponding exactly to
  the columns specified in `item_cols`. Required if `method = "text"`.

- prefix:

  An optional string to add before each standardized item number.
  (default = `"HSR_"`)

## Value

A data frame with renamed column names for the matched HiTOP-SR items.
