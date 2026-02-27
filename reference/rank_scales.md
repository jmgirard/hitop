# Rank and list the top-\\n\\ scales per row

For each row of selected scale columns, return a comma-separated string
of the `top` highest (or lowest) scale names, optionally removing a
common `prefix` from the column names and optionally appending the
result to the original data as a tibble.

## Usage

``` r
rank_scales(
  data,
  scales,
  prefix = NULL,
  top = 5,
  dir = "high",
  append = TRUE,
  tibble = TRUE
)
```

## Arguments

- data:

  A data frame containing the scale columns.

- scales:

  A character or integer vector identifying the columns in `data` to
  rank (names or positions). These columns should be numeric.

- prefix:

  A length-1 string giving a leading pattern to strip from each selected
  column name before concatenation, or `NULL` to keep names as is. The
  match is interpreted as a regular expression anchored to the start of
  the name (i.e., `"^"` is prepended). Special regex characters in
  `prefix` will be treated as regex metacharacters.

- top:

  Integer (length 1). The number of columns to include per row after
  ranking. Must be between 1 and `length(scales)`.

- dir:

  Direction of ranking: `"high"` for largest values first or `"low"` for
  smallest values first.

- append:

  Logical. If `TRUE`, bind the result as a new column to `data`. If
  `FALSE`, return only the result vector.

- tibble:

  Logical. If `TRUE`, coerce the output to a tibble via
  [`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
  (applies whether or not `append` is `TRUE`).

## Value

If `append = FALSE` and `tibble = FALSE`: a character vector of length
`nrow(data)` where each element is a comma-separated list of the
selected scale names in ranked order.

If `append = TRUE`: a data frame (or tibble if `tibble = TRUE`) equal to
`data` with an added character column `out`.

If `append = FALSE` and `tibble = TRUE`: a tibble with one column `out`.

## Details

Ranking is performed row-wise using
[`order()`](https://rdrr.io/r/base/order.html) on the selected columns.
Ties are resolved by the original column order (the default behavior of
`order`), which is alphabetical if using the package's scoring
functions. Missing values are placed last by
[`order()`](https://rdrr.io/r/base/order.html) and will be included only
if there are fewer than `top` non-missing values in a row.

If `prefix` is not `NULL`, the function removes the leading pattern
`paste0("^", prefix)` from each selected column name before
concatenation.

When `append = TRUE`, the appended column is named `"out"`. The order of
existing columns in `data` is preserved.
