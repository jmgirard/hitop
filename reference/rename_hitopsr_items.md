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
  prefix = "hsr_"
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

  A string pasted literally before each standardized item number, which
  is zero-padded to three digits: `hsr_001` to `hsr_405` under the
  default, the pattern the shipped datasets and the package's REDCap
  export use (the Qualtrics export writes `HSR_001`). (default =
  `"hsr_"`)

## Value

A data frame with renamed column names for the matched HiTOP-SR items.
Inputs that could not be matched keep their names, and each of the three
warnings this function raises carries a condition class callers may
catch or suppress by. Under `method = "original"`, if no column matches
a legacy name, nothing is renamed and the report is
`hitop_no_columns_matched`. Under `method = "text"`, an `item_text`
entry matching no item is skipped and named in a `hitop_unmatched_items`
report. Under either method, if some but not all of the 405 items were
renamed, the completeness report is `hitop_incomplete_rename`.

## Examples

``` r
# Rename legacy item-pool columns to standard HiTOP-SR item names
legacy <- head(hitopsr_items$Original, 3)
df <- as.data.frame(matrix(0, nrow = 2, ncol = 3,
                           dimnames = list(NULL, legacy)))
names(rename_hitopsr_items(df, method = "original"))
#> Warning: Only 3 out of 405 HiTOP-SR items were successfully matched and renamed.
#> ℹ Note: If you plan to use `score_hitopsr()`, ensure uncollected items exist in
#>   the data frame as `NA` columns.
#> [1] "hsr_001" "hsr_002" "hsr_003"
```
