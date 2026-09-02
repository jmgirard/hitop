# Label PID-5 Columns with Semantic Descriptions

Add literal item text or clean scale names as attributes to data frame
columns, making them readable by data viewers and reporting packages.

## Usage

``` r
label_pid5(
  data,
  target = c("items", "scales"),
  version = c("FULL", "SF", "BF"),
  prefix = NULL
)
```

## Arguments

- data:

  A data frame containing PID-5 items or scales.

- target:

  A string specifying what to label: `"items"` to label raw item columns
  with questionnaire text, or `"scales"` to label computed scale
  columns. (default = `"items"`)

- version:

  A string specifying the PID-5 form the columns belong to: `"FULL"`
  (220 items), `"SF"` (100 items), or `"BF"` (25 items). Matched
  case-insensitively. The three forms number their items independently
  and score different sets of scales, so the form named here decides
  both the text attached to an item column and which scale columns are
  recognized. (default = `"FULL"`)

- prefix:

  A string specifying the prefix used on the column names. `NULL`
  resolves to the default for the given `target` and `version`: under
  `target = "items"`, the form's own stem (`"pid5_"`, `"pid5sf_"` or
  `"pid5bf_"`), the pattern the shipped datasets and the package's
  REDCap export use; under `target = "scales"`, `"pid_"`, which is what
  [`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
  writes under its own default `prefix`. (default = `NULL`)

  Item columns are expected as the prefix followed by the item number
  zero-padded to the width of the form's largest item number (`pid5_001`
  to `pid5_220` for the full form, `pid5bf_01` to `pid5bf_25` for the
  brief form). A column carrying the prefix and a number that is not one
  of those expected names is not labelled, and a warning of class
  `hitop_unpadded_items` names it, in a sentence per kind: a number
  padded to some other width is reported as not zero-padded to the
  form's width, and a number outside the form's range is reported as out
  of range, whatever its padding. That warning is raised whether or not
  any other column matched. Scale columns are expected as the prefix
  followed by the scale's `camelCase` name.

## Value

A data frame with labeled columns. Columns the named form does not
recognize keep whatever attributes they had. The validity-scale columns
[`validity_pid5()`](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
writes and the `_se` columns `score_pid5(calc_se = TRUE)` writes are not
labelled. If no column matched the expected names at all, `data` is
returned unchanged and a warning says so; the `hitop_unpadded_items`
report still names every prefixed item column it found.

## Examples

``` r
# Attach item text as a `label` attribute to the raw item columns
labeled <- label_pid5(sim_pid5bf, target = "items", version = "BF")
attr(labeled$pid5bf_01, "label")
#> [1] "People would describe me as reckless"
```
