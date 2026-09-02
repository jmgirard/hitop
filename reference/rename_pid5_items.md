# Rename Columns to Standard PID-5 Item Names

Rename data frame columns to the standard PID-5 item names of one form,
matching either on the item number carried in the current column name or
on the literal item prompt text. The standard names are the ones the
package's REDCap and Qualtrics exports write and the shipped datasets
carry: `pid5_001` to `pid5_220` for the full form, `pid5sf_001` to
`pid5sf_100` for the short form, and `pid5bf_01` to `pid5bf_25` for the
brief form.

## Usage

``` r
rename_pid5_items(
  data,
  version = c("FULL", "SF", "BF"),
  method = c("number", "text"),
  item_cols = NULL,
  item_text = NULL,
  from_prefix = "pid_",
  prefix = NULL
)
```

## Arguments

- data:

  A data frame containing the PID-5 items.

- version:

  A string specifying the PID-5 form the items belong to: `"FULL"` (220
  items), `"SF"` (100 items), or `"BF"` (25 items). Matched
  case-insensitively. (default = `"FULL"`)

- method:

  A string specifying the matching method: `"number"` to rename columns
  spelled `from_prefix` followed by an item number, or `"text"` to match
  against the literal item prompt text in `pid_items$Text`. (default =
  `"number"`)

  The three forms number their items independently, so `"number"` reads
  the digits as an item number of the form named by `version`: under
  `version = "SF"`, `pid_7` is short-form item 7, not the full-form item
  the short form numbers 7. Data labelled by full-form item numbers must
  be renamed with `version = "FULL"` first, or matched with
  `method = "text"`.

- item_cols:

  An optional character vector of current column names to be renamed.
  Required if `method = "text"`.

- item_text:

  An optional character vector of item texts corresponding exactly to
  the columns specified in `item_cols`. Required if `method = "text"`.

- from_prefix:

  A string matched literally at the start of a column name under
  `method = "number"`, before the item number. The default is the
  spelling this package's own PID-5 datasets used before they were
  renamed to match the exports. (default = `"pid_"`)

- prefix:

  A string pasted literally before each standardized item number, which
  is zero-padded to the width of the form's largest item number. `NULL`
  resolves to the form's own stem: `"pid5_"`, `"pid5sf_"` or
  `"pid5bf_"`. (default = `NULL`)

## Value

A data frame with renamed column names for the matched PID-5 items.
Columns that could not be matched keep their names. Under
`method = "number"`, a column spelled like an item of the instrument
whose number names no item of this form, and under `method = "text"`, an
`item_text` entry matching no item of this form, are skipped and named
in a warning of class `hitop_unmatched_items`, which callers may catch
or suppress by class. A column not spelled like an item number is left
alone and not reported.

## Examples

``` r
# Rename columns named as this package's datasets were before the rename
df <- data.frame(pid_1 = c(0, 1), pid_2 = c(2, 3), age = c(30, 40))
names(suppressWarnings(rename_pid5_items(df, version = "FULL")))
#> [1] "pid5_001" "pid5_002" "age"     
```
