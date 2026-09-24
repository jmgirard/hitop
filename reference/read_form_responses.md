# Read hitop-form response files into one data frame

Reads the CSV files that the hitop-form web page saves, one file per
participant, or the CSV download of a store the page sends to, one row
per participant, and binds them into one tibble that the scoring
functions take as it is. The page is at
<https://jmgirard.github.io/hitop-form/>.

## Usage

``` r
read_form_responses(path)
```

## Arguments

- path:

  A directory that holds the response files, or a character vector of
  paths to them. A directory is read as every file in it whose name ends
  in `.csv` (in either case). The paths are sorted in the C locale
  before they are read, so the rows come back in the same order however
  the paths were supplied.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per response row. The first eight columns are `study`, `participant`
and `instrument` as character, `form_build` as `Date`, `submitted` as
`POSIXct` in UTC, and `item_order`, `prolific_study` and
`prolific_session` as character, each `NA` on a row from a file without
that column and on a blank cell. The item columns follow as integers, in
the column order of the first file after sorting. An item the
participant left blank is `NA`.

## Details

A file the page saves holds one header row and one response row. A
store's download, such as a Google Sheet's CSV export or a Supabase
table's, holds one header row and one row per participant. Every
response row of every file is a row of the result, the files in path
order and the rows in file order. The first eight columns of the result
are `study`, `participant`, `instrument`, `form_build`, `submitted`,
`item_order`, `prolific_study` and `prolific_session`. The item columns
follow from the ninth, one per item, named by the instrument's file stem
and the item number (`hitopsr_001`, `hitopbr_01`, `pid5_001`,
`pid5sf_001`, `pid5bf_01`). A module form saves only the module's items.
The page keeps the item columns in the order it showed the items, except
under a study link that asks for a random order: the page then draws a
new order for each participant, keeps the item columns in the
instrument's order (a module's items in the order its descriptor lists
them, which
[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
writes ascending), and writes `item_order`.

`item_order` is the order the participant saw the items, as item numbers
with no leading zero, joined by single spaces with none at either end
(`hitopbr_01` is 1). The page writes it under a random order and not
otherwise. A file may hold the column anywhere after `submitted`, as a
store's download may append it after the item columns, and the result
places it sixth. A file may also lack it: its rows then hold `NA` there.
Scoring does not read the column, and it is not an item column, so it
does not enter the check that every file holds the same item columns. A
cell that is not blank and does not list the file's item numbers, each
once, is an error naming the file and the response row.

`prolific_study` and `prolific_session` hold the study and session
identifiers that Prolific adds to a study link, when the file records
them for a study recruited through Prolific. A file may hold either or
both anywhere after `submitted`, and the result places `prolific_study`
seventh and `prolific_session` eighth. A row from a file without a
column holds `NA` in it, and so does a blank cell. Scoring does not read
them, and they are not item columns, so neither enters the check that
every file holds the same item columns. The cells are read as written,
with no check on their content.

Every file must carry the same item columns in the same order, because a
set of files that differ cannot be one data frame: a full HiTOP-SR
beside a module, or two modules that shuffled their items differently,
need separate calls. A file that does not look like one the page saved
(first columns other than the five the page writes first, a column that
appears twice, a header with no response row, an item value that is not
a whole number or is outside R's integer range, a date that does not
parse) is an error naming the file. A `submitted` stamp may carry
fractional seconds.

**Errors.** Files whose item columns differ from the first file's in
name, in count or in order stop the read under the condition class
`hitop_form_responses_mismatch`, and the message names each file that
differs and how. A directory holding no `.csv` file stops it under
`hitop_form_responses_none`. Both classes are a public contract a caller
can catch by name.

## See also

[`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md),
[`score_hitopbr()`](https://jmgirard.github.io/hitop/reference/score_hitopbr.md),
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
and
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md),
which score the item columns; the Collecting Responses Online article
walks the Google Sheet route from the study link to the scores, and the
modules article and
[`vignette("pid5_scoring")`](https://jmgirard.github.io/hitop/articles/pid5_scoring.md)
show the hand-off for a module and for the PID-5.

## Examples

``` r
# Two files as the page saves them, here written by hand.
dir <- tempfile("responses")
dir.create(dir)
writeLines(
  c("study,participant,instrument,form_build,submitted,hitopbr_01,hitopbr_02",
    "demo,p001,hitopbr,2026-09-20,2026-09-20T21:20:36Z,4,1"),
  file.path(dir, "demo_p001.csv")
)
writeLines(
  c("study,participant,instrument,form_build,submitted,hitopbr_01,hitopbr_02",
    "demo,p002,hitopbr,2026-09-20,2026-09-21T09:02:11Z,2,"),
  file.path(dir, "demo_p002.csv")
)

responses <- read_form_responses(dir)
responses
#> # A tibble: 2 × 10
#>   study participant instrument form_build submitted           item_order
#>   <chr> <chr>       <chr>      <date>     <dttm>              <chr>     
#> 1 demo  p001        hitopbr    2026-09-20 2026-09-20 21:20:36 NA        
#> 2 demo  p002        hitopbr    2026-09-20 2026-09-21 09:02:11 NA        
#> # ℹ 4 more variables: prolific_study <chr>, prolific_session <chr>,
#> #   hitopbr_01 <int>, hitopbr_02 <int>

unlink(dir, recursive = TRUE)
```
