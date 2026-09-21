# Read hitop-form response files into one data frame

Reads the CSV files that the hitop-form web page saves, one file per
participant, and binds them into one tibble that the scoring functions
take as it is. The page is at <https://jmgirard.github.io/hitop-form/>.

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
row per file. The first five columns are `study`, `participant` and
`instrument` as character, `form_build` as `Date` and `submitted` as
`POSIXct` in UTC. The item columns follow as integers, in the column
order of the first file after sorting. An item the participant left
blank is `NA`.

## Details

Each file the page saves holds one header row and one response row. The
first five columns are `study`, `participant`, `instrument`,
`form_build` and `submitted`; the item columns follow, one per item,
named by the instrument's file stem and the item number (`hitopsr_001`,
`hitopbr_01`, `pid5_001`, `pid5sf_001`, `pid5bf_01`). A module form
saves only the module's items, in the order the form showed them.

Every file must carry the same item columns in the same order, because a
set of files that differ cannot be one data frame: a full HiTOP-SR
beside a module, or two modules that shuffled their items differently,
need separate calls. A file that does not look like one the page saved
(other lead columns, a column that appears twice, more than one response
row, an item value that is not a whole number or is outside R's integer
range, a date that does not parse) is an error naming the file. A
`submitted` stamp may carry fractional seconds.

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
which score the item columns; the modules article and
[`vignette("pid5_scoring")`](https://jmgirard.github.io/hitop/articles/pid5_scoring.md)
show the whole hand-off.

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
#> # A tibble: 2 × 7
#>   study participant instrument form_build submitted           hitopbr_01
#>   <chr> <chr>       <chr>      <date>     <dttm>                   <int>
#> 1 demo  p001        hitopbr    2026-09-20 2026-09-20 21:20:36          4
#> 2 demo  p002        hitopbr    2026-09-20 2026-09-21 09:02:11          2
#> # ℹ 1 more variable: hitopbr_02 <int>

unlink(dir, recursive = TRUE)
```
