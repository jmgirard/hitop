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
row per response row. The first eight columns are `study`,
`participant`, `instrument` and `form_build` as character, `submitted`
as `POSIXct` in UTC, and `item_order`, `prolific_study` and
`prolific_session` as character, each `NA` on a row from a file without
that column and on a blank cell. The item columns follow as integers, in
the column order of the first file after sorting. An item the
participant left blank is `NA`. The answer columns, when any file holds
one, come last as character, in order of first appearance, each `NA` on
a row from a file without it and on a blank cell.

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
writes ascending and the page requires ascending), and writes
`item_order`.

`form_build` is the build date of the instrument's export the page
showed, as `YYYY-MM-DD`. The result keeps each cell as written, so the
column is character. Convert it with
[`as.Date()`](https://rdrr.io/r/base/as.Date.html) when you need a date
and the file holds one instrument.

**Several instruments in one file.** A file's item columns may form two
or more groups, one per instrument. The columns of each group sit side
by side and hold one stem, and no stem appears in two groups. The
optional lead columns and the answer columns may sit anywhere after
`submitted`, between two groups or inside one, and they do not split a
group. The `instrument` cell holds the stems in the order of the groups,
joined by single spaces (`hitopbr pid5bf`). The `form_build` cell holds
one date per stem, in the same order, joined by single spaces
(`2026-09-20 2026-09-18`). The result holds the item columns in file
order. Score each instrument in its own call, with its columns chosen by
stem, as `grep("^pid5bf_", names(responses), value = TRUE)` chooses the
PID-5-BF's. The hitop-form page writes such a file for a study link
whose `instruments` field lists two or three instruments.

`item_order` is the order the participant saw the items, as item numbers
with no leading zero, joined by single spaces with none at either end
(`hitopbr_01` is 1). In a file of several instruments it holds one group
per stem, in the order of the `instrument` cell, joined by a space, a
bar and a space (`2 1 | 3 1 2`), each group that instrument's item
numbers in the order shown. The page writes it under a random order and
not otherwise. A file may hold the column anywhere after `submitted`, as
a store's download may append it after the item columns, and the result
places it sixth. A file may also lack it: its rows then hold `NA` there.
Scoring does not read the column, and it is not an item column, so it
does not enter the check that every file holds the same item columns. A
cell that is not blank and does not list the file's item numbers, each
once, in one group per stem, is an error naming the file and the
response row. A group does not name its stem, so the reader cannot tell
two groups apart when their instruments hold the same item numbers:
swapped, they read as written.

`prolific_study` and `prolific_session` hold the study and session
identifiers that Prolific adds to a study link, when the file records
them for a study recruited through Prolific. The hitop-form page writes
them when the link builder's recruiting site is Prolific, and then takes
the participant identifier from the Prolific ID in the page's address.
Under another recruiting site (SONA, CloudResearch Connect) the page
takes the identifier from that site's address parameter into
`participant` and writes no further column. A file may hold either or
both anywhere after `submitted`, and the result places `prolific_study`
seventh and `prolific_session` eighth. A row from a file without a
column holds `NA` in it, and so does a blank cell. Scoring does not read
them, and they are not item columns, so neither enters the check that
every file holds the same item columns. The cells are read as written,
with no check on their content.

A column whose name starts with `q_` is an answer column: it holds the
answer to a question of the researcher's own, named `q_` and then the
question's name (`q_age`). A file may hold answer columns anywhere after
`submitted`, and the result places them after the item columns. They
come in the order the reader first meets them, with the files in path
order and each file read from left to right. Each is character, read as
written with no conversion, and a blank cell is `NA`. Files that hold
different answer columns read together, and a row from a file without a
column holds `NA` in it. Answer columns are not item columns, so they do
not enter the check that every file holds the same item columns. After
`q_`, a name must hold a lower-case letter and then up to 29 lower-case
letters, digits or underscores, the pattern `^q_[a-z][a-z0-9_]{0,29}$`.
A name that starts with `q_` and does not match is an error naming the
file and the column.

Every file must carry the same item columns in the same order, because a
set of files that differ cannot be one data frame: a full HiTOP-SR
beside a module, or two modules that shuffled their items differently,
need separate calls. A file that does not look like one the page saved
is an error naming the file: a UTF-16 or UTF-32 file (described below),
a line holding a NUL byte or a byte sequence that is not UTF-8, a line
outside a quoted cell made only of spaces and tabs, no header row (a
zero-byte file, blank lines only, a UTF-8 byte-order mark only), first
columns other than the five the page writes first, a column that appears
twice, a header with no response row, a response row holding fewer or
more fields than the header, an answer column whose name does not match
the pattern above, an item column whose name is not a stem of lower-case
letters and digits, an underscore and the item number (`hitopbr_01`, not
`foo` or `Hitopbr_01`), a stem whose columns another stem's columns
split (`hitopbr_01`, `pid5bf_01`, `hitopbr_02`), an `instrument` cell
that differs from the item columns' stems in file order (`pid5bf` beside
`hitopbr_01`, or `hitopbr` beside `hitopbr_01` and `pid5bf_01`), an item
value that is not a whole number or is outside R's integer range, a
`form_build` cell whose date count differs from the stem count, or a
date that does not parse. An error on a line names the lines at fault,
counted from the file's first line. An error on a row names the response
rows at fault, counted from the first row after the header. An error on
an item value names each cell at fault as its response row, its column
and the value as written, the first five cells and a count of the rest.
The errors on a NUL byte or a byte sequence that is not UTF-8, on a line
of spaces and tabs, on a row's field count and on an `instrument` cell
likewise name the first five lines or rows at fault and a count of the
rest. The error on a UTF-16 or UTF-32 file names the encoding and no
line, and asks that the file be saved as UTF-8. A file that starts with
the byte-order mark FF FE 00 00 or 00 00 FE FF is taken as UTF-32, and
one that starts with FF FE or FE FF as UTF-16. A file with no mark is
read as UTF-32 and then as UTF-16, each little-endian and then
big-endian. In each encoding the lines are split on that encoding's line
feed. Empty lines and lines of a lone carriage return at the top are
skipped. The first line that is not blank then has its trailing carriage
return dropped. If it holds only printable ASCII characters and tabs,
the file is taken as that encoding. A file of blank lines only in one of
these encodings is also taken as that encoding when it holds at least
one line feed. A file taken as UTF-32 is refused as UTF-32, not as
UTF-16. A UTF-16 or UTF-32 file the rule does not take is read as UTF-8,
and a NUL byte in it meets the byte error. The field count of a row
reads `#` as data and a quoted cell holding a line break as one cell, as
the read does. A `submitted` stamp may carry fractional seconds.

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
#>   <chr> <chr>       <chr>      <chr>      <dttm>              <chr>     
#> 1 demo  p001        hitopbr    2026-09-20 2026-09-20 21:20:36 NA        
#> 2 demo  p002        hitopbr    2026-09-20 2026-09-21 09:02:11 NA        
#> # ℹ 4 more variables: prolific_study <chr>, prolific_session <chr>,
#> #   hitopbr_01 <int>, hitopbr_02 <int>

unlink(dir, recursive = TRUE)
```
