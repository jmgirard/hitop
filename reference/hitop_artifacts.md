# Distribution Artifact Manifest

Version manifest for the prebuilt instrument artifacts. They ship in the
package at `inst/extdata/` and are distributed from the package
website's download pages, which serve their own byte-identical copy.
Each build of an artifact adds a row (the full history is kept), so the
latest row per `file` describes the currently distributed file. Artifact
revisions are identified by build date; the instrument version (e.g.,
`"1.0"`) is the version of the instrument itself and changes only when
its publisher revises it. To check which build you have, compare your
downloaded file's MD5 checksum (e.g.,
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html)) against the
`md5` column.

## Usage

``` r
hitop_artifacts
```

## Format

A [tibble](https://tibble.tidyverse.org/reference/tibble.html) with one
row per artifact build and 7 columns:

- file:

  Artifact file name, the same in `inst/extdata/` and on the website's
  download pages

- instrument:

  Instrument the artifact administers

- format:

  Artifact format: `"docx_us"`, `"docx_a4"`, `"qualtrics"`, `"redcap"`,
  or `"json"`

- instrument_version:

  Version of the instrument itself

- build_date:

  Date this build of the artifact was generated

- md5:

  MD5 checksum of the built file

- changes:

  What changed in this build

## Examples

``` r
hitop_artifacts
#> # A tibble: 64 × 7
#>    file            instrument format instrument_version build_date md5   changes
#>    <chr>           <chr>      <chr>  <chr>              <date>     <chr> <chr>  
#>  1 hitopbr_A4.docx HiTOP-BR   docx_… 1.0                2026-07-16 f629… Versio…
#>  2 hitopbr_A4.docx HiTOP-BR   docx_… 1.0                2026-08-30 57c6… One it…
#>  3 hitopbr_qualtr… HiTOP-BR   qualt… 1.0                2026-07-16 d3cc… Versio…
#>  4 hitopbr_redcap… HiTOP-BR   redcap 1.0                2026-07-16 93a4… Versio…
#>  5 hitopbr_US.docx HiTOP-BR   docx_… 1.0                2026-07-16 0e16… Versio…
#>  6 hitopbr_US.docx HiTOP-BR   docx_… 1.0                2026-08-30 178d… One it…
#>  7 hitopbr.json    HiTOP-BR   json   1.0                2026-09-20 4768… First …
#>  8 hitopdat_A4.do… HiTOP-DAT  docx_… 1.0                2026-10-16 612a… First …
#>  9 hitopdat_qualt… HiTOP-DAT  qualt… 1.0                2026-10-16 20a4… First …
#> 10 hitopdat_redca… HiTOP-DAT  redcap 1.0                2026-10-16 6965… First …
#> # ℹ 54 more rows
```
