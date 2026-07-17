# Distribution Artifact Manifest

Version manifest for the prebuilt instrument artifacts distributed in
`inst/extdata/` and on the package website's download pages. Each build
of an artifact adds a row (the full history is kept), so the latest row
per `file` describes the currently distributed file. Artifact revisions
are identified by build date; the instrument version (e.g., `"1.0"`) is
the version of the instrument itself and changes only when its publisher
revises it. To check which build you have, compare your downloaded
file's MD5 checksum (e.g.,
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

  Artifact file name in `inst/extdata/`

- instrument:

  Instrument the artifact administers

- format:

  Artifact format: `"docx_us"`, `"docx_a4"`, `"qualtrics"`, or
  `"redcap"`

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
#> # A tibble: 24 × 7
#>    file            instrument format instrument_version build_date md5   changes
#>    <chr>           <chr>      <chr>  <chr>              <date>     <chr> <chr>  
#>  1 hitopbr_A4.docx HiTOP-BR   docx_… 1.0                2026-07-16 f629… Versio…
#>  2 hitopbr_qualtr… HiTOP-BR   qualt… 1.0                2026-07-16 d3cc… Versio…
#>  3 hitopbr_redcap… HiTOP-BR   redcap 1.0                2026-07-16 93a4… Versio…
#>  4 hitopbr_US.docx HiTOP-BR   docx_… 1.0                2026-07-16 0e16… Versio…
#>  5 hitophsum_A4.d… HiTOP-HSUM docx_… 1.0                2026-07-16 621e… Versio…
#>  6 hitophsum_qual… HiTOP-HSUM qualt… 1.0                2026-07-16 fdd0… Rebuil…
#>  7 hitophsum_redc… HiTOP-HSUM redcap 1.0                2026-07-16 e308… Versio…
#>  8 hitophsum_US.d… HiTOP-HSUM docx_… 1.0                2026-07-16 116c… Versio…
#>  9 hitopsr_A4.docx HiTOP-SR   docx_… 1.0                2026-07-16 aec8… Versio…
#> 10 hitopsr_qualtr… HiTOP-SR   qualt… 1.0                2026-07-16 151f… Versio…
#> # ℹ 14 more rows
```
