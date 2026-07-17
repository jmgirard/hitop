# Generate a REDCap Instrument ZIP File for the PID-5-BF

Generate a REDCap Instrument ZIP File for the PID-5-BF

## Usage

``` r
generate_redcap_pid5bf(
  file = "pid5bf_redcap.zip",
  form_name = "pid5bf_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.

- form_name:

  Character string. The internal name of the form in REDCap.

- required:

  Logical. Whether the items should be marked as required.

- breaks:

  Integer or `NULL`. The number of items to display before a page break.

## Examples

``` r
# Write a PID-5-BF REDCap instrument ZIP to a temporary location
generate_redcap_pid5bf(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpwNgZqD/file1b7731049c23.zip
```
