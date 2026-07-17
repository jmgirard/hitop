# Generate a REDCap Instrument ZIP File for the PID-5 (Full)

Generate a REDCap Instrument ZIP File for the PID-5 (Full)

## Usage

``` r
generate_redcap_pid5(
  file = "pid5_redcap.zip",
  form_name = "pid5_questionnaire",
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

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a PID-5 (full) REDCap instrument ZIP to a temporary location
generate_redcap_pid5(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpkPXG4U/file1af09a92fe6.zip
```
