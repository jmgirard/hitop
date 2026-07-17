# Generate a REDCap Instrument ZIP File for the HiTOP-SR

Generates a REDCap-compatible data dictionary for the Hierarchical
Taxonomy of Psychopathology - Self-Report (HiTOP-SR) and packages it
into an Instrument ZIP file for easy uploading.

## Usage

``` r
generate_redcap_hitopsr(
  file = "hitopsr_redcap.zip",
  form_name = "hitopsr_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitopsr_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitopsr_questionnaire"`.

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- breaks:

  Integer or `NULL`. The number of items to display before inserting a
  page break. Set to `0` or `NULL` to disable pagination entirely.
  Defaults to `15`.

## Value

Invisibly returns the path to the created file (`file`).

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a HiTOP-SR REDCap instrument ZIP to a temporary location
generate_redcap_hitopsr(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpfeymYQ/file1a1828cdfb30.zip
```
