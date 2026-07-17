# Generate a REDCap Instrument ZIP File for the HiTOP-BR

Generates a REDCap-compatible data dictionary for the Hierarchical
Taxonomy of Psychopathology - Brief Report (HiTOP-BR) and packages it
into an Instrument ZIP file for easy uploading.

## Usage

``` r
generate_redcap_hitopbr(
  file = "hitopbr_redcap.zip",
  form_name = "hitopbr_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitopbr_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitopbr_questionnaire"`.

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
# Write a HiTOP-BR REDCap instrument ZIP to a temporary location
generate_redcap_hitopbr(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpW0rjpC/file1ac222f24b7d.zip
```
