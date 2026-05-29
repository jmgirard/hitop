# Generate a REDCap Instrument ZIP File for the HiTOP-HSUM

Generates a REDCap-compatible data dictionary for the Hierarchical
Taxonomy of Psychopathology - Substance Use Module (HiTOP-HSUM) and
packages it into an Instrument ZIP file for easy uploading.

## Usage

``` r
generate_redcap_hitophsum(
  file = "hitophsum_redcap.zip",
  form_name = "hitophsum_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitophsum_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitophsum_questionnaire"`.

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- breaks:

  Integer or `NULL`. The number of items to display before inserting a
  page break. Set to `0` or `NULL` to disable pagination entirely.
  Defaults to `15`.

## Value

Invisibly returns the path to the created file (`file`).
