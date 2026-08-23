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
  breaks = 15,
  module = NULL,
  subset = NULL
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

- module:

  An optional
  [`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
  object restricting the instrument to the items of the chosen scales,
  keeping their original HiTOP-SR item numbers. This is deliberately
  unlike
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
  whose module forms are numbered `1` to `n`: here an item number names
  a collected data column, so renumbering would rename variables in
  dictionaries already in the field. (default = `NULL`)

- subset:

  Deprecated. The former name of `module`; supplying it warns. Supplying
  both `module` and `subset` is an error. (default = `NULL`)

## Value

Invisibly returns the path to the created file (`file`).

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a HiTOP-SR REDCap instrument ZIP to a temporary location
generate_redcap_hitopsr(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/Rtmp7qn1I8/file1a5f5928eead.zip

# A two-scale module, original numbering preserved (unlike the Word form)
generate_redcap_hitopsr(
  file = tempfile(fileext = ".zip"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
)
#> ✔ Instrument successfully zipped to /tmp/Rtmp7qn1I8/file1a5f65b72c4c.zip
```
