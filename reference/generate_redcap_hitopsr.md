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
  descriptor = NULL,
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
  dictionaries already in the field. The zero-padding is the full
  instrument's for the same reason: item 4 is `hsr_004` in a module
  dictionary as in the complete one, never `hsr_04`. (default = `NULL`)

- descriptor:

  An optional path to write a module descriptor to, beside the
  instrument file. The saved file records which scales the form covers
  and which instrument items they draw on, so
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  hands the module straight back to
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  at scoring time. A call passing no `module` writes a descriptor naming
  every scale, describing the full administration. Written before the
  instrument file, so an unwritable path is reported before any form is
  produced; if the instrument file then cannot be written, the
  descriptor is removed again, a file that was already at that path
  included. It must name a path of its own: an empty string, or the same
  path as `file`, is refused rather than leaving you with no descriptor
  and no error. Once both files are on disk the descriptor's path is
  announced on the console, after the message naming the instrument ZIP.
  (default = `NULL`)

- subset:

  Deprecated. The former name of `module`; supplying it warns. Supplying
  both `module` and `subset` is an error. (default = `NULL`)

## Value

Invisibly returns the path to the created file (`file`).

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
and
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
for the descriptor file.

## Examples

``` r
# Write a HiTOP-SR REDCap instrument ZIP to a temporary location
generate_redcap_hitopsr(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpCwaZfM/file1d7f66b6d036.zip

# A two-scale module, original numbering preserved (unlike the Word form)
generate_redcap_hitopsr(
  file = tempfile(fileext = ".zip"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
)
#> ✔ Instrument successfully zipped to /tmp/RtmpCwaZfM/file1d7f5e137428.zip
```
