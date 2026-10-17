# Generate a Qualtrics Import File for the HiTOP-DAT

Creates a text file formatted for the Qualtrics Advanced Format import
tool containing the 382 items of the HiTOP Digital Assessment and
Tracker (HiTOP-DAT) battery. The item questions are `hdat_001` to
`hdat_382` in battery order (`hitopdat_items$Item`), each a
single-answer multiple-choice question whose choices are the values and
labels of the item's own answer set (`hitopdat_choices`). Each answer
carries its scoring value from `hitopdat_choices$Value` as its Qualtrics
recode value, written in the file as the answer's `[[Choice:<value>]]`
tag. With `include_instructions = TRUE`, each measure that has
instruction text opens with a descriptive text question holding it: six
of the seven measures, all but the CAPE. That question's ID is
`<id_prefix>_<measure>_instructions`, where `<measure>` is the measure's
name in lower case with each run of other characters made `_`
(`hdat_whodas_instructions`, `hdat_idas_ii_instructions`). Each measure
after the first starts a new page.

## Usage

``` r
generate_qualtrics_hitopdat(
  file = "hitopdat_qualtrics.txt",
  block_name = "HiTOP-DAT",
  id_prefix = "hdat",
  include_instructions = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitopdat_qualtrics.txt"`.

- block_name:

  Character string specifying the name of the block in Qualtrics.
  Defaults to `"HiTOP-DAT"`.

- id_prefix:

  Character string specifying the prefix for the question IDs. Defaults
  to `"hdat"`.

- include_instructions:

  Logical. If `TRUE` (default), includes each measure's instruction text
  as a descriptive text question.

- breaks:

  Integer or `NULL`. The number of items of a measure to display before
  inserting a page break, counted from the start of each measure. Set to
  `0` or `NULL` to break pages only between measures. Defaults to `15`.

## Value

Invisibly returns the path to the created file (`file`).

## See also

[`generate_docx_hitopdat()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopdat.md)
and
[`generate_redcap_hitopdat()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopdat.md)
for the Word forms and the REDCap import file. Step-by-step import
instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a HiTOP-DAT Qualtrics import file to a temporary location
generate_qualtrics_hitopdat(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmpIYUd8a/file19bc7a18ad59.txt
```
