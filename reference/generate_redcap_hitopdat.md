# Generate a REDCap Instrument ZIP File for the HiTOP-DAT

Generates a REDCap data dictionary for the HiTOP Digital Assessment and
Tracker (HiTOP-DAT) battery and packages it into an Instrument ZIP file
for uploading. The 382 item fields are `hdat_001` to `hdat_382` in
battery order (`hitopdat_items$Item`), each a radio field whose choices
are the values and labels of the item's own answer set
(`hitopdat_choices`). Each of the seven measures opens with a
descriptive field holding its instruction text, except the CAPE, which
has none. The field is named `hdat_<measure>_instructions`, where
`<measure>` is the measure's name in lower case with each run of other
characters made `_` (`hdat_whodas_instructions`,
`hdat_idas_ii_instructions`). Each measure after the first starts a new
page. A page break inside a measure restates the measure's instruction
text in the section header of the page's first item.

## Usage

``` r
generate_redcap_hitopdat(
  file = "hitopdat_redcap.zip",
  form_name = "hitopdat_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitopdat_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitopdat_questionnaire"`.

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- breaks:

  Integer or `NULL`. The number of items of a measure to display before
  inserting a page break, counted from the start of each measure. Set to
  `0` or `NULL` to break pages only between measures. Defaults to `15`.

## Value

Invisibly returns the path to the created file (`file`).

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a HiTOP-DAT REDCap instrument ZIP to a temporary location
generate_redcap_hitopdat(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpTOhkvh/file1a8b1282ec7e.zip
```
