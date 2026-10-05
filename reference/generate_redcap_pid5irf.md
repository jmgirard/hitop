# Generate a REDCap Instrument ZIP File for the PID-5 Informant Form

The 218 items of the PID-5 Informant Form (PID-5-IRF), in the form's
order with the informant wording (`pid_items$TextIRF`). The instructions
field ends with the form's rating prompt and the stem "He or she…" that
each item completes, and the response options are the form's 0 to 3
labels. With `breaks`, the section header that starts each later page
restates the prompt and stem. The item fields are `pid5irf_001` to
`pid5irf_218`, the names
[`rename_pid5_items()`](https://jmgirard.github.io/hitop/reference/rename_pid5_items.md)
and
[`label_pid5()`](https://jmgirard.github.io/hitop/reference/label_pid5.md)
use with `version = "IRF"`. Score them with
`score_pid5(version = "IRF")`.

## Usage

``` r
generate_redcap_pid5irf(
  file = "pid5irf_redcap.zip",
  form_name = "pid5irf_questionnaire",
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
# Write a PID-5 Informant Form REDCap instrument ZIP to a temporary location
generate_redcap_pid5irf(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpogTRNl/file1a4c423c6be8.zip
```
