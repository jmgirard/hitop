# Generate a REDCap Instrument ZIP File for the PID5BF+M

The 36 items of the modified brief form of the PID-5 (PID5BF+M), in BF+M
order with their PID-5 text, and the instructions and response options
of the other PID-5 forms. The item fields are `pid5bfpm_01` to
`pid5bfpm_36`, the names
[`rename_pid5_items()`](https://jmgirard.github.io/hitop/reference/rename_pid5_items.md)
and
[`label_pid5()`](https://jmgirard.github.io/hitop/reference/label_pid5.md)
use with `version = "BFPM"`. Score them with
`score_pid5(version = "BFPM")`.

## Usage

``` r
generate_redcap_pid5bfpm(
  file = "pid5bfpm_redcap.zip",
  form_name = "pid5bfpm_questionnaire",
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
# Write a PID5BF+M REDCap instrument ZIP to a temporary location
generate_redcap_pid5bfpm(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmppWeB9M/file1b254bc62398.zip
```
