# Generate a REDCap Instrument ZIP File for the PID-5 Child Form (Ages 11 to 17)

The 220 items of the PID-5 child form for ages 11 to 17, which are the
adult full form's items in the same order with the same keying, and the
child form's instructions and response options. The item fields are the
adult form's, `pid5_001` to `pid5_220`, the names
[`rename_pid5_items()`](https://jmgirard.github.io/hitop/reference/rename_pid5_items.md)
and
[`label_pid5()`](https://jmgirard.github.io/hitop/reference/label_pid5.md)
use with `version = "FULL"`. Score them with
`score_pid5(version = "FULL")`. Because the field names are the adult
form's, one REDCap project cannot hold this instrument and the one
[`generate_redcap_pid5()`](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5.md)
writes.

## Usage

``` r
generate_redcap_pid5child(
  file = "pid5child_redcap.zip",
  form_name = "pid5child_questionnaire",
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

## References

Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., & Skodol, A.
E. (2013). *The Personality Inventory for DSM-5 (PID-5)—Child Age
11–17*. American Psychiatric Association.

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a PID-5 child form REDCap instrument ZIP to a temporary location
generate_redcap_pid5child(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpLegEec/file1a375beaff25.zip
```
