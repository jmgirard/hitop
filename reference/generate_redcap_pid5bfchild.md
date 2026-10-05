# Generate a REDCap Instrument ZIP File for the PID-5-BF Child Form (Ages 11 to 17)

The 25 items of the PID-5-BF child form for ages 11 to 17, which are the
adult brief form's items in the same order with the same keying, and the
child form's instructions and response options. The item fields are the
adult form's, `pid5bf_01` to `pid5bf_25`, the names
[`rename_pid5_items()`](https://jmgirard.github.io/hitop/reference/rename_pid5_items.md)
and
[`label_pid5()`](https://jmgirard.github.io/hitop/reference/label_pid5.md)
use with `version = "BF"`. Score them with `score_pid5(version = "BF")`.
Because the field names are the adult form's, one REDCap project cannot
hold this instrument and the one
[`generate_redcap_pid5bf()`](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5bf.md)
writes.

## Usage

``` r
generate_redcap_pid5bfchild(
  file = "pid5bfchild_redcap.zip",
  form_name = "pid5bfchild_questionnaire",
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
E. (2013). *The Personality Inventory for DSM-5—Brief Form
(PID-5-BF)—Child Age 11–17*. American Psychiatric Association.

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a PID-5-BF child form REDCap instrument ZIP to a temporary location
generate_redcap_pid5bfchild(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/Rtmpk03S8b/file1aaa5beef084.zip
```
