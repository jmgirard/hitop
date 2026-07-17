# Generate a REDCap Instrument ZIP File for the PID-5-SF

Generate a REDCap Instrument ZIP File for the PID-5-SF

## Usage

``` r
generate_redcap_pid5sf(
  file = "pid5sf_redcap.zip",
  form_name = "pid5sf_questionnaire",
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

## Examples

``` r
# Write a PID-5-SF REDCap instrument ZIP to a temporary location
generate_redcap_pid5sf(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpwNgZqD/file1b771764c28b.zip
```
