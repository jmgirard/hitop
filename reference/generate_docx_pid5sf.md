# Generate a Word Document for the PID-5-SF

Generate a Word Document for the PID-5-SF

## Usage

``` r
generate_docx_pid5sf(
  file = "pid5sf.docx",
  papersize = c("us", "a4"),
  title = "PID-5-SF",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
)
```

## Arguments

- file:

  Character string specifying the output file path.

- papersize:

  Character string specifying the paper dimensions. Must be one of
  `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.

- title:

  Character string for the document header title.

- include_scoring:

  Logical. If `TRUE`, appends a page break and scoring instructions.

- font_size:

  Numeric value specifying the base font size in points.

- font_family:

  Character string specifying the font family to be used.

## Examples

``` r
# \donttest{
# Write a PID-5-SF paper form to a temporary Word document
generate_docx_pid5sf(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpHXmrG7/file1a3667cae27f.docx
# }
```
