# Generate a Word Document for the HiTOP-SR Assessment

Creates a formatted Microsoft Word document containing the Hierarchical
Taxonomy of Psychopathology - Self-Report (HiTOP-SR) items,
instructions, and optional scoring keys. The 405 items are formatted
into a single continuous table.

## Usage

``` r
generate_docx_hitopsr(
  file = "hitopsr.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-SR (v1.0)",
  include_scoring = TRUE,
  include_subscales = FALSE,
  font_size = 10,
  font_family = "Times New Roman"
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitopsr.docx"`.

- papersize:

  Character string specifying the paper dimensions. Must be one of
  `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.

- title:

  Character string for the document header title. Defaults to
  `"HiTOP-SR (v1.0)"`.

- include_scoring:

  Logical. If `TRUE` (default), appends a page break and the scoring
  instructions table.

- include_subscales:

  Logical. If `TRUE`, appends optional subscales to the scoring
  instructions table. Defaults to `FALSE`.

- font_size:

  Numeric value specifying the base font size in points. Defaults to
  `10`.

- font_family:

  Character string specifying the font family to be used. Defaults to
  `"Times New Roman"`.

## Value

Invisibly returns the path to the created file (`file`).

## Examples

``` r
# \donttest{
# Write a HiTOP-SR paper form to a temporary Word document
generate_docx_hitopsr(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpHXmrG7/file1a366d46bfdd.docx
# }
```
