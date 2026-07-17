# Generate a Word Document for the HiTOP-HSUM Assessment Overview

Creates a formatted Microsoft Word document containing the Hierarchical
Taxonomy of Psychopathology - Substance Use Module (HSUM) overview,
branching logic, and items.

## Usage

``` r
generate_docx_hitophsum(
  file = "hitophsum_overview.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-HSUM (v1.0) Overview",
  font_size = 10,
  font_family = "Times New Roman"
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitophsum_overview.docx"`.

- papersize:

  Character string specifying the paper dimensions. Must be one of
  `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.

- title:

  Character string for the document header title. Defaults to
  `"HiTOP-HSUM (v1.0) Overview"`.

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
# Write a HiTOP-HSUM paper form to a temporary Word document
generate_docx_hitophsum(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpW0rjpC/file1ac232dd18f0.docx
# }
```
