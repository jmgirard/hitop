# Generate a Word Document for the HiTOP-BR Assessment

Creates a formatted Microsoft Word document containing the Hierarchical
Taxonomy of Psychopathology - Brief Report (HiTOP-BR) items,
instructions, and optional scoring keys.

## Usage

``` r
generate_docx_hitopbr(
  file = "hitopbr_1.0.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-BR (v1.0)",
  include_scoring = TRUE,
  font_size = 10,
  font_family = "Times New Roman"
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitopbr_1.0.docx"`.

- papersize:

  Character string specifying the paper dimensions. Must be one of
  `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.

- title:

  Character string for the document header title. Defaults to
  `"HiTOP-BR (v1.0)"`.

- include_scoring:

  Logical. If `TRUE` (default), appends a page break and the scoring
  instructions table.

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
# Write a HiTOP-BR paper form to a temporary Word document
generate_docx_hitopbr(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/Rtmpm0sy0f/file1ad813d89e08.docx
# }
```
