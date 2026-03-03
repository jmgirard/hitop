# Generate a Word Document for the HiTOP-BR Assessment

Creates a formatted Microsoft Word document containing the Hierarchical
Taxonomy of Psychopathology - Brief Report (HiTOP-BR) items,
instructions, and optional scoring keys.

## Usage

``` r
generate_docx_hitopbr(
  file = "hitopbr_1.0.docx",
  papersize = c("letter", "a4"),
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
  `"letter"` or `"a4"`. Defaults to `"letter"`.

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
