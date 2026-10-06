# Generate a Word Document for the PID5BF+M

Write the 36-item modified brief form of the PID-5 (PID5BF+M; Bach et
al., 2020) as a paper form. The items are numbered 1 to 36 in BF+M
order, with their PID-5 text, and the instructions and response options
are those of the other PID-5 forms. The scoring page lists the 2 items
of each of the 18 facets, then the 3 facets each of the 6 domains
averages, as
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
scores them with `version = "BFPM"`.

## Usage

``` r
generate_docx_pid5bfpm(
  file = "pid5bfpm.docx",
  papersize = c("us", "a4"),
  title = "PID5BF+M",
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

## References

Bach, B., Kerber, A., Aluja, A., Bastiaens, T., Keeley, J. W., Claes,
L., Fossati, A., Gutierrez, F., Oliveira, S. E. S., Pires, R., Riegel,
K. D., Rolland, J.-P., Roskam, I., Sellbom, M., Somma, A., Spanemberg,
L., Strus, W., Thimm, J. C., Wright, A. G. C., & Zimmermann, J. (2020).
International assessment of DSM-5 and ICD-11 personality disorder
traits: Toward a common nosology in DSM-5.1. *Psychopathology, 53*(3-4),
179-188. [doi:10.1159/000507589](https://doi.org/10.1159/000507589)

## Examples

``` r
# \donttest{
# Write a PID5BF+M paper form to a temporary Word document
generate_docx_pid5bfpm(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/Rtmp7vmOY9/file1b10139dfac4.docx
# }
```
