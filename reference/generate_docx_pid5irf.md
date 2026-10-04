# Generate a Word Document for the PID-5 Informant Form

Write the 218-item PID-5 Informant Form (PID-5-IRF; Markon et al.,
2013), on which an adult informant rates the person receiving care, as a
paper form. The items are numbered 1 to 218 in the form's order, with
the informant wording (`pid_items$TextIRF`). The form's opening
instructions come first; its rating prompt and the stem "He or she…"
that each item completes head the item table on every page, as on the
printed form. The scoring page lists each of the 25 facets with its
informant item numbers, marking the 14 reverse-scored items with (R), as
[`score_pid5()`](https://jmgirard.github.io/hitop/reference/score_pid5.md)
scores them with `version = "IRF"`. The footer carries the APA copyright
and permission notice the form prints.

## Usage

``` r
generate_docx_pid5irf(
  file = "pid5irf.docx",
  papersize = c("us", "a4"),
  title = "PID-5-IRF (Informant Form)",
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

Markon, K. E., Quilty, L. C., Bagby, R. M., & Krueger, R. F. (2013).
*The Personality Inventory for DSM-5—Informant Form (PID-5-IRF)—Adult*.
American Psychiatric Association. See also Markon et al. (2013),
*Assessment, 20*(3), 370-383.
[doi:10.1177/1073191113486513](https://doi.org/10.1177/1073191113486513)

## Examples

``` r
# \donttest{
# Write a PID-5 Informant Form paper form to a temporary Word document
generate_docx_pid5irf(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpcbGw35/file1aef158048ee.docx
# }
```
