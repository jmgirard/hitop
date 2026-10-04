# Generate a Word Document for the PID-5-BF Child Form (Ages 11 to 17)

Write the 25-item PID-5-BF child form for ages 11 to 17 (Krueger et al.,
2013) as a paper form. Its items, their order and their keying are those
of the adult brief form, so the items are `pid_items$Text` numbered 1 to
25 and the scoring page is the one
[`generate_docx_pid5bf()`](https://jmgirard.github.io/hitop/reference/generate_docx_pid5bf.md)
prints. The instructions are the child form's, and the footer carries
the APA copyright and permission notice the child form prints. Score the
responses with `score_pid5(version = "BF")`.

## Usage

``` r
generate_docx_pid5bfchild(
  file = "pid5bfchild.docx",
  papersize = c("us", "a4"),
  title = "PID-5-BF, Child Age 11–17",
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

Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., & Skodol, A.
E. (2013). *The Personality Inventory for DSM-5—Brief Form
(PID-5-BF)—Child Age 11–17*. American Psychiatric Association.

## Examples

``` r
# \donttest{
# Write a PID-5-BF child form to a temporary Word document
generate_docx_pid5bfchild(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/Rtmp8yjdwZ/file1afc788774d4.docx
# }
```
