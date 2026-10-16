# Generate a Word Document for the HiTOP-DAT

Write the 382 items of the HiTOP Digital Assessment and Tracker
(HiTOP-DAT) battery as a paper form. The items are numbered 1 to 382 in
battery order (`hitopdat_items$Item`), through its seven measures: the
WHODAS, IDAS-II, AUDIT, DUDIT, CAPE, CAT-PD and PHQ-15. Each measure's
instruction text comes before its first item; the CAPE has none. Each
item shows the values of its own answer set (`hitopdat_choices`), and
the legend above each run of items that share an answer set gives that
set's values and labels. The footer credits each measure's authors. The
form has no scoring page, because the package ships no HiTOP-DAT scoring
key.

## Usage

``` r
generate_docx_hitopdat(
  file = "hitopdat.docx",
  papersize = c("us", "a4"),
  title = "HiTOP-DAT",
  font_size = 10,
  font_family = "Times New Roman"
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitopdat.docx"`.

- papersize:

  Character string specifying the paper dimensions. Must be one of
  `"us"` (8.5x11 inches) or `"a4"` (210x297 mm). Defaults to `"us"`.

- title:

  Character string for the document header title. Defaults to
  `"HiTOP-DAT"`.

- font_size:

  Numeric value greater than 0 specifying the base font size in points.
  Defaults to `10`.

- font_family:

  Character string specifying the font family to be used. Defaults to
  `"Times New Roman"`.

## Value

Invisibly returns the path to the created file (`file`).

## See also

[hitopdat_items](https://jmgirard.github.io/hitop/reference/hitopdat_items.md),
[hitopdat_choices](https://jmgirard.github.io/hitop/reference/hitopdat_choices.md),
and
[`generate_redcap_hitopdat()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopdat.md)
for the REDCap import file.

## Examples

``` r
# \donttest{
# Write a HiTOP-DAT paper form to a temporary Word document
generate_docx_hitopdat(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpwfEheJ/file1a84f96f417.docx
# }
```
