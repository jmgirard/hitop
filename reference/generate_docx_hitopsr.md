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
  title = NULL,
  include_scoring = TRUE,
  include_subscales = FALSE,
  font_size = 10,
  font_family = "Times New Roman",
  module = NULL,
  renumber = TRUE,
  randomize = FALSE,
  descriptor = NULL,
  subset = NULL
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

  Character string for the document header title, printed verbatim. The
  default (`NULL`) resolves by what the form contains:
  `"HiTOP-SR Module (v1.0)"` when `module` is supplied and
  `"HiTOP-SR (v1.0)"` otherwise, so a form built from a few scales is
  not headed as the full 405-item instrument. (default = `NULL`)

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

- module:

  An optional
  [`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
  object restricting the form to the items of the chosen scales. Cannot
  be combined with `include_subscales = TRUE`. (default = `NULL`)

- renumber:

  Logical. If `TRUE` (default), the printed items are numbered `1` to
  `n` down the page, so a module form does not show the full
  instrument's gapped numbers. Set to `FALSE` to print each item's
  original HiTOP-SR number instead. The scoring page always uses
  whichever numbers are printed. This differs from
  [`generate_qualtrics_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md)
  and
  [`generate_redcap_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md),
  which never renumber, because there an item number names a collected
  data column.

- randomize:

  Logical. If `TRUE`, the items are printed in a random order. On a
  renumbered module form the document also carries a crosswalk from each
  printed number back to its original HiTOP-SR number, so the form is
  scoreable from the paper alone; that crosswalk is printed whether or
  not `include_scoring` appends the key. It is *not* printed when
  `module` is `NULL` (405 pairs would be one dense paragraph) or when
  `renumber = FALSE` (the printed numbers are already the original ones)
  — in both cases read the order from the `item_order` attribute
  described under Value. There is no `seed` argument: call
  [`set.seed()`](https://rdrr.io/r/base/Random.html) before this
  function to make an order reproducible. (default = `FALSE`)

  **Scoring data collected on a shuffled form.**
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  addresses a module's items by their position in `module$items`, which
  is ascending original order — not the order a shuffled form prints
  them in. Reorder the collected columns through `item_order` before
  scoring: `collected[order(attr(out, "item_order"))]`. Scoring
  printed-order columns directly returns wrong scale scores and raises
  no error — or pass `descriptor` and let the saved file carry the order
  for you. The recipe assumes `collected` is in the order the form
  printed, its first column holding the answer to the paper's item 1;
  columns already in instrument order need no reordering, and applying
  it to them scrambles what was right.

- descriptor:

  An optional path to write a module descriptor to, beside the Word
  file. The saved file records which scales the form covers and which
  instrument items they draw on, so
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  hands the module straight back to
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  at scoring time. A call passing no `module` writes a descriptor naming
  every scale, describing the full administration. With
  `randomize = TRUE` it also records the printed order, returned on the
  read module's `item_order` attribute — the record a shuffled
  whole-instrument form otherwise leaves nowhere, since no crosswalk is
  printed for one. Written before the Word file, so an unwritable path
  is reported before any form is produced; if the Word file then cannot
  be written, the descriptor is removed again, a file that was already
  at that path included. It must name a path of its own: an empty
  string, or the same path as `file`, is refused rather than leaving you
  with no descriptor and no error. Once both files are on disk the
  descriptor's path is announced on the console, after the message
  naming the Word file. (default = `NULL`)

- subset:

  Deprecated. The former name of `module`; supplying it warns. Supplying
  both `module` and `subset` is an error. (default = `NULL`)

## Value

Invisibly returns the path to the created file (`file`), carrying an
`item_order` attribute: the original HiTOP-SR item numbers in the order
they were printed. It is present on every call, and is simply ascending
unless `randomize = TRUE`.

## See also

[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
and
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
for the descriptor file.

## Examples

``` r
# \donttest{
# Write a HiTOP-SR paper form to a temporary Word document
generate_docx_hitopsr(file = tempfile(fileext = ".docx"))
#> ✔ Document successfully created at /tmp/RtmpATPdZI/file1d781aa6d801.docx

# A module containing only two scales, printed as items 1 to 8
generate_docx_hitopsr(
  file = tempfile(fileext = ".docx"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
)
#> ✔ Document successfully created at /tmp/RtmpATPdZI/file1d7854854d81.docx

# The same module keeping the full instrument's own item numbers
generate_docx_hitopsr(
  file = tempfile(fileext = ".docx"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
  renumber = FALSE
)
#> ✔ Document successfully created at /tmp/RtmpATPdZI/file1d787d09e70.docx

# A shuffled form; the scoring page carries the crosswalk back
set.seed(1)
out <- generate_docx_hitopsr(
  file = tempfile(fileext = ".docx"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
  randomize = TRUE
)
#> ✔ Document successfully created at /tmp/RtmpATPdZI/file1d784efd727e.docx
attr(out, "item_order")
#> [1]  66 144 389 109 260 118 291 202

# The same form with a descriptor saved beside it; the descriptor carries
# the printed order, so the collected columns can be put back in order
# without keeping a note by hand
f <- tempfile(fileext = ".json")
generate_docx_hitopsr(
  file = tempfile(fileext = ".docx"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss")),
  randomize = TRUE,
  descriptor = f
)
#> ✔ Document successfully created at /tmp/RtmpATPdZI/file1d786bbb6f19.docx
#> ✔ Module descriptor successfully written to /tmp/RtmpATPdZI/file1d7837322f8e.json
attr(read_module(f), "item_order")
#> [1] 109 118 291  66 202 144 389 260
# }
```
