# Generate a Qualtrics Import File for the HiTOP-SR

Creates a text file formatted for the Qualtrics Advanced Format import
tool containing the Hierarchical Taxonomy of Psychopathology -
Self-Report (HiTOP-SR) items and instructions.

## Usage

``` r
generate_qualtrics_hitopsr(
  file = "hitopsr_qualtrics.txt",
  block_name = "HiTOP-SR",
  id_prefix = "HSR",
  include_instructions = TRUE,
  breaks = 15,
  module = NULL,
  descriptor = NULL,
  subset = NULL
)
```

## Arguments

- file:

  Character string specifying the output file path. Defaults to
  `"hitopsr_qualtrics.txt"`.

- block_name:

  Character string specifying the name of the block in Qualtrics.
  Defaults to `"HiTOP-SR"`.

- id_prefix:

  Character string specifying the prefix for the question IDs. Defaults
  to `"HSR"`.

- include_instructions:

  Logical. If `TRUE` (default), includes the starting instructions as a
  descriptive text block.

- breaks:

  Integer or `NULL`. The number of items to display before inserting a
  page break. Set to `0` or `NULL` to disable pagination. Defaults to
  `15`.

- module:

  An optional
  [`hitop_module()`](https://jmgirard.github.io/hitop/reference/hitop_module.md)
  object restricting the file to the items of the chosen scales, keeping
  their original HiTOP-SR item numbers. This is deliberately unlike
  [`generate_docx_hitopsr()`](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md),
  whose module forms are numbered `1` to `n`: here an item number names
  a collected data column, so renumbering would rename variables in
  dictionaries already in the field. The zero-padding is the full
  instrument's for the same reason: item 4 is `HSR_004` in a module file
  as in the complete one, never `HSR_04`. (default = `NULL`)

- descriptor:

  An optional path to write a module descriptor to, beside the
  instrument file. The saved file records which scales the form covers
  and which instrument items they draw on, so
  [`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
  hands the module straight back to
  [`score_hitopsr()`](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
  at scoring time. A call passing no `module` writes a descriptor naming
  every scale, describing the full administration. Written before the
  instrument file, so an unwritable path is reported before any form is
  produced; if the instrument file then cannot be written, the
  descriptor is removed again, a file that was already at that path
  included. It must name a path of its own: an empty string, or the same
  path as `file`, is refused rather than leaving you with no descriptor
  and no error. Once both files are on disk the descriptor's path is
  announced on the console, after the message naming the import file.
  (default = `NULL`)

- subset:

  Deprecated. The former name of `module`; supplying it warns. Supplying
  both `module` and `subset` is an error. (default = `NULL`)

## Value

Invisibly returns the path to the created file (`file`).

## See also

[`write_module()`](https://jmgirard.github.io/hitop/reference/write_module.md)
and
[`read_module()`](https://jmgirard.github.io/hitop/reference/read_module.md)
for the descriptor file.

## Examples

``` r
# Write a HiTOP-SR Qualtrics import file to a temporary location
generate_qualtrics_hitopsr(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmpvEbjkY/file1db05cc89120.txt

# A two-scale module, original numbering preserved (unlike the Word form)
generate_qualtrics_hitopsr(
  file = tempfile(fileext = ".txt"),
  module = hitop_module("hitopsr", c("Agoraphobia", "Appetite Loss"))
)
#> ✔ Qualtrics import file successfully created at /tmp/RtmpvEbjkY/file1db058819370.txt
```
