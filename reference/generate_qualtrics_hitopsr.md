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

- subset:

  An optional
  [`hitop_subset()`](https://jmgirard.github.io/hitop/reference/hitop_subset.md)
  object restricting the file to the items of the chosen scales, keeping
  their original HiTOP-SR item numbers. (default = `NULL`)

## Value

Invisibly returns the path to the created file (`file`).

## Examples

``` r
# Write a HiTOP-SR Qualtrics import file to a temporary location
generate_qualtrics_hitopsr(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmpiTXAFK/file1bd15f6a1456.txt

# A two-scale subset, original numbering preserved
generate_qualtrics_hitopsr(
  file = tempfile(fileext = ".txt"),
  subset = hitop_subset("hitopsr", c("Agoraphobia", "Appetite Loss"))
)
#> ✔ Qualtrics import file successfully created at /tmp/RtmpiTXAFK/file1bd11c3713a7.txt
```
