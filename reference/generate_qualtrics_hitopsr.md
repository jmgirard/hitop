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
  breaks = 15
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

## Value

Invisibly returns the path to the created file (`file`).
