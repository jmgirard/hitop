# Generate a Qualtrics Import File for the PID-5 (Full)

Generate a Qualtrics Import File for the PID-5 (Full)

## Usage

``` r
generate_qualtrics_pid5(
  file = "pid5_qualtrics.txt",
  block_name = "PID-5",
  id_prefix = "PID5",
  include_instructions = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string specifying the output file path.

- block_name:

  Character string specifying the name of the block in Qualtrics.

- id_prefix:

  Character string specifying the prefix for the question IDs.

- include_instructions:

  Logical. If `TRUE`, includes instructions block.

- breaks:

  Integer or `NULL`. The number of items to display before a page break.

## Examples

``` r
# Write a PID-5 (full) Qualtrics import file to a temporary location
generate_qualtrics_pid5(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmplRrr0M/file1ac062a4b612.txt
```
