# Generate a Qualtrics Import File for the PID-5-BF

Generate a Qualtrics Import File for the PID-5-BF

## Usage

``` r
generate_qualtrics_pid5bf(
  file = "pid5bf_qualtrics.txt",
  block_name = "PID-5-BF",
  id_prefix = "PID5BF",
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
# Write a PID-5-BF Qualtrics import file to a temporary location
generate_qualtrics_pid5bf(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmpC27CBq/file1acf52bfd1a.txt
```
