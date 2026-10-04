# Generate a Qualtrics Import File for the PID5BF+M

The 36 items of the modified brief form of the PID-5 (PID5BF+M), in BF+M
order with their PID-5 text, and the instructions and response options
of the other PID-5 forms. Score the export with
`score_pid5(version = "BFPM")`.

## Usage

``` r
generate_qualtrics_pid5bfpm(
  file = "pid5bfpm_qualtrics.txt",
  block_name = "PID5BF+M",
  id_prefix = "PID5BFPM",
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
# Write a PID5BF+M Qualtrics import file to a temporary location
generate_qualtrics_pid5bfpm(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/Rtmp8yjdwZ/file1afc73cf012.txt
```
