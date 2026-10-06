# Generate a Qualtrics Import File for the PID-5 Child Form (Ages 11 to 17)

The 220 items of the PID-5 child form for ages 11 to 17, which are the
adult full form's items in the same order with the same keying, and the
child form's instructions and response options. The question IDs are the
adult form's, `PID5_001` to `PID5_220`, so the export scores with
`score_pid5(version = "FULL")` as an adult export does.

## Usage

``` r
generate_qualtrics_pid5child(
  file = "pid5child_qualtrics.txt",
  block_name = "PID-5 Child",
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

## References

Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., & Skodol, A.
E. (2013). *The Personality Inventory for DSM-5 (PID-5)—Child Age
11–17*. American Psychiatric Association.

## Examples

``` r
# Write a PID-5 child form Qualtrics import file to a temporary location
generate_qualtrics_pid5child(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/Rtmpy8EyDU/file1a4446fa874.txt
```
