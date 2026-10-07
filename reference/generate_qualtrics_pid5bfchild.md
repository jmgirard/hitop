# Generate a Qualtrics Import File for the PID-5-BF Child Form (Ages 11 to 17)

The 25 items of the PID-5-BF child form for ages 11 to 17, which are the
adult brief form's items in the same order with the same keying, and the
child form's instructions and response options. The question IDs are the
adult form's, `PID5BF_01` to `PID5BF_25`, so the export scores with
`score_pid5(version = "BF")` as an adult export does.

## Usage

``` r
generate_qualtrics_pid5bfchild(
  file = "pid5bfchild_qualtrics.txt",
  block_name = "PID-5-BF Child",
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

## References

Krueger, R. F., Derringer, J., Markon, K. E., Watson, D., & Skodol, A.
E. (2013). *The Personality Inventory for DSM-5—Brief Form
(PID-5-BF)—Child Age 11–17*. American Psychiatric Association.

## Examples

``` r
# Write a PID-5-BF child form Qualtrics import file to a temporary location
generate_qualtrics_pid5bfchild(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/Rtmpmkz7jC/file1a6d48ffdb7.txt
```
