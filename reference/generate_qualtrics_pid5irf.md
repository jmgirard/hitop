# Generate a Qualtrics Import File for the PID-5 Informant Form

The 218 items of the PID-5 Informant Form (PID-5-IRF), in the form's
order with the informant wording (`pid_items$TextIRF`). The instructions
block ends with the form's rating prompt and the stem "He or she…" that
each item completes, and the response options are the form's 0 to 3
labels. With `breaks`, a descriptive block restating the prompt and stem
opens every page after the first. The question IDs are `PID5IRF_001` to
`PID5IRF_218`. Score the export with `score_pid5(version = "IRF")`.

## Usage

``` r
generate_qualtrics_pid5irf(
  file = "pid5irf_qualtrics.txt",
  block_name = "PID-5-IRF",
  id_prefix = "PID5IRF",
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
# Write a PID-5 Informant Form Qualtrics import file to a temporary location
generate_qualtrics_pid5irf(file = tempfile(fileext = ".txt"))
#> ✔ Qualtrics import file successfully created at /tmp/RtmpcbGw35/file1aef33ee8c4c.txt
```
