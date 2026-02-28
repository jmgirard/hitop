# Generate a Qualtrics Advanced Format Import File

Creates a text file formatted for the Qualtrics Advanced Format import
tool. It takes a data frame of survey items and a list of instructions,
combining them into a structured text file with specific block and
question tags. This function is designed to work seamlessly with the
package's built-in datasets.

## Usage

``` r
generate_qualtrics_txt(items, instructions, file_path = "qualtrics_import.txt")
```

## Arguments

- items:

  A data frame containing the survey items. The first column must
  contain the item identifiers (e.g., item numbers), and a column named
  `Text` must contain the question text.

- instructions:

  A list containing the survey instructions and response options. It
  must include a `start` character string for the initial descriptive
  block, and an `options` data frame containing `value` and `label`
  columns for the multiple-choice options.

- file_path:

  A character string specifying the output file path. Defaults to
  `"qualtrics_import.txt"`.

## Value

Invisibly returns `NULL`. The function creates a text file at the
specified `file_path` and prints a success message to the console.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate a Qualtrics text file using the package's built-in datasets
generate_qualtrics_txt(
  items = hitopbr_items,
  instructions = hitopbr_instructions,
  file_path = "hitopbr_survey.txt"
)
} # }
```
