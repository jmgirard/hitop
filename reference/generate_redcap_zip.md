# Generate a REDCap Instrument ZIP File

Generates a REDCap-compatible data dictionary from item and instruction
data frames and packages it into an Instrument ZIP file for easy
uploading.

## Usage

``` r
generate_redcap_zip(
  items,
  instructions,
  form_name = NULL,
  required = TRUE,
  breaks = 15,
  file_path = NULL
)
```

## Arguments

- items:

  A data frame containing the questionnaire items. The first column
  should contain the item IDs, and a `Text` column should contain the
  item text.

- instructions:

  A list containing the instructions and response options. Expects
  `options$value` and `options$label` for choices, and `start[1]` for
  the main instruction text.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  the lowercase instrument name appended with "\_questionnaire".

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- breaks:

  Integer or `NULL`. The number of items to display before inserting a
  page break. Set to `0` or `NULL` to disable pagination entirely.
  Defaults to `15`.

- file_path:

  Character string. The destination path for the output ZIP file.
  Defaults to the current working directory with a filename derived from
  the instrument.

## Value

Invisibly returns a data frame containing the compiled REDCap data
dictionary.

## Importing (Uploading) Instruments into a REDCap Project

1.  Log in to your REDCap account and navigate to the desired project.

2.  Click on the "Designer" link in the left menu bar under "Project
    Home and Design".

3.  In the main page, under "Data Collection Instruments", look for the
    "Upload instrument ZIP" option and click the "Upload" button.

4.  Click "Choose File", navigate to where you have the measure saved as
    a ZIP folder, and select the ZIP folder containing your instrument.

5.  Click "Upload instrument ZIP" button to begin the import process.

6.  Find the imported instrument in your list of measures and review for
    accuracy.

7.  Test the instrument to ensure proper functionality within your
    project.

## Examples

``` r
if (FALSE) { # \dontrun{
generate_redcap_zip(
  items = hitopbr_items,
  instructions = hitopbr_instructions,
  breaks = 15,
  file_path = "hitopbr_redcap.zip"
)
} # }
```
