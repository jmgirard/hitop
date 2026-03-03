# Generate a REDCap Instrument ZIP File for the HiTOP-SR

Generates a REDCap-compatible data dictionary for the Hierarchical
Taxonomy of Psychopathology - Self-Report (HiTOP-SR) and packages it
into an Instrument ZIP file for easy uploading.

## Usage

``` r
generate_redcap_hitopsr(
  file = "hitopsr_redcap.zip",
  form_name = "hitopsr_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitopsr_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitopsr_questionnaire"`.

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- breaks:

  Integer or `NULL`. The number of items to display before inserting a
  page break. Set to `0` or `NULL` to disable pagination entirely.
  Defaults to `15`.

## Value

Invisibly returns the path to the created file (`file`).

## Details

**Importing (Uploading) Instruments into a REDCap Project:**

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
