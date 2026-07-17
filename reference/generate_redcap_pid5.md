# Generate a REDCap Instrument ZIP File for the PID-5 (Full)

Generate a REDCap Instrument ZIP File for the PID-5 (Full)

## Usage

``` r
generate_redcap_pid5(
  file = "pid5_redcap.zip",
  form_name = "pid5_questionnaire",
  required = TRUE,
  breaks = 15
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.

- form_name:

  Character string. The internal name of the form in REDCap.

- required:

  Logical. Whether the items should be marked as required.

- breaks:

  Integer or `NULL`. The number of items to display before a page break.

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

## Examples

``` r
# Write a PID-5 (full) REDCap instrument ZIP to a temporary location
generate_redcap_pid5(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpzJvTJf/file1abf2107b4a5.zip
```
