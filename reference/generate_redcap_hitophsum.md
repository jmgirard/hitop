# Generate a REDCap Instrument ZIP File for the HiTOP-HSUM

Generates a REDCap-compatible data dictionary for the Hierarchical
Taxonomy of Psychopathology - Substance Use Module (HiTOP-HSUM) and
packages it into an Instrument ZIP file for easy uploading.

## Usage

``` r
generate_redcap_hitophsum(
  file = "hitophsum_redcap.zip",
  form_name = "hitophsum_questionnaire",
  required = TRUE,
  other_drug_rule = c("most_frequent", "per_drug")
)
```

## Arguments

- file:

  Character string. The destination path for the output ZIP file.
  Defaults to `"hitophsum_redcap.zip"`.

- form_name:

  Character string. The internal name of the form in REDCap. Defaults to
  `"hitophsum_questionnaire"`.

- required:

  Logical. Whether the items should be marked as required. Defaults to
  `TRUE`.

- other_drug_rule:

  Character string. How symptom items are gated for the "other drug"
  substances (every substance except alcohol and nicotine). The default,
  `"most_frequent"`, follows the source module's looping rule: symptom
  items are shown only for the most frequently used other drug that is
  used at least monthly (drugs whose frequency is "Prefer not to say"
  are ignored; when two or more drugs tie for the highest frequency, the
  symptom items are shown for each of them). `"per_drug"` loosens the
  gate so that every other drug used at least monthly gets its own
  symptom items, regardless of relative frequency. Alcohol and nicotine
  symptom gating is unaffected by this argument.

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

## Examples

``` r
# Write a HiTOP-HSUM REDCap instrument ZIP to a temporary location
generate_redcap_hitophsum(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/Rtmpr1zGL9/file1adc475b8a1b.zip
```
