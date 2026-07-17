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

## See also

Step-by-step import instructions for Qualtrics and REDCap:
<https://jmgirard.github.io/hitop/articles/import-instructions.html>

## Examples

``` r
# Write a HiTOP-HSUM REDCap instrument ZIP to a temporary location
generate_redcap_hitophsum(file = tempfile(fileext = ".zip"))
#> ✔ Instrument successfully zipped to /tmp/RtmpkPXG4U/file1af0677dd288.zip
```
