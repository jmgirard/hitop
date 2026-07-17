# HiTOP-HSUM Instrument

Welcome to the HiTOP Harmful Substance Use Module (HiTOP-HSUM)
instrument resources page. This questionnaire contains a total of 650
possible items across 3 assessment stages. Complex skip/display logic is
used, such that the vast majority of respondents will only see a subset
of possible items. Here you can download a DOCX summary file that can be
printed and shared with IRB as well as ready-to-use imports for both
Qualtrics and REDCap (both of which already implement the complex
skip/display logic mentioned above). Scoring functions are still under
development but should be available soon.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for an overview or sending to the
IRB.

[English (A4
Paper)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_A4.docx)
[English (US
Paper)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_US.docx)

##### 📊 Qualtrics Import

Use this survey file to easily import the instrument directly into your
Qualtrics surveys.

[English (QSF
File)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_qualtrics.qsf)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_hitophsum.html#details)

Scoring functions and example data coming soon.

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                      | Format           | Instrument version | Build date |
|---------------------------|------------------|--------------------|------------|
| `hitophsum_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-07-16 |
| `hitophsum_qualtrics.qsf` | Qualtrics import | 1.0                | 2026-07-16 |
| `hitophsum_redcap.zip`    | REDCap import    | 1.0                | 2026-07-16 |
| `hitophsum_US.docx`       | DOCX (US paper)  | 1.0                | 2026-07-16 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-07-16

Versioning system introduced: files renamed from the \_1.0\_ filename
scheme and DOCX footers gain a build stamp.  
`hitophsum_A4.docx`, `hitophsum_redcap.zip`, `hitophsum_US.docx`

2026-07-16

Rebuilt from the corrected item data via the Qualtrics API: fixes
duplicated questions and the empty cigar-quantity dropdown; SurveyName
carries the build stamp.  
`hitophsum_qualtrics.qsf`
