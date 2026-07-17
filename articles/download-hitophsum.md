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
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (QSF
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_qualtrics.qsf)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitophsum_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_hitophsum.html#details)

Scoring functions and example data coming soon.

### Versions

Artifact builds are identified by their build date. The instrument
itself is version 1.0; a new build date means the distributed file
changed (see the history below for what changed). To check which build
you downloaded, compare its MD5 checksum (e.g., with
[`tools::md5sum()`](https://rdrr.io/r/tools/md5sum.html) in R) against
the manifest below; the same information is available in the package as
`hitop_artifacts`.

| File | Format | Instrument version | Build date | MD5 checksum |
|:---|:---|:---|:---|:---|
| hitophsum_A4.docx | docx_a4 | 1.0 | 2026-07-16 | 621ea1441f28f804eb5479f2e4baa531 |
| hitophsum_qualtrics.qsf | qualtrics | 1.0 | 2026-07-16 | fdd097d2bf5ed86a16a05a60a0d8c5e5 |
| hitophsum_redcap.zip | redcap | 1.0 | 2026-07-16 | e308b6c069c863b6536635a8a88a2cbe |
| hitophsum_US.docx | docx_us | 1.0 | 2026-07-16 | 116c3dada96106ac4a1104032b86fa8e |

Current builds {.table}

| Build date | File | Changes |
|:---|:---|:---|
| 2026-07-16 | hitophsum_A4.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitophsum_qualtrics.qsf | Rebuilt from the corrected item data via the Qualtrics API (M19): fixes duplicated questions and the empty cigar-quantity dropdown; SurveyName carries the build stamp. |
| 2026-07-16 | hitophsum_redcap.zip | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitophsum_US.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |

Version history {.table}
