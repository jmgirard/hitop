# HiTOP-BR Instrument

Welcome to the HiTOP Brief Report (HiTOP-BR) instrument resources page.
This questionnaire contains 45 items and can be used to score 8 scales
indexing different aspects of psychopathology. Here you can download
ready-to-use versions of the instrument or explore the powerful tools
provided by the `hitop` R package to customize, score, and analyze your
data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopbr_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopbr_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopbr_qualtrics.txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopbr_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopbr.html#details)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a complete toolkit for working with the
HiTOP-BR. If you are comfortable using R, you can leverage these
functions to streamline your workflow.

##### 📋 Instrument Information

Access the complete item dictionary and structural hierarchy directly
from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/hitopbr_items.md)
[Scales](https://jmgirard.github.io/hitop/reference/hitopbr_scales.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_hitopbr.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopbr.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopbr.md)

##### 🧮 Scoring & Reliability

Calculate subscales, handle reverse-coding, and generate reliability
metrics in one step, or access the underlying internal consistency
functions directly.

[Score
HiTOP-BR](https://jmgirard.github.io/hitop/reference/score_hitopbr.md)
[Alpha
Reliability](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
[Omega
Reliability](https://jmgirard.github.io/hitop/reference/calc_omega.md)

##### 📊 Example Datasets

Test your analysis pipelines using built-in datasets, including a real
validation sample and simulated respondent data.

[Real Data](https://jmgirard.github.io/hitop/reference/ku_hitopbr.md)
[Simulated
Data](https://jmgirard.github.io/hitop/reference/sim_hitopbr.md)

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
| hitopbr_A4.docx | docx_a4 | 1.0 | 2026-07-16 | f62953d9a31409cbd64cbc7dc1f3fb95 |
| hitopbr_qualtrics.txt | qualtrics | 1.0 | 2026-07-16 | d3cc31cc56b2c8b48aef560d279a4381 |
| hitopbr_redcap.zip | redcap | 1.0 | 2026-07-16 | 93a4498c31520b67792a392cf8567ea7 |
| hitopbr_US.docx | docx_us | 1.0 | 2026-07-16 | 0e16e855d2a127603ac6d8915e77ba63 |

Current builds {.table}

| Build date | File | Changes |
|:---|:---|:---|
| 2026-07-16 | hitopbr_A4.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopbr_qualtrics.txt | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopbr_redcap.zip | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopbr_US.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |

Version history {.table}
