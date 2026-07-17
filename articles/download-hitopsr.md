# HiTOP-SR Instrument

Welcome to the HiTOP Self-Report (HiTOP-SR) instrument resources page.
This questionnaire contains 405 items and can be used to score 76 scales
and 17 subscales indexing different aspects of psychopathology. Here you
can download ready-to-use versions of the instrument or explore the
powerful tools provided by the `hitop` R package to customize, score,
and analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopsr_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopsr_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopsr_qualtrics.txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/hitopsr_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.html#details)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a complete toolkit for working with the
HiTOP-SR. If you are comfortable using R, you can leverage these
functions to streamline your workflow.

##### 📋 Instrument Information

Access the complete item dictionary, structural hierarchy, and subscale
definitions directly from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/hitopsr_items.md)
[Scales](https://jmgirard.github.io/hitop/reference/hitopsr_scales.md)
[Subscales](https://jmgirard.github.io/hitop/reference/hitopsr_subscales.md)
[Definitions](https://jmgirard.github.io/hitop/articles/scales-hitopsr.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_hitopsr.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopsr.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopsr.md)

##### 🧮 Scoring & Reliability

Calculate subscales, handle reverse-coding, and generate reliability
metrics in one step, or access the underlying internal consistency
functions directly.

[Score
HiTOP-SR](https://jmgirard.github.io/hitop/reference/score_hitopsr.md)
[Alpha
Reliability](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
[Omega
Reliability](https://jmgirard.github.io/hitop/reference/calc_omega.md)

##### 📊 Example Datasets

Test your analysis pipelines using built-in datasets, including a real
validation sample and simulated respondent data.

[Real Data](https://jmgirard.github.io/hitop/reference/ku_hitopsr.md)
[Simulated
Data](https://jmgirard.github.io/hitop/reference/sim_hitopsr.md)

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
| hitopsr_A4.docx | docx_a4 | 1.0 | 2026-07-16 | aec89eaf15d7db00a8a9749ac045c679 |
| hitopsr_qualtrics.txt | qualtrics | 1.0 | 2026-07-16 | 151f508795f5208d0e54334468850177 |
| hitopsr_redcap.zip | redcap | 1.0 | 2026-07-16 | 090a3ee66bcdb13bb5becfcf91cce3ad |
| hitopsr_US.docx | docx_us | 1.0 | 2026-07-16 | d2275234acf0f6cd09a3e866a0044002 |

Current builds {.table}

| Build date | File | Changes |
|:---|:---|:---|
| 2026-07-16 | hitopsr_A4.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopsr_qualtrics.txt | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopsr_redcap.zip | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | hitopsr_US.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |

Version history {.table}
