# PID-5 Instrument

Welcome to the Personality Inventory for DSM-5 (PID-5) instrument
resources page. This questionnaire contains 220 items and can be used to
score 5 domain scales and 25 facet subscales indexing different aspects
of personality pathology. Here you can download ready-to-use versions of
the instrument or explore the powerful tools provided by the `hitop` R
package to customize, score, and analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_qualtrics.txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5.html#details)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a complete toolkit for working with the
PID-5. If you are comfortable using R, you can leverage these functions
to streamline your workflow.

##### 📋 Instrument Information

Access the complete item dictionary and structural hierarchy directly
from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Scales](https://jmgirard.github.io/hitop/reference/pid_scales.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5.md)

##### 🧮 Scoring & Reliability

Calculate subscales, handle reverse-coding, and generate reliability
metrics in one step, or access the underlying internal consistency
functions directly.

[Score PID-5](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Validity
Scales](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
[Alpha
Reliability](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
[Omega
Reliability](https://jmgirard.github.io/hitop/reference/calc_omega.md)

##### 📊 Example Datasets

Test your analysis pipelines using built-in datasets of simulated
respondent data.

[Simulated Data](https://jmgirard.github.io/hitop/reference/sim_pid5.md)

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
| pid5_A4.docx | docx_a4 | 1.0 | 2026-07-16 | 42011a74067ce078457c650988853a5f |
| pid5_qualtrics.txt | qualtrics | 1.0 | 2026-07-16 | f11cd79e83b30d1f3de8da2cfb703b71 |
| pid5_redcap.zip | redcap | 1.0 | 2026-07-16 | b55dc10562e88b39c1c96cdcecc98dc1 |
| pid5_US.docx | docx_us | 1.0 | 2026-07-16 | b7d23dd52928604efe9408e8bd93bf35 |

Current builds {.table}

| Build date | File | Changes |
|:---|:---|:---|
| 2026-07-16 | pid5_A4.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5_qualtrics.txt | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5_redcap.zip | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5_US.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |

Version history {.table}
