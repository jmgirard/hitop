# PID-5-BF Instrument

Welcome to the Personality Inventory for DSM-5 Brief Form (PID-5-BF)
instrument resources page. This questionnaire contains 25 items and can
be used to score 5 domain scales indexing different aspects of
personality pathology. Here you can download ready-to-use versions of
the instrument or explore the powerful tools provided by the `hitop` R
package to customize, score, and analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5bf_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5bf_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5bf_qualtrics.txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5bf_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5.html#details)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a complete toolkit for working with the
PID-5-SF. If you are comfortable using R, you can leverage these
functions to streamline your workflow.

##### 📋 Instrument Information

Access the complete item dictionary and structural hierarchy directly
from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Scales](https://jmgirard.github.io/hitop/reference/pid_scales.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5bf.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5bf.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5bf.md)

##### 🧮 Scoring & Reliability

Calculate subscales, handle reverse-coding, and generate reliability
metrics in one step, or access the underlying internal consistency
functions directly.

[Score
PID-5-BF](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Validity
Scales](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
[Alpha
Reliability](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
[Omega
Reliability](https://jmgirard.github.io/hitop/reference/calc_omega.md)

##### 📊 Example Datasets

Test your analysis pipelines using built-in datasets of simulated
respondent data.

[Simulated
Data](https://jmgirard.github.io/hitop/reference/sim_pid5bf.md)

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
| pid5bf_A4.docx | docx_a4 | 1.0 | 2026-07-16 | 417f07adf713e7267de1b705cb45a777 |
| pid5bf_qualtrics.txt | qualtrics | 1.0 | 2026-07-16 | 93fd646314e7283a9530ffd054389aa0 |
| pid5bf_redcap.zip | redcap | 1.0 | 2026-07-16 | 4c925abba617cc502279e58d3f287df9 |
| pid5bf_US.docx | docx_us | 1.0 | 2026-07-16 | 2e26e61eba9897656bb15abafd6bea67 |

Current builds {.table}

| Build date | File | Changes |
|:---|:---|:---|
| 2026-07-16 | pid5bf_A4.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5bf_qualtrics.txt | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5bf_redcap.zip | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |
| 2026-07-16 | pid5bf_US.docx | Versioning system introduced (D-016): renamed from the *1.0* filename scheme; DOCX footers gain a build stamp. |

Version history {.table}
