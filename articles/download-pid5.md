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
Paper)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_A4.docx)
[English (US
Paper)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-07-16](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

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

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                 | Format           | Instrument version | Build date |
|----------------------|------------------|--------------------|------------|
| `pid5_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-07-16 |
| `pid5_qualtrics.txt` | Qualtrics import | 1.0                | 2026-07-16 |
| `pid5_redcap.zip`    | REDCap import    | 1.0                | 2026-07-16 |
| `pid5_US.docx`       | DOCX (US paper)  | 1.0                | 2026-07-16 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-07-16

Versioning system introduced: files renamed from the \_1.0\_ filename
scheme and DOCX footers gain a build stamp.  
`pid5_A4.docx`, `pid5_qualtrics.txt`, `pid5_redcap.zip`, `pid5_US.docx`
