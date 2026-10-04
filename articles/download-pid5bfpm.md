# PID5BF+M Instrument

Welcome to the resources page for the modified brief form of the
Personality Inventory for DSM-5 (PID5BF+M; Bach et al., 2020). This
questionnaire contains 36 items. It scores 18 facets of 2 items each and
6 domains, each the average of 3 facet scores. The items take their
wording from the PID-5, and the forms use the PID-5 instructions and
response options. Here you can download ready-to-use versions of the
instrument or use the `hitop` R package to customize, score, and analyze
your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)2026-10-03](https://jmgirard.github.io/hitop/downloads/pid5bfpm_A4.docx)
[English (US
Paper)2026-10-03](https://jmgirard.github.io/hitop/downloads/pid5bfpm_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)2026-10-03](https://jmgirard.github.io/hitop/downloads/pid5bfpm_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-10-03](https://jmgirard.github.io/hitop/downloads/pid5bfpm_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a toolkit for working with the PID5BF+M. If
you are comfortable using R, you can use these functions in your
workflow.

##### 📋 Instrument Information

Access the item dictionary, the facet keys and the facet-to-domain map
directly from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Facets](https://jmgirard.github.io/hitop/reference/pid_scales.md)
[Domains](https://jmgirard.github.io/hitop/reference/pid_bfpm_domains.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5bfpm.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5bfpm.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5bfpm.md)

##### 🧮 Scoring & Reliability

Score the facets and domains with `version = "BFPM"`, or compute alpha
and omega for each facet and domain. Omega is `NA` for the 2-item
facets.

[Score
PID5BF+M](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Reliability](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                     | Format           | Instrument version | Build date |
|--------------------------|------------------|--------------------|------------|
| `pid5bfpm_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-10-03 |
| `pid5bfpm_qualtrics.txt` | Qualtrics import | 1.0                | 2026-10-03 |
| `pid5bfpm_redcap.zip`    | REDCap import    | 1.0                | 2026-10-03 |
| `pid5bfpm_US.docx`       | DOCX (US paper)  | 1.0                | 2026-10-03 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-10-03

First build of the PID5BF+M forms: the 36 items in BF+M order with their
PID-5 text, and the instructions and response options of the other PID-5
forms.  
`pid5bfpm_A4.docx`, `pid5bfpm_qualtrics.txt`, `pid5bfpm_redcap.zip`,
`pid5bfpm_US.docx`
