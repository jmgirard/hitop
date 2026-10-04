# PID-5-BF Child Form Instrument

Welcome to the resources page for the Personality Inventory for DSM-5
Brief Form child form for ages 11 to 17 (PID-5-BF; Krueger et al.,
2013). The child completes this 25-item questionnaire about themselves.
Its items, their order, its instructions and its scoring key are those
of the adult PID-5-BF, and the Word forms here carry the APA notice that
the child form prints. Here you can download ready-to-use versions of
the instrument or use the `hitop` R package to customize, score, and
analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5bfchild_A4.docx)
[English (US
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5bfchild_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5bfchild_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5bfchild_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a toolkit for working with the PID-5-BF
child form. If you are comfortable using R, you can use these functions
in your workflow.

##### 📋 Instrument Information

Access the item dictionary and the domain keys directly from the package
namespace. The child form uses the adult brief form's items and keys.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Domains](https://jmgirard.github.io/hitop/reference/pid_scales.md)

##### 🛠️ Custom File Generation

Need to change the paper size, font, title or page breaks? Build
customized DOCX, Qualtrics, and REDCap files programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5bfchild.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5bfchild.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5bfchild.md)

##### 🧮 Scoring & Reliability

Score the domains and total with `version = "BF"`, as for the adult
form, or compute alpha and omega for each domain. The package has no
child norms, so do not norm child scores against the adult tables.

[Score
PID-5-BF](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Reliability](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                        | Format           | Instrument version | Build date |
|-----------------------------|------------------|--------------------|------------|
| `pid5bfchild_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-10-04 |
| `pid5bfchild_qualtrics.txt` | Qualtrics import | 1.0                | 2026-10-04 |
| `pid5bfchild_redcap.zip`    | REDCap import    | 1.0                | 2026-10-04 |
| `pid5bfchild_US.docx`       | DOCX (US paper)  | 1.0                | 2026-10-04 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-10-04

First build of the PID-5 and PID-5-BF child forms for ages 11 to 17: the
adult forms' items in the same order, the child forms' instructions and
0 to 3 response options; the Word footer carries the APA notice the
forms print.  
`pid5bfchild_A4.docx`, `pid5bfchild_qualtrics.txt`,
`pid5bfchild_redcap.zip`, `pid5bfchild_US.docx`
