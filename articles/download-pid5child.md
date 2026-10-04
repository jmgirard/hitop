# PID-5 Child Form Instrument

Welcome to the resources page for the Personality Inventory for DSM-5
child form for ages 11 to 17 (Krueger et al., 2013). The child completes
this 220-item questionnaire about themselves. Its items, their order and
their scoring key are those of the adult PID-5. Its instructions put
“right” and “wrong” in quotation marks, and the Word forms here carry
the APA notice that the child form prints. The APA form also labels its
instructions “Instructions to the child receiving care”, which these
forms leave out, as the adult forms leave out their label. Here you can
download ready-to-use versions of the instrument or use the `hitop` R
package to customize, score, and analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5child_A4.docx)
[English (US
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5child_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5child_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5child_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a toolkit for working with the PID-5 child
form. If you are comfortable using R, you can use these functions in
your workflow.

##### 📋 Instrument Information

Access the item dictionary and the facet keys directly from the package
namespace. The child form uses the adult full form's items and keys.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Facets](https://jmgirard.github.io/hitop/reference/pid_scales.md)
[Domains](https://jmgirard.github.io/hitop/reference/pid_domains.md)

##### 🛠️ Custom File Generation

Need to change the paper size, font, title or page breaks? Build
customized DOCX, Qualtrics, and REDCap files programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5child.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5child.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5child.md)

##### 🧮 Scoring & Reliability

Score the facets and domains with `version = "FULL"`, as for the adult
form, or compute alpha and omega for each facet. The package has no
child norms, so do not norm child scores against the adult tables.

[Score PID-5](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Reliability](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                      | Format           | Instrument version | Build date |
|---------------------------|------------------|--------------------|------------|
| `pid5child_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-10-04 |
| `pid5child_qualtrics.txt` | Qualtrics import | 1.0                | 2026-10-04 |
| `pid5child_redcap.zip`    | REDCap import    | 1.0                | 2026-10-04 |
| `pid5child_US.docx`       | DOCX (US paper)  | 1.0                | 2026-10-04 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-10-04

First build of the PID-5 and PID-5-BF child forms for ages 11 to 17: the
adult forms' items in the same order, the child forms' instructions and
0 to 3 response options; the Word footer carries the APA notice the
forms print.  
`pid5child_A4.docx`, `pid5child_qualtrics.txt`, `pid5child_redcap.zip`,
`pid5child_US.docx`
