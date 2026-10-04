# PID-5 Informant Form Instrument

Welcome to the resources page for the Personality Inventory for DSM-5
Informant Form (PID-5-IRF; Markon et al., 2013). On this 218-item
questionnaire, an adult informant rates the person receiving care. It
scores the same 25 facets and 5 domains as the self-report PID-5, and
each item completes the stem “He or she…”. Here you can download
ready-to-use versions of the instrument or use the `hitop` R package to
customize, score, and analyze your data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5irf_A4.docx)
[English (US
Paper)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5irf_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5irf_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)2026-10-04](https://jmgirard.github.io/hitop/downloads/pid5irf_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a toolkit for working with the PID-5
Informant Form. If you are comfortable using R, you can use these
functions in your workflow.

##### 📋 Instrument Information

Access the item dictionary, with the informant wording in `TextIRF`, and
the facet keys directly from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Facets](https://jmgirard.github.io/hitop/reference/pid_scales.md)
[Domains](https://jmgirard.github.io/hitop/reference/pid_domains.md)

##### 🛠️ Custom File Generation

Need to change the paper size, font, title or page breaks? Build
customized DOCX, Qualtrics, and REDCap files programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5irf.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5irf.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5irf.md)

##### 🧮 Scoring & Reliability

Score the facets and domains with `version = "IRF"`, or compute alpha
and omega for each facet. The package has no informant norms, so do not
norm or plot informant scores as self-report scores.

[Score
PID-5-IRF](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Reliability](https://jmgirard.github.io/hitop/reference/reliability_pid5.md)

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                    | Format           | Instrument version | Build date |
|-------------------------|------------------|--------------------|------------|
| `pid5irf_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-10-04 |
| `pid5irf_qualtrics.txt` | Qualtrics import | 1.0                | 2026-10-04 |
| `pid5irf_redcap.zip`    | REDCap import    | 1.0                | 2026-10-04 |
| `pid5irf_US.docx`       | DOCX (US paper)  | 1.0                | 2026-10-04 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-10-04

First build of the PID-5 Informant Form: the 218 items in the form's
order with the informant wording, the form's instructions, its rating
prompt and stem restated at the head of every page, and its 0 to 3
response options; the Word footer carries the APA notice the form
prints.  
`pid5irf_A4.docx`, `pid5irf_qualtrics.txt`, `pid5irf_redcap.zip`,
`pid5irf_US.docx`
