# PID-5-SF Instrument

Welcome to the Personality Inventory for DSM-5 Short Form (PID-5-SF)
instrument resources page. This questionnaire contains 100 items and can
be used to score 5 domains scales and 25 facet subscales indexing
different aspects of personality pathology. Here you can download
ready-to-use versions of the instrument or explore the powerful tools
provided by the `hitop` R package to customize, score, and analyze your
data.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5sf_1.0_A4.docx)
[English (US
Paper)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5sf_1.0_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to easily import the instrument
directly into your Qualtrics surveys.

[English (TXT
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5sf_1.0_qualtrics.txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the instrument as a new
instrument in your REDCap project.

[English (ZIP
File)](https://github.com/jmgirard/hitop/raw/main/inst/extdata/pid5sf_1.0_redcap.zip)
[Import
Instructions](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5.html#details)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package provides a complete toolkit for working with the
PID-5-SF. If you are comfortable using R, you can leverage these
functions to streamline your workflow.

##### 📋 Instrument Information

Access the complete item dictionary, structural hierarchy, and subscale
definitions directly from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/pid_items.md)
[Scales](https://jmgirard.github.io/hitop/reference/pid_scales.md)

##### 🛠️ Custom File Generation

Need to change the formatting, adapt the instructions, or translate the
text? Build customized DOCX, Qualtrics, and REDCap files
programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_pid5sf.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_pid5sf.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_pid5sf.md)

##### 🧮 Scoring & Reliability

Calculate subscales, handle reverse-coding, and generate reliability
metrics in one step, or access the underlying internal consistency
functions directly.

[Score
PID-5-SF](https://jmgirard.github.io/hitop/reference/score_pid5.md)
[Validity
Scales](https://jmgirard.github.io/hitop/reference/validity_pid5.md)
[Alpha
Reliability](https://jmgirard.github.io/hitop/reference/calc_alpha.md)
[Omega
Reliability](https://jmgirard.github.io/hitop/reference/calc_omega.md)

##### 📊 Example Datasets

Test your analysis pipelines using built-in datasets, including a real
validation sample and simulated respondent data.

[Real Data](https://jmgirard.github.io/hitop/reference/ku_pid5sf.md)
[Simulated
Data](https://jmgirard.github.io/hitop/reference/sim_pid5sf.md)
