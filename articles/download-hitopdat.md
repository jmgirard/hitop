# HiTOP-DAT Instrument

Welcome to the HiTOP Digital Assessment and Tracker (HiTOP-DAT)
resources page. The HiTOP-DAT is a battery of 382 items from seven
published measures: the WHODAS, IDAS-II, AUDIT, DUDIT, CAPE, CAT-PD and
PHQ-15. Here you can download printable Word forms and Qualtrics and
REDCap import files. Each form gives each measure’s instructions, where
it has them, before its first item and shows each item with its own
response options. The `hitop` package does not score the HiTOP-DAT yet.

### Ready-to-Use Downloads

Choose the format that best fits your immediate research needs.

##### 📄 Printable Document

Use these Microsoft Word documents for printing, paper administration,
or sending to the IRB.

[English (A4
Paper)2026-10-16](https://jmgirard.github.io/hitop/downloads/hitopdat_A4.docx)
[English (US
Paper)2026-10-16](https://jmgirard.github.io/hitop/downloads/hitopdat_US.docx)

##### 📊 Qualtrics Import

Use this specially formatted text file to import the battery directly
into your Qualtrics surveys.

[English (TXT
File)2026-10-16](https://jmgirard.github.io/hitop/downloads/hitopdat_qualtrics.txt)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#qualtrics-txt)

##### 🏥 REDCap Import

Use this compressed archive file to import the battery as a new
instrument in your REDCap project.

[English (ZIP
File)2026-10-16](https://jmgirard.github.io/hitop/downloads/hitopdat_redcap.zip)

[Import
Instructions](https://jmgirard.github.io/hitop/articles/import-instructions.html#redcap-zip)

------------------------------------------------------------------------

### Explore the R Package Features

The `hitop` package holds the battery's items, response options and
scales, and can build the printable, Qualtrics and REDCap files from
scratch, so you can adapt them before fielding the battery.

##### 📋 Instrument Information

Access the item list, the response options of each item, and the scale
memberships directly from the package namespace.

[Items](https://jmgirard.github.io/hitop/reference/hitopdat_items.md)
[Response
Options](https://jmgirard.github.io/hitop/reference/hitopdat_choices.md)
[Scales](https://jmgirard.github.io/hitop/reference/hitopdat_scales.md)

##### 🛠️ Custom File Generation

Need to change the formatting or the page breaks? Build customized DOCX,
Qualtrics and REDCap files programmatically.

[Printable](https://jmgirard.github.io/hitop/reference/generate_docx_hitopdat.md)
[Qualtrics](https://jmgirard.github.io/hitop/reference/generate_qualtrics_hitopdat.md)
[REDCap](https://jmgirard.github.io/hitop/reference/generate_redcap_hitopdat.md)

### Versions

Every download button above shows its file’s build date; a new build
date means the distributed file changed. The instrument itself is
version 1.0.

Current builds & version history

#### Current builds

| File                     | Format           | Instrument version | Build date |
|--------------------------|------------------|--------------------|------------|
| `hitopdat_A4.docx`       | DOCX (A4 paper)  | 1.0                | 2026-10-16 |
| `hitopdat_qualtrics.txt` | Qualtrics import | 1.0                | 2026-10-16 |
| `hitopdat_redcap.zip`    | REDCap import    | 1.0                | 2026-10-16 |
| `hitopdat_US.docx`       | DOCX (US paper)  | 1.0                | 2026-10-16 |

If your downloaded file shows an older build date, simply re-download it
to get the latest build. The full build manifest (including file
checksums) ships in the package as `hitop_artifacts`.

#### Version history

2026-10-16

First build of the HiTOP-DAT Word forms and REDCap dictionary: the 382
battery items in battery order, each measure's instruction before its
first item, and each item's own answer set; the Word footer names each
measure's source.  
`hitopdat_A4.docx`, `hitopdat_redcap.zip`, `hitopdat_US.docx`

2026-10-16

First build of the HiTOP-DAT Qualtrics import file: the 382 battery
items in battery order, each measure's instruction, where it has one,
before its first item, and each item's own answer set with its scoring
values in the \[\[Choice:\<value\>\]\] tags.  
`hitopdat_qualtrics.txt`
