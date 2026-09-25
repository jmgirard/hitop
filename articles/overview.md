# HiTOP Instruments

The HiTOP Society maintains three self-report measures of
psychopathology at different lengths and levels of coverage. Choose the
one that fits your study below, then head to its page to download
ready-to-use files or explore the `hitop` R package tools for scoring
and customization.

##### 📋 HiTOP Self-Report (SR)

405 items · 76 scales · 17 subscales

The full-length measure, offering the most detailed coverage of the
HiTOP hierarchy from broad spectra down to narrow subscales.

[View & download
→](https://jmgirard.github.io/hitop/articles/download-hitopsr.md)

##### 📄 HiTOP Brief Report (BR)

45 items · 8 scales

A short screening measure spanning the HiTOP hierarchy — six spectra
plus the Externalizing superspectrum and a general p-factor — ideal when
respondent time is limited.

[View & download
→](https://jmgirard.github.io/hitop/articles/download-hitopbr.md)

##### 🧪 HiTOP Harmful Substance Use (HSUM)

Up to 650 items · 3 assessment stages

A dedicated substance-use module with skip/display logic, so most
respondents answer only a small subset of items.

[View & download
→](https://jmgirard.github.io/hitop/articles/download-hitophsum.md)

### From instrument to scores

1.  **Choose an instrument** from the cards above. Each instrument's
    page holds its downloads and links to the package's tools for it.
2.  **Choose a route** on that page: a printable Word form, a [Qualtrics
    or REDCap
    import](https://jmgirard.github.io/hitop/articles/import-instructions.md),
    or, on every page but the HiTOP-HSUM's, an [online
    form](https://jmgirard.github.io/hitop/articles/online-collection.md)
    opened from a study link.
3.  **Collect responses**. Paper and survey-platform responses arrive as
    your own data; an online form [sends each participant's
    responses](https://jmgirard.github.io/hitop/articles/online-collection.html#make-the-study-link)
    to the store named in the study link, or saves them as a file.
4.  **Read and score** the data in R:
    [`read_form_responses()`](https://jmgirard.github.io/hitop/reference/read_form_responses.md)
    reads an online form's file or table, and the
    [HiTOP-SR](https://jmgirard.github.io/hitop/articles/hitopsr_scoring.md),
    [HiTOP-BR](https://jmgirard.github.io/hitop/articles/hitopbr_scoring.md)
    and
    [PID-5](https://jmgirard.github.io/hitop/articles/pid5_scoring.md)
    tutorials show the scoring functions.
